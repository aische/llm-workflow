module LLM.Workflow.Workflow where

import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM
  ( ChatResponse (..),
    GeneratableObject,
    GenerateError (..),
    GenerateErrorResult (..),
    GenerateResult,
    RuntimeArgs,
    StreamChunk (..),
    ToolCall (..),
    ToolResult (..),
    Turn (..),
    Usage (..),
    createGenRequest,
    emptyUsage,
    genObject,
    generateTextWithFallbacks,
    streamTextWithFallbacks,
  )
import LLM.Agent.ToolUtils (createToolContext, getResolvedTools)
import LLM.Agent.Types (Agent (..), RuntimeArgs (..), ToolMap)
import LLM.Workflow.BBEngine
  ( appendBodyOutput,
    catchLeafCell,
    completeLeafCell,
    dualWriteSlot,
    failLeafCell,
    initLoopSliceAt,
    mkPolicyView,
    nodeOutputFromFinal,
    pushComposite,
    pushLeaf,
    resolveChildPath,
    resolveSidePath,
    startLeafCell,
    topPath,
    updateRunningHistory,
  )
import LLM.Workflow.Blackboard
    ( buildLabelEnv,
      innermostLoopPathFromStack,
      syntheticLabel,
      initialRunnerState )
import LLM.Workflow.ToolUtils (executeTool, mkSomeSubmit)
import LLM.Workflow.Types
  ( AgentWithModels (agent, models),
    CID (..),
    CompositeKind (..),
    Path,
    PathFrame (FrameLeaf),
    Instance (..),
    Kont (..),
    Label (..),
    LoopKind (..),
    PathFrame (..),
    Pending (..),
    PolicySite (..),
    Prompt (..),
    Blackboard (..),
    LoopSlice (..),
    PromptArgs (..),
    RunnerState (..),
    Side (..),
    SlotKey (..),
    SomeSubmit (ssName, ssDecode),
    Stack (..),
    Step (..),
    ToolOutcome (ToolReply, ToolWorkflow, ToolYield),
    Trigger (..),
    Workflow (..),
  )
import LLM.Workflow.Utils
  ( CatchFrame (CatchFrame),
    ensureAgentTool,
    extendToolMap,
    lookupHistory,
    pendingToFinal,
    pendingToTurns,
    pendingToolRoundCount,
    respToAssistantTurn,
    runLoopDecPolicy,
    runLoopFeedPolicy,
    runMapPolicy,
    runMergePolicy,
    runSeqPolicy,
    showKont,
    showStep,
    stackSize,
    unwindPastTools,
    unwindToCatch,
    updateHistory,
  )
import Unsafe.Coerce (unsafeCoerce)

callLLM :: ToolMap ToolOutcome -> RuntimeArgs -> Pending -> IO (GenerateResult ChatResponse)
callLLM toolMap rt pending = do
  let messages = pendingToTurns pending
  generateTextWithFallbacks (createGenRequest ToolReply pending.prompt.agent.agent toolMap rt messages) pending.prompt.agent.models

streamLLM :: (StreamChunk -> IO ()) -> ToolMap ToolOutcome -> RuntimeArgs -> Pending -> IO (GenerateResult ChatResponse)
streamLLM onChunk toolMap rt pending = do
  let messages = pendingToTurns pending
      toolMap' = extendToolMap pending.submitTool toolMap
  streamTextWithFallbacks onChunk (createGenRequest ToolReply pending.prompt.agent.agent toolMap' rt messages) pending.prompt.agent.models

showStreamChunk :: StreamChunk -> Text
showStreamChunk = \case
  AnswerDelta txt -> txt
  ReasoningDelta txt -> txt
  PreambleDelta txt -> txt
  StreamToolCallChunk toolCall -> T.pack (show toolCall)

submitRequiredError :: GenerateError
submitRequiredError = unsafeCoerce ("Agent must call the submit tool to finish" :: Text)

startToolRound ::
  Pending ->
  Maybe CID ->
  Usage ->
  ChatResponse ->
  Kont o r ->
  (Usage, Step Text, Kont Text r)
startToolRound pending mcid uAcc resp konts =
  let (_text, assistantTurn, toolCalls) = respToAssistantTurn resp
      usage = fromMaybe emptyUsage resp.respUsage
      (toolCall, toolCalls') = case toolCalls of
        (tc : tcs) -> (tc, tcs)
        [] -> error "startToolRound: no tool calls"
   in ( uAcc <> usage,
        RunTool pending assistantTurn toolCall,
        KTool pending mcid assistantTurn toolCalls' [] toolCall (unsafeCoerce konts)
      )

callLLMO :: (GeneratableObject a) => ToolMap ToolOutcome -> RuntimeArgs -> Pending -> IO (GenerateResult (a, Usage))
callLLMO toolMap rt pending = do
  let messages = pendingToTurns pending
  r <- genObject (createGenRequest ToolReply pending.prompt.agent.agent toolMap rt messages) pending.prompt.agent.models
  case r of
    Left errResult -> pure $ Left errResult.gerError
    Right (value, usage) -> pure $ Right (value, usage)

cidToSlot :: CID -> SlotKey
cidToSlot = SlotByCid

lookupLoopSlice :: Path -> Blackboard -> Maybe LoopSlice
lookupLoopSlice path (Blackboard _ slices _ _ _) = Map.lookup path slices

leafSite :: RunnerState a -> (Path, Label)
leafSite rs = case rs.rsPathStack of
  FrameLeaf lbl path : _ -> (path, lbl)
  _ -> (resolveChildPath rs (syntheticLabel 0), syntheticLabel 0)

runWorkflow :: ToolMap ToolOutcome -> RuntimeArgs -> Workflow i o -> i -> IO (Either GenerateError o, Usage)
runWorkflow toolMap rt workflow i = do
  let labelEnv = case buildLabelEnv workflow of
        Right e -> e
        Left err -> error ("buildLabelEnv: " <> show err)
      bbInput = unsafeCoerce i :: PromptArgs
      runner =
        initialRunnerState
          bbInput
          (Stack emptyUsage (RunWorkflow workflow i) KEmpty)
          labelEnv
  loop toolMap rt runner

usageCents :: Usage -> Int
usageCents u = round (u.usageTotalCost * 100)

loop :: ToolMap ToolOutcome -> RuntimeArgs -> RunnerState (Stack (Either GenerateError o)) -> IO (Either GenerateError o, Usage)
loop toolMap rt rs = do
  rs' <- eval toolMap rt rs
  case isDone rs'.rsStack of
    (Just result, usage) -> pure (result, usage)
    (Nothing, _usage) -> loop toolMap rt rs'

isDone :: Stack (Either GenerateError o) -> (Maybe (Either GenerateError o), Usage)
isDone (Stack usage (RunFinish e) KEmpty) = (Just (unsafeCoerce e), usage)
isDone (Stack usage _ _) = (Nothing, usage)

withStack ::
  (Stack (Either GenerateError o) -> Stack (Either GenerateError o)) ->
  RunnerState (Stack (Either GenerateError o)) ->
  RunnerState (Stack (Either GenerateError o))
withStack f rs = rs {rsStack = f rs.rsStack}

eval :: ToolMap ToolOutcome -> RuntimeArgs -> RunnerState (Stack (Either GenerateError o)) -> IO (RunnerState (Stack (Either GenerateError o)))
eval toolMap rt rs@RunnerState {rsStack = Stack uAcc step konts} = do
  let space = T.replicate (stackSize konts) " "
      cents = "(¢ " <> T.pack (show (usageCents uAcc)) <> ")"
  _ <- TIO.putStrLn $ space <> cents <> " " <> showStep step <> T.unwords (map (" : " <>) (showKont konts))
  case step of
    RunObject pending -> do
      result <- callLLMO toolMap rt pending
      case result of
        Left err -> pure $ withStack (\_ -> Stack uAcc (RunThrow err) konts) rs
        Right (value, usage) -> pure $ withStack (\_ -> Stack (uAcc <> usage) (RunReturn value) konts) rs
    RunPrompt pending mcid -> do
      let agentName = pending.prompt.agent.agent.agName
      _ <- TIO.putStr $ space <> agentName <> ": "
      result <- streamLLM (TIO.putStr . showStreamChunk) toolMap rt pending
      case result of
        Left err -> pure $ withStack (\_ -> Stack uAcc (RunThrow err) konts) rs
        Right resp -> do
          let (text, assistantTurn, toolCalls) = respToAssistantTurn resp
          case toolCalls of
            [] ->
              case pending.submitTool of
                Just _ ->
                  pure $
                    withStack (\_ -> Stack (uAcc <> fromMaybe emptyUsage resp.respUsage) (RunThrow submitRequiredError) konts) rs
                Nothing ->
                  let h = pendingToTurns pending ++ [assistantTurn]
                      usage = fromMaybe emptyUsage resp.respUsage
                      rs' = case rs.rsCurrentLeaf of
                        Nothing -> rs
                        Just (path, iters) ->
                          completeLeafCell path iters (pendingToFinal pending text assistantTurn) usage h rs
                   in pure $
                        withStack
                          ( \_ ->
                              Stack
                                (uAcc <> usage)
                                (RunReturn $ pendingToFinal pending text assistantTurn)
                                (maybe konts (\cid -> KUpdateHistory cid h konts) mcid)
                          )
                          rs'
            _ ->
              if pendingToolRoundCount pending >= pending.prompt.agent.agent.agMaxToolRounds
                then pure $ withStack (\_ -> Stack uAcc (RunThrow GErrToolExceeded) konts) rs
                else
                  let (uAcc', step', konts') = startToolRound pending mcid uAcc resp konts
                      rs' = case rs.rsCurrentLeaf of
                        Nothing -> rs
                        Just (path, iters) ->
                          updateRunningHistory path iters (pendingToTurns pending) rs
                   in pure $ withStack (\_ -> Stack uAcc' step' konts') rs'
    RunTool pending _assistantTurn toolCall -> do
      let ctx = createToolContext pending.prompt.agent.agent pending.prompt.history emptyUsage rt
          toolMap' = extendToolMap pending.submitTool toolMap
          tools = getResolvedTools ToolReply pending.prompt.agent.agent toolMap' rt
      result <- executeTool rt.rtHooks ctx tools toolCall
      case result of
        ToolWorkflow workflow args -> do
          pure $ withStack (\_ -> Stack uAcc (RunWorkflow workflow args) konts) rs
        ToolReply text -> do
          pure $ withStack (\_ -> Stack uAcc (RunReturn text) konts) rs
        ToolYield val ->
          case pending.submitTool of
            Just submit | toolCall.tcName == submit.ssName ->
              case submit.ssDecode val of
                Right decoded ->
                  pure $ withStack (\_ -> Stack uAcc (RunReturn (unsafeCoerce decoded)) (unwindPastTools konts)) rs
                Left err ->
                  pure $ withStack (\_ -> Stack uAcc (RunReturn ("Submit decode error: " <> err)) konts) rs
            _ ->
              pure $ withStack (\_ -> Stack uAcc (RunReturn "Unexpected ToolYield") konts) rs
    RunWorkflow workflow i -> case workflow of
      WLabel _ wf ->
        pure $ withStack (\_ -> Stack uAcc (RunWorkflow wf i) konts) rs
      WPrompt a mbcid -> do
        let (leafPath, leafLbl) = leafSite rs
            i' = unsafeCoerce i :: PromptArgs
            h = maybe i'.history (lookupHistory konts) mbcid
            pending = Pending {prompt = Prompt {agent = a, prompt = i'.prompt, history = h}, toolRounds = [], submitTool = Nothing}
            rs' =
              startLeafCell leafPath rs.rsInstIters (Just a.agent.agName) $
                if case rs.rsPathStack of FrameLeaf {} : _ -> True; _ -> False
                  then rs
                  else pushLeaf leafLbl leafPath rs
         in pure $ withStack (\_ -> Stack uAcc (RunPrompt pending mbcid) konts) rs'
      WObject a -> do
        let (leafPath, leafLbl) = leafSite rs
            i' = unsafeCoerce i :: PromptArgs
            pending = Pending {prompt = Prompt {agent = a, prompt = i'.prompt, history = i'.history}, toolRounds = [], submitTool = Nothing}
            rs' =
              if case rs.rsPathStack of FrameLeaf {} : _ -> True; _ -> False
                then rs
                else pushLeaf leafLbl leafPath rs
         in pure $ withStack (\_ -> Stack uAcc (RunObject pending) konts) rs'
      WAgentSubmit @o name agentWithModels mbcid -> do
        let (leafPath, leafLbl) = leafSite rs
            i' = unsafeCoerce i :: PromptArgs
            h = maybe i'.history (lookupHistory konts) mbcid
            agent' = ensureAgentTool name agentWithModels
            submit = mkSomeSubmit (Proxy @o) name "Submit the final structured result."
            pending =
              Pending
                { prompt = Prompt {agent = agent', prompt = i'.prompt, history = h},
                  toolRounds = [],
                  submitTool = Just submit
                }
            rs' =
              startLeafCell leafPath rs.rsInstIters (Just agent'.agent.agName) $
                if case rs.rsPathStack of FrameLeaf {} : _ -> True; _ -> False
                  then rs
                  else pushLeaf leafLbl leafPath rs
         in pure $ withStack (\_ -> Stack uAcc (unsafeCoerce (RunPrompt pending mbcid)) konts) rs'
      WBlackboardPrompt assembler agent -> do
        let (leafPath, leafLbl) = leafSite rs
            loopPath = fromMaybe (topPath rs.rsPathStack) (innermostLoopPathFromStack rs.rsPathStack)
            bv = mkPolicyView rs (PolicySite loopPath TriggerLoopDecider Nothing Nothing)
            args = assembler bv
            pending =
              Pending
                { prompt = Prompt {agent = agent, prompt = args.prompt, history = args.history},
                  toolRounds = [],
                  submitTool = Nothing
                }
            rs' =
              startLeafCell leafPath rs.rsInstIters (Just agent.agent.agName) $
                if case rs.rsPathStack of FrameLeaf {} : _ -> True; _ -> False
                  then rs
                  else pushLeaf leafLbl leafPath rs
         in pure $ withStack (\_ -> Stack uAcc (RunPrompt pending Nothing) konts) rs'
      WSeq workflow1 workflow2 pol -> do
        let seqPath = topPath rs.rsPathStack
            site = PolicySite seqPath TriggerSeq Nothing Nothing
            rs' = pushComposite CompSeq (syntheticLabel 0) seqPath rs
         in pure $ withStack (\_ -> Stack uAcc (RunWorkflow workflow1 i) (KSeq1 workflow2 pol site konts)) rs'
      WPar workflow1 workflow2 pol -> do
        let parPath = topPath rs.rsPathStack
            rs' = pushComposite CompPar (syntheticLabel 0) parPath rs
            leftPath = resolveChildPath rs' (syntheticLabel 0)
            rs'' = pushLeaf (syntheticLabel 0) leftPath rs'
         in pure $ withStack (\_ -> Stack uAcc (RunWorkflow workflow1 i) (KPar1 i workflow2 pol konts)) rs''
      WLift f -> do
        o <- f i
        pure $ withStack (\_ -> Stack uAcc (RunReturn o) konts) rs
      WMap workflow1 f -> do
        let mapPath = topPath rs.rsPathStack
            site = PolicySite mapPath TriggerMap Nothing Nothing
            rs' = pushComposite CompMap (syntheticLabel 0) mapPath rs
         in pure $ withStack (\_ -> Stack uAcc (RunWorkflow workflow1 i) (KMap f site konts)) rs'
      WLoop n wf policy cids -> do
        let loopPath = topPath rs.rsPathStack
            scope = map cidToSlot cids
            site = PolicySite loopPath TriggerLoopFeedback Nothing Nothing
            rs' =
              initLoopSliceAt loopPath FixedCount n (unsafeCoerce i) scope $
                pushComposite CompLoop (Label "loop") loopPath rs
         in pure $ withStack (\_ -> Stack uAcc (RunWorkflow wf i) (KLoop (n - 1) wf policy (Map.fromList [(cid, []) | cid <- cids]) site konts)) rs'
      WLoopWhile maxIterations decider decisionPolicy cids policy wf -> do
        let loopPath = topPath rs.rsPathStack
            scope = map cidToSlot cids
            site = PolicySite loopPath TriggerLoopFeedback Nothing Nothing
            rs' =
              initLoopSliceAt loopPath While maxIterations (unsafeCoerce i) scope $
                pushComposite CompLoop (Label "refinement-loop") loopPath rs
         in pure $
              withStack
                ( \_ ->
                    Stack
                      uAcc
                      (RunWorkflow wf i)
                      (KLoopWhile maxIterations 1 wf policy decider decisionPolicy (Map.fromList [(cid, []) | cid <- cids]) i [] site konts)
                )
                rs'
      WLiftW f -> do
        wf <- f (fst i)
        let nestPath = topPath rs.rsPathStack
            rs' = pushComposite CompNest (syntheticLabel 0) nestPath rs
         in pure $ withStack (\_ -> Stack uAcc (RunWorkflow wf (snd i)) konts) rs'
      WCatch o wf -> do
        let catchPath = topPath rs.rsPathStack
            rs' = pushComposite CompCatch (syntheticLabel 0) catchPath rs
         in pure $ withStack (\_ -> Stack uAcc (RunWorkflow wf i) (KCatch o konts)) rs'
    RunFinish _ ->
      pure rs
    RunThrow err ->
      case unwindToCatch konts of
        Just (CatchFrame caughtValue k) ->
          let rs' = case rs.rsCurrentLeaf of
                Nothing -> rs
                Just (path, iters) ->
                  catchLeafCell path iters (unsafeCoerce caughtValue) err rs
           in pure $ withStack (\_ -> Stack uAcc (RunReturn caughtValue) k) rs'
        Nothing -> do
          let rs' = case rs.rsCurrentLeaf of
                Nothing -> rs
                Just (path, iters) -> failLeafCell path iters err rs
           in pure $ withStack (\_ -> Stack uAcc (RunFinish (Left err)) KEmpty) rs'
    RunReturn o -> case konts of
      KEmpty -> pure $ withStack (\_ -> Stack uAcc (RunFinish (Right (unsafeCoerce o))) KEmpty) rs
      KTool pending mcid assistantTurn toolCalls toolResults toolCall k ->
        let tr = ToolResult toolCall.tcId toolCall.tcName o
            toolResults' = tr : toolResults
         in case toolCalls of
              (toolCall' : toolCalls') ->
                pure $
                  withStack
                    (\_ -> Stack uAcc (RunTool pending assistantTurn toolCall') (KTool pending mcid assistantTurn toolCalls' toolResults' toolCall' k))
                    rs
              [] -> do
                let pending' = pending {toolRounds = pending.toolRounds ++ [assistantTurn, ToolTurn toolResults']}
                 in if pendingToolRoundCount pending' >= pending'.prompt.agent.agent.agMaxToolRounds
                      then pure $ withStack (\_ -> Stack uAcc (RunThrow GErrToolExceeded) k) rs
                      else
                        let rs' = case rs.rsCurrentLeaf of
                              Nothing -> rs
                              Just (path, iters) ->
                                updateRunningHistory path iters (pendingToTurns pending') rs
                         in pure $ withStack (\_ -> Stack uAcc (RunPrompt pending' mcid) k) rs'
      KSeq1 workflow2 pol site k -> do
        let predInst = Instance (resolveChildPath rs (syntheticLabel 0)) rs.rsInstIters
            bv = mkPolicyView rs site {psPredecessor = Just predInst, psSelf = Just predInst, psTrigger = TriggerSeq}
            o' = runSeqPolicy pol bv o
            w2Path = resolveChildPath rs (syntheticLabel 1)
            rs' = pushLeaf (syntheticLabel 1) w2Path rs
         in pure $ withStack (\_ -> Stack uAcc (RunWorkflow workflow2 o') k) rs'
      KPar1 i' workflow2 pol k ->
        let rightPath = resolveSidePath rs SideRight
            rs' = pushLeaf (syntheticLabel 1) rightPath rs
         in pure $ withStack (\_ -> Stack uAcc (RunWorkflow workflow2 i') (KPar2 o pol (PolicySite (topPath rs.rsPathStack) TriggerParMerge Nothing Nothing) k)) rs'
      KPar2 x pol site k -> do
        let bv = mkPolicyView rs site {psTrigger = TriggerParMerge}
            merged = runMergePolicy pol bv x o
         in pure $ withStack (\_ -> Stack uAcc (RunReturn merged) k) rs
      KMap pol site k -> do
        let bv = mkPolicyView rs site {psTrigger = TriggerMap}
            mapped = runMapPolicy pol bv o
         in pure $ withStack (\_ -> Stack uAcc (RunReturn mapped) k) rs
      KLoop n workflow policy cids site k -> do
        let loopPath = site.psLocal
            rs' = appendBodyOutput loopPath (nodeOutputFromFinal (unsafeCoerce o)) rs
            bv = mkPolicyView rs' site {psTrigger = TriggerLoopFeedback}
            slice = fromMaybe (error "KLoop: missing slice") (lookupLoopSlice loopPath rs'.rsBlackboard)
         in if n < 1
              then pure $ withStack (\_ -> Stack uAcc (RunReturn o) k) rs'
              else do
                let nextInput = runLoopFeedPolicy policy bv slice o
                 in pure $ withStack (\_ -> Stack uAcc (RunWorkflow workflow nextInput) (KLoop (n - 1) workflow policy cids site k)) rs'
      KLoopWhile maxIterations iteration workflow policy decider decisionPolicy cids _currentInput outputsRev site k -> do
        let loopPath = site.psLocal
            rs' = appendBodyOutput loopPath (nodeOutputFromFinal (unsafeCoerce o)) rs
            bv = mkPolicyView rs' site {psTrigger = TriggerLoopFeedback}
            slice = fromMaybe (error "KLoopWhile: missing slice") (lookupLoopSlice loopPath rs'.rsBlackboard)
            nextInput = runLoopFeedPolicy policy bv slice o
            outputsRev' = o : outputsRev
            deciderSite = PolicySite loopPath TriggerLoopDecider Nothing Nothing
            deciderInput = PromptArgs {history = [], prompt = ""}
         in if iteration >= maxIterations
              then pure $ withStack (\_ -> Stack uAcc (RunReturn o) k) rs'
              else do
                let deciderPath = resolveChildPath rs' (Label "decider")
                    rs'' = pushLeaf (Label "decider") deciderPath rs'
                 in pure $
                      withStack
                        ( \_ ->
                            Stack
                              uAcc
                              (RunWorkflow decider deciderInput)
                              ( KLoopWhileDecision
                                  maxIterations
                                  iteration
                                  workflow
                                  policy
                                  decider
                                  decisionPolicy
                                  cids
                                  nextInput
                                  outputsRev'
                                  o
                                  deciderSite
                                  k
                              )
                        )
                        rs''
      KLoopWhileDecision maxIterations iteration workflow policy decider decisionPolicy cids nextInput outputsRev lastOutput site k -> do
        let loopPath = site.psLocal
            bv = mkPolicyView rs site {psTrigger = TriggerLoopDecider}
            slice = fromMaybe (error "KLoopWhileDecision: missing slice") (lookupLoopSlice loopPath rs.rsBlackboard)
         in if runLoopDecPolicy decisionPolicy bv slice o
              then do
                let bodyPath = resolveChildPath rs (Label "body")
                    rs' = pushLeaf (Label "body") bodyPath rs {rsInstIters = (iteration + 1) : rs.rsInstIters}
                 in pure $
                      withStack
                        ( \_ ->
                            Stack
                              uAcc
                              (RunWorkflow workflow nextInput)
                              ( KLoopWhile
                                  maxIterations
                                  (iteration + 1)
                                  workflow
                                  policy
                                  decider
                                  decisionPolicy
                                  cids
                                  nextInput
                                  outputsRev
                                  site
                                  k
                              )
                        )
                        rs'
              else pure $ withStack (\_ -> Stack uAcc (RunReturn lastOutput) k) rs
      KUpdateHistory cid history k -> do
        let rs' = dualWriteSlot (cidToSlot cid) history rs
         in pure $ withStack (\_ -> Stack uAcc step $ updateHistory cid history k) rs'
      KCatch _r k ->
        pure $ withStack (\_ -> Stack uAcc step k) rs
