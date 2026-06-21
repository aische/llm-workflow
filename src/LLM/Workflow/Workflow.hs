module LLM.Workflow.Workflow where

import Control.Monad (when)
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
    appendLoopDecision,
    bumpLoopIteration,
    catchLeafCell,
    completeLeafCell,
    completeObjectCell,
    currentLeafCoords,
    dualWriteSlot,
    enterChildPath,
    exitLoopScope,
    failLeafCell,
    initLoopSliceAt,
    mkPolicyView,
    nodeOutputFromFinal,
    parentPath,
    popCompositeFrame,
    popParFrames,
    popPathFrame,
    pushComposite,
    pushLeaf,
    pushParSide,
    pushScope,
    resolveChildPath,
    resolveSidePath,
    setLoopNextInput,
    startLeafCell,
    topPath,
    updateRunningHistory,
    writeMapCell,
    writeNestCell,
  )
import LLM.Workflow.Blackboard
  ( buildLabelEnv,
    extendLabelEnv,
    initialRunnerState,
    innermostLoopPathFromStack,
    labelEnvResolve,
    lookupSlot,
    syntheticLabel,
  )
import LLM.Workflow.ToolUtils (executeTool, mkSomeSubmit)
import LLM.Workflow.Types
  ( AgentWithModels (agent, models),
    Blackboard (..),
    CID (..),
    CompositeKind (..),
    HistoryMode (..),
    Instance (..),
    Kont (..),
    Label (..),
    LoopKind (..),
    LoopSlice (..),
    Path,
    PathFrame (..),
    Pending (..),
    PolicySite (..),
    Prompt (..),
    PromptArgs (..),
    RunnerState (..),
    Side (..),
    SlotKey (..),
    SomeSubmit (ssDecode, ssName),
    Stack (..),
    Step (..),
    ToolOutcome (ToolReply, ToolWorkflow, ToolYield),
    Trigger (..),
    Workflow (..),
    historyPersistKey,
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

wfDebugEnabled :: Bool
wfDebugEnabled = True

-- wfDebugEnabled = unsafePerformIO $ (== Just "1") <$> lookupEnv "WF_DEBUG"
-- {-# NOINLINE wfDebugEnabled #-}

debugEvalLine :: Text -> IO ()
debugEvalLine line =
  when wfDebugEnabled $ TIO.putStrLn line

debugAgentStream :: StreamChunk -> IO ()
debugAgentStream chunk =
  when wfDebugEnabled $ TIO.putStr (showStreamChunk chunk)

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
  HistoryMode ->
  Usage ->
  ChatResponse ->
  Kont o r ->
  (Usage, Step Text, Kont Text r)
startToolRound pending mode uAcc resp konts =
  let (_text, assistantTurn, toolCalls) = respToAssistantTurn resp
      usage = fromMaybe emptyUsage resp.respUsage
      (toolCall, toolCalls') = case toolCalls of
        (tc : tcs) -> (tc, tcs)
        [] -> error "startToolRound: no tool calls"
   in ( uAcc <> usage,
        RunTool pending assistantTurn toolCall,
        KTool pending mode assistantTurn toolCalls' [] toolCall (unsafeCoerce konts)
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

lookupAgentHistory :: Kont o r -> HistoryMode -> PromptArgs -> RunnerState s -> [Turn]
lookupAgentHistory konts mode i' rs =
  case historyPersistKey mode of
    Nothing -> i'.history
    Just key ->
      let slotTurns = lookupSlot rs.rsBlackboard rs.rsPathStack key
       in if null slotTurns
            then lookupHistory konts key
            else slotTurns

failCurrentLeaf :: GenerateError -> RunnerState a -> RunnerState a
failCurrentLeaf err rs =
  case currentLeafCoords rs of
    Nothing -> rs
    Just (path, iters) -> failLeafCell path iters err rs

persistKont :: HistoryMode -> [Turn] -> Kont o r -> Kont o r
persistKont mode turns kont =
  case historyPersistKey mode of
    Nothing -> kont
    Just key -> KUpdateHistory key turns kont

policyTriggerForPrompt :: RunnerState a -> Trigger
policyTriggerForPrompt rs =
  case innermostLoopPathFromStack rs.rsPathStack of
    Just _ -> TriggerLoopDecider
    Nothing -> TriggerSeq

leafSite :: RunnerState a -> (Path, Label)
leafSite rs = case rs.rsPathStack of
  FrameLeaf lbl path : _ -> (path, lbl)
  _ -> (resolveChildPath rs (syntheticLabel 0), syntheticLabel 0)

agentLeafSite :: RunnerState a -> (Path, Label)
agentLeafSite rs = case rs.rsPathStack of
  FrameScope lbl path : _ -> (path, lbl)
  FrameLeaf lbl path : _ -> (path, lbl)
  _ -> (resolveChildPath rs (syntheticLabel 0), syntheticLabel 0)

ensureLeafFrame :: Label -> Path -> RunnerState a -> RunnerState a
ensureLeafFrame lbl path rs =
  case rs.rsPathStack of
    FrameLeaf existingLbl existingPath : _ | existingLbl == lbl && existingPath == path -> rs
    _ -> pushLeaf lbl path rs

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
  _ <- debugEvalLine $ space <> cents <> " " <> showStep step <> T.unwords (map (" : " <>) (showKont konts))
  case step of
    RunObject pending -> do
      result <- callLLMO toolMap rt pending
      case result of
        Left err ->
          let rs' = failCurrentLeaf err rs
           in pure $ withStack (\_ -> Stack uAcc (RunThrow err) konts) rs'
        Right (value, usage) ->
          let rs' = case currentLeafCoords rs of
                Nothing -> rs
                Just (path, iters) -> completeObjectCell path iters value usage rs
           in pure $ withStack (\_ -> Stack (uAcc <> usage) (RunReturn value) konts) rs'
    RunPrompt pending mode -> do
      let agentName = pending.prompt.agent.agent.agName
      when wfDebugEnabled $ TIO.putStr (space <> agentName <> ": ")
      result <- streamLLM debugAgentStream toolMap rt pending
      case result of
        Left err ->
          let rs' = failCurrentLeaf err rs
           in pure $ withStack (\_ -> Stack uAcc (RunThrow err) konts) rs'
        Right resp -> do
          let (text, assistantTurn, toolCalls) = respToAssistantTurn resp
          case toolCalls of
            [] ->
              case pending.submitTool of
                Just _ ->
                  let rs' = failCurrentLeaf submitRequiredError rs
                   in pure $
                        withStack (\_ -> Stack (uAcc <> fromMaybe emptyUsage resp.respUsage) (RunThrow submitRequiredError) konts) rs'
                Nothing ->
                  let h = pendingToTurns pending ++ [assistantTurn]
                      usage = fromMaybe emptyUsage resp.respUsage
                      rs' = case currentLeafCoords rs of
                        Nothing -> rs
                        Just (path, iters) ->
                          completeLeafCell path iters (pendingToFinal pending text assistantTurn) usage h rs
                   in pure $
                        withStack
                          ( \_ ->
                              Stack
                                (uAcc <> usage)
                                (RunReturn $ pendingToFinal pending text assistantTurn)
                                (persistKont mode h konts)
                          )
                          rs'
            _ ->
              if pendingToolRoundCount pending >= pending.prompt.agent.agent.agMaxToolRounds
                then
                  let rs' = failCurrentLeaf GErrToolExceeded rs
                   in pure $ withStack (\_ -> Stack uAcc (RunThrow GErrToolExceeded) konts) rs'
                else
                  let (uAcc', step', konts') = startToolRound pending mode uAcc resp konts
                      rs' = case currentLeafCoords rs of
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
      WLabel lbl wf -> do
        let lblPath = labelEnvResolve rs.rsLabelEnv (parentPath rs.rsPathStack) lbl
            rs' = pushScope lbl lblPath rs
         in pure $ withStack (\_ -> Stack uAcc (RunWorkflow wf i) (KPopFrame konts)) rs'
      WPrompt a historyMode -> do
        let (leafPath, leafLbl) = agentLeafSite rs
            i' = unsafeCoerce i :: PromptArgs
            h = lookupAgentHistory konts historyMode i' rs
            pending = Pending {prompt = Prompt {agent = a, prompt = i'.prompt, history = h}, toolRounds = [], submitTool = Nothing}
            rs' =
              startLeafCell leafPath rs.rsInstIters (Just a.agent.agName) $
                ensureLeafFrame leafLbl leafPath rs
         in pure $ withStack (\_ -> Stack uAcc (RunPrompt pending historyMode) konts) rs'
      WObject a -> do
        let (leafPath, leafLbl) = agentLeafSite rs
            i' = unsafeCoerce i :: PromptArgs
            pending = Pending {prompt = Prompt {agent = a, prompt = i'.prompt, history = i'.history}, toolRounds = [], submitTool = Nothing}
            rs' =
              startLeafCell leafPath rs.rsInstIters (Just a.agent.agName) $
                ensureLeafFrame leafLbl leafPath rs
         in pure $ withStack (\_ -> Stack uAcc (RunObject pending) konts) rs'
      WAgentSubmit @o name agentWithModels historyMode -> do
        let (leafPath, leafLbl) = agentLeafSite rs
            i' = unsafeCoerce i :: PromptArgs
            h = lookupAgentHistory konts historyMode i' rs
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
                ensureLeafFrame leafLbl leafPath rs
         in pure $ withStack (\_ -> Stack uAcc (unsafeCoerce (RunPrompt pending historyMode)) konts) rs'
      WBlackboardPrompt assembler agent -> do
        let (leafPath, leafLbl) = agentLeafSite rs
            localPath = fromMaybe (topPath rs.rsPathStack) (innermostLoopPathFromStack rs.rsPathStack)
            bv = mkPolicyView rs (PolicySite localPath (policyTriggerForPrompt rs) Nothing Nothing)
            args = assembler bv
            pending =
              Pending
                { prompt = Prompt {agent = agent, prompt = args.prompt, history = args.history},
                  toolRounds = [],
                  submitTool = Nothing
                }
            rs' =
              startLeafCell leafPath rs.rsInstIters (Just agent.agent.agName) $
                ensureLeafFrame leafLbl leafPath rs
         in pure $ withStack (\_ -> Stack uAcc (RunPrompt pending HistoryEphemeral) konts) rs'
      WSeq workflow1 workflow2 pol -> do
        let seqPath = topPath rs.rsPathStack
            site = PolicySite seqPath TriggerSeq Nothing Nothing
            rs' = pushComposite CompSeq (syntheticLabel 0) seqPath rs
            w1Path = resolveChildPath rs' (syntheticLabel 0)
            rs'' = pushScope (syntheticLabel 0) w1Path rs'
         in pure $ withStack (\_ -> Stack uAcc (RunWorkflow workflow1 i) (KSeq1 workflow2 pol site konts)) rs''
      WPar workflow1 workflow2 pol -> do
        let parPath = topPath rs.rsPathStack
            mergeSite = PolicySite parPath TriggerParMerge Nothing Nothing
            rs' = pushComposite CompPar (syntheticLabel 0) parPath rs
            leftPath = resolveChildPath rs' (syntheticLabel 0)
            rs'' = pushParSide (syntheticLabel 0) SideLeft leftPath rs'
         in pure $ withStack (\_ -> Stack uAcc (RunWorkflow workflow1 i) (KPar1 i workflow2 pol mergeSite konts)) rs''
      WLift f -> do
        o <- f i
        pure $ withStack (\_ -> Stack uAcc (RunReturn o) konts) rs
      WMap workflow1 f -> do
        let mapPath = enterChildPath rs (syntheticLabel 0)
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
            bodyPath = resolveChildPath rs' (Label "body")
            rs'' = pushScope (Label "body") bodyPath rs' {rsInstIters = 1 : rs'.rsInstIters}
            slotMap = Map.fromList [(k, []) | k <- scope]
         in pure $ withStack (\_ -> Stack uAcc (RunWorkflow wf i) (KLoop (n - 1) wf policy slotMap site konts)) rs''
      WLoopWhile maxIterations decider decisionPolicy cids policy wf -> do
        let loopPath = topPath rs.rsPathStack
            scope = map cidToSlot cids
            site = PolicySite loopPath TriggerLoopFeedback Nothing Nothing
            rs' =
              initLoopSliceAt loopPath While maxIterations (unsafeCoerce i) scope $
                pushComposite CompLoop (Label "refinement-loop") loopPath rs
            bodyPath = resolveChildPath rs' (Label "body")
            rs'' = pushScope (Label "body") bodyPath rs' {rsInstIters = 1 : rs'.rsInstIters}
            slotMap = Map.fromList [(k, []) | k <- scope]
         in pure $
              withStack
                ( \_ ->
                    Stack
                      uAcc
                      (RunWorkflow wf i)
                      (KLoopWhile maxIterations 1 wf policy decider decisionPolicy slotMap i site konts)
                )
                rs''
      WLiftW f -> do
        wfNested <- f (fst i)
        let nestPath = enterChildPath rs (syntheticLabel 0)
            labelEnv' = case extendLabelEnv nestPath wfNested rs.rsLabelEnv of
              Right e -> e
              Left err -> error ("extendLabelEnv: " <> show err)
            site = PolicySite nestPath TriggerMap Nothing Nothing
            rs' =
              pushComposite
                CompNest
                (syntheticLabel 0)
                nestPath
                rs
                  { rsLabelEnv = labelEnv'
                  }
         in pure $ withStack (\_ -> Stack uAcc (RunWorkflow wfNested (snd i)) (KNest site konts)) rs'
      WCatch o wf -> do
        let catchPath = topPath rs.rsPathStack
            rs' = pushComposite CompCatch (syntheticLabel 0) catchPath rs
         in pure $ withStack (\_ -> Stack uAcc (RunWorkflow wf i) (KCatch o konts)) rs'
    RunFinish _ ->
      pure rs
    RunThrow err ->
      case unwindToCatch konts of
        Just (CatchFrame caughtValue k) ->
          let rs' = case currentLeafCoords rs of
                Nothing -> popPathFrame rs
                Just (path, iters) ->
                  catchLeafCell path iters (unsafeCoerce caughtValue) err rs
           in pure $ withStack (\_ -> Stack uAcc (RunReturn caughtValue) k) (popCompositeFrame CompCatch (popPathFrame rs'))
        Nothing -> do
          let rs' = failCurrentLeaf err rs
           in pure $ withStack (\_ -> Stack uAcc (RunFinish (Left err)) KEmpty) rs'
    RunReturn o -> case konts of
      KEmpty -> pure $ withStack (\_ -> Stack uAcc (RunFinish (Right (unsafeCoerce o))) KEmpty) rs
      KPopFrame k -> do
        let rs' = popPathFrame rs
         in pure $ withStack (\_ -> Stack uAcc (RunReturn o) k) rs'
      KPopComposite kind k -> do
        let rs' = popCompositeFrame kind rs
         in pure $ withStack (\_ -> Stack uAcc (RunReturn o) k) rs'
      KTool pending mode assistantTurn toolCalls toolResults toolCall k ->
        let tr = ToolResult toolCall.tcId toolCall.tcName o
            toolResults' = tr : toolResults
         in case toolCalls of
              (toolCall' : toolCalls') ->
                pure $
                  withStack
                    (\_ -> Stack uAcc (RunTool pending assistantTurn toolCall') (KTool pending mode assistantTurn toolCalls' toolResults' toolCall' k))
                    rs
              [] -> do
                let pending' = pending {toolRounds = pending.toolRounds ++ [assistantTurn, ToolTurn toolResults']}
                 in if pendingToolRoundCount pending' >= pending'.prompt.agent.agent.agMaxToolRounds
                      then pure $ withStack (\_ -> Stack uAcc (RunThrow GErrToolExceeded) k) (failCurrentLeaf GErrToolExceeded rs)
                      else
                        let rs' = case currentLeafCoords rs of
                              Nothing -> rs
                              Just (path, iters) ->
                                updateRunningHistory path iters (pendingToTurns pending') rs
                         in pure $ withStack (\_ -> Stack uAcc (RunPrompt pending' mode) k) rs'
      KSeq1 workflow2 pol site k -> do
        let rsAfterW1 = popPathFrame rs
            predInst = Instance (resolveChildPath rsAfterW1 (syntheticLabel 0)) rsAfterW1.rsInstIters
            selfInst = Instance (resolveChildPath rsAfterW1 (syntheticLabel 1)) rsAfterW1.rsInstIters
            bv = mkPolicyView rsAfterW1 site {psPredecessor = Just predInst, psSelf = Just selfInst, psTrigger = TriggerSeq}
            o' = runSeqPolicy pol bv o
            w2Path = resolveChildPath rsAfterW1 (syntheticLabel 1)
            rs' = pushScope (syntheticLabel 1) w2Path rsAfterW1
         in pure $ withStack (\_ -> Stack uAcc (RunWorkflow workflow2 o') (KPopFrame (KPopComposite CompSeq k))) rs'
      KPar1 i' workflow2 pol site k ->
        let rightPath = resolveSidePath rs SideRight
            rs' = pushParSide (syntheticLabel 1) SideRight rightPath rs
         in pure $ withStack (\_ -> Stack uAcc (RunWorkflow workflow2 i') (KPar2 o pol site k)) rs'
      KPar2 x pol site k -> do
        let bv = mkPolicyView rs site {psTrigger = TriggerParMerge}
            merged = runMergePolicy pol bv x o
            rs' = popParFrames rs
         in pure $ withStack (\_ -> Stack uAcc (RunReturn merged) k) rs'
      KMap pol site k -> do
        let bv = mkPolicyView rs site {psTrigger = TriggerMap}
            rawOut = nodeOutputFromFinal (unsafeCoerce o)
            mapped = runMapPolicy pol bv o
            mappedOut = nodeOutputFromFinal (unsafeCoerce mapped)
            rs' =
              writeMapCell site.psLocal rs.rsInstIters rawOut mappedOut rs
                |> popPathFrame
         in pure $ withStack (\_ -> Stack uAcc (RunReturn mapped) k) rs'
      KLoop n workflow policy slots site k -> do
        let loopPath = site.psLocal
            rs' = appendBodyOutput loopPath (nodeOutputFromFinal (unsafeCoerce o)) rs
            bv = mkPolicyView rs' site {psTrigger = TriggerLoopFeedback}
            slice = fromMaybe (error "KLoop: missing slice") (lookupLoopSlice loopPath rs'.rsBlackboard)
         in if n < 1
              then do
                let rs'' = exitLoopScope rs'
                 in pure $ withStack (\_ -> Stack uAcc (RunReturn o) k) rs''
              else do
                let nextInput = runLoopFeedPolicy policy bv slice o
                    nextIter = slice.lsIteration + 1
                    rsUpdated =
                      setLoopNextInput loopPath (unsafeCoerce nextInput) $
                        bumpLoopIteration loopPath nextIter rs'
                    rsWithIters =
                      rsUpdated {rsInstIters = case rsUpdated.rsInstIters of _ : rest -> nextIter : rest; _ -> [nextIter]}
                 in pure $ withStack (\_ -> Stack uAcc (RunWorkflow workflow nextInput) (KLoop (n - 1) workflow policy slots site k)) rsWithIters
      KLoopWhile maxIterations iteration workflow policy decider decisionPolicy slots _currentInput site k -> do
        let loopPath = site.psLocal
            rs' =
              bumpLoopIteration loopPath iteration $
                appendBodyOutput loopPath (nodeOutputFromFinal (unsafeCoerce o)) rs
            bv = mkPolicyView rs' site {psTrigger = TriggerLoopFeedback}
            slice = fromMaybe (error "KLoopWhile: missing slice") (lookupLoopSlice loopPath rs'.rsBlackboard)
            nextInput = runLoopFeedPolicy policy bv slice o
            deciderSite = PolicySite loopPath TriggerLoopDecider Nothing Nothing
            deciderInput = PromptArgs {history = [], prompt = ""}
         in if iteration >= maxIterations
              then do
                let rs'' = exitLoopScope rs'
                 in pure $ withStack (\_ -> Stack uAcc (RunReturn o) k) rs''
              else do
                let rsUpdated = setLoopNextInput loopPath (unsafeCoerce nextInput) rs'
                    deciderPath = resolveChildPath rsUpdated (Label "decider")
                    rs'' = pushLeaf (Label "decider") deciderPath rsUpdated
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
                                  slots
                                  nextInput
                                  o
                                  deciderSite
                                  k
                              )
                        )
                        rs''
      KLoopWhileDecision maxIterations iteration workflow policy decider decisionPolicy slots nextInput lastOutput site k -> do
        let loopPath = site.psLocal
            bv = mkPolicyView rs site {psTrigger = TriggerLoopDecider}
            slice = fromMaybe (error "KLoopWhileDecision: missing slice") (lookupLoopSlice loopPath rs.rsBlackboard)
            continue = runLoopDecPolicy decisionPolicy bv slice o
            rs' = appendLoopDecision loopPath continue rs
         in if continue
              then do
                let nextIteration = iteration + 1
                    bodyPath = resolveChildPath rs (Label "body")
                    rs'' =
                      pushScope (Label "body") bodyPath $
                        bumpLoopIteration
                          loopPath
                          nextIteration
                          rs'
                            { rsInstIters = case rs.rsInstIters of _ : rest -> nextIteration : rest; _ -> [nextIteration]
                            }
                 in pure $
                      withStack
                        ( \_ ->
                            Stack
                              uAcc
                              (RunWorkflow workflow nextInput)
                              ( KLoopWhile
                                  maxIterations
                                  nextIteration
                                  workflow
                                  policy
                                  decider
                                  decisionPolicy
                                  slots
                                  nextInput
                                  site
                                  k
                              )
                        )
                        rs''
              else do
                let rs'' = exitLoopScope rs'
                 in pure $ withStack (\_ -> Stack uAcc (RunReturn lastOutput) k) rs''
      KUpdateHistory key history k -> do
        let rs' = dualWriteSlot key history rs
         in pure $ withStack (\_ -> Stack uAcc step $ updateHistory key history k) rs'
      KCatch caught k -> do
        let rs' = popCompositeFrame CompCatch (popPathFrame rs)
         in pure $ withStack (\_ -> Stack uAcc (RunReturn caught) k) rs'
      KNest site k -> do
        let rs' =
              writeNestCell site.psLocal rs.rsInstIters (nodeOutputFromFinal (unsafeCoerce o)) rs
                |> popPathFrame
         in pure $ withStack (\_ -> Stack uAcc (RunReturn o) k) rs'

(|>) :: a -> (a -> b) -> b
x |> f = f x
