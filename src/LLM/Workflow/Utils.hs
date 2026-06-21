module LLM.Workflow.Utils where

import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import LLM (getToolCalls)
import LLM.Agent (Agent (..))
import LLM.Agent.Types (ToolMap)
import LLM.Core.Types
  ( ChatResponse (..),
    ToolCall (..),
    ToolResult (..),
    Turn (AssistantTurn, ToolTurn, UserTurn),
  )
import LLM.Workflow.Types
  ( AgentWithModels (..),
    AnyLoopDecPolicy (..),
    AnyLoopFeedPolicy (..),
    AnyMapPolicy (..),
    AnyMergePolicy (..),
    AnySeqPolicy (..),
    BlackboardView (..),
    CID,
    Final (..),
    Kont (..),
    LoopSlice (..),
    LoopFeedPolicy,
    LoopDecPolicy,
    SeqPolicyBB,
    MergePolicy (..),
    Pending (prompt, toolRounds),
    Prompt (..),
    PromptArgs (PromptArgs, history, prompt),
    SomeSubmit (..),
    Step (..),
    ToolOutcome,
    TranscriptPolicy (..),
  )
import Unsafe.Coerce (unsafeCoerce)

-- * Conversation mapping functions -----------------------------------------

pendingToFinal :: Pending -> Text -> Turn -> Final
pendingToFinal pending text assistantTurn =
  Final
    { prompt = Just pending.prompt,
      history = pending.prompt.history,
      newMessages = [UserTurn pending.prompt.prompt] ++ pending.toolRounds ++ [assistantTurn],
      text = text
    }

pendingToTurns :: Pending -> [Turn]
pendingToTurns pending = pending.prompt.history ++ [UserTurn pending.prompt.prompt] ++ pending.toolRounds

-- | Number of completed tool rounds in a pending agent step.
-- Each round adds an assistant turn (with tool calls) and a tool-result turn.
pendingToolRoundCount :: Pending -> Int
pendingToolRoundCount pending = length pending.toolRounds `div` 2

turnsToConversationText :: [Turn] -> Text
turnsToConversationText turns = T.unlines (map showTurn turns)
  where
    showTurn turn = case turn of
      UserTurn text -> "User: " <> text <> "\n"
      AssistantTurn text _ _ -> "Assistant: " <> text <> "\n"
      ToolTurn toolResults -> "Tool: " <> T.unwords (map (\x -> x.trName) toolResults) <> "\n"

respToAssistantTurn :: ChatResponse -> (Text, Turn, [ToolCall])
respToAssistantTurn cr = (cr.respText, AssistantTurn cr.respText cr.respReasoning toolCalls, toolCalls)
  where
    toolCalls = getToolCalls cr

emptyFinal :: Text -> Final
emptyFinal text =
  Final
    { prompt = Nothing,
      history = [],
      newMessages = [],
      text = text
    }

-- * Transcript policy functions --------------------------------------------

transcriptPolicy :: TranscriptPolicy i o -> i -> o
transcriptPolicy (TranscriptPolicyFunc f) i = f i
transcriptPolicy TranscriptFinalToPromptArgs final = PromptArgs {history = [], prompt = final.text}
transcriptPolicy TranscriptFinalText final = final.text
transcriptPolicy TranscriptSummaryText final = "Summary: " <> turnsToConversationText allMessages
  where
    allMessages = final.history ++ final.newMessages

mergePolicy :: MergePolicy o1 o2 o -> o1 -> o2 -> o
mergePolicy (MergePolicyFunc f) o1 o2 = f o1 o2
mergePolicy MergePolicyFinalToPromptArgs final1 final2 = PromptArgs {history = [], prompt = final1.text <> "\n\n\n" <> final2.text}

-- * AnyPolicy dispatch -----------------------------------------------------

seqPolicy :: TranscriptPolicy i o -> AnySeqPolicy i o
seqPolicy = LegacyTranscript

mergePolicyAny :: MergePolicy o1 o2 o -> AnyMergePolicy o1 o2 o
mergePolicyAny = LegacyMerge

loopFeedPolicy :: TranscriptPolicy o i -> AnyLoopFeedPolicy i o
loopFeedPolicy = LegacyLoopFeed

loopDecPolicy :: TranscriptPolicy d Bool -> AnyLoopDecPolicy d
loopDecPolicy = LegacyLoopDec

blackboardLoopFeed :: LoopFeedPolicy i o -> AnyLoopFeedPolicy i o
blackboardLoopFeed = BlackboardLoopFeed

blackboardLoopDec :: LoopDecPolicy d -> AnyLoopDecPolicy d
blackboardLoopDec = BlackboardLoopDec

seqPolicyBB :: SeqPolicyBB o -> AnySeqPolicy i o
seqPolicyBB = BlackboardSeqOnly

mapPolicy :: TranscriptPolicy o o' -> AnyMapPolicy o o'
mapPolicy = LegacyMap

runSeqPolicy :: AnySeqPolicy i o -> BlackboardView -> i -> o
runSeqPolicy (LegacyTranscript p) _ i = transcriptPolicy p i
runSeqPolicy (BlackboardSeq p) bv i = p bv i
runSeqPolicy (BlackboardSeqOnly p) bv _ = p bv

runMergePolicy :: AnyMergePolicy o1 o2 o -> BlackboardView -> o1 -> o2 -> o
runMergePolicy (LegacyMerge p) _ x y = mergePolicy p x y
runMergePolicy (BlackboardPar p) bv x y = p bv x y

runLoopFeedPolicy :: AnyLoopFeedPolicy i o -> BlackboardView -> LoopSlice -> o -> i
runLoopFeedPolicy (LegacyLoopFeed p) _ _ o = transcriptPolicy p o
runLoopFeedPolicy (BlackboardLoopFeed p) bv slice o = p bv slice o

runLoopDecPolicy :: AnyLoopDecPolicy d -> BlackboardView -> LoopSlice -> d -> Bool
runLoopDecPolicy (LegacyLoopDec p) _ _ d = transcriptPolicy p d
runLoopDecPolicy (BlackboardLoopDec p) bv slice d = p bv slice d

runMapPolicy :: AnyMapPolicy o o' -> BlackboardView -> o -> o'
runMapPolicy (LegacyMap p) _ o = transcriptPolicy p o
runMapPolicy (BlackboardMap p) bv o = p bv o

adaptSeqPolicy :: TranscriptPolicy i o -> BlackboardView -> i -> o
adaptSeqPolicy pol _ = transcriptPolicy pol

adaptParPolicy :: MergePolicy x y o -> BlackboardView -> x -> y -> o
adaptParPolicy pol _ = mergePolicy pol

-- * Lookup and update history functions ------------------------------------

lookupHistory :: Kont o r -> CID -> [Turn]
lookupHistory kont cid = case kont of
  KEmpty -> []
  KTool _pending _mcid _assistantTurn _toolCalls _toolResults _toolCall k -> lookupHistory k cid
  KSeq1 _workflow2 _pol _site k -> lookupHistory k cid
  KPar1 _i _workflow2 _mergePolicy k -> lookupHistory k cid
  KPar2 _x _mergePolicy _site k -> lookupHistory k cid
  KMap _pol _site k -> lookupHistory k cid
  KUpdateHistory _cid _history k -> lookupHistory k cid
  KLoop _n _workflow _policy cids _site k ->
    case Map.lookup cid cids of
      Nothing -> lookupHistory k cid
      Just history -> history
  KLoopWhile _maxIterations _iteration _workflow _policy _decider _decisionPolicy cids _currentInput _outputsRev _site k ->
    case Map.lookup cid cids of
      Nothing -> lookupHistory k cid
      Just history -> history
  KLoopWhileDecision _maxIterations _iteration _workflow _policy _decider _decisionPolicy cids _nextInput _outputsRev _lastOutput _site k ->
    case Map.lookup cid cids of
      Nothing -> lookupHistory k cid
      Just history -> history
  KPopFrame k -> lookupHistory k cid
  KNest _site k -> lookupHistory k cid
  KCatch _o k -> lookupHistory k cid

updateHistory :: CID -> [Turn] -> Kont o r -> Kont o r
updateHistory cid history kont = case kont of
  KEmpty -> KEmpty
  KTool pending mcid assistantTurn toolCalls toolResults toolCall k ->
    KTool pending mcid assistantTurn toolCalls toolResults toolCall (updateHistory cid history k)
  KSeq1 workflow2 pol site k ->
    KSeq1 workflow2 pol site (updateHistory cid history k)
  KPar1 i workflow2 pol k ->
    KPar1 i workflow2 pol (updateHistory cid history k)
  KPar2 x pol site k ->
    KPar2 x pol site (updateHistory cid history k)
  KMap pol site k ->
    KMap pol site (updateHistory cid history k)
  KLoop n workflow pol cids site k -> case Map.lookup cid cids of
    Nothing -> KLoop n workflow pol cids site (updateHistory cid history k)
    Just _h -> KLoop n workflow pol (Map.insert cid history cids) site k
  KLoopWhile maxIterations iteration workflow pol decider decisionPolicy cids currentInput outputsRev site k -> case Map.lookup cid cids of
    Nothing -> KLoopWhile maxIterations iteration workflow pol decider decisionPolicy cids currentInput outputsRev site (updateHistory cid history k)
    Just _h -> KLoopWhile maxIterations iteration workflow pol decider decisionPolicy (Map.insert cid history cids) currentInput outputsRev site k
  KLoopWhileDecision maxIterations iteration workflow pol decider decisionPolicy cids nextInput outputsRev lastOutput site k -> case Map.lookup cid cids of
    Nothing -> KLoopWhileDecision maxIterations iteration workflow pol decider decisionPolicy cids nextInput outputsRev lastOutput site (updateHistory cid history k)
    Just _h -> KLoopWhileDecision maxIterations iteration workflow pol decider decisionPolicy (Map.insert cid history cids) nextInput outputsRev lastOutput site k
  KUpdateHistory c h k ->
    KUpdateHistory c h (updateHistory cid history k)
  KPopFrame k -> KPopFrame (updateHistory cid history k)
  KNest site k -> KNest site (updateHistory cid history k)
  KCatch o k -> KCatch o (updateHistory cid history k)

stackSize :: Kont o r -> Int
stackSize kont = case kont of
  KEmpty -> 0
  KTool _pending _mcid _assistantTurn _toolCalls _toolResults _toolCall k -> 1 + stackSize k
  KSeq1 _workflow2 _pol _site k -> 1 + stackSize k
  KPar1 _i _workflow2 _mergePolicy k -> 1 + stackSize k
  KPar2 _x _mergePolicy _site k -> 1 + stackSize k
  KMap _pol _site k -> 1 + stackSize k
  KLoop _n _workflow _policy _cids _site k -> 1 + stackSize k
  KUpdateHistory _cid _history k -> 1 + stackSize k
  KPopFrame k -> 1 + stackSize k
  KNest _site k -> 1 + stackSize k
  KCatch _o k -> 1 + stackSize k
  KLoopWhile _maxIterations _iteration _workflow _policy _decider _decisionPolicy _cids _currentInput _outputsRev _site k -> 1 + stackSize k
  KLoopWhileDecision _maxIterations _iteration _workflow _policy _decider _decisionPolicy _cids _nextInput _outputsRev _lastOutput _site k -> 1 + stackSize k

data CatchFrame r where
  CatchFrame :: o -> Kont o r -> CatchFrame r

unwindToCatch :: Kont o r -> Maybe (CatchFrame r)
unwindToCatch kont = case kont of
  KEmpty -> Nothing
  KTool _pending _mcid _assistantTurn _toolCalls _toolResults _toolCall k -> unwindToCatch k
  KSeq1 _workflow2 _pol _site k -> unwindToCatch k
  KPar1 _i _workflow2 _mergePolicy k -> unwindToCatch k
  KPar2 _x _mergePolicy _site k -> unwindToCatch k
  KMap _pol _site k -> unwindToCatch k
  KLoop _n _workflow _policy _cids _site k -> unwindToCatch k
  KLoopWhile _maxIterations _iteration _workflow _policy _decider _decisionPolicy _cids _currentInput _outputsRev _site k -> unwindToCatch k
  KLoopWhileDecision _maxIterations _iteration _workflow _policy _decider _decisionPolicy _cids _nextInput _outputsRev _lastOutput _site k -> unwindToCatch k
  KUpdateHistory _cid _history k -> unwindToCatch k
  KPopFrame k -> unwindToCatch k
  KNest _site k -> unwindToCatch k
  KCatch o k -> Just (CatchFrame o k)

unwindPastTools :: Kont o r -> Kont o r
unwindPastTools kont = case kont of
  KTool _pending _mcid _assistantTurn _toolCalls _toolResults _toolCall k ->
    unwindPastTools (unsafeCoerce k)
  other -> other

extendToolMap :: Maybe SomeSubmit -> ToolMap ToolOutcome -> ToolMap ToolOutcome
extendToolMap Nothing toolMap = toolMap
extendToolMap (Just submit) toolMap =
  Map.insert submit.ssName submit.ssTool toolMap

ensureAgentTool :: Text -> AgentWithModels -> AgentWithModels
ensureAgentTool toolName (AgentWithModels ag models) =
  if toolName `elem` ag.agTools
    then AgentWithModels ag models
    else AgentWithModels ag {agTools = toolName : ag.agTools} models

-- * Show functions for debugging -------------------------------------------

showAgent :: Pending -> Text
showAgent pending = pending.prompt.agent.agent.agName

showStep :: Step o -> Text
showStep step =
  case step of
    RunPrompt pending _mcid -> "RunPrompt " <> showAgent pending
    RunObject pending -> "RunObject " <> showAgent pending
    RunReturn _o -> "RunReturn"
    RunTool _pending _assistantTurn toolCall -> "RunTool " <> toolCall.tcName
    RunThrow _err -> "RunThrow " <> T.pack (show _err)
    RunWorkflow _workflow _i -> "RunWorkflow"
    RunFinish _ -> "RunFinish"

showKont :: Kont o r -> [Text]
showKont kont =
  case kont of
    KEmpty -> []
    KTool pending _mcid _assistantTurn _toolCalls _toolResults toolCall k -> "KTool " <> toolCall.tcName <> " (" <> showAgent pending <> ")" : showKont k
    KSeq1 _workflow2 _pol _site k -> "KSeq1" : showKont k
    KPar1 _i _workflow2 _mergePolicy k -> "KPar1" : showKont k
    KPar2 _x _mergePolicy _site k -> "KPar2" : showKont k
    KMap _pol _site k -> "KMap" : showKont k
    KLoopWhile _maxIterations iteration _workflow _policy _decider _decisionPolicy _cids _currentInput _outputsRev _site k ->
      "KLoopWhile " <> T.pack (show iteration) <> "/" <> T.pack (show _maxIterations) : showKont k
    KLoopWhileDecision _maxIterations iteration _workflow _policy _decider _decisionPolicy _cids _nextInput _outputsRev _lastOutput _site k ->
      "KLoopWhileDecision " <> T.pack (show iteration) <> "/" <> T.pack (show _maxIterations) : showKont k
    KLoop _n _workflow _policy _cids _site k -> "KLoop " <> T.pack (show _n) : showKont k
    KUpdateHistory cid _history k -> "KUpdateHistory " <> T.pack (show cid) : showKont k
    KPopFrame k -> "KPopFrame" : showKont k
    KNest _site k -> "KNest" : showKont k
    KCatch _o k -> "KCatch" : showKont k
