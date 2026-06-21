module LLM.Workflow.Types where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.UUID.Types (UUID)
import LLM (Tool (..))
import LLM.Agent (Agent (..))
import LLM.Core.Types
  ( ToolCall (..),
    ToolResult (..),
    Turn (..),
  )
import LLM.Core.Usage (Usage)
import LLM.Generate (GeneratableObject, GenerateError, ModelWithFallbacks)

data ToolOutcome
  = ToolReply Text
  | ToolWorkflow (Workflow PromptArgs Text) PromptArgs
  | ToolYield Value

data SomeSubmit = SomeSubmit
  { ssName :: Text,
    ssDecode :: Value -> Either Text Value,
    ssTool :: Tool ToolOutcome
  }

data PromptArgs = PromptArgs
  { history :: [Turn],
    prompt :: Text
  }
  deriving (Show)

data Prompt = Prompt
  { agent :: AgentWithModels,
    prompt :: Text,
    history :: [Turn]
  }

instance Show Prompt where
  show Prompt {agent, prompt, history} = "Prompt {agent = " <> show agent <> ", prompt = " <> show prompt <> ", history = " <> show history <> "}"

data Pending = Pending
  { prompt :: Prompt,
    toolRounds :: [Turn],
    submitTool :: Maybe SomeSubmit
  }

instance Show Pending where
  show Pending {prompt, toolRounds, submitTool} =
    "Pending {prompt = "
      <> show prompt
      <> ", toolRounds = "
      <> show toolRounds
      <> ", submitTool = "
      <> show (fmap (.ssName) submitTool)
      <> "}"

data Final = Final
  { prompt :: Maybe Prompt,
    history :: [Turn],
    newMessages :: [Turn],
    text :: Text
  }

instance Show Final where
  show f = "Final {text = " <> show f.text <> "}"

-- ---------------------------------------------------------------------------
-- Blackboard types
-- ---------------------------------------------------------------------------

newtype Label = Label {unLabel :: Text}
  deriving (Eq, Ord, Show)

data Path
  = Root
  | Child Path Label
  deriving (Eq, Ord, Show)

data Instance = Instance
  { instPath :: Path,
    instIters :: [Int]
  }
  deriving (Eq, Ord, Show)

data CellStatus
  = CellPending
  | CellRunning
  | CellDone
  | CellFailed
  | CellCaught
  deriving (Eq, Ord, Show, Enum, Bounded)

data NodeOutput
  = OutFinal Final
  | OutText Text
  | OutValue Value

instance Show NodeOutput where
  show = \case
    OutFinal f -> "OutFinal " <> show f
    OutText t -> "OutText " <> show t
    OutValue _ -> "OutValue <json>"

data Cell = Cell
  { cellStatus :: CellStatus,
    cellOutput :: Maybe NodeOutput,
    cellRawOutput :: Maybe NodeOutput,
    cellHistory :: [Turn],
    cellAgent :: Maybe Text,
    cellUsage :: Usage,
    cellError :: Maybe GenerateError
  }
  deriving (Show)

data LoopKind = FixedCount | While
  deriving (Eq, Ord, Show)

data LoopSlice = LoopSlice
  { lsKind :: LoopKind,
    lsIteration :: Int,
    lsMax :: Int,
    lsSlots :: Map.Map SlotKey [Turn],
    lsPersistScope :: [SlotKey],
    lsOutputs :: [NodeOutput],
    lsLatestOutput :: Maybe NodeOutput,
    lsInput :: PromptArgs,
    lsNextInput :: Maybe PromptArgs,
    lsDecisions :: [Bool]
  }
  deriving (Show)

data Blackboard = Blackboard
  { bbCells :: Map.Map Instance Cell,
    bbSlices :: Map.Map Path LoopSlice,
    bbInput :: PromptArgs,
    bbUsage :: Usage,
    bbCurrent :: Maybe Instance
  }
  deriving (Show)

data Trigger
  = TriggerSeq
  | TriggerParLeft
  | TriggerParRight
  | TriggerParMerge
  | TriggerLoopBody
  | TriggerLoopFeedback
  | TriggerLoopDecider
  | TriggerMap
  | TriggerCatch
  deriving (Eq, Ord, Show)

data BlackboardView = BlackboardView
  { bvBoard :: Blackboard,
    bvLocal :: Path,
    bvSelf :: Maybe Instance,
    bvPredecessor :: Maybe Instance,
    bvTrigger :: Trigger,
    bvPathStack :: PathStack,
    bvInstIters :: [Int],
    bvLabelEnv :: LabelEnv
  }
  deriving (Show)

data PolicySite = PolicySite
  { psLocal :: Path,
    psTrigger :: Trigger,
    psPredecessor :: Maybe Instance,
    psSelf :: Maybe Instance
  }
  deriving (Show)

data CompositeKind = CompSeq | CompPar | CompLoop | CompMap | CompCatch | CompNest
  deriving (Eq, Ord, Show)

data Side = SideLeft | SideRight
  deriving (Eq, Ord, Show)

data PathFrame
  = FrameRoot
  | FrameComposite CompositeKind Label Path
  | FrameScope Label Path
  | FrameLeaf Label Path
  | FrameParSide Label Side Path
  deriving (Eq, Show)

type PathStack = [PathFrame]

data RunnerState o = RunnerState
  { rsBlackboard :: Blackboard,
    rsPathStack :: PathStack,
    rsInstIters :: [Int],
    rsLabelEnv :: LabelEnv,
    rsCurrentLeaf :: Maybe (Path, [Int]),
    rsStack :: o
  }
  deriving (Show)

data HistoryMode
  = HistoryEphemeral
  | HistoryPersist SlotKey
  deriving (Eq, Ord, Show)

data SlotKey
  = SlotByCid CID
  | SlotByLabel Label
  deriving (Eq, Ord, Show)

data LabelEnv = LabelEnv
  { leChildPaths :: Map.Map (Path, Label) Path,
    leNodePath :: Map.Map Label Path
  }
  deriving (Show)

data LabelEnvError
  = DuplicateLabel Path Label
  deriving (Show, Eq)

class GetSlotKeys a where
  getSlotKeys :: a -> [SlotKey]

instance GetSlotKeys SlotKey where
  getSlotKeys = pure

-- ---------------------------------------------------------------------------
-- CID / policies
-- ---------------------------------------------------------------------------

newtype CID = CID {cid :: UUID}
  deriving (Eq, Ord, Show)

class GetCid a where
  getCid :: a -> [CID]

instance GetCid (Workflow i o) where
  getCid :: forall i' o'. Workflow i' o' -> [CID]
  getCid (WPrompt _ag (Just cid)) = [cid]
  getCid (WAgentSubmit _ _ (Just cid)) = [cid]
  getCid _ = []

instance GetSlotKeys (Workflow i o) where
  getSlotKeys (WPrompt _ (Just cid)) = [SlotByCid cid]
  getSlotKeys (WAgentSubmit _ _ (Just cid)) = [SlotByCid cid]
  getSlotKeys _ = []

data TranscriptPolicy i o where
  TranscriptPolicyFunc :: (i -> o) -> TranscriptPolicy i o
  TranscriptFinalToPromptArgs :: TranscriptPolicy Final PromptArgs
  TranscriptFinalText :: TranscriptPolicy Final Text
  TranscriptSummaryText :: TranscriptPolicy Final Text

data MergePolicy o1 o2 o where
  MergePolicyFunc :: (o1 -> o2 -> o) -> MergePolicy o1 o2 o
  MergePolicyFinalToPromptArgs :: MergePolicy Final Final PromptArgs

-- Blackboard-aware policy types
type SeqPolicy i o = BlackboardView -> i -> o
type ParPolicy x y o = BlackboardView -> x -> y -> o
type LoopFeedPolicy i o = BlackboardView -> LoopSlice -> o -> i
type LoopDecPolicy d = BlackboardView -> LoopSlice -> d -> Bool
type MapPolicy o o' = BlackboardView -> o -> o'
type SeqPolicyBB o = BlackboardView -> o

data AnySeqPolicy i o where
  LegacyTranscript :: TranscriptPolicy i o -> AnySeqPolicy i o
  BlackboardSeq :: SeqPolicy i o -> AnySeqPolicy i o
  BlackboardSeqOnly :: SeqPolicyBB o -> AnySeqPolicy i o

data AnyMergePolicy o1 o2 o where
  LegacyMerge :: MergePolicy o1 o2 o -> AnyMergePolicy o1 o2 o
  BlackboardPar :: ParPolicy o1 o2 o -> AnyMergePolicy o1 o2 o

data AnyLoopFeedPolicy i o where
  LegacyLoopFeed :: TranscriptPolicy o i -> AnyLoopFeedPolicy i o
  BlackboardLoopFeed :: LoopFeedPolicy i o -> AnyLoopFeedPolicy i o

data AnyLoopDecPolicy d where
  LegacyLoopDec :: TranscriptPolicy d Bool -> AnyLoopDecPolicy d
  BlackboardLoopDec :: LoopDecPolicy d -> AnyLoopDecPolicy d

data AnyMapPolicy o o' where
  LegacyMap :: TranscriptPolicy o o' -> AnyMapPolicy o o'
  BlackboardMap :: MapPolicy o o' -> AnyMapPolicy o o'

data LoopContext i o = LoopContext
  { lcIteration :: Int,
    lcMaxIterations :: Int,
    lcInput :: i,
    lcNextInput :: i,
    lcOutput :: o,
    lcOutputs :: [o]
  }

data Workflow i o where
  WPrompt :: AgentWithModels -> Maybe CID -> Workflow PromptArgs Final
  WObject :: (GeneratableObject a) => AgentWithModels -> Workflow PromptArgs a
  WAgentSubmit ::
    (GeneratableObject a, FromJSON a, ToJSON a, AC.HasCodec a) =>
    Text ->
    AgentWithModels ->
    Maybe CID ->
    Workflow PromptArgs a
  WLabel :: Label -> Workflow i o -> Workflow i o
  WSeq :: Workflow i x -> Workflow y o -> AnySeqPolicy x y -> Workflow i o
  WPar :: Workflow i x -> Workflow i y -> AnyMergePolicy x y o -> Workflow i o
  WLift :: (i -> IO o) -> Workflow i o
  WLiftW :: (i -> IO (Workflow i' o)) -> Workflow (i, i') o
  WBlackboardPrompt :: (BlackboardView -> PromptArgs) -> AgentWithModels -> Workflow i Final
  WMap :: Workflow i o -> AnyMapPolicy o o' -> Workflow i o'
  WLoop :: Int -> Workflow i o -> AnyLoopFeedPolicy i o -> [CID] -> Workflow i o
  WLoopWhile :: Int -> Workflow PromptArgs d -> AnyLoopDecPolicy d -> [CID] -> AnyLoopFeedPolicy i o -> Workflow i o -> Workflow i o
  WCatch :: o -> Workflow i o -> Workflow i o

data Step o where
  RunPrompt :: Pending -> Maybe CID -> Step Final
  RunObject :: (GeneratableObject a) => Pending -> Step a
  RunReturn :: o -> Step o
  RunTool :: Pending -> Turn -> ToolCall -> Step Text
  RunThrow :: GenerateError -> Step o
  RunWorkflow :: Workflow i o -> i -> Step o
  RunFinish :: Either GenerateError o -> Step o

data Kont o r where
  KEmpty :: Kont o r
  KTool :: Pending -> Maybe CID -> Turn -> [ToolCall] -> [ToolResult] -> ToolCall -> Kont Final r -> Kont Text r
  KSeq1 :: Workflow y o -> AnySeqPolicy x y -> PolicySite -> Kont o r -> Kont x r
  KPar1 :: i -> Workflow i y -> AnyMergePolicy x y o -> Kont o r -> Kont x r
  KPar2 :: x -> AnyMergePolicy x y o -> PolicySite -> Kont o r -> Kont y r
  KMap :: AnyMapPolicy o o' -> PolicySite -> Kont o' r -> Kont o r
  KLoop :: Int -> Workflow i o -> AnyLoopFeedPolicy i o -> (Map.Map CID [Turn]) -> PolicySite -> Kont o r -> Kont o r
  KLoopWhile :: Int -> Int -> Workflow i o -> AnyLoopFeedPolicy i o -> Workflow PromptArgs d -> AnyLoopDecPolicy d -> (Map.Map CID [Turn]) -> i -> [o] -> PolicySite -> Kont o r -> Kont o r
  KLoopWhileDecision :: Int -> Int -> Workflow i o -> AnyLoopFeedPolicy i o -> Workflow PromptArgs d -> AnyLoopDecPolicy d -> (Map.Map CID [Turn]) -> i -> [o] -> o -> PolicySite -> Kont o r -> Kont d r
  KUpdateHistory :: CID -> [Turn] -> Kont o r -> Kont o r
  KCatch :: o -> Kont o r -> Kont o r
  KPopFrame :: Kont o r -> Kont o r
  KNest :: PolicySite -> Kont o r -> Kont o r

data Stack r where
  Stack :: Usage -> (Step o) -> (Kont o r) -> Stack r

data TypedWorkflowTool c a = TypedWorkflowTool
  { twtName :: Text,
    twtDescription :: Text,
    twtReadonly :: Bool,
    twtExecute :: c -> a -> IO ToolOutcome
  }

data AgentWithModels = AgentWithModels
  { agent :: Agent,
    models :: ModelWithFallbacks
  }

instance Show AgentWithModels where
  show AgentWithModels {agent} = "AgentWithModels {agent = " <> show agent.agName <> "}"
