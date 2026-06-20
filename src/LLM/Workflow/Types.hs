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

newtype CID = CID {cid :: UUID}
  deriving (Eq, Ord, Show)

class GetCid a where
  getCid :: a -> [CID]

instance GetCid (Workflow i o) where
  getCid :: forall i' o'. Workflow i' o' -> [CID]
  getCid (WPrompt _ag (Just cid)) = [cid]
  getCid (WAgentSubmit _ _ (Just cid)) = [cid]
  getCid _ = []

data TranscriptPolicy i o where
  TranscriptPolicyFunc :: (i -> o) -> TranscriptPolicy i o
  TranscriptFinalToPromptArgs :: TranscriptPolicy Final PromptArgs
  TranscriptFinalText :: TranscriptPolicy Final Text
  TranscriptSummaryText :: TranscriptPolicy Final Text

data MergePolicy o1 o2 o where
  MergePolicyFunc :: (o1 -> o2 -> o) -> MergePolicy o1 o2 o
  MergePolicyFinalToPromptArgs :: MergePolicy Final Final PromptArgs

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
  WSeq :: Workflow i x -> Workflow y o -> TranscriptPolicy x y -> Workflow i o
  WPar :: Workflow i x -> Workflow i y -> MergePolicy x y o -> Workflow i o
  WLift :: (i -> IO o) -> Workflow i o
  WLiftW :: (i -> IO (Workflow i' o)) -> Workflow (i, i') o
  WMap :: Workflow i o -> TranscriptPolicy o o' -> Workflow i o'
  WLoop :: Int -> Workflow i o -> TranscriptPolicy o i -> [CID] -> Workflow i o
  WLoopWhile :: Int -> Workflow (LoopContext i o) d -> TranscriptPolicy d Bool -> [CID] -> TranscriptPolicy o i -> Workflow i o -> Workflow i o
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
  KSeq1 :: Workflow y o -> TranscriptPolicy x y -> Kont o r -> Kont x r
  KPar1 :: i -> Workflow i y -> MergePolicy x y o -> Kont o r -> Kont x r
  KPar2 :: x -> MergePolicy x y o -> Kont o r -> Kont y r
  KMap :: TranscriptPolicy o o' -> Kont o' r -> Kont o r
  KLoop :: Int -> Workflow i o -> TranscriptPolicy o i -> (Map.Map CID [Turn]) -> Kont o r -> Kont o r
  KLoopWhile :: Int -> Int -> Workflow i o -> TranscriptPolicy o i -> Workflow (LoopContext i o) d -> TranscriptPolicy d Bool -> (Map.Map CID [Turn]) -> i -> [o] -> Kont o r -> Kont o r
  KLoopWhileDecision :: Int -> Int -> Workflow i o -> TranscriptPolicy o i -> Workflow (LoopContext i o) d -> TranscriptPolicy d Bool -> (Map.Map CID [Turn]) -> i -> [o] -> o -> Kont o r -> Kont d r
  KUpdateHistory :: CID -> [Turn] -> Kont o r -> Kont o r
  KCatch :: o -> Kont o r -> Kont o r

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
