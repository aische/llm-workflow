{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Autodocodec qualified as AC
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException (SomeException), catch)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import Heptapod (generate)
import LLM
  ( AbortSignal,
    Agent (..),
    GenerateEvent,
    Hooks (..),
    RuntimeArgs (..),
    ThinkingMode (..),
    Tool (..),
    ToolContext,
    claudeGateway,
    deepSeekGateway,
    defaultDebugHooks,
    mkFsConfig,
  )
import LLM.Agent.Types (ToolMap)
import LLM.Core.Types (LLMHooks (..), ToolDef (..), Turn (UserTurn))
import LLM.Core.Usage (PricingInfo (..), Usage)
import LLM.Generate.Logger (noHooks)
import LLM.Generate.ModelConfig (ModelConfig (..), ModelWithFallbacks (..))
import LLM.Generate.Types
  ( GenerateErrorResult (..),
    GenerateTextResult (..),
    StreamChunk (..),
  )
import LLM.Load.FsTools (fsTools')
import LLM.Load.LoadModels (loadModelsOrThrow)
import LLM.Workflow (ToolOutcome (ToolReply), emptyFinal)
import LLM.Workflow.ToolUtils (typedWorkflowToolToTool, workflowToolTyped)
import LLM.Workflow.Types
  ( AgentWithModels (..),
    CID (CID),
    Final (..),
    GetCid (..),
    Kont,
    LoopContext (..),
    Prompt (..),
    PromptArgs (..),
    TranscriptPolicy (TranscriptFinalText, TranscriptFinalToPromptArgs, TranscriptPolicyFunc, TranscriptSummaryText),
    TypedWorkflowTool,
    Workflow (..),
  )
import LLM.Workflow.Workflow
  ( eval,
    loop,
    runWorkflow,
  )
import System.Environment (getArgs, getEnv)
import Wf1 (buildWf1Workflow)

main :: IO ()
main = do
  (_gpt, llama, _haiku, gemini, mistral, deepseek) <-
    loadModelsOrThrow
      "./model-catalog.json"
      ("gpt_4_1", "llama_3_2", "haiku_4_5", "gemini_2_5_flash", "mistral", "deepseek4flash")

  let _models1 = ModelWithFallbacks {mwfModel = llama, mwfFallbacks = []}
      _models2 = ModelWithFallbacks {mwfModel = mistral, mwfFallbacks = []}
      _models3 = ModelWithFallbacks {mwfModel = gemini, mwfFallbacks = []}
      _models4 = ModelWithFallbacks {mwfModel = deepseek, mwfFallbacks = []}
  -- _models4 = ModelWithFallbacks {mwfModel = deepseek, mwfFallbacks = [gpt, gemini, haiku]}

  let wf1 = buildWf1Workflow (_models4, _models4)
      p1 =
        "Audit the project in the current workspace: identify correctness, safety, and maintainability risks, \
        \with actionable recommendations and a concise final report."

  toolMap <-
    fsTools' ToolReply "./user-workspace/" -- put some code files in this directory
      <&> addTools
        [ typedWorkflowToolToTool $
            subagent "subagent" "Use this tool to gain expert knowledge about a topic. Provide a topic." $
              \args _ctx ->
                (WMap wf1 TranscriptSummaryText, PromptArgs {history = [], prompt = "Ask the expert about the topic: " <> args.prompt})
        ]
  let orchestrator = WPrompt (AgentWithModels orchestratorAgent _models4) Nothing
  (t, usage) <- run Nothing toolMap p1 orchestrator
  TIO.putStrLn t
  TIO.putStrLn $ "Usage: " <> T.pack (show usage)

orchestratorAgent :: Agent
orchestratorAgent =
  Agent
    { agName = "orchestrator",
      agSystemPrompt =
        Just
          "You are a helpful assistant. You may delegate work using tools:\n\
          \- subagent: filesystem-capable child agent for a single task",
      agTools = ["subagent"],
      agMaxToolRounds = 5,
      agContextWindow = Just 3
    }

-- ---------------------------------------------------------------------------
-- utils - move them to separate files later
-- ---------------------------------------------------------------------------

mkAgent :: Agent -> ModelWithFallbacks -> Bool -> IO (Workflow PromptArgs Final)
mkAgent ag models False = pure $ WPrompt (AgentWithModels ag models) Nothing
mkAgent ag models True = do
  cid <- CID <$> generate
  pure $ WPrompt (AgentWithModels ag models) (Just cid)

mkLoop :: (GetCid x) => Int -> TranscriptPolicy o i -> [x] -> Workflow i o -> Workflow i o
mkLoop n policy scope wf = WLoop n wf policy cids
  where
    cids = concatMap getCid scope :: [CID]

mkLoopWhile :: (GetCid x) => Int -> TranscriptPolicy o i -> Workflow (LoopContext i o) d -> TranscriptPolicy d Bool -> [x] -> Workflow i o -> Workflow i o
mkLoopWhile maxIterations bodyPolicy decider decisionPolicy scope = WLoopWhile maxIterations decider decisionPolicy cids bodyPolicy
  where
    cids = concatMap getCid scope :: [CID]

addTools :: [Tool ToolOutcome] -> ToolMap ToolOutcome -> ToolMap ToolOutcome
addTools tools toolMap = toolMap <> Map.fromList [(tool.toolDef.toolName, tool) | tool <- tools]

newtype SubagentArgs = SubagentArgs
  { prompt :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec SubagentArgs)

instance AC.HasCodec SubagentArgs where
  codec :: AC.JSONCodec SubagentArgs
  codec =
    AC.object "precise prompt for the subagent" $
      SubagentArgs <$> AC.requiredField "prompt" "a precise prompt for the subagent" AC..= (\x -> x.prompt)

subagent :: Text -> Text -> (SubagentArgs -> ToolContext -> (Workflow PromptArgs Text, PromptArgs)) -> TypedWorkflowTool ToolContext SubagentArgs
subagent = workflowToolTyped

printGenerateResult :: Either GenerateErrorResult GenerateTextResult -> IO ()
printGenerateResult = \case
  Left err -> do
    putStrLn "Generation failed:"
    print err
  Right ok -> do
    putStrLn "Final text:"
    TIO.putStrLn ok.gtrText
    putStrLn "Usage:"
    print ok.gtrUsage

onStreamChunk :: StreamChunk -> IO ()
onStreamChunk = \case
  AnswerDelta txt -> TIO.putStr txt
  ReasoningDelta txt -> TIO.putStr txt
  PreambleDelta txt -> TIO.putStr txt
  StreamToolCallChunk _ -> pure ()

printEvent :: GenerateEvent -> IO ()
printEvent ev = do
  putStrLn "--------------------------------"
  print ev

llmHooks :: Hooks -> LLMHooks
llmHooks hooks =
  LLMHooks
    { onLLMRequest = hooks.onRequest,
      onLLMResponse = hooks.onResponse,
      onLLMResponseError = hooks.onResponseError
    }

run :: Maybe AbortSignal -> ToolMap ToolOutcome -> Text -> Workflow PromptArgs Final -> IO (Text, Usage)
run abortSignal toolMap prompt wf = do
  genId <- generate
  let rt =
        RuntimeArgs
          { rtGenerationId = genId,
            rtAbortSignal = abortSignal,
            rtLLMHooks = llmHooks defaultDebugHooks,
            rtHooks =
              defaultDebugHooks
                { onLog = \level msg -> TIO.putStrLn ("[" <> T.pack (show level) <> "] " <> msg)
                },
            rtOnEvent = printEvent,
            rtReadonly = False
          }
  r <- runWorkflow toolMap rt wf (PromptArgs {history = [], prompt})
  case r of
    (Left err, usage) -> pure ("Error: " <> T.pack (show err), usage)
    (Right final, usage) -> pure (final.text, usage)
