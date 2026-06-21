module Main where

import Autodocodec qualified as AC
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON)
import Data.Functor ((<&>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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
    Tool (..),
    ToolContext,
    defaultDebugHooks,
  )
import LLM.Agent.Types (ToolMap)
import LLM.Core.Types (LLMHooks (..), ToolDef (..))
import LLM.Core.Usage (Usage)
import LLM.Generate.ModelConfig (ModelWithFallbacks (..))
import LLM.Generate.Types
  ( GenerateErrorResult (..),
    GenerateTextResult (..),
    StreamChunk (..),
  )
import LLM.Load.FsTools (fsTools')
import LLM.Load.LoadModels (loadModelsOrThrow)
import LLM.Workflow
    ( ToolOutcome(ToolReply, ToolWorkflow),
      AgentWithModels(..),
      CID(..),
      Final(..),
      GetCid(..),
      PromptArgs(..),
      TypedWorkflowTool(..),
      Workflow(..) )
import LLM.Workflow.ToolUtils (typedWorkflowToolToTool)
import LLM.Workflow.Types (AnyLoopDecPolicy, AnyLoopFeedPolicy, TranscriptPolicy (TranscriptFinalText))
import LLM.Workflow.Utils (mapPolicy)
import LLM.Workflow.Workflow
  ( runWorkflow,
  )
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

  let wf1 = buildWf1Workflow (_models4, _models4)
      p1 =
        "Audit the project in the current workspace: identify correctness, safety, and maintainability risks, \
        \with actionable recommendations and a concise final report."

  subagentUsed <- newIORef False
  toolMap <-
    fsTools' ToolReply "./user-workspace/"
      <&> addTools
        [ typedWorkflowToolToTool $
            subagentOnce
              subagentUsed
              wf1
              p1
              "subagent"
              "Run a complete workspace audit workflow (one shot only). Call exactly once, then use writefile."
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
        Just $
          T.unlines
            [ "You are an orchestrator with two tools:",
              "- subagent: runs a complete audit workflow. Call it ONCE with the full audit request. Notice that the subagent has no writefile tool. Do not call subagent again.",
              "- writefile: saves text to a file. use after subagent has completed.",
              "Procedure:",
              "1. Call subagent once.",
              "2. Call writefile once with path audit_report.txt and a concise summary of the audit.",
              "3. Reply with a one-line confirmation. Do not call any more tools.",
              "4. Quit."
            ],
      agTools = ["subagent", "writefile"],
      agMaxToolRounds = 3,
      agContextWindow = Nothing
    }

-- ---------------------------------------------------------------------------
-- utils - move them to separate files later
-- ---------------------------------------------------------------------------

mkAgent :: Agent -> ModelWithFallbacks -> Bool -> IO (Workflow PromptArgs Final)
mkAgent ag models False = pure $ WPrompt (AgentWithModels ag models) Nothing
mkAgent ag models True = do
  cid <- CID <$> generate
  pure $ WPrompt (AgentWithModels ag models) (Just cid)

mkLoop :: (GetCid x) => Int -> AnyLoopFeedPolicy i o -> [x] -> Workflow i o -> Workflow i o
mkLoop n policy scope wf = WLoop n wf policy cids
  where
    cids = concatMap getCid scope :: [CID]

mkLoopWhile :: (GetCid x) => Int -> AnyLoopFeedPolicy i o -> Workflow PromptArgs d -> AnyLoopDecPolicy d -> [x] -> Workflow i o -> Workflow i o
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
    AC.object "subagent invocation" $
      SubagentArgs
        <$> AC.requiredField "prompt" "Brief note for logging; the full audit task is fixed." AC..= (\x -> x.prompt)

subagentOnce ::
  IORef Bool ->
  Workflow PromptArgs Final ->
  Text ->
  Text ->
  Text ->
  TypedWorkflowTool ToolContext SubagentArgs
subagentOnce usedRef wf auditPrompt name description =
  TypedWorkflowTool name description False $ \_ctx _args ->
    liftIO $ do
      used <- readIORef usedRef
      if used
        then
          pure $
            ToolReply
              "Subagent audit already completed. Do NOT call subagent again. \
              \Call writefile with path audit_report.txt and a concise summary, \
              \then reply with a short confirmation and no further tool calls."
        else do
          writeIORef usedRef True
          pure $
            ToolWorkflow
              (WMap wf (mapPolicy TranscriptFinalText))
              PromptArgs {history = [], prompt = auditPrompt}

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
