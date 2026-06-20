module LLM.Workflow.ToolUtils
  ( executeTool,
    mkSomeSubmit,
    toTypedWorkflowTool,
    workflowToolTyped,
    typedWorkflowToolToTool,
  )
where

import Autodocodec qualified as AC
import Autodocodec.Schema (jsonSchemaVia)
import Control.Exception (SomeException (..), try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as AE
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import LLM
  ( Hooks (..),
    Tool (..),
    ToolCall (..),
    ToolContext (..),
    ToolDef (..),
    TypedTool (..),
  )
import LLM.Agent.ToolUtils (getSchema)
import LLM.Generate (GeneratableObject)
import LLM.Workflow.Types
  ( PromptArgs,
    SomeSubmit (..),
    ToolOutcome (..),
    TypedWorkflowTool (..),
    Workflow,
  )

-- -- | Execute a single tool call by looking it up in the tool list
executeTool :: Hooks -> ToolContext -> [Tool ToolOutcome] -> ToolCall -> IO ToolOutcome
executeTool hooks ctx tools tc = case lookup tc.tcName toolMap of
  Nothing -> pure $ ToolReply ("Unknown tool: " <> tc.tcName)
  Just exec -> do
    liftIO $ hooks.onToolCall tc.tcName (AE.toJSON tc.tcArguments)
    result <- try (exec ctx tc.tcArguments)
    case result of
      Right outcome -> pure outcome
      Left (e :: SomeException) -> do
        liftIO $ hooks.onToolError tc.tcName (T.pack (show e))
        pure $ ToolReply ("Tool error: " <> T.pack (show e))
  where
    toolMap = [(t.toolDef.toolName, t.toolExecute) | t <- tools]

mkSomeSubmit ::
  forall a.
  (GeneratableObject a, FromJSON a, ToJSON a, AC.HasCodec a) =>
  Proxy a ->
  Text ->
  Text ->
  SomeSubmit
mkSomeSubmit _proxy name description =
  let typedTool :: TypedWorkflowTool ToolContext a
      typedTool =
        TypedWorkflowTool
          { twtName = name,
            twtDescription = description,
            twtReadonly = False,
            twtExecute = \_ctx args -> pure (ToolYield (AE.toJSON args))
          }
      tool = typedWorkflowToolToTool typedTool
   in SomeSubmit
        { ssName = name,
          ssDecode =
            \v -> case AE.fromJSON @a v of
              AE.Success a -> Right (AE.toJSON a)
              AE.Error e -> Left (T.pack e),
          ssTool = tool
        }

toTypedWorkflowTool :: (AC.HasCodec t, FromJSON t) => TypedTool ToolContext t -> TypedWorkflowTool ToolContext t
toTypedWorkflowTool (TypedTool name descr readonly exec) =
  TypedWorkflowTool
    { twtName = name,
      twtDescription = descr,
      twtReadonly = readonly,
      twtExecute = \ctx args -> ToolReply <$> liftIO (exec ctx args)
    }

typedWorkflowToolToTool :: (AC.HasCodec t, FromJSON t) => TypedWorkflowTool ToolContext t -> Tool ToolOutcome
typedWorkflowToolToTool t@(TypedWorkflowTool name descr readonly exec) =
  Tool
    { toolDef =
        ToolDef
          { toolName = name,
            toolDescription = descr,
            toolReadonly = readonly,
            toolParameters = AE.toJSON $ jsonSchemaVia $ getSchema t
          },
      toolExecute = \ctx argsvalue ->
        case AE.fromJSON argsvalue of
          AE.Error e -> pure $ ToolReply $ "Error: Parsing arguments failed " <> T.pack (show e)
          AE.Success args -> exec ctx args
    }

workflowToolTyped :: (AC.HasCodec a) => Text -> Text -> (a -> ctx -> (Workflow PromptArgs Text, PromptArgs)) -> TypedWorkflowTool ctx a
workflowToolTyped name description mkWorkflow =
  TypedWorkflowTool
    { twtName = name,
      twtDescription = description,
      twtReadonly = False,
      twtExecute = \ctx args -> do
        let (workflow, promptArgs) = mkWorkflow args ctx
        pure $ ToolWorkflow workflow promptArgs
    }
