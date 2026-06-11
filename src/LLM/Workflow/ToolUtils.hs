module LLM.Workflow.ToolUtils
  ( createToolContext,
    getSchema,
    toTool,
    windowOffset,
    executeTool,
    toTypedWorkflowTool,
    workflowToolTyped,
    typedWorkflowToolToTool,
  )
where

import Autodocodec qualified as AC
import Autodocodec.Schema (jsonSchemaVia)
import Control.Exception (SomeException (..), try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON)
import Data.Aeson qualified as AE
import Data.Text (Text)
import Data.Text qualified as T
import LLM
  ( Hooks (..),
    Tool (..),
    ToolCall (..),
    ToolContext (..),
    ToolDef (..),
    Turn,
    TypedTool (..),
  )
import LLM.Agent (Agent (..), RuntimeArgs (..))
import LLM.Core.Types (Turn (..))
import LLM.Core.Usage (Usage (..))
import LLM.Workflow.Types
  ( PromptArgs,
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

createToolContext ::
  Agent ->
  [Turn] ->
  Usage ->
  RuntimeArgs ->
  ToolContext
createToolContext agent messages roundUsage rt =
  ToolContext
    { tcConversation = messages,
      tcUsage = roundUsage,
      tcWindowOffset = windowOffset agent.agContextWindow messages,
      tcRuntimeArgs = rt
    }

getSchema :: (AC.HasCodec t, FromJSON t) => tool ToolContext t -> AC.JSONCodec t
getSchema _ = AC.codec

toTypedWorkflowTool :: (AC.HasCodec t, FromJSON t) => TypedTool ToolContext t -> TypedWorkflowTool ToolContext t
toTypedWorkflowTool (TypedTool name descr readonly exec) =
  TypedWorkflowTool
    { twtName = name,
      twtDescription = descr,
      twtReadonly = readonly,
      twtExecute = \ctx args -> ToolReply <$> liftIO (exec ctx args)
    }

class (AC.HasCodec a, FromJSON a) => ToTool t a where
  toTool :: t ToolContext a -> Tool ToolOutcome

instance (AC.HasCodec a, FromJSON a) => ToTool TypedTool a where
  toTool :: TypedTool ToolContext a -> Tool ToolOutcome
  toTool = typedWorkflowToolToTool . toTypedWorkflowTool

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

-- filterReadonlyTools :: Bool -> [Tool m] -> [Tool m]
-- filterReadonlyTools False tools = tools
-- filterReadonlyTools True tools = filter (\x -> x.toolDef.toolReadonly) tools

-- | Compute the index where the visible window starts.
-- The window includes the last @n@ user messages and all turns that follow
-- each of them (assistant replies, tool rounds, etc.).
-- Returns 0 (no windowing) when the window is 'Nothing' or the conversation
-- contains fewer than @n@ user messages.
windowOffset :: Maybe Int -> [Turn] -> Int
windowOffset Nothing _ = 0
windowOffset (Just n) conv = findNthUserFromEnd n conv

-- | Find the index of the Nth 'UserTurn' from the end of a conversation.
-- Returns 0 if there are fewer than @n@ user messages.
findNthUserFromEnd :: Int -> [Turn] -> Int
findNthUserFromEnd 0 _conv = 0
findNthUserFromEnd n conv = go (length conv - 1) n
  where
    go idx remaining
      | idx < 0 = 0
      | remaining <= 0 = idx + 1
      | otherwise = case conv !! idx of
          UserTurn _ -> go (idx - 1) (remaining - 1)
          _ -> go (idx - 1) remaining

-- createGenRequest :: (MonadIO m) => Agent -> RuntimeArgs m -> [Turn] -> GenRequest
-- createGenRequest agent rt messages =
--   let offset = windowOffset agent.agContextWindow messages
--       tools = getResolvedTools agent rt
--    in GenRequest
--         { grSystemPrompt = agent.agSystemPrompt,
--           grTools = map (\x -> x.toolDef) tools,
--           grMessages = drop offset messages,
--           grAbortSignal = rt.rtAbortSignal,
--           grLLMHooks = rt.rtLLMHooks,
--           grHooks = rt.rtHooks
--         }

-- getResolvedTools :: (MonadIO m) => Agent -> RuntimeArgs m -> [Tool m]
-- getResolvedTools agent rt = filterReadonlyTools rt.rtReadonly tools ++ getHistoryTool agent
--   where
--     tools = getToolsFromMap rt.rtToolMap agent.agTools

-- getToolsFromMap :: ToolMap m -> [Text] -> [Tool m]
-- getToolsFromMap toolMap toolNames = toolNames >>= lookupTool
--   where
--     lookupTool name = case Map.lookup name toolMap of
--       Just tool -> [tool]
--       Nothing -> []

-- getHistoryTool :: (MonadIO m) => Agent -> [Tool m]
-- getHistoryTool agent = case agent.agContextWindow of
--   Just n | n > 0 -> [typedWorkflowToolToTool historyToolTyped]
--   _ -> []
