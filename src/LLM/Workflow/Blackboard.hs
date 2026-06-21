module LLM.Workflow.Blackboard
  ( module LLM.Workflow.Types,
    emptyBlackboard,
    emptyCell,
    emptyLoopSlice,
    emptyLabelEnv,
    appendLoopOutput,
    cellPartialHistory,
    initialRunnerState,
    lookupSlot,
    updateSlot,
    syntheticLabel,
    loopBodyPath,
    loopDeciderPath,
    currentPath,
    currentInstance,
    innermostLoopPath,
    innermostLoopPathFromStack,
    scopeRoot,
    atPath,
    atLabel,
    globalPath,
    globalLabel,
    atBodyLabel,
    cellFinal,
    cellHistory,
    cellText,
    cellOutputJson,
    nodeOutputText,
    loopOutputTexts,
    labelPath,
    loopPathFromBodyAgent,
    localCompleted,
    predecessor,
    parBranch,
    loopSlice,
    loopSliceAt,
    completedIterationsAt,
    latestCompletedIterationAt,
    priorIterations,
    showBlackboard,
    showPathStack,
    showRunnerState,
    showPath,
    showInstance,
    buildLabelEnv,
    extendLabelEnv,
    labelEnvResolve,
    labelEnvRolePath,
    labelEnvPaths,
    collectSlotKeys,
  )
where

import Control.Monad (foldM)
import Data.Aeson (Value, encode)
import Data.ByteString.Lazy qualified as BL
import Data.List (find, sortOn)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import LLM.Core.Usage (emptyUsage)
import LLM.Core.Types (Turn)
import LLM.Workflow.Types
import Unsafe.Coerce (unsafeCoerce)

-- ---------------------------------------------------------------------------
-- Constructors
-- ---------------------------------------------------------------------------

emptyCell :: Cell
emptyCell =
  Cell
    { cellStatus = CellPending,
      cellOutput = Nothing,
      cellRawOutput = Nothing,
      cellHistory = [],
      cellAgent = Nothing,
      cellUsage = emptyUsage,
      cellError = Nothing
    }

emptyLoopSlice :: LoopKind -> Int -> PromptArgs -> LoopSlice
emptyLoopSlice kind maxIter input =
  LoopSlice
    { lsKind = kind,
      lsIteration = 1,
      lsMax = maxIter,
      lsSlots = Map.empty,
      lsPersistScope = [],
      lsOutputs = [],
      lsLatestOutput = Nothing,
      lsInput = input,
      lsNextInput = Nothing,
      lsDecisions = []
    }

emptyBlackboard :: PromptArgs -> Blackboard
emptyBlackboard input =
  Blackboard
    { bbCells = Map.empty,
      bbSlices = Map.empty,
      bbInput = input,
      bbUsage = emptyUsage,
      bbCurrent = Nothing
    }

emptyLabelEnv :: LabelEnv
emptyLabelEnv = LabelEnv Map.empty Map.empty

appendLoopOutput :: NodeOutput -> LoopSlice -> LoopSlice
appendLoopOutput out slice =
  let outputs = slice.lsOutputs ++ [out]
   in slice
        { lsOutputs = outputs,
          lsLatestOutput = Just out
        }

initialRunnerState :: PromptArgs -> o -> LabelEnv -> RunnerState o
initialRunnerState input stack labelEnv =
  RunnerState
    { rsBlackboard = emptyBlackboard input,
      rsPathStack = [FrameRoot],
      rsInstIters = [],
      rsLabelEnv = labelEnv,
      rsStack = stack
    }

syntheticLabel :: Int -> Label
syntheticLabel n = Label ("_" <> T.pack (show n))

-- ---------------------------------------------------------------------------
-- Cell helpers
-- ---------------------------------------------------------------------------

cellPartialHistory :: Cell -> Maybe [Turn]
cellPartialHistory cell
  | cell.cellStatus == CellRunning = Just cell.cellHistory
  | otherwise = Nothing

isCompleted :: Cell -> Bool
isCompleted cell = cell.cellStatus `elem` [CellDone, CellCaught]

-- ---------------------------------------------------------------------------
-- History persistence
-- ---------------------------------------------------------------------------

collectSlotKeys :: (GetSlotKeys a) => [a] -> [SlotKey]
collectSlotKeys = concatMap getSlotKeys

findOwningLoopPath :: Blackboard -> PathStack -> SlotKey -> Maybe Path
findOwningLoopPath bb stack key =
  let loopPaths =
        [ path
          | FrameComposite CompLoop _ path <- reverse stack
        ]
   in find
        ( \path ->
            case Map.lookup path bb.bbSlices of
              Nothing -> False
              Just slice -> key `elem` slice.lsPersistScope
        )
        loopPaths

lookupSlot :: Blackboard -> PathStack -> SlotKey -> [Turn]
lookupSlot bb stack key =
  case findOwningLoopPath bb stack key of
    Nothing -> []
    Just path ->
      case Map.lookup path bb.bbSlices of
        Nothing -> []
        Just slice -> Map.findWithDefault [] key slice.lsSlots

updateSlot :: Blackboard -> PathStack -> SlotKey -> [Turn] -> Blackboard
updateSlot bb stack key turns =
  case findOwningLoopPath bb stack key of
    Nothing -> bb
    Just path ->
      bb
        { bbSlices =
            Map.adjust
              (\slice -> slice {lsSlots = Map.insert key turns slice.lsSlots})
              path
              bb.bbSlices
        }

-- ---------------------------------------------------------------------------
-- Path helpers
-- ---------------------------------------------------------------------------

loopBodyPath :: Path -> Path
loopBodyPath loopP = Child loopP (Label "body")

loopDeciderPath :: Path -> Path
loopDeciderPath loopP = Child loopP (Label "decider")

currentPath :: PathStack -> Maybe Path
currentPath [] = Nothing
currentPath (FrameRoot : _) = Just Root
currentPath (FrameComposite _ _ path : _) = Just path
currentPath (FrameScope _ path : _) = Just path
currentPath (FrameLeaf _ path : _) = Just path
currentPath (FrameParSide _ _ path : _) = Just path

currentInstance :: PathStack -> [Int] -> Maybe Instance
currentInstance stack iters = Instance <$> currentPath stack <*> pure iters

innermostLoopPathFromStack :: PathStack -> Maybe Path
innermostLoopPathFromStack =
  findLoopPath . reverse
  where
    findLoopPath [] = Nothing
    findLoopPath (FrameComposite CompLoop _ path : _) = Just path
    findLoopPath (_ : rest) = findLoopPath rest

innermostLoopPath :: BlackboardView -> Maybe Path
innermostLoopPath bv = innermostLoopPathFromStack bv.bvPathStack

isDescendantOf :: Path -> Path -> Bool
isDescendantOf child parent =
  child == parent || case child of
    Child p _ -> isDescendantOf p parent
    Root -> False

isAncestorOf :: Path -> Path -> Bool
isAncestorOf ancestor path = isDescendantOf path ancestor

-- ---------------------------------------------------------------------------
-- Query API
-- ---------------------------------------------------------------------------

labelPath :: BlackboardView -> Label -> Maybe Path
labelPath bv lbl = Map.lookup lbl bv.bvLabelEnv.leNodePath

loopPathFromBodyAgent :: LabelEnv -> Label -> Maybe Path
loopPathFromBodyAgent env lbl =
  Map.lookup lbl env.leNodePath >>= \agentPath ->
    case agentPath of
      Child (Child loopP (Label "body")) _ -> Just loopP
      _ -> Nothing

scopeRoot :: BlackboardView -> Path
scopeRoot bv = bv.bvLocal

lookupCell :: Blackboard -> Instance -> Maybe Cell
lookupCell bb inst = Map.lookup inst bb.bbCells

lookupCompletedCell :: Blackboard -> Instance -> Maybe Cell
lookupCompletedCell bb inst =
  case lookupCell bb inst of
    Just cell | isCompleted cell -> Just cell
    _ -> Nothing

atPath :: BlackboardView -> Path -> Maybe Cell
atPath bv path =
  let inst = Instance path bv.bvInstIters
   in lookupCompletedCell bv.bvBoard inst

atLabel :: BlackboardView -> Label -> Maybe Cell
atLabel bv lbl = atPath bv (Child (scopeRoot bv) lbl)

atBodyLabel :: BlackboardView -> Label -> Maybe Cell
atBodyLabel bv lbl =
  case innermostLoopPath bv of
    Just loopP -> atPath bv (Child (loopBodyPath loopP) lbl)
    Nothing ->
      case labelPath bv lbl of
        Nothing -> Nothing
        Just path ->
          case atPath bv path of
            Just cell -> Just cell
            Nothing -> latestCompletedIterationAt bv path

globalPath :: BlackboardView -> Path -> Maybe Cell
globalPath bv path =
  lookupCompletedCell bv.bvBoard (Instance path [])

globalLabel :: BlackboardView -> Label -> Maybe Cell
globalLabel bv lbl =
  case Map.lookup lbl bv.bvLabelEnv.leNodePath of
    Just path -> globalPath bv path
    Nothing -> Nothing

cellFinal :: Cell -> Final
cellFinal cell =
  case cell.cellOutput of
    Just (OutFinal f) -> f
    Just (OutText t) -> Final {prompt = Nothing, history = [], newMessages = [], text = t}
    _ -> Final {prompt = Nothing, history = [], newMessages = [], text = ""}

cellHistory :: Cell -> [Turn]
cellHistory = (.cellHistory)

cellText :: Cell -> Text
cellText cell = (cellFinal cell).text

cellOutputJson :: Cell -> Maybe Value
cellOutputJson cell =
  case cell.cellOutput of
    Just (OutValue v) -> Just v
    _ -> Nothing

nodeOutputText :: NodeOutput -> Text
nodeOutputText = \case
  OutFinal f -> f.text
  OutText t -> t
  OutValue v -> decodeUtf8 (BL.toStrict (encode v))

loopOutputTexts :: LoopSlice -> [Text]
loopOutputTexts = map nodeOutputText . (.lsOutputs)

localCompleted :: BlackboardView -> [Cell]
localCompleted bv =
  Map.elems $
    Map.filterWithKey
      ( \inst cell ->
          inst.instPath `isDescendantOf` scopeRoot bv
            && inst.instIters == bv.bvInstIters
            && isCompleted cell
      )
      bv.bvBoard.bbCells

predecessor :: BlackboardView -> Maybe Cell
predecessor bv =
  case bv.bvPredecessor of
    Nothing -> Nothing
    Just inst -> lookupCompletedCell bv.bvBoard inst

parBranch :: BlackboardView -> Side -> Maybe Cell
parBranch bv side =
  let local = scopeRoot bv
      idx = case side of SideLeft -> 0; SideRight -> 1
      branchPath = Child local (syntheticLabel idx)
   in deepestCompletedUnder bv branchPath

deepestCompletedUnder :: BlackboardView -> Path -> Maybe Cell
deepestCompletedUnder bv path =
  fmap snd (listToMaybe (sortOn (Down . fst) candidates))
  where
    candidates =
      [ (pathDepth path inst.instPath, cell)
        | (inst, cell) <- Map.toList bv.bvBoard.bbCells,
          inst.instIters == bv.bvInstIters,
          isCompleted cell,
          path `isAncestorOf` inst.instPath
      ]
    pathDepth :: Path -> Path -> Int
    pathDepth ancestor candidate =
      if ancestor == candidate
        then 0
        else case candidate of
          Child parent _ -> 1 + pathDepth ancestor parent
          Root -> 0

loopSlice :: BlackboardView -> Maybe LoopSlice
loopSlice bv =
  case innermostLoopPath bv of
    Just path -> loopSliceAt bv path
    Nothing -> Nothing

loopSliceAt :: BlackboardView -> Path -> Maybe LoopSlice
loopSliceAt bv path = Map.lookup path bv.bvBoard.bbSlices

completedIterationsAt :: BlackboardView -> Path -> [Cell]
completedIterationsAt bv path =
  map snd $
    sortOn fst
      [ (inst.instIters, cell)
        | (inst, cell) <- Map.toList bv.bvBoard.bbCells,
          inst.instPath == path,
          not (null inst.instIters),
          isCompleted cell
      ]

latestCompletedIterationAt :: BlackboardView -> Path -> Maybe Cell
latestCompletedIterationAt bv path = listToMaybe (reverse (completedIterationsAt bv path))

priorIterations :: BlackboardView -> Path -> [Cell]
priorIterations bv path =
  case bv.bvInstIters of
    current : rest | current > 1 ->
      [ cell
        | n <- [1 .. current - 1],
          let inst = Instance path (n : rest),
          Just cell <- [lookupCompletedCell bv.bvBoard inst]
      ]
    _ ->
      case completedIterationsAt bv path of
        [] -> []
        cells -> init cells

-- ---------------------------------------------------------------------------
-- Show / debug
-- ---------------------------------------------------------------------------

showPath :: Path -> Text
showPath Root = ""
showPath (Child parent lbl) = showPath parent <> "/" <> lbl.unLabel

showInstance :: Instance -> Text
showInstance inst =
  showPath inst.instPath
    <> if null inst.instIters
      then ""
      else "@" <> T.pack (show inst.instIters)

showCellStatus :: CellStatus -> Text
showCellStatus = T.pack . show

showBlackboard :: Blackboard -> Text
showBlackboard bb =
  T.unlines $
    [ "Blackboard {",
      "  input: " <> T.pack (show bb.bbInput.prompt),
      "  current: " <> maybe "Nothing" showInstance bb.bbCurrent,
      "  cells:"
    ]
      ++ [ "    " <> showInstance inst <> " [" <> showCellStatus cell.cellStatus <> "]"
           | (inst, cell) <- Map.toList bb.bbCells
         ]
      ++ [ "  slices:"
         ]
      ++ [ "    " <> showPath path <> " iter=" <> T.pack (show slice.lsIteration)
           | (path, slice) <- Map.toList bb.bbSlices
         ]
      ++ ["}"]

showPathStack :: PathStack -> Text
showPathStack stack =
  T.unlines $
    zipWith (\i f -> T.pack (show i) <> ": " <> showPathFrame f) [0 :: Int ..] stack
  where
    showPathFrame = \case
      FrameRoot -> "Root"
      FrameComposite kind lbl path -> "Composite " <> T.pack (show kind) <> " " <> lbl.unLabel <> " @ " <> showPath path
      FrameScope lbl path -> "Scope " <> lbl.unLabel <> " @ " <> showPath path
      FrameLeaf lbl path -> "Leaf " <> lbl.unLabel <> " @ " <> showPath path
      FrameParSide lbl side path -> "ParSide " <> lbl.unLabel <> " " <> T.pack (show side) <> " @ " <> showPath path

showRunnerState :: (Show o) => RunnerState o -> Text
showRunnerState rs =
  T.unlines
    [ showBlackboard rs.rsBlackboard,
      "PathStack:",
      showPathStack rs.rsPathStack,
      "instIters: " <> T.pack (show rs.rsInstIters),
      "stack: " <> T.pack (show rs.rsStack)
    ]

-- ---------------------------------------------------------------------------
-- Label environment
-- ---------------------------------------------------------------------------

buildLabelEnv :: Workflow i o -> Either LabelEnvError LabelEnv
buildLabelEnv wf = buildLabelEnvAt Root wf emptyLabelEnv

extendLabelEnv :: Path -> Workflow i o -> LabelEnv -> Either LabelEnvError LabelEnv
extendLabelEnv = buildLabelEnvAt

labelEnvResolve :: LabelEnv -> Path -> Label -> Path
labelEnvResolve env parent lbl =
  Map.findWithDefault (Child parent lbl) (parent, lbl) env.leChildPaths

labelEnvRolePath :: LabelEnv -> Path -> Text -> Path
labelEnvRolePath env parent role = labelEnvResolve env parent (Label role)

labelEnvPaths :: LabelEnv -> Map.Map Label Path
labelEnvPaths = (.leNodePath)

buildLabelEnvAt :: Path -> Workflow i o -> LabelEnv -> Either LabelEnvError LabelEnv
buildLabelEnvAt parent wf env = case wf of
  WLabel lbl inner ->
    buildLabeledAt parent lbl inner env
  WPrompt {} -> pure env
  WObject {} -> pure env
  WAgentSubmit {} -> pure env
  WSeq w1 w2 _ ->
    buildComposite parent env
      [ (syntheticLabel 0, unsafeCoerce w1),
        (syntheticLabel 1, unsafeCoerce w2)
      ]
  WPar w1 w2 _ ->
    buildComposite parent env
      [ (syntheticLabel 0, unsafeCoerce w1),
        (syntheticLabel 1, unsafeCoerce w2)
      ]
  WLift _ -> pure env
  WLiftW _ -> pure env
  WBlackboardPrompt _ _ -> pure env
  WMap inner _ ->
    buildComposite parent env [(syntheticLabel 0, inner)]
  WLoop _ inner _ _ ->
    buildComposite parent env [(Label "body", inner)]
  WLoopWhile _ decider _ _ _ body ->
    buildComposite
      parent
      env
      [ (Label "decider", unsafeCoerce decider),
        (Label "body", unsafeCoerce body)
      ]
  WCatch _ inner ->
    buildComposite parent env [(syntheticLabel 0, inner)]

buildLabeledAt :: Path -> Label -> Workflow i o -> LabelEnv -> Either LabelEnvError LabelEnv
buildLabeledAt parent lbl inner env =
  let childPath = Child parent lbl
   in case Map.lookup lbl env.leNodePath of
        Just existing | existing /= childPath ->
          Left (DuplicateLabel parent lbl)
        _ -> do
          let env' =
                env
                  { leChildPaths = Map.insert (parent, lbl) childPath env.leChildPaths,
                    leNodePath = Map.insert lbl childPath env.leNodePath
                  }
          buildLabelEnvAt childPath inner env'

buildComposite ::
  Path ->
  LabelEnv ->
  [(Label, Workflow i o)] ->
  Either LabelEnvError LabelEnv
buildComposite parent = foldM
    ( \e (lbl, childWf) -> do
        let childPath = Child parent lbl
            e' =
              e
                { leChildPaths = Map.insert (parent, lbl) childPath e.leChildPaths
                }
        buildLabelEnvAt childPath childWf e'
    )
