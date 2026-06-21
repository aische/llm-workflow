module LLM.Workflow.BBEngine
  ( mkPolicyView,
    pushComposite,
    pushScope,
    pushLeaf,
    pushParSide,
    popPathFrame,
    popCompositeFrame,
    popParFrames,
    startLeafCell,
    completeLeafCell,
    completeObjectCell,
    catchLeafCell,
    failLeafCell,
    updateRunningHistory,
    initLoopSliceAt,
    updateLoopSliceAt,
    bumpLoopIteration,
    appendLoopDecision,
    setLoopNextInput,
    appendBodyOutput,
    writeMapCell,
    writeNestCell,
    dualWriteSlot,
    nodeOutputFromFinal,
    exitLoopScope,
    topPath,
    parentPath,
    resolveChildPath,
    resolveSidePath,
    enterChildPath,
    currentLeafCoords,
  )
where

import Data.Aeson (ToJSON, toJSON)
import Data.Map qualified as Map
import Data.Text (Text)
import LLM.Core.Types (Turn)
import LLM.Core.Usage (Usage)
import LLM.Generate (GenerateError)
import LLM.Workflow.Blackboard
  ( appendLoopOutput,
    emptyCell,
    emptyLoopSlice,
    labelEnvResolve,
    syntheticLabel,
    updateSlot,
  )
import LLM.Workflow.Types
  ( Blackboard (..),
    BlackboardView (..),
    Cell (..),
    CellStatus (..),
    CompositeKind (..),
    Final (..),
    Instance (..),
    Label (..),
    LoopKind (..),
    LoopSlice (..),
    NodeOutput (OutFinal, OutValue),
    Path (..),
    PathFrame (..),
    PathStack,
    PolicySite (..),
    PromptArgs (..),
    RunnerState (..),
    Side (..),
    SlotKey (..),
  )

topPath :: PathStack -> Path
topPath stack = case stack of
  [] -> Root
  FrameRoot : _ -> Root
  FrameComposite _ _ path : _ -> path
  FrameScope _ path : _ -> path
  FrameLeaf _ path : _ -> path
  FrameParSide _ _ path : _ -> path

parentPath :: PathStack -> Path
parentPath stack = case stack of
  [] -> Root
  [FrameRoot] -> Root
  FrameComposite _ _ path : _ -> path
  FrameScope _ path : _ -> path
  FrameLeaf _ path : _ -> path
  FrameParSide _ _ path : _ -> path
  _ -> Root

pushComposite :: CompositeKind -> Label -> Path -> RunnerState a -> RunnerState a
pushComposite kind lbl path rs =
  rs {rsPathStack = FrameComposite kind lbl path : rs.rsPathStack}

pushScope :: Label -> Path -> RunnerState a -> RunnerState a
pushScope lbl path rs =
  rs {rsPathStack = FrameScope lbl path : rs.rsPathStack}

pushLeaf :: Label -> Path -> RunnerState a -> RunnerState a
pushLeaf lbl path rs =
  rs {rsPathStack = FrameLeaf lbl path : rs.rsPathStack}

pushParSide :: Label -> Side -> Path -> RunnerState a -> RunnerState a
pushParSide lbl side path rs =
  rs {rsPathStack = FrameParSide lbl side path : rs.rsPathStack}

popPathFrame :: RunnerState a -> RunnerState a
popPathFrame rs =
  rs {rsPathStack = drop 1 rs.rsPathStack}

popCompositeFrame :: CompositeKind -> RunnerState a -> RunnerState a
popCompositeFrame kind rs =
  case rs.rsPathStack of
    FrameComposite k _ _ : rest | k == kind -> rs {rsPathStack = rest}
    _ -> rs

popParFrames :: RunnerState a -> RunnerState a
popParFrames =
  popCompositeFrame CompPar . popParSideFrame . popParSideFrame
  where
    popParSideFrame rs =
      case rs.rsPathStack of
        FrameParSide {} : rest -> rs {rsPathStack = rest}
        _ -> rs

exitLoopScope :: RunnerState a -> RunnerState a
exitLoopScope rs =
  popCompositeFrame CompLoop $
    popPathFrame rs {rsInstIters = drop 1 rs.rsInstIters}

currentLeafCoords :: RunnerState a -> Maybe (Path, [Int])
currentLeafCoords rs =
  case rs.rsBlackboard.bbCurrent of
    Nothing -> Nothing
    Just inst -> Just (inst.instPath, inst.instIters)

enterChildPath :: RunnerState a -> Label -> Path
enterChildPath = resolveChildPath

startLeafCell :: Path -> [Int] -> Maybe Text -> RunnerState a -> RunnerState a
startLeafCell path iters agentName rs =
  let inst = Instance path iters
      cell = (emptyCell {cellStatus = CellRunning, cellAgent = agentName})
      bb = rs.rsBlackboard
      bb' =
        bb
          { bbCells = Map.insert inst cell bb.bbCells,
            bbCurrent = Just inst
          }
   in rs {rsBlackboard = bb'}

updateRunningHistory :: Path -> [Int] -> [Turn] -> RunnerState a -> RunnerState a
updateRunningHistory path iters turns rs =
  let inst = Instance path iters
   in rs
        { rsBlackboard =
            rs.rsBlackboard
              { bbCells =
                  Map.adjust (\c -> c {cellHistory = turns}) inst rs.rsBlackboard.bbCells
              }
        }

completeLeafCell :: Path -> [Int] -> Final -> Usage -> [Turn] -> RunnerState a -> RunnerState a
completeLeafCell path iters final u turns rs =
  let inst = Instance path iters
      out = OutFinal final
      cell =
        (emptyCell {cellStatus = CellDone, cellUsage = u, cellHistory = turns})
          { cellOutput = Just out
          }
      bb = rs.rsBlackboard
      bb' =
        bb
          { bbCells = Map.insert inst cell bb.bbCells,
            bbCurrent = Nothing,
            bbUsage = bb.bbUsage <> u
          }
   in popPathFrame rs {rsBlackboard = bb'}

completeObjectCell :: (ToJSON v) => Path -> [Int] -> v -> Usage -> RunnerState s -> RunnerState s
completeObjectCell path iters value u rs =
  let inst = Instance path iters
      cell =
        (emptyCell {cellStatus = CellDone, cellUsage = u})
          { cellOutput = Just (OutValue (toJSON value))
          }
      bb = rs.rsBlackboard
      bb' =
        bb
          { bbCells = Map.insert inst cell bb.bbCells,
            bbCurrent = Nothing,
            bbUsage = bb.bbUsage <> u
          }
   in popPathFrame rs {rsBlackboard = bb'}

failLeafCell :: Path -> [Int] -> GenerateError -> RunnerState a -> RunnerState a
failLeafCell path iters err rs =
  let inst = Instance path iters
      cell = (emptyCell {cellStatus = CellFailed, cellError = Just err})
      bb = rs.rsBlackboard
      bb' = bb {bbCells = Map.insert inst cell bb.bbCells, bbCurrent = Nothing}
   in popPathFrame rs {rsBlackboard = bb'}

catchLeafCell :: Path -> [Int] -> Final -> GenerateError -> RunnerState a -> RunnerState a
catchLeafCell path iters fallback err rs =
  let inst = Instance path iters
      cell =
        (emptyCell {cellStatus = CellCaught, cellError = Just err})
          { cellOutput = Just (OutFinal fallback)
          }
      bb = rs.rsBlackboard
      bb' = bb {bbCells = Map.insert inst cell bb.bbCells, bbCurrent = Nothing}
   in popPathFrame rs {rsBlackboard = bb'}

nodeOutputFromFinal :: Final -> NodeOutput
nodeOutputFromFinal = OutFinal

initLoopSliceAt :: Path -> LoopKind -> Int -> PromptArgs -> [SlotKey] -> RunnerState a -> RunnerState a
initLoopSliceAt path kind maxIter input scope rs =
  let slice = (emptyLoopSlice kind maxIter input) {lsPersistScope = scope}
      bb = rs.rsBlackboard
   in rs {rsBlackboard = bb {bbSlices = Map.insert path slice bb.bbSlices}}

updateLoopSliceAt :: Path -> (LoopSlice -> LoopSlice) -> RunnerState a -> RunnerState a
updateLoopSliceAt path f rs =
  case Map.lookup path rs.rsBlackboard.bbSlices of
    Nothing -> rs
    Just slice ->
      rs {rsBlackboard = rs.rsBlackboard {bbSlices = Map.insert path (f slice) rs.rsBlackboard.bbSlices}}

bumpLoopIteration :: Path -> Int -> RunnerState a -> RunnerState a
bumpLoopIteration path iter = updateLoopSliceAt path (\s -> s {lsIteration = iter})

setLoopNextInput :: Path -> PromptArgs -> RunnerState a -> RunnerState a
setLoopNextInput path input = updateLoopSliceAt path (\s -> s {lsNextInput = Just input})

appendLoopDecision :: Path -> Bool -> RunnerState a -> RunnerState a
appendLoopDecision path decision = updateLoopSliceAt path (\s -> s {lsDecisions = s.lsDecisions ++ [decision]})

appendBodyOutput :: Path -> NodeOutput -> RunnerState a -> RunnerState a
appendBodyOutput loopPath out rs =
  let bb = rs.rsBlackboard
   in case Map.lookup loopPath bb.bbSlices of
        Nothing -> rs
        Just slice ->
          rs
            { rsBlackboard =
                bb {bbSlices = Map.insert loopPath (appendLoopOutput out slice) bb.bbSlices}
            }

writeMapCell :: Path -> [Int] -> NodeOutput -> NodeOutput -> RunnerState a -> RunnerState a
writeMapCell path iters raw mapped rs =
  let inst = Instance path iters
      cell =
        (emptyCell {cellStatus = CellDone})
          { cellRawOutput = Just raw,
            cellOutput = Just mapped
          }
      bb = rs.rsBlackboard
   in rs {rsBlackboard = bb {bbCells = Map.insert inst cell bb.bbCells}}

writeNestCell :: Path -> [Int] -> NodeOutput -> RunnerState a -> RunnerState a
writeNestCell path iters out rs =
  let inst = Instance path iters
      cell = (emptyCell {cellStatus = CellDone}) {cellOutput = Just out}
      bb = rs.rsBlackboard
   in rs {rsBlackboard = bb {bbCells = Map.insert inst cell bb.bbCells}}

dualWriteSlot :: SlotKey -> [Turn] -> RunnerState a -> RunnerState a
dualWriteSlot key turns rs =
  rs {rsBlackboard = updateSlot rs.rsBlackboard rs.rsPathStack key turns}

mkPolicyView :: RunnerState a -> PolicySite -> BlackboardView
mkPolicyView rs site =
  BlackboardView
    { bvBoard = rs.rsBlackboard,
      bvLocal = site.psLocal,
      bvSelf = site.psSelf,
      bvPredecessor = site.psPredecessor,
      bvTrigger = site.psTrigger,
      bvPathStack = rs.rsPathStack,
      bvInstIters = rs.rsInstIters,
      bvLabelEnv = rs.rsLabelEnv
    }

resolveChildPath :: RunnerState a -> Label -> Path
resolveChildPath rs = labelEnvResolve rs.rsLabelEnv (parentPath rs.rsPathStack)

resolveSidePath :: RunnerState a -> Side -> Path
resolveSidePath rs side =
  let idx = case side of SideLeft -> 0; SideRight -> 1
   in resolveChildPath rs (syntheticLabel idx)
