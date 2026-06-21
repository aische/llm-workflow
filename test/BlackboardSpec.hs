module BlackboardSpec (spec) where

import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import LLM (Agent (..))
import LLM.Core.Types (Turn (UserTurn))
import LLM.Workflow.Blackboard
import LLM.Workflow.Utils (loopDecPolicy, loopFeedPolicy)
import Test.Tasty
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

spec :: TestTree
spec =
  testGroup
    "Blackboard"
    [ pathNavigationTests,
      instItersTests,
      loopSliceTests,
      slotPersistenceTests,
      scopedAccessTests,
      wCatchParBranchTests,
      labelEnvTests
    ]

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

mkFinal :: Text -> Final
mkFinal t = Final {prompt = Nothing, history = [], newMessages = [], text = t}

data LoopDecision = LoopDecision {shouldContinue :: Bool, reason :: Text}
  deriving (Show)

_dummyAgent :: AgentWithModels
_dummyAgent =
  AgentWithModels
    { agent = Agent {agName = "test", agSystemPrompt = Nothing, agTools = [], agMaxToolRounds = 1, agContextWindow = Nothing},
      models = undefined
    }

_dummyDecPolicy :: AnyLoopDecPolicy LoopDecision
_dummyDecPolicy = loopDecPolicy (TranscriptPolicyFunc (\(LoopDecision c _) -> c))

doneCell :: Text -> Cell
doneCell t =
  (emptyCell {cellStatus = CellDone})
    { cellOutput = Just (OutFinal (mkFinal t))
    }

caughtCell :: Text -> Cell
caughtCell t =
  (emptyCell {cellStatus = CellCaught})
    { cellOutput = Just (OutFinal (mkFinal t))
    }

runningCell :: [Turn] -> Cell
runningCell turns =
  (emptyCell {cellStatus = CellRunning})
    { cellHistory = turns
    }

seqPath :: Path
seqPath = Child Root (Label "seq")

parPath :: Path
parPath = Child Root (Label "par")

loopPath :: Path
loopPath = Child Root (Label "refinement-loop")

bodyPath :: Path
bodyPath = loopBodyPath loopPath

refinerPath :: Path
refinerPath = Child bodyPath (Label "refiner")

reviewerAPath :: Path
reviewerAPath = Child parPath (syntheticLabel 0)

reviewerBPath :: Path
reviewerBPath = Child parPath (syntheticLabel 1)

inst :: Path -> [Int] -> Instance
inst = Instance

mkBoard :: Map.Map Instance Cell -> Blackboard
mkBoard cells =
  (emptyBlackboard (PromptArgs {history = [], prompt = "root"}))
    { bbCells = cells
    }

mkView :: Path -> Trigger -> [Int] -> Blackboard -> BlackboardView
mkView local trigger iters board =
  BlackboardView
    { bvBoard = board,
      bvLocal = local,
      bvSelf = Nothing,
      bvPredecessor = Nothing,
      bvTrigger = trigger,
      bvPathStack =
        [ FrameRoot,
          FrameComposite CompLoop (Label "refinement-loop") loopPath
        ],
      bvInstIters = iters,
      bvLabelEnv = emptyLabelEnv
    }

-- ---------------------------------------------------------------------------
-- Path navigation
-- ---------------------------------------------------------------------------

pathNavigationTests :: TestTree
pathNavigationTests =
  testGroup
    "path navigation"
    [ testCase "synthetic labels" $
        syntheticLabel 0 @?= Label "_0",
      testCase "loop body/decider paths" $ do
        loopBodyPath loopPath @?= bodyPath
        loopDeciderPath loopPath @?= Child loopPath (Label "decider"),
      testCase "showPath" $
        showPath refinerPath @?= "/refinement-loop/body/refiner"
    ]

-- ---------------------------------------------------------------------------
-- instIters lifecycle
-- ---------------------------------------------------------------------------

instItersTests :: TestTree
instItersTests =
  testGroup
    "instIters"
    [ testCase "nested iteration stacks address distinct cells" $ do
        let i1 = inst refinerPath [1]
            i2 = inst refinerPath [2]
            i12 = inst refinerPath [1, 2]
            board =
              mkBoard $
                Map.fromList
                  [ (i1, doneCell "iter-1"),
                    (i2, doneCell "iter-2"),
                    (i12, doneCell "inner-1-outer-2")
                  ]
            bv = mkView loopPath TriggerLoopDecider [2] board
        cellText <$> atPath bv refinerPath @?= Just "iter-2",
      testCase "priorIterations returns earlier iterations only" $ do
        let board =
              mkBoard $
                Map.fromList
                  [ (inst refinerPath [1], doneCell "a"),
                    (inst refinerPath [2], doneCell "b"),
                    (inst refinerPath [3], doneCell "c")
                  ]
            bv = mkView loopPath TriggerLoopDecider [3] board
            prior = priorIterations bv refinerPath
        map cellText prior @?= ["a", "b"]
    ]

-- ---------------------------------------------------------------------------
-- Loop slice derivation
-- ---------------------------------------------------------------------------

loopSliceTests :: TestTree
loopSliceTests =
  testGroup
    "loop slice"
    [ testCase "appendLoopOutput derives lsLatestOutput" $ do
        let slice = emptyLoopSlice While 4 (PromptArgs {history = [], prompt = "in"})
            out = OutFinal (mkFinal "body-out")
            slice' = appendLoopOutput out slice
        case slice'.lsLatestOutput of
          Just (OutFinal f) -> f.text @?= "body-out"
          _ -> assertFailure "expected OutFinal",
      testCase "loopSlice reads innermost loop" $ do
        let slice = emptyLoopSlice While 4 (PromptArgs {history = [], prompt = "in"})
            board =
              (mkBoard Map.empty)
                { bbSlices = Map.singleton loopPath slice
                }
            bv = mkView loopPath TriggerLoopFeedback [1] board
        maybe (assertFailure "no slice") (\s -> s.lsIteration @?= 1) (loopSlice bv)
    ]

-- ---------------------------------------------------------------------------
-- Slot persistence (acceptance tests 1–4)
-- ---------------------------------------------------------------------------

slotPersistenceTests :: TestTree
slotPersistenceTests =
  testGroup
    "slot persistence"
    [ testCase "1 ephemeral agent in loop — empty persistScope" $ do
        let slice =
              (emptyLoopSlice While 4 (PromptArgs {history = [], prompt = "in"}))
                { lsPersistScope = []
                }
            board =
              (mkBoard Map.empty)
                { bbSlices = Map.singleton loopPath slice
                }
            stack =
              [ FrameRoot,
                FrameComposite CompLoop (Label "refinement-loop") loopPath
              ]
            key = SlotByLabel (Label "refiner")
        lookupSlot board stack key @?= []
        lookupSlot (updateSlot board stack key [UserTurn "x"]) stack key @?= [],
      testCase "2 persisted agent in loop — cid in scope" $ do
        let key = SlotByLabel (Label "refiner")
            turns = [UserTurn "remembered"]
            slice =
              (emptyLoopSlice While 4 (PromptArgs {history = [], prompt = "in"}))
                { lsPersistScope = [key],
                  lsSlots = Map.singleton key turns
                }
            board =
              (mkBoard Map.empty)
                { bbSlices = Map.singleton loopPath slice
                }
            stack =
              [ FrameRoot,
                FrameComposite CompLoop (Label "loop") loopPath
              ]
        lookupSlot board stack key @?= turns,
      testCase "3 nested loops — outer scope only" $ do
        let outerPath = Child Root (Label "outer")
            innerPath = Child outerPath (Label "inner")
            key = SlotByLabel (Label "agent")
            outerSlice =
              (emptyLoopSlice While 3 (PromptArgs {history = [], prompt = "o"}))
                { lsPersistScope = [key],
                  lsSlots = Map.singleton key [UserTurn "outer-slot"]
                }
            innerSlice =
              (emptyLoopSlice While 3 (PromptArgs {history = [], prompt = "i"}))
                { lsPersistScope = []
                }
            board =
              (mkBoard Map.empty)
                { bbSlices =
                    Map.fromList
                      [ (outerPath, outerSlice),
                        (innerPath, innerSlice)
                      ]
                }
            stack =
              [ FrameRoot,
                FrameComposite CompLoop (Label "outer") outerPath,
                FrameComposite CompLoop (Label "inner") innerPath
              ]
        lookupSlot board stack key @?= [UserTurn "outer-slot"],
      testCase "4 bubble-up — inner loop without scope, outer with scope" $ do
        let outerPath = Child Root (Label "outer")
            innerPath = Child outerPath (Label "inner")
            key = SlotByLabel (Label "agent")
            outerSlice =
              (emptyLoopSlice While 3 (PromptArgs {history = [], prompt = "o"}))
                { lsPersistScope = [key]
                }
            innerSlice = emptyLoopSlice While 3 (PromptArgs {history = [], prompt = "i"})
            board =
              (mkBoard Map.empty)
                { bbSlices =
                    Map.fromList
                      [ (outerPath, outerSlice),
                        (innerPath, innerSlice)
                      ]
                }
            stack =
              [ FrameRoot,
                FrameComposite CompLoop (Label "outer") outerPath,
                FrameComposite CompLoop (Label "inner") innerPath
              ]
            updated = updateSlot board stack key [UserTurn "bubbled"]
        lookupSlot updated stack key @?= [UserTurn "bubbled"]
    ]

-- ---------------------------------------------------------------------------
-- Scoped access / PolicySite
-- ---------------------------------------------------------------------------

scopedAccessTests :: TestTree
scopedAccessTests =
  testGroup
    "scoped access"
    [ testCase "predecessor at TriggerSeq" $ do
        let predInst = inst (Child seqPath (Label "planner")) []
            board = mkBoard (Map.singleton predInst (doneCell "plan"))
            bv =
              BlackboardView
                { bvBoard = board,
                  bvLocal = seqPath,
                  bvSelf = Nothing,
                  bvPredecessor = Just predInst,
                  bvTrigger = TriggerSeq,
                  bvPathStack = [FrameRoot, FrameComposite CompSeq (Label "seq") seqPath],
                  bvInstIters = [],
                  bvLabelEnv = emptyLabelEnv
                }
        cellText <$> predecessor bv @?= Just "plan",
      testCase "atLabel relative to scopeRoot" $ do
        let board =
              mkBoard $
                Map.singleton (inst (Child seqPath (Label "refiner")) []) (doneCell "refined")
            bv =
              BlackboardView
                { bvBoard = board,
                  bvLocal = seqPath,
                  bvSelf = Nothing,
                  bvPredecessor = Nothing,
                  bvTrigger = TriggerSeq,
                  bvPathStack = [FrameRoot, FrameComposite CompSeq (Label "seq") seqPath],
                  bvInstIters = [],
                  bvLabelEnv = emptyLabelEnv
                }
        cellText <$> atLabel bv (Label "refiner") @?= Just "refined",
      testCase "atBodyLabel inside loop" $ do
        let board =
              mkBoard $
                Map.singleton (inst refinerPath [2]) (doneCell "body-refined")
            bv = mkView loopPath TriggerLoopDecider [2] board
        cellText <$> atBodyLabel bv (Label "refiner") @?= Just "body-refined",
      testCase "running cells not visible by default" $ do
        let board =
              mkBoard $
                Map.singleton (inst refinerPath [1]) (runningCell [UserTurn "partial"])
            bv = mkView loopPath TriggerLoopFeedback [1] board
        isNothing (atPath bv refinerPath) @?= True,
      testCase "cellPartialHistory opt-in for running" $ do
        let cell = runningCell [UserTurn "partial"]
        cellPartialHistory cell @?= Just [UserTurn "partial"]
    ]

-- ---------------------------------------------------------------------------
-- WCatch + parBranch
-- ---------------------------------------------------------------------------

wCatchParBranchTests :: TestTree
wCatchParBranchTests =
  testGroup
    "WCatch parBranch"
    [ testCase "caught branch visible to parBranch" $ do
        let board =
              mkBoard $
                Map.fromList
                  [ (inst reviewerAPath [], caughtCell "Reviewer A failed"),
                    (inst reviewerBPath [], doneCell "review-b")
                  ]
            bv =
              BlackboardView
                { bvBoard = board,
                  bvLocal = parPath,
                  bvSelf = Nothing,
                  bvPredecessor = Nothing,
                  bvTrigger = TriggerParMerge,
                  bvPathStack = [FrameRoot, FrameComposite CompPar (Label "par") parPath],
                  bvInstIters = [],
                  bvLabelEnv = emptyLabelEnv
                }
        cellText <$> parBranch bv SideLeft @?= Just "Reviewer A failed"
        cellText <$> parBranch bv SideRight @?= Just "review-b"
    ]

-- ---------------------------------------------------------------------------
-- LabelEnv
-- ---------------------------------------------------------------------------

labelEnvTests :: TestTree
labelEnvTests =
  testGroup
    "LabelEnv"
    [ testCase "buildLabelEnv assigns role labels for WLoopWhile" $ do
        let wf :: Workflow () ()
            wf =
              WLoopWhile
                3
                (WLift (\_ -> pure ()))
                (loopDecPolicy (TranscriptPolicyFunc (const True)))
                []
                (loopFeedPolicy (TranscriptPolicyFunc (const ())))
                (WLift (\_ -> pure ()))
            env = case buildLabelEnv wf of
              Left _ -> error "buildLabelEnv failed"
              Right e -> e
        labelEnvRolePath env Root "body" @?= Child Root (Label "body")
        labelEnvRolePath env Root "decider" @?= Child Root (Label "decider"),
      testCase "WLabel provides stable path" $ do
        let wf :: Workflow () ()
            wf = WLabel (Label "planner") (WLift (\_ -> pure ()))
            env = case buildLabelEnv wf of
              Left _ -> error "buildLabelEnv failed"
              Right e -> e
        labelEnvResolve env Root (Label "planner")
          @?= Child Root (Label "planner")
    ]
