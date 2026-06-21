module BlackboardSpec (spec) where

import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import LLM (Agent (..))
import LLM.Core.Types (Turn (UserTurn))
import LLM.Workflow.Blackboard
import LLM.Core.Usage (emptyUsage)
import LLM.Workflow.BBEngine
  ( enterChildPath,
    parentPath,
    pushComposite,
    pushParSide,
    pushScope,
    resolveChildPath,
    topPath,
  )
import LLM.Workflow (emptyFinal)
import LLM.Workflow.Utils (loopDecPolicy, loopFeedPolicy, mapPolicy, seqPolicy)
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
      labelEnvTests,
      pathAlignmentTests,
      queryRegressionTests,
      postLoopQueryTests
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

testRunnerState :: LabelEnv -> RunnerState (Stack ())
testRunnerState = initialRunnerState
    (PromptArgs {history = [], prompt = ""})
    (Stack emptyUsage (RunFinish (Right ())) KEmpty)

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
        cellText <$> parBranch bv SideRight @?= Just "review-b",
      testCase "labeled par branch finds deepest completed cell" $ do
        let labeledLeft = Child reviewerAPath (Label "reviewer-a")
            labeledRight = Child reviewerBPath (Label "reviewer-b")
            board =
              mkBoard $
                Map.fromList
                  [ (inst labeledLeft [], doneCell "labeled-a"),
                    (inst labeledRight [], doneCell "labeled-b")
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
        cellText <$> parBranch bv SideLeft @?= Just "labeled-a"
        cellText <$> parBranch bv SideRight @?= Just "labeled-b"
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

-- ---------------------------------------------------------------------------
-- Path alignment (LabelEnv vs runtime resolution)
-- ---------------------------------------------------------------------------

pathAlignmentTests :: TestTree
pathAlignmentTests =
  testGroup
    "path alignment"
    [ testCase "WMap descends to child path before inner workflow" $ do
        let wf :: Workflow () ()
            wf =
              WMap
                (WLabel (Label "inner") (WLift (\_ -> pure ())))
                (mapPolicy (TranscriptPolicyFunc id))
            env = case buildLabelEnv wf of
              Left err -> error $ show err
              Right e -> e
            rs = testRunnerState env
            mapPath = enterChildPath rs (syntheticLabel 0)
        mapPath @?= Child Root (Label "_0"),
      testCase "WLabel + scope path matches buildLabelEnv" $ do
        let wf :: Workflow () ()
            wf =
              WSeq
                (WLabel (Label "planner") (WLift (\_ -> pure ())))
                (WLift (\_ -> pure ()))
                (seqPolicy (TranscriptPolicyFunc id))
            env = case buildLabelEnv wf of
              Left err -> error $ show err
              Right e -> e
            rs0 = testRunnerState env
            rs1 = pushComposite CompSeq (syntheticLabel 0) (topPath rs0.rsPathStack) rs0
            w1Path = resolveChildPath rs1 (syntheticLabel 0)
            rs2 = pushScope (syntheticLabel 0) w1Path rs1
            plannerPath = labelEnvResolve env w1Path (Label "planner")
        plannerPath @?= Child (Child Root (Label "_0")) (Label "planner")
        parentPath rs2.rsPathStack @?= w1Path,
      testCase "Wf1 buildLabelEnv succeeds with distinct refiner labels" $ do
        let wf :: Workflow PromptArgs Final
            wf =
              WMap
                ( WSeq
                    (WLabel (Label "planner") (WLift (\_ -> pure (emptyFinal ""))))
                    ( WLabel (Label "refinement-loop") $
                        WLoopWhile
                          2
                          (WLift (\_ -> pure ()))
                          (loopDecPolicy (TranscriptPolicyFunc (const True)))
                          []
                          (loopFeedPolicy (TranscriptPolicyFunc (const PromptArgs {history = [], prompt = ""})))
                          (WLabel (Label "loop-refiner") (WLift (\_ -> pure (emptyFinal ""))))
                    )
                    (seqPolicy TranscriptFinalToPromptArgs)
                )
                (mapPolicy (TranscriptPolicyFunc id))
            expectedLoopRefiner =
              let refinementLoopPath =
                    Child (Child (Child Root (Label "_0")) (Label "_1")) (Label "refinement-loop")
                  refinementBodyPath = Child refinementLoopPath (Label "body")
               in Child refinementBodyPath (Label "loop-refiner")
        case buildLabelEnv wf of
          Left err -> assertFailure $ show err
          Right env ->
            Map.lookup (Label "loop-refiner") env.leNodePath @?= Just expectedLoopRefiner
    ]

queryRegressionTests :: TestTree
queryRegressionTests =
  testGroup
    "query regressions"
    [ testCase "globalLabel ignores active loop instIters" $ do
        let plannerPath = Child Root (Label "planner")
            board = mkBoard (Map.singleton (inst plannerPath []) (doneCell "plan-text"))
            bv =
              BlackboardView
                { bvBoard = board,
                  bvLocal = loopPath,
                  bvSelf = Nothing,
                  bvPredecessor = Nothing,
                  bvTrigger = TriggerLoopDecider,
                  bvPathStack = [FrameRoot, FrameComposite CompLoop (Label "refinement-loop") loopPath],
                  bvInstIters = [2],
                  bvLabelEnv =
                    emptyLabelEnv
                      { leNodePath = Map.singleton (Label "planner") plannerPath
                      }
                }
        cellText <$> globalLabel bv (Label "planner") @?= Just "plan-text",
      testCase "WPar merge site uses par composite path" $ do
        let env = emptyLabelEnv
            rs0 = testRunnerState env
            parPath' = Child Root (Label "reviewers")
            rs1 = pushComposite CompPar (syntheticLabel 0) parPath' rs0
            leftPath = resolveChildPath rs1 (syntheticLabel 0)
            rs2 = pushParSide (syntheticLabel 0) SideLeft leftPath rs1
        topPath rs2.rsPathStack @?= leftPath
        let mergeSite = PolicySite parPath' TriggerParMerge Nothing Nothing
        mergeSite.psLocal @?= parPath'
    ]

-- ---------------------------------------------------------------------------
-- Post-loop query API
-- ---------------------------------------------------------------------------

postLoopQueryTests :: TestTree
postLoopQueryTests =
  testGroup
    "post-loop queries"
    [ testCase "completedIterationsAt returns all iteration cells after loop exit" $ do
        let board =
              mkBoard $
                Map.fromList
                  [ (inst refinerPath [1], doneCell "ref-1"),
                    (inst refinerPath [2], doneCell "ref-2"),
                    (inst refinerPath [3], doneCell "ref-3")
                  ]
            bv =
              BlackboardView
                { bvBoard = board,
                  bvLocal = Child Root (Label "refinement-loop"),
                  bvSelf = Nothing,
                  bvPredecessor = Nothing,
                  bvTrigger = TriggerSeq,
                  bvPathStack = [FrameRoot, FrameScope (Label "refinement-loop") (Child Root (Label "refinement-loop"))],
                  bvInstIters = [],
                  bvLabelEnv =
                    emptyLabelEnv
                      { leNodePath = Map.singleton (Label "loop-refiner") refinerPath
                      }
                }
        map cellText (completedIterationsAt bv refinerPath) @?= ["ref-1", "ref-2", "ref-3"],
      testCase "atBodyLabel falls back to latest iteration via label env" $ do
        let board = mkBoard (Map.singleton (inst refinerPath [2]) (doneCell "latest-ref"))
            bv =
              BlackboardView
                { bvBoard = board,
                  bvLocal = Child Root (Label "refinement-loop"),
                  bvSelf = Nothing,
                  bvPredecessor = Nothing,
                  bvTrigger = TriggerSeq,
                  bvPathStack = [FrameRoot],
                  bvInstIters = [],
                  bvLabelEnv =
                    emptyLabelEnv
                      { leNodePath = Map.singleton (Label "loop-refiner") refinerPath
                      }
                }
        cellText <$> atBodyLabel bv (Label "loop-refiner") @?= Just "latest-ref",
      testCase "priorIterations after loop exit excludes latest iteration" $ do
        let board =
              mkBoard $
                Map.fromList
                  [ (inst refinerPath [1], doneCell "a"),
                    (inst refinerPath [2], doneCell "b")
                  ]
            bv =
              BlackboardView
                { bvBoard = board,
                  bvLocal = Root,
                  bvSelf = Nothing,
                  bvPredecessor = Nothing,
                  bvTrigger = TriggerSeq,
                  bvPathStack = [FrameRoot],
                  bvInstIters = [],
                  bvLabelEnv = emptyLabelEnv
                }
        map cellText (priorIterations bv refinerPath) @?= ["a"],
      testCase "loopSliceAt reads slice without loop frame on stack" $ do
        let slice = emptyLoopSlice While 4 (PromptArgs {history = [], prompt = "in"})
            board =
              (mkBoard Map.empty)
                { bbSlices = Map.singleton loopPath slice
                }
            bv =
              BlackboardView
                { bvBoard = board,
                  bvLocal = Root,
                  bvSelf = Nothing,
                  bvPredecessor = Nothing,
                  bvTrigger = TriggerSeq,
                  bvPathStack = [FrameRoot],
                  bvInstIters = [],
                  bvLabelEnv = emptyLabelEnv
                }
        maybe (assertFailure "no slice") (\s -> s.lsMax @?= 4) (loopSliceAt bv loopPath),
      testCase "loopPathFromBodyAgent derives loop path from labeled body agent" $ do
        loopPathFromBodyAgent
          (emptyLabelEnv {leNodePath = Map.singleton (Label "loop-refiner") refinerPath})
          (Label "loop-refiner")
          @?= Just loopPath,
      testCase "cellOutputJson only surfaces OutValue outputs" $ do
        isNothing (cellOutputJson (doneCell "text")) @?= True
        isNothing (cellOutputJson ((emptyCell {cellStatus = CellDone}) {cellOutput = Just (OutText "plain")})) @?= True,
      testCase "loopOutputTexts reads accumulated body outputs" $ do
        let slice =
              appendLoopOutput (OutFinal (mkFinal "two")) $
                appendLoopOutput (OutFinal (mkFinal "one")) $
                  emptyLoopSlice While 3 (PromptArgs {history = [], prompt = "in"})
        loopOutputTexts slice @?= ["one", "two"]
    ]
