module Wf1
  ( buildWf1Workflow,
  )
where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM (Agent (..))
import LLM.Generate (ModelWithFallbacks)
import LLM.Workflow
  ( AgentWithModels (..),
    AnyMergePolicy (..),
    BlackboardView (..),
    Cell (..),
    Label (..),
    LoopSlice (..),
    Path (..),
    PromptArgs (..),
    Side (..),
    Workflow (..),
    atBodyLabel,
    blackboardLoopDec,
    blackboardLoopFeed,
    cellText,
    emptyFinal,
    globalLabel,
    innermostLoopPath,
    loopBodyPath,
    loopSlice,
    mapPolicy,
    parBranch,
    priorIterations,
    seqPolicy,
    seqPolicyBB,
  )
import LLM.Workflow.Types
  ( Final (..),
    TranscriptPolicy (TranscriptFinalToPromptArgs, TranscriptPolicyFunc),
  )

data LoopDecision = LoopDecision
  { shouldContinue :: Bool,
    reason :: Text
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via (AC.Autodocodec LoopDecision)

instance AC.HasCodec LoopDecision where
  codec :: AC.JSONCodec LoopDecision
  codec =
    AC.object "workflow loop decision" $
      LoopDecision
        <$> AC.requiredField "shouldContinue" "Set to true when another refinement pass is required." AC..= (\x -> x.shouldContinue)
        <*> AC.requiredField "reason" "Brief reason for the decision." AC..= (\x -> x.reason)

buildWf1Workflow :: (ModelWithFallbacks, ModelWithFallbacks) -> Workflow PromptArgs Final
buildWf1Workflow (models, models2) =
  WMap
    (WSeq initialDraft (mkConditionalLoop 4 loopRefiner deciderWorkflow) (seqPolicy TranscriptFinalToPromptArgs))
    (mapPolicy (TranscriptPolicyFunc (\result -> result {text = "WF1 Result\n\n" <> result.text})))
  where
    planner = WLabel (Label "planner") $ WPrompt (AgentWithModels plannerAgent models) Nothing
    reviewerA = WLabel (Label "reviewer-a") $ WPrompt (AgentWithModels reviewerAgentA models2) Nothing
    reviewerB = WLabel (Label "reviewer-b") $ WPrompt (AgentWithModels reviewerAgentB models) Nothing
    refiner = WLabel (Label "refiner") $ WPrompt (AgentWithModels refinerAgent models) Nothing
    loopRefiner = WLabel (Label "loop-refiner") $ WPrompt (AgentWithModels refinerAgent models) Nothing
    finalizer = WLabel (Label "finalizer") $ WPrompt (AgentWithModels finalizerAgent models) Nothing
    deciderSubmit :: Workflow PromptArgs LoopDecision
    deciderSubmit = WLabel (Label "decider") $ WAgentSubmit "submit_decision" (AgentWithModels deciderAgent models) Nothing

    initialDraft =
      WSeq planner reviewersCombined (seqPolicy TranscriptFinalToPromptArgs)

    reviewersCombined =
      WLabel (Label "reviewers") $
        WSeq
          (WPar safeReviewerA safeReviewerB reviewersToRefinerInput)
          refiner
          (seqPolicy (TranscriptPolicyFunc id))

    safeReviewerA = WCatch (emptyFinal "Reviewer A failed") reviewerA
    safeReviewerB = WCatch (emptyFinal "Reviewer B failed") reviewerB

    reviewersToRefinerInput :: AnyMergePolicy Final Final PromptArgs
    reviewersToRefinerInput =
      BlackboardPar $ \bv _ _ ->
        let reviewA = cellText <$> parBranch bv SideLeft
            reviewB = cellText <$> parBranch bv SideRight
         in PromptArgs
              { history = [],
                prompt =
                  T.unlines
                    [ "You are receiving two completed reviewer drafts.",
                      "Consolidate them into a final refined review now.",
                      "Do not ask for more input. Do not wait for user action.",
                      "",
                      "Reviewer A Draft:",
                      fromMaybe "" reviewA,
                      "",
                      "Reviewer B Draft:",
                      fromMaybe "" reviewB,
                      "",
                      "Required output:",
                      "1) Consolidated findings by severity",
                      "2) Concrete next steps per finding",
                      "3) Brief conflict-resolution notes where drafts disagree"
                    ]
              }

    deciderWorkflow =
      WSeq
        (WBlackboardPrompt wf1DeciderPrompt (AgentWithModels deciderAgent models))
        deciderSubmit
        (seqPolicy TranscriptFinalToPromptArgs)

    mkConditionalLoop maxIterations body decider =
      WLabel (Label "refinement-loop") $
        WSeq
          ( WLoopWhile
              maxIterations
              decider
              (blackboardLoopDec wf1DeciderContinue)
              []
              (blackboardLoopFeed wf1RefinerLoopFeed)
              body
          )
          finalizer
          (seqPolicyBB wf1FinalizerInput)

refinerBodyPath :: BlackboardView -> Path
refinerBodyPath bv =
  case innermostLoopPath bv of
    Nothing -> Child Root (Label "loop-refiner")
    Just loopP -> Child (loopBodyPath loopP) (Label "loop-refiner")

maybeCellText :: Maybe Cell -> Text
maybeCellText = maybe "" cellText

wf1DeciderPrompt :: BlackboardView -> PromptArgs
wf1DeciderPrompt bv =
  let slice = loopSlice bv
      iter = maybe 1 (.lsIteration) slice
      maxIter = maybe 1 (.lsMax) slice
      planner = maybeCellText (globalLabel bv (Label "planner"))
      reviewA = maybeCellText (globalLabel bv (Label "reviewer-a"))
      reviewB = maybeCellText (globalLabel bv (Label "reviewer-b"))
      refinerPath = refinerBodyPath bv
      priorLines =
        concatMap
          (\(n, cell) -> ["Iteration " <> T.pack (show n) <> ":", cellText cell, ""])
          (zip ([1 :: Int ..] :: [Int]) (priorIterations bv refinerPath))
      currentRef = maybeCellText (atBodyLabel bv (Label "loop-refiner"))
   in PromptArgs
        { history = [],
          prompt =
            T.unlines $
              [ "Decide whether another refinement iteration is needed.",
                "Current iteration: " <> T.pack (show iter) <> "/" <> T.pack (show maxIter),
                "",
                "Original plan:",
                planner,
                "",
                "Reviewer A:",
                reviewA,
                "",
                "Reviewer B:",
                reviewB,
                ""
              ]
                ++ (if null priorLines then [] else "Prior refiner outputs:" : priorLines)
                ++ [ "",
                     "Latest refiner output:",
                     currentRef,
                     "",
                     "Call submit_decision with shouldContinue and reason.",
                     "Set shouldContinue=false when output is coherent, deduplicated, and actionable.",
                     "Set shouldContinue=true when meta/process language appears (FAILED, BLOCKED, submission issues)."
                   ]
        }

wf1RefinerLoopFeed :: BlackboardView -> LoopSlice -> Final -> PromptArgs
wf1RefinerLoopFeed bv slice _ =
  let iter = slice.lsIteration
      maxIter = slice.lsMax
      planner = maybeCellText (globalLabel bv (Label "planner"))
      reviewA = maybeCellText (globalLabel bv (Label "reviewer-a"))
      reviewB = maybeCellText (globalLabel bv (Label "reviewer-b"))
      refinerPath = refinerBodyPath bv
      priorLines =
        concatMap
          (\(n, cell) -> ["Prior refiner " <> T.pack (show n) <> ":", cellText cell, ""])
          (zip ([1 :: Int ..] :: [Int]) (priorIterations bv refinerPath))
   in PromptArgs
        { history = [],
          prompt =
            T.unlines $
              [ "Refinement iteration " <> T.pack (show iter) <> "/" <> T.pack (show maxIter),
                "",
                "Re-read the full audit context from the blackboard and produce an improved consolidation.",
                "",
                "Original plan:",
                planner,
                "",
                "Reviewer A:",
                reviewA,
                "",
                "Reviewer B:",
                reviewB,
                ""
              ]
                ++ priorLines
                ++ [ "",
                     "Required output:",
                     "1) Consolidated findings by severity",
                     "2) Concrete next steps per finding",
                     "3) Brief conflict-resolution notes where drafts disagree",
                     "Do not ask for more input. Deliver the refined result immediately."
                   ]
        }

wf1DeciderContinue :: BlackboardView -> LoopSlice -> LoopDecision -> Bool
wf1DeciderContinue bv slice (LoopDecision wants _) =
  (slice.lsIteration < slice.lsMax)
    && ( let refinerPath = refinerBodyPath bv
             priorTexts = map cellText (priorIterations bv refinerPath)
             currentText = maybeCellText (atBodyLabel bv (Label "loop-refiner"))
             regressing = not (null priorTexts) && currentText == last priorTexts
             metaFailure =
               any
                 (`T.isInfixOf` currentText)
                 ["FAILED", "BLOCKED", "missing submission", "process failed"]
          in (metaFailure || (wants && not regressing))
       )

wf1FinalizerInput :: BlackboardView -> PromptArgs
wf1FinalizerInput bv =
  let planner = maybeCellText (globalLabel bv (Label "planner"))
      reviewA = maybeCellText (globalLabel bv (Label "reviewer-a"))
      reviewB = maybeCellText (globalLabel bv (Label "reviewer-b"))
      refinerPath = refinerBodyPath bv
      priorRefs = map cellText (priorIterations bv refinerPath)
      latestRef = maybeCellText (atBodyLabel bv (Label "loop-refiner"))
      allRefs =
        case priorRefs of
          [] -> [latestRef]
          _
            | latestRef == last priorRefs -> priorRefs
            | T.null latestRef -> priorRefs
            | otherwise -> priorRefs ++ [latestRef]
      iterInfo =
        maybe "unknown" (\s -> T.pack (show s.lsIteration) <> "/" <> T.pack (show s.lsMax)) (loopSlice bv)
   in PromptArgs
        { history = [],
          prompt =
            T.unlines $
              [ "Produce the final audit report from the full blackboard context below.",
                "Refinement loop completed after " <> iterInfo <> " iteration(s).",
                "",
                "Original plan:",
                planner,
                "",
                "Reviewer A findings:",
                reviewA,
                "",
                "Reviewer B findings:",
                reviewB,
                "",
                "Refinement history:"
              ]
                ++ concatMap (\t -> ["---", t, ""]) allRefs
                ++ [ "Required output order:",
                     "1) Executive summary",
                     "2) Findings by severity",
                     "3) Recommended action plan."
                   ]
        }

plannerAgent :: Agent
plannerAgent =
  Agent
    { agName = "wf1-planner",
      agSystemPrompt =
        Just $
          T.unlines
            [ "You are a planning agent with filesystem access.",
              "Use readdir and readfile to inspect the target project scope from the user prompt.",
              "Produce a concise audit plan with: files to inspect, risks, and expected output format.",
              "Do not fabricate file contents; only report what you inspected."
            ],
      agTools = ["readdir", "directory_tree", "read_file_paginated", "grep"],
      agMaxToolRounds = 30,
      agContextWindow = Nothing
    }

reviewerAgentA :: Agent
reviewerAgentA =
  Agent
    { agName = "wf1-reviewer-a",
      agSystemPrompt =
        Just $
          T.unlines
            [ "You are reviewer A (correctness and safety focus).",
              "Use readfile to inspect candidate files from the plan and conversation context.",
              "Return findings with severity levels and concrete evidence.",
              "If no issues are found, explicitly state that and what you checked.",
              "Do not discuss workflow status, missing submissions, blocked process, or reviewer coordination."
            ],
      agTools = ["read_file_paginated", "readdir", "grep"],
      agMaxToolRounds = 30,
      agContextWindow = Nothing
    }

reviewerAgentB :: Agent
reviewerAgentB =
  Agent
    { agName = "wf1-reviewer-b",
      agSystemPrompt =
        Just $
          T.unlines
            [ "You are reviewer B (maintainability and testability focus).",
              "Use readfile and readdir as needed.",
              "Return a structured review: missing tests, design debts, and improvement suggestions.",
              "Ground every claim in inspected files.",
              "Do not discuss workflow status, missing submissions, blocked process, or reviewer coordination."
            ],
      agTools = ["read_file_paginated", "readdir", "grep"],
      agMaxToolRounds = 30,
      agContextWindow = Nothing
    }

refinerAgent :: Agent
refinerAgent =
  Agent
    { agName = "wf1-refiner",
      agSystemPrompt =
        Just $
          T.unlines
            [ "You are a refiner agent.",
              "Input is a combined review draft assembled from the blackboard.",
              "Improve clarity, remove duplicates, and ensure each finding has actionable next steps.",
              "Keep technical precision and do not drop important findings.",
              "Never ask the user for more input. Produce the refined result immediately.",
              "Never claim the process failed or is blocked. Deliver a technical consolidation only.",
              "Every finding must be tied to inspected files or explicit evidence from the reviewer drafts."
            ],
      agTools = [],
      agMaxToolRounds = 4,
      agContextWindow = Nothing
    }

finalizerAgent :: Agent
finalizerAgent =
  Agent
    { agName = "wf1-finalizer",
      agSystemPrompt =
        Just $
          T.unlines
            [ "You are the finalizer.",
              "You receive the full audit context assembled from the blackboard.",
              "Produce the final audit report in this order:",
              "1) Executive summary",
              "2) Findings by severity",
              "3) Recommended action plan.",
              "Keep output concise and implementation-oriented.",
              "Never output review-process status language such as FAILED/BLOCKED/missing submissions.",
              "If evidence is weak, state uncertainty per finding, but still provide technical hypotheses and next verification steps."
            ],
      agTools = ["writefile"],
      agMaxToolRounds = 3,
      agContextWindow = Nothing
    }

deciderAgent :: Agent
deciderAgent =
  Agent
    { agName = "wf1-decider",
      agSystemPrompt =
        Just $
          T.unlines
            [ "You decide whether the refinement loop should continue.",
              "You receive a rich blackboard snapshot: plan, reviewer outputs, prior refiner iterations, and the latest refiner output.",
              "When ready, call submit_decision with shouldContinue and reason.",
              "Set shouldContinue=true only if substantial issues remain unresolved,",
              "or if the report quality is still too low for handoff.",
              "Prefer stopping once the report is coherent, deduplicated, and actionable.",
              "Set shouldContinue=true when output is meta/process-oriented (e.g. FAILED, BLOCKED, submission issues) instead of technical.",
              "Do not respond with raw JSON in plain text; always use the submit_decision tool."
            ],
      agTools = [],
      agMaxToolRounds = 2,
      agContextWindow = Nothing
    }
