module Wf1
  ( buildWf1Workflow,
  )
where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM (Agent (..))
import LLM.Generate (ModelWithFallbacks)
import LLM.Workflow
  ( AgentWithModels (..),
    LoopContext (..),
    PromptArgs (..),
    Workflow (..),
    emptyFinal,
  )
import LLM.Workflow.Types
  ( Final (..),
    MergePolicy (MergePolicyFunc),
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
    (WSeq initialDraft (mkConditionalLoop 4 refiner deciderWorkflow) TranscriptFinalToPromptArgs)
    (TranscriptPolicyFunc (\result -> result {text = "WF1 Result\n\n" <> result.text}))
  where
    planner = WPrompt (AgentWithModels plannerAgent models) Nothing
    reviewerA = WPrompt (AgentWithModels reviewerAgentA models2) Nothing
    reviewerB = WPrompt (AgentWithModels reviewerAgentB models) Nothing
    refiner = WPrompt (AgentWithModels refinerAgent models) Nothing
    finalizer = WPrompt (AgentWithModels finalizerAgent models) Nothing
    deciderSubmit = WAgentSubmit "submit_decision" (AgentWithModels deciderAgent models) Nothing

    initialDraft =
      WSeq planner reviewersCombined TranscriptFinalToPromptArgs

    reviewersCombined =
      WSeq
        (WPar safeReviewerA safeReviewerB reviewersToRefinerInput)
        refiner
        (TranscriptPolicyFunc id)
      where
        safeReviewerA = WCatch (emptyFinal "Reviewer A failed") reviewerA
        safeReviewerB = WCatch (emptyFinal "Reviewer B failed") reviewerB

    reviewersToRefinerInput :: MergePolicy Final Final PromptArgs
    reviewersToRefinerInput =
      MergePolicyFunc $ \reviewA reviewB ->
        PromptArgs
          { history = [],
            prompt =
              T.unlines
                [ "You are receiving two completed reviewer drafts.",
                  "Consolidate them into a final refined review now.",
                  "Do not ask for more input. Do not wait for user action.",
                  "",
                  "Reviewer A Draft:",
                  reviewA.text,
                  "",
                  "Reviewer B Draft:",
                  reviewB.text,
                  "",
                  "Required output:",
                  "1) Consolidated findings by severity",
                  "2) Concrete next steps per finding",
                  "3) Brief conflict-resolution notes where drafts disagree"
                ]
          }

    deciderWorkflow =
      WSeq
        (WLift (pure . loopContextToPromptArgs))
        deciderSubmit
        (TranscriptPolicyFunc id)

    mkConditionalLoop maxIterations body decider =
      WSeq
        (WLoopWhile maxIterations decider shouldContinuePolicy [] TranscriptFinalToPromptArgs body)
        finalizer
        TranscriptFinalToPromptArgs

    shouldContinuePolicy :: TranscriptPolicy LoopDecision Bool
    shouldContinuePolicy = TranscriptPolicyFunc (\d -> d.shouldContinue)

loopContextToPromptArgs :: LoopContext PromptArgs Final -> PromptArgs
loopContextToPromptArgs ctx =
  PromptArgs
    { history = [],
      prompt =
        T.unlines
          [ "Decide if another refinement iteration is needed.",
            "Iteration: " <> T.pack (show ctx.lcIteration) <> "/" <> T.pack (show ctx.lcMaxIterations),
            "Current prompt:",
            ctx.lcNextInput.prompt,
            "",
            "Latest output:",
            ctx.lcOutput.text,
            "",
            "Call the submit_decision tool with shouldContinue and reason."
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
              "Input is a combined review draft.",
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
            [ "You decide whether the loop should continue.",
              "You receive loop context including current output and past outputs.",
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