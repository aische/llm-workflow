# llm-workflow

A **stack-based, multi-agent LLM workflow engine** for Haskell.

Define pipelines as a typed embedded DSL (`Workflow i o`): prompt agents, structured tool
submits, sequential and parallel composition, fixed and conditional loops, error recovery,
and nested workflows callable from tools. Execution is driven by a continuation stack that
handles LLM calls, tool rounds, transcript policies, usage accounting, and errors.

Built on [`llm-simple`](../llm-simple) for model calls, agents, and filesystem tools.

## Proof of concept

This project is an **experimental proof-of-concept**, not a production-ready library.

The goal is to explore a stack-based interpreter and composable workflow EDSL for
multi-agent LLM pipelines in Haskell. APIs, naming, and internals may change without
notice. The included demo (`app/Wf1.hs`, `app/Main.hs`) exercises the design; it is
not a polished product or a supported orchestration framework.

Use it to study or prototype the approach — not as a stable dependency.

## Why not just a prompt chain?

- **Composable** — `WSeq`, `WPar`, `WLoop`, `WLoopWhile`, `WCatch`, and `WMap` are first-class combinators.
- **Multi-agent** — different agents, system prompts, tools, and model fallbacks per step.
- **Structured output** — `WAgentSubmit` forces typed JSON via a dedicated submit tool.
- **Nested workflows** — expose a full workflow as a tool (`ToolWorkflow`).
- **Inspectable runtime** — step and continuation tracing during execution.

## Architecture

```
Workflow EDSL (what you write)
        │
        ▼
runWorkflow / eval loop
        │
        ▼
Stack ( Step, Kont )     ← control plane (scheduling, tool rounds)
        │
        ▼
llm-simple               ← generate, stream, tools, usage
```

At combinator boundaries, **transcript policies** (`TranscriptPolicy`, `MergePolicy`) wire
the previous step's output into the next step's input. Conversation history can be shared
across agents via optional `CID` keys on the continuation stack.

## Core combinators

| Combinator | Role |
|------------|------|
| `WPrompt` | Run an agent to completion (with optional tool rounds). |
| `WObject` | Generate a typed JSON object from an agent. |
| `WAgentSubmit` | Agent must call a submit tool with a typed result. |
| `WSeq` | Run two workflows in sequence; policy maps output → input. |
| `WPar` | Run two branches from the same input; policy merges outputs. |
| `WLoop` | Fixed iteration count with feedback policy. |
| `WLoopWhile` | Body + decider agent; loop while decider returns true. |
| `WCatch` | On failure, resume with a fallback value. |
| `WMap` | Post-process the final output via a transcript policy. |
| `WLift` / `WLiftW` | Embed pure IO or a dynamically chosen sub-workflow. |

## Quick example

```haskell
import LLM.Workflow

planner :: Workflow PromptArgs Final
planner = WPrompt (AgentWithModels plannerAgent models) Nothing

reviewer :: Workflow PromptArgs Final
reviewer = WPrompt (AgentWithModels reviewerAgent models) Nothing

pipeline :: Workflow PromptArgs Final
pipeline =
  WSeq planner reviewer TranscriptFinalToPromptArgs
```

`TranscriptFinalToPromptArgs` passes the predecessor's `Final.text` as the next prompt.

For a richer pipeline — planner, parallel reviewers, refiner, conditional loop, and
finalizer — see [`app/Wf1.hs`](app/Wf1.hs).

## Workflows as tools

A workflow can be exposed to an outer agent as a tool. The demo in [`app/Main.hs`](app/Main.hs)
runs a thin orchestrator that calls a `subagent` tool once; the tool spawns the full Wf1 audit
workflow via `ToolWorkflow`:

```haskell
ToolWorkflow (WMap wf TranscriptFinalText) (PromptArgs {history = [], prompt = auditPrompt})
```

This pattern is useful when you want a simple outer agent to delegate a multi-step pipeline
without re-implementing orchestration logic in the prompt.

## Running the demo

Prerequisites:

- [`llm-simple`](../llm-simple) checked out alongside this repo (see `cabal.project`).
- A configured [`model-catalog.json`](model-catalog.json) with the model names referenced in `Main.hs`.
- API keys / local providers as required by your catalog entries.
- A [`user-workspace/`](user-workspace/) directory for filesystem tools (created automatically or provided).

```bash
cabal build
cabal run llm-workflow
```

The executable runs an orchestrator that delegates to the Wf1 multi-agent audit workflow and
writes a summary report.

## Library API

The main entry points are:

- `runWorkflow` — execute a `Workflow i o` with a tool map and runtime args.
- `generateTextWF` — run a single agent through the same engine (supports `ToolWorkflow` tools).

Modules:

| Module | Purpose |
|--------|---------|
| `LLM.Workflow` | Public re-exports. |
| `LLM.Workflow.Types` | `Workflow` GADT, policies, stack types. |
| `LLM.Workflow.Workflow` | Interpreter (`runWorkflow`, `generateTextWF`). |
| `LLM.Workflow.ToolUtils` | Tool helpers, including workflow-as-tool wiring. |
| `LLM.Workflow.Utils` | Policy helpers, history lookup, tracing. |

## Status and limitations

- **`WPar` runs sequentially** — branches execute one after another (`KPar1` then `KPar2`), not concurrently. The combinator models structural parallelism for merging outputs, not parallel execution.
- **Policies are local** — each policy receives only the immediate predecessor value (or pair of branch outputs). Cross-step context requires hand-built prompts or custom `MergePolicyFunc` / `TranscriptPolicyFunc` wiring. A labeled blackboard for path-addressable workflow state is planned; see [`blackboard-planning.md`](blackboard-planning.md).

## License

BSD-3-Clause. See [LICENSE](LICENSE).
