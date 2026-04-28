# workflow DSL and concert execution

Orchestra supports a YAML workflow language that describes multi-step agent
programs. Workflows are first-class programs: they can be run directly, queued
for background execution, or triggered automatically by a listener.

## running a workflow

Pass a `.yaml` file directly to `run`:

```
orchestra run workflow.yaml
```

The workflow is parsed, compiled into a _Concert_ (see below), and evaluated
immediately. All the usual flags apply:

```
orchestra run --debug workflow.yaml
```

## workflow file structure

```yaml
name: my-workflow
description: Optional human-readable description

# Default repository for every step. Both fields are optional if every
# step supplies its own upstream/fork.
upstream: owner/repo
fork: your-org/fork

# Global mutable variables. Initialized to null; steps can write to them.
variables:
  counter:
    type: int
  notes:
    type: list string

steps:
  step-name:
    # step definition (see below)
```

### step types

Every key under `steps` is a step name. The value specifies what the step does.

#### task step

Run an agent task:

```yaml
steps:
  implement:
    task:
      agent: claude        # optional backend agent name
      model: sonnet        # optional model override
      read-only: true      # mount repo read-only (default: false)
      prompt: "Implement feature X."

      # Override the program-level repository for this step only:
      upstream: other-owner/other-repo
      fork: your-org/other-fork

      # Pass values from the environment as input to the agent.
      # Values are appended to the prompt as a JSON object.
      input:
        - previous-step.output-name
        - global-var-name

      # Declare what values the agent must return. The agent receives
      # a JSON schema instruction appended to its prompt and must call
      # the submit_task_output MCP tool with a conforming JSON object.
      output:
        result:
          type: string
        score:
          type: int
          write_to: total-score   # also write this value to a global variable

      # Emit the context label for the task session (used for resumption).
      context: my-series
```

#### for step

Iterate over a list variable. The loop variable is bound for each iteration:

```yaml
steps:
  process-items:
    for:
      item: choose-step.items   # VarRef to a list value
    task:
      prompt: "Process the item."
      input:
        - item
      output:
        result:
          type: string
```

An optional guard filters which items are processed:

```yaml
  process-items:
    for:
      item: choose-step.items
    if:
      item > 0
    task:
      prompt: "Process positive items only."
      input:
        - item
```

#### conditional step (if / guard)

Skip a step unless a condition holds:

```yaml
steps:
  bail-on-hard-task:
    if:
      evaluate.difficulty > 8
    flow:
      exit
```

A guard can also be placed on a task step:

```yaml
  optional-review:
    if:
      evaluate.needs_review
    task:
      prompt: "Review the completed work."
      read-only: true
```

#### write step

Mutate a global variable without running an agent:

```yaml
steps:
  reset-counter:
    write:
      counter: 0          # assign a literal (not yet supported)
```

Write operations available in code: `assign` (copy from another variable),
`increment`, `decrement`.

#### flow control

Exit the entire workflow, break out of the current loop, or skip to the next
iteration:

```yaml
steps:
  abort-if-empty:
    if:
      result.items
    flow:
      exit   # or: break, continue
```

### variable references

Variable names used in `input`, `if`, `for.source` and `write_to` fields:

| Form | Example | Refers to |
|------|---------|-----------|
| `step.output` | `evaluate.score` | Named output from a previous step |
| `variable-name` | `total-score` | Global variable declared in `variables:` |
| bare name in `for` body | `item` | The current loop variable |

### output types

The `type` field in an `output` block accepts:

| Type | JSON representation |
|------|---------------------|
| `string` | JSON string |
| `int` | JSON integer (may be negative) |
| `nat` | JSON non-negative integer |
| `bool` | JSON boolean |
| `list string` | JSON array of strings |
| `list int` | JSON array of integers |

Nested list types (e.g. `list (list string)`) are also supported.

### MCP tools for input and output

When a task step declares `input` or `output`, orchestra exposes corresponding
MCP tools to the agent running that step:

- **`get_task_input`** — available when the step's input type is non-unit.
  Returns the serialized input value. The tool description includes a
  human-readable type description and the JSON schema for the value.
- **`submit_task_output`** — available when the step's output type is non-unit.
  Accepts a `value` field conforming to the output type's JSON schema. The
  agent must call this tool exactly once before finishing.

## example workflows

### simple sequence

```yaml
name: plan-implement-review
upstream: owner/repo
fork: your-org/fork

steps:
  plan:
    task:
      model: sonnet
      read-only: true
      prompt: "Make a detailed plan for implementing the given task."

  implement:
    task:
      model: sonnet
      prompt: "Implement the plan from the previous step."

  review:
    task:
      model: sonnet
      read-only: true
      prompt: "Review the completed implementation."
```

### conditional exit

```yaml
name: guarded-implementation
upstream: owner/repo
fork: your-org/fork

steps:
  evaluate-difficulty:
    task:
      model: sonnet
      read-only: true
      prompt: "Rate the difficulty of this task from 0 to 10."
      output:
        difficulty:
          type: int

  check-difficulty:
    if:
      evaluate-difficulty.difficulty > 8
    flow:
      exit

  implement:
    task:
      model: sonnet
      prompt: "Implement the task."
```

### for loop with typed output

```yaml
name: joke-machine
upstream: owner/repo
fork: your-org/fork

variables:
  total-joke-score:
    type: int

steps:
  choose-animals:
    task:
      model: opus
      read-only: true
      prompt: "Choose 5 different animals."
      output:
        animals:
          type: list string

  joke:
    for:
      animal: choose-animals.animals
    task:
      model: sonnet
      read-only: true
      input:
        - animal
      prompt: "Tell a joke about the given animal."
      output:
        joke:
          type: string

  evaluate:
    task:
      model: opus
      read-only: true
      input:
        - choose-animals.animals
        - joke.joke
      prompt: "Rate all the jokes combined from 0 to 5."
      output:
        score:
          type: int
          write_to: total-joke-score
```

## concert execution model

Internally, workflows compile to _Concert_ programs — a free monad over two
primitive operations:

- **`run t input`** — run task `t` with the given typed input and return its
  typed output.
- **`while cond body`** — repeatedly execute `body` while `cond` evaluates to
  true.

Two evaluators are provided:

### `eval` — direct evaluation

Each `run` step is executed immediately in the current process via
`TaskRunner.runIOTask`. Steps execute serially; the process blocks until the
step completes. Used when running `orchestra run workflow.yaml` directly.

### `evalQueued` — queue-based evaluation

Each `run` step is submitted as a `QueueEntry` to the queue and the concert
fiber suspends until the queue daemon signals completion. Used when a listener
triggers a workflow via `workflow_path`.

`evalQueued` relies on `ConcertManager` — a registry of one-shot `IO.Promise`
values keyed by step ID. The sequence per step is:

1. Concert fiber registers a promise with `ConcertManager.register stepKey`.
2. A `QueueEntry` is written with `concertStepKey = stepKey`.
3. The fiber calls `IO.wait promise.result!` and suspends.
4. The queue daemon picks up the entry, runs the task, and calls
   `ConcertManager.signal stepKey result` with the serialized output.
5. The promise is resolved; the fiber wakes up and continues with the decoded
   output value.

Multiple concerts can be in flight simultaneously. Each concert fiber holds its
own suspended state and its own set of registered promises.

## queue integration

### startup and resilience

When the queue daemon starts it performs two cleanup steps:

1. **`markStaleRunningAsUnfinished`** — entries stuck in `running` from a
   previous crash are moved to `unfinished`.
2. **`cancelStaleConcertEntries`** — any `unfinished` entry that carries a
   `concertStepKey` is cancelled immediately. The concert fiber that registered
   the promise died with the previous daemon process and cannot be resumed.

### queue entry fields for concerts

`QueueEntry` carries three additional fields used by concerts:

| Field | JSON key | Purpose |
|-------|----------|---------|
| `concertStepKey` | `concert_step_key` | Links the entry to a suspended concert fiber |
| `inputType` | `input_type` | Typed input for the MCP `get_task_input` tool |
| `outputType` | `output_type` | Typed output for the MCP `submit_task_output` tool |
| `inputJson` | `input_json` | Serialized input value delivered via `get_task_input` |

### parallel-safe claiming

A `BaseMutex` (`claimMutex`) serialises the read-then-mark-running pair so
that concurrent workers cannot claim the same entry. The worker loop structure
is:

```
loop:
  acquire claimMutex
  entry ← nextPending
  if entry: mark entry running
  release claimMutex
  if entry: runEntry entry
  else: sleep 1 s
```

Each running task receives its own `CancellationToken`. All active tokens are
held in a mutex-protected array keyed by a monotonic `Nat` ID. The file-watcher
that detects `cancel.request` iterates the array and cancels every token.

## listeners with workflow_path

A listener action can trigger a multi-step workflow instead of a single task by
setting `workflow_path`:

```json
{
  "name": "issue-workflow",
  "source": {
    "type": "github-issues",
    "repos": [{"upstream": "owner/repo", "fork": "your-org/fork"}],
    "trigger": "@orchestra"
  },
  "action": {
    "upstream": "{{upstream}}",
    "fork": "{{fork}}",
    "mode": "fork",
    "prompt_template": "Issue #{{issue_number}}: {{body}}",
    "workflow_path": "/path/to/workflow.yaml"
  },
  "interval_seconds": 120
}
```

When `workflow_path` is set the listener:

1. Reads and parses the YAML workflow.
2. Substitutes `upstream` and `fork` from the event into the parsed program.
3. Spawns a new `IO.asTask` fiber that calls `evalQueued` with the concert
   manager and app config.

The fiber runs independently. The main listener poll loop continues without
waiting for the workflow to complete. Multiple workflow instances can run
concurrently.

When `workflow_path` is absent the listener enqueues a single `QueueEntry` as
before.

## per-step repository override

Both the workflow YAML and the `TaskSpec` structure support optional `upstream`
and `fork` fields. When present on a step they take precedence over the
program-level defaults:

```yaml
steps:
  cross-repo-task:
    task:
      upstream: other-owner/other-repo
      fork: your-org/other-fork
      prompt: "Work on the other repository."
```

When absent the program-level `upstream`/`fork` values are used. Steps that
span different repositories within a single workflow are therefore fully
supported.
