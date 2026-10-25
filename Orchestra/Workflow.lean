import Orchestra.Concert.Basic
import Lean.Data.Json

open Lean (Json FromJson ToJson)

namespace Orchestra

/-- Two-tier variable reference: global variable, step output, or loop-bound variable. -/
inductive VarRef where
  | global     : String → VarRef
  | stepOutput : String → String → VarRef
  | loopVar    : String → VarRef
  deriving Repr, BEq

/-- Condition expression for guards and filters. -/
inductive Cond where
  | ref : VarRef → Cond
  | gt  : VarRef → Int → Cond
  | lt  : VarRef → Int → Cond
  | eq  : VarRef → Int → Cond
  | mem : VarRef → VarRef → Cond
  | not : Cond → Cond
  | and : Cond → Cond → Cond
  | or  : Cond → Cond → Cond
  deriving Repr

/-- Global variable mutation. -/
inductive WriteOp where
  | assign    : VarRef → WriteOp
  | increment : WriteOp
  | decrement : WriteOp
  deriving Repr

/-- Named output produced by a task step. -/
structure OutputSpec where
  name    : String
  type    : ResultType
  writeTo : Option String := none
  deriving Repr

/-- Task specification for a workflow step. -/
structure TaskSpec where
  agent         : Option String   := none
  model         : Option String   := none
  /-- Maximum spend in USD for this step. Defaults to 4.0 if not set. -/
  budget        : Option Float    := none
  /-- Optional MCP tools to enable for this step, beyond the always-available ones
      (`health`, `refresh_token`, `get_pr_comments`) and the `get_task_input` /
      `submit_task_output` pair, which a step gets from its declared `input` / `output`
      regardless of what is named here.

      When absent, `TaskRunner.resolveTools` falls back to `mode` — which a concert always fixes
      at `fork`, so the step gets no tools at all. That fallback is the only behaviour a step
      could have before this field existed, and it remains what a step that names no tools gets.

      Validated against `knownTools` when the workflow is parsed. -/
  tools         : Option (List String) := none
  prompt        : String
  readOnly      : Bool            := false
  input         : List VarRef     := []
  output        : List OutputSpec := []
  context       : Option String   := none
  /-- Override the program-level upstream repository for this step. -/
  upstream      : Option Repository := none
  /-- Override the program-level fork repository for this step. -/
  fork          : Option Repository := none
  systemPrompt  : Option String   := none
  prependPrompt : Option String   := none
  backend       : Option String   := none
  /-- Issue or PR number to act on (required for the `triage` backend). -/
  issueNumber   : Option Nat      := none
  /-- The event variable `issue-number` names instead of a number — `issue-number: {{pr_number}}`
      — resolved into `issueNumber` by `WorkflowProgram.instantiate` once the event is known.
      The one field other than `prompt` an event fills in, and only with a whole number: see
      `instantiate` for why nothing else is. -/
  issueNumberVar : Option String  := none
  /-- Labels to add to the issue or PR when using the `triage` backend. -/
  triageAddLabels    : List String := []
  /-- Labels to remove from the issue or PR when using the `triage` backend. -/
  triageRemoveLabels : List String := []
  /-- The identity this step is performed under (`Orchestra.Identity`), by name.

      Per step rather than per program: the steps of one workflow are different jobs, and the
      reviewing step and the implementing step are the two most likely to want different
      identities. A program-wide default would have to be overridable per step anyway, and one
      way to say a thing beats two. -/
  identity           : Option String := none
  /-- The step's kleis source and narrowings (`kleis-token`, `kleis-spec`, `kleis-narrow`), as a
      task file's `kleis_token`, `kleis_spec` and `kleis_narrow` (`Orchestra.Kleis`). The token is
      a `{{secret}}` reference, quoted in the YAML. -/
  kleisToken         : Option String := none
  kleisSpec          : Option String := none
  kleisNarrow        : List String := []
  /-- The spec of the listener action that started the workflow, consulted after the step's own
      source and its identity's (`IOTask.kleisRoleSpec`). Not read from the YAML: set by
      `WorkflowProgram.withListenerKleis`. -/
  kleisRoleSpec      : Option String := none
  deriving Repr

/-- The tool names a step's `tools` field may ask for: the optional MCP tools and the project
    permission groups, which is what `Server.toolsList` filters on.

    The always-available tools (`health`, `refresh_token`, `get_pr_comments`) are left out
    because naming one grants nothing — they are already there.

    This is `Project.Role.knownPermissions` plus `merge_pr` and `create_repository`. A role is a
    template dispatched at whatever issue comes along, so it may neither land a pull request nor
    create a repository; a workflow step is written for one job, and naming the step that merges
    — or the step that creates the repository the rest of the workflow fills — is a decision the
    author has already made.

    Kept beside the field it validates, so the parser does not have to link the MCP server or the
    project tooling to reject a typo — which is the point of validating at all. An unknown name
    is otherwise invisible until the agent finds itself without the tool it was told to use. -/
def TaskSpec.knownTools : List String :=
  ["create_pr", "merge_pr", "comment", "label_issue", "create_repository",
   "manage_issues", "work_issues", "review_issues"]

/-- Specifies that a step iterates over a list. -/
structure ForClause where
  loopVar : String
  source  : VarRef
  deriving Repr

inductive FlowControl where
  | exit | doBreak | doContinue
  deriving Repr

inductive StepAction where
  | task  : TaskSpec → StepAction
  | write : String → WriteOp → StepAction
  | flow  : FlowControl → StepAction
  deriving Repr

structure Step where
  name   : String
  «for»  : Option ForClause := none
  guard  : Option Cond      := none
  action : StepAction
  deriving Repr

structure WorkflowProgram where
  name        : String
  description : Option String          := none
  upstream    : Option Repository := none
  fork        : Option Repository := none
  variables   : List (String × ResultType) := []
  steps       : List Step
  deriving Repr

/-- Every task step of `prog` with a listener action's `kleis_spec` as its role-level spec and
    the action's `kleis_narrow` added to its own: what a listener says of "every task it queues"
    holds of the tasks its workflow runs, as it does of the one task a listener without a workflow
    queues (`Listener.buildQueueEntry`). A narrowing left off a workflow's steps would be access
    the listener's author believes was taken away. -/
def WorkflowProgram.withListenerKleis (prog : WorkflowProgram) (spec : Option String)
    (narrow : List String) : WorkflowProgram :=
  { prog with steps := prog.steps.map fun s =>
      match s.action with
      | .task t => { s with action := .task { t with
          kleisRoleSpec := t.kleisRoleSpec <|> spec
          kleisNarrow := (t.kleisNarrow ++ narrow).eraseDups } }
      | _ => s }

/-- Replace each `{{name}}` in `template` whose name `vars` holds with its value, in one pass.

    One pass, so that what a value says is never read as a template itself: an issue title that
    contains `{{body}}` lands in the prompt as those eight characters, not as the issue's body.
    A placeholder `vars` does not hold is left standing, as `Listener.renderTemplate` leaves it. -/
partial def renderPlaceholders (template : String) (vars : List (String × String)) : String :=
  go template.toList []
where
  go : List Char → List Char → String
    | [], acc => String.ofList acc.reverse
    | '{' :: '{' :: rest, acc =>
      match closing rest [] with
      | some (name, after) =>
        match vars.lookup (String.ofList name) with
        | some value => go after (value.toList.reverse ++ acc)
        | none => go after (('}' :: '}' :: name.reverse) ++ '{' :: '{' :: acc)
      | none => go rest ('{' :: '{' :: acc)
    | c :: rest, acc => go rest (c :: acc)
  /-- The name up to the next `}}`, and what follows it; `none` when a `{` or a newline comes
      first, so `{{ {{x}}` renders its inner placeholder rather than swallowing the outer brace. -/
  closing : List Char → List Char → Option (List Char × List Char)
    | '}' :: '}' :: rest, name => some (name.reverse, rest)
    | '{' :: _, _ => none
    | '\n' :: _, _ => none
    | c :: rest, name => closing rest (c :: name)
    | [], _ => none

/-- The program with an event's variables filled in: every task step's `prompt`, and the
    `issue-number` of a step that names a variable for it.

    Filled in after the YAML is parsed, never before. Rendering the whole file first — which is
    what a listener used to do — put the event's text into the YAML *source*, so an issue title
    with a quote and a newline in it could close the prompt's string and write any field of the
    step after it: `read-only: false`, `tools`, `kleis-spec`, its `backend` and `budget`. Here an
    event can only ever be text inside a prompt, whatever it contains.

    The prompt is the only field rendered as text, because it is the only one meant to carry what
    somebody else wrote. `issue-number` takes a variable whole, and only when it reads as a whole
    number: it is what the event is *about*, which a workflow started by an event has no other way
    to name, and a number cannot say anything but which issue. Every other field — the tools, the
    access, the backend, the model, the budget, the repositories, the context label, the system
    prompt — is what the operator wrote, and a placeholder in one stays exactly the text it is.

    `vars` empty is a workflow run by hand, which has no event: prompts keep their placeholders,
    and a step whose issue number was to come from one is refused rather than run without it. -/
def WorkflowProgram.instantiate (prog : WorkflowProgram) (vars : List (String × String)) :
    Except String WorkflowProgram := do
  let steps ← prog.steps.mapM fun s => do
    match s.action with
    | .task t =>
      let issueNumber ← match t.issueNumberVar with
        | none => pure t.issueNumber
        | some v =>
          match vars.lookup v with
          | none => throw s!"step '{s.name}': issue-number is '{"{{"}{v}{"}}"}', which the event \
did not supply"
          | some value =>
            match value.trimAscii.toString.toNat? with
            | some n => pure (some n)
            | none => throw s!"step '{s.name}': issue-number is '{"{{"}{v}{"}}"}', which the event \
supplied as '{value}', not a whole number"
      pure { s with action := .task { t with
        prompt := renderPlaceholders t.prompt vars, issueNumber, issueNumberVar := none } }
    | _ => pure s
  return { prog with steps }

namespace Workflow

open Concert

private abbrev Env := List (String × Json)

private inductive LoopCtrl where
  | normal | doBreak | doContinue | doExit
  deriving Repr, BEq

private abbrev WorkflowM := StateT (Env × LoopCtrl) Concert

private def envKey : VarRef → String
  | .global n       => n
  | .stepOutput s o => s!"{s}.{o}"
  | .loopVar n      => n

private def envLookup (env : Env) (ref : VarRef) : Option Json :=
  List.lookup (envKey ref) env

private def envSet (env : Env) (key : String) (val : Json) : Env :=
  (key, val) :: env.filter (·.1 != key)

private def envAppend (env : Env) (key : String) (val : Json) : Env :=
  match List.lookup key env with
  | some (.arr arr) => envSet env key (.arr (arr.push val))
  | _               => envSet env key (.arr #[val])

private def jsonToInt (j : Json) : Option Int :=
  (FromJson.fromJson? j : Except String Int).toOption

private def evalCond (env : Env) : Cond → Bool
  | .ref r     => match envLookup env r with
                  | some (.bool b) => b
                  | _              => false
  | .gt r n    => ((envLookup env r).bind jsonToInt).map (fun x => decide (x > n)) |>.getD false
  | .lt r n    => ((envLookup env r).bind jsonToInt).map (fun x => decide (x < n)) |>.getD false
  | .eq r n    => ((envLookup env r).bind jsonToInt).map (fun x => decide (x == n)) |>.getD false
  | .mem r xs  => match envLookup env r, envLookup env xs with
                  | some v, some (.arr arr) => arr.any (· == v)
                  | _, _                    => false
  | .not c     => !evalCond env c
  | .and c1 c2 => evalCond env c1 && evalCond env c2
  | .or  c1 c2 => evalCond env c1 || evalCond env c2

private def buildInputSection (env : Env) (refs : List VarRef) : String :=
  let vals := refs.filterMap fun r => (envLookup env r).map (envKey r, ·)
  if vals.isEmpty then ""
  else "\n\nInput:\n" ++ (Json.mkObj vals).compress

private def writeOutput (stepName : String) (spec : OutputSpec)
    (v : Json) (accumulate : Bool) (env : Env) : Env :=
  let key := s!"{stepName}.{spec.name}"
  let env' := if accumulate then envAppend env key v else envSet env key v
  match spec.writeTo with
  | some g => envSet env' g v
  | none   => env'

private def execTask (prog : WorkflowProgram) (stepName : String) (spec : TaskSpec)
    (accumulate : Bool := false) : WorkflowM Unit := do
  let (env, _) ← get
  -- A step inherits the program's repositories unless it names its own, and a workflow that
  -- names neither runs its steps repository-independent: in a scratch workspace, with the
  -- repository-scoped tools withheld. That is the shape a workflow coordinating several projects
  -- through the issue tracker takes, where there is no one repository to check out.
  --
  -- Half a pair is refused rather than half-inherited. `upstream` from the program and `fork`
  -- from the step is a combination somebody meant; `upstream` alone, at either level, is a
  -- workflow missing a line.
  let repo ← match spec.upstream <|> prog.upstream, spec.fork <|> prog.fork with
    | none,          none      => pure none
    | some upstream, some fork => pure (some { upstream, fork : RepoPair })
    | _,             _         => StateT.lift Concert.abort
  let inputSection := buildInputSection env spec.input
  if spec.output.isEmpty then
    let ioTask : IOTask .unit .unit := {
      repo, mode := .fork
      prompt := spec.prompt ++ inputSection
      agent := spec.agent, model := spec.model, budget := spec.budget
      tools := spec.tools
      readOnly := spec.readOnly
      systemPrompt := spec.systemPrompt, prependPrompt := spec.prependPrompt
      backend := spec.backend
      issueNumber := spec.issueNumber
      triageAddLabels := spec.triageAddLabels
      triageRemoveLabels := spec.triageRemoveLabels
      identity := spec.identity
      kleisToken := spec.kleisToken, kleisSpec := spec.kleisSpec
      kleisRoleSpec := spec.kleisRoleSpec
      kleisNarrow := spec.kleisNarrow
    }
    StateT.lift (run ioTask ())
  else
    let mappingFields := spec.output.map fun o => (o.name, o.type)
    let outInstr := s!"\n\nCommunicate your result by calling the `submit_task_output` MCP tool with a JSON object matching this schema:\n{(ResultType.mapping mappingFields).toJsonSchema.compress}"
    let ioTask : IOTask .unit (.mapping mappingFields) := {
      repo, mode := .fork
      prompt := spec.prompt ++ inputSection ++ outInstr
      agent := spec.agent, model := spec.model, budget := spec.budget
      tools := spec.tools
      readOnly := spec.readOnly
      systemPrompt := spec.systemPrompt, prependPrompt := spec.prependPrompt
      backend := spec.backend
      issueNumber := spec.issueNumber
      triageAddLabels := spec.triageAddLabels
      triageRemoveLabels := spec.triageRemoveLabels
      identity := spec.identity
      kleisToken := spec.kleisToken, kleisSpec := spec.kleisSpec
      kleisRoleSpec := spec.kleisRoleSpec
      kleisNarrow := spec.kleisNarrow
    }
    let j ← StateT.lift (run ioTask ())
    modify fun (env', ctrl) =>
      let env'' := spec.output.foldl (fun acc ospec =>
        match j.getObjVal? ospec.name with
        | .ok v    => writeOutput stepName ospec v accumulate acc
        | .error _ => acc) env'
      (env'', ctrl)

private def execWrite (varName : String) (op : WriteOp) : WorkflowM Unit :=
  modify fun (env, ctrl) =>
    let newVal : Json := match op with
      | .assign ref  => (envLookup env ref).getD .null
      | .increment   => ToJson.toJson (((envLookup env (.global varName)).bind jsonToInt |>.getD 0) + 1)
      | .decrement   => ToJson.toJson (((envLookup env (.global varName)).bind jsonToInt |>.getD 0) - 1)
    (envSet env varName newVal, ctrl)

private def runAction (prog : WorkflowProgram) (stepName : String)
    (action : StepAction) (accumulate : Bool) : WorkflowM Unit :=
  match action with
  | .task spec    => execTask prog stepName spec accumulate
  | .write var op => execWrite var op
  | .flow .exit       => modify fun (e, _) => (e, .doExit)
  | .flow .doBreak    => modify fun (e, _) => (e, .doBreak)
  | .flow .doContinue => modify fun (e, _) => (e, .doContinue)

private partial def runForItems (prog : WorkflowProgram) (step : Step)
    (loopVar : String) (guard : Option Cond) (items : List Json) : WorkflowM Unit :=
  match items with
  | [] => return ()
  | item :: rest => do
    modify fun (env, ctrl) => (envSet env loopVar item, ctrl)
    let (env', _) ← get
    if guard.map (evalCond env') |>.getD true then
      runAction prog step.name step.action true
    let (_, ctrl) ← get
    match ctrl with
    | .doBreak    => modify fun (e, _) => (e, .normal)
    | .doExit     => return ()
    | .doContinue => do modify fun (e, _) => (e, .normal); runForItems prog step loopVar guard rest
    | .normal     => runForItems prog step loopVar guard rest

private def runStep (prog : WorkflowProgram) (step : Step) : WorkflowM Unit := do
  let (env, _) ← get
  match step.«for» with
  | none =>
    if step.guard.map (evalCond env) |>.getD true then
      runAction prog step.name step.action false
  | some fc =>
    let items : List Json := match envLookup env fc.source with
      | some (.arr arr) => arr.toList
      | some v          => [v]
      | none            => []
    runForItems prog step fc.loopVar step.guard items

private partial def runSteps (prog : WorkflowProgram) : List Step → WorkflowM Unit
  | []           => return ()
  | step :: rest => do
    runStep prog step
    let (_, ctrl) ← get
    if ctrl != .doExit then runSteps prog rest

def WorkflowProgram.toConcert (prog : WorkflowProgram)
    (initVars : List (String × Json) := []) : Concert Unit :=
  let initEnv : Env := prog.variables.map fun (name, _) =>
    (name, (List.lookup name initVars).getD .null)
  StateT.run' (runSteps prog prog.steps) (initEnv, .normal)

end Workflow
end Orchestra
