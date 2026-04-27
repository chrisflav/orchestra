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
  agent    : Option String   := none
  model    : Option String   := none
  prompt   : String
  readOnly : Bool            := false
  input    : List VarRef     := []
  output   : List OutputSpec := []
  context  : Option String   := none
  /-- Override the program-level upstream repository for this step. -/
  upstream : Option String   := none
  /-- Override the program-level fork repository for this step. -/
  fork     : Option String   := none
  deriving Repr

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
  upstream    : String := ""
  fork        : String := ""
  variables   : List (String × ResultType) := []
  steps       : List Step
  deriving Repr

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
  let upstream     := spec.upstream.getD prog.upstream
  let fork         := spec.fork.getD prog.fork
  let inputSection := buildInputSection env spec.input
  if spec.output.isEmpty then
    let ioTask : IOTask .unit .unit := {
      upstream, fork, mode := .fork
      prompt := spec.prompt ++ inputSection
      agent := spec.agent, model := spec.model, readOnly := spec.readOnly
    }
    StateT.lift (run ioTask ())
  else
    let schema := Json.mkObj (spec.output.map fun o => (o.name, o.type.toJsonSchema))
    let outInstr := s!"\n\nReturn a JSON object with these fields:\n{schema.compress}"
    let ioTask : IOTask .unit .string := {
      upstream, fork, mode := .fork
      prompt := spec.prompt ++ inputSection ++ outInstr
      agent := spec.agent, model := spec.model, readOnly := spec.readOnly
    }
    let result ← StateT.lift (run ioTask ())
    match Json.parse result with
    | .error _ => StateT.lift Concert.abort
    | .ok j    =>
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

def WorkflowProgram.toConcert (prog : WorkflowProgram) : Concert Unit :=
  let initEnv : Env := prog.variables.map fun (name, _) => (name, .null)
  StateT.run' (runSteps prog prog.steps) (initEnv, .normal)

end Workflow
end Orchestra
