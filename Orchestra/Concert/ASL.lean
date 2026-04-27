import Orchestra.Concert.Basic
import Orchestra.Concert.Json

open Lean (Json FromJson)

namespace Orchestra.Concert.ASL

open Orchestra

-- `Concert α : Type 1`, so `Except String (Concert α) : Type 1`.
-- Standard `getObjValAs?` returns `Except.{0,0}`, which cannot be mixed with
-- `Except.{0,1}` in `do` notation.  These two helpers bridge the gap.

/-- Bind a Type 0 `Except` result into a Type 1 continuation. -/
private def withParsed {α : Type} {β : Type 1}
    (e : Except String α) (f : α → Except String β) : Except String β :=
  match e with
  | .ok a    => f a
  | .error s => .error s

/-- Sequence two Type 1 `Except` computations. -/
private def andThen {α β : Type 1}
    (e : Except String α) (f : α → Except String β) : Except String β :=
  match e with
  | .ok a    => f a
  | .error s => .error s

/-- Parse an `IOTask` from an ASL Task state's JSON.
    Looks for fields in the `Parameters` sub-object first,
    falling back to the state object itself. -/
private def parseIOTask (state : Json) : Except String (IOTask .unit .unit) :=
  let params := (state.getObjVal? "Parameters").toOption.getD state
  FromJson.fromJson? params

/-- Parse an ASL state machine JSON into a `Concert Unit` program.

    Supported state types:
    - **Task** — parsed as `Concert.run` with an `IOTask .unit .unit`
      (fields read from `Parameters` or the state object)
    - **Pass** — no-op, proceeds to next state
    - **Succeed** — `Concert.pure ()`
    - **Fail** — `Concert.abort`

    Transitions follow `Next` / `End` fields.
    Choice, Parallel, Wait, and Map states are not yet supported. -/
partial def fromASL (j : Json) : Except String (Concert Unit) :=
  withParsed (j.getObjValAs? String "StartAt") fun startAt =>
  withParsed (j.getObjVal? "States") fun states =>
  compileState states startAt
where
  compileState (states : Json) (name : String) : Except String (Concert Unit) :=
    withParsed (states.getObjVal? name) fun state =>
    withParsed (state.getObjValAs? String "Type") fun type =>
    match type with
    | "Task" =>
        withParsed (parseIOTask state) fun ioTask =>
        andThen (transition states state) fun rest =>
        .ok (run ioTask () >>= fun () => rest)
    | "Pass" =>
        transition states state
    | "Succeed" => .ok (Concert.pure ())
    | "Fail"    => .ok Concert.abort
    | t => .error s!"unsupported ASL state type: {t}"
  transition (states : Json) (state : Json) : Except String (Concert Unit) :=
    match state.getObjValAs? Bool "End" with
    | .ok true => .ok (Concert.pure ())
    | _ =>
        withParsed (state.getObjValAs? String "Next") fun next =>
        compileState states next

end Orchestra.Concert.ASL
