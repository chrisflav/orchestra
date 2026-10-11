import Lean.Data.Json
import Orchestra.Store
import Orchestra.TaskStore

/-!
# Where a running task has got to

A task is a loop — `init.sh`, then for each attempt `before.sh`, the agent and `validation.sh`, then
`after.sh` and the close — run by one daemon worker, and until now everything about where it was
lived in that worker's local variables. A daemon restart lost it, and with it the task: the next
daemon could only resume the conversation in a new pod (`Queue.resumeInterrupted`).

On a backend whose runs outlive the daemon (`Exec.Runner`), the agent and its scripts are still
running after the restart, and all the next daemon needs in order to carry the task on is to know
**which step it was at, in which environment, behind which MCP endpoint**. That is this record,
written by the task runner *before* each step starts (so that a daemon dying between writing it and
starting the step leaves a step that is simply started by the next one) and removed when the task
ends.

Everything else the loop holds is rebuilt rather than stored. Run ids are deterministic per step
(`agent-<attempt>`, `validate-<attempt>`, …) and starting one that exists only attaches to it, so
the next daemon resumes the loop at the recorded attempt and *replays* it: each step finds its run
already going or already finished — with its exit status and its output on disk in the pod — and
the decisions the loop makes from them come out as they did the first time. The attempt is what
makes the replay short; the phase is for the startup decision (a task in a phase nobody can pick up
is not picked up) and for a person asking where a stuck task is.

Secrets are in here — the task's MCP token, and its kleis token when there is one — because the
agent in the pod holds them and the next daemon has to accept them. They live exactly as long as the
task does, in the same database that holds the GitHub App's own record of the task, and the row is
removed when the task ends.
-/

open Lean (Json ToJson FromJson toJson fromJson?)

namespace Orchestra.RunState

/-- Which step of the task loop a task is at. Written before the step starts. -/
inductive Phase where
  /-- The environment is open and the MCP server up; `init.sh` next. -/
  | init
  /-- Attempt `attempt`: `before.sh`, the agent, `validation.sh`. -/
  | attempt
  /-- The loop is over: `after.sh`, then closing the environment. -/
  | after
  /-- Closing the environment: copying back, the finish command, deleting the pod. -/
  | closing
deriving Repr, BEq, Inhabited, DecidableEq

def Phase.toString : Phase → String
  | .init => "init"
  | .attempt => "attempt"
  | .after => "after"
  | .closing => "closing"

def Phase.ofString? : String → Option Phase
  | "init" => some .init
  | "attempt" => some .attempt
  | "after" => some .after
  | "closing" => some .closing
  | _ => none

/-- The kleis token minted for the task, kept so that the daemon that ends the task can revoke it
    (`Kleis.revoke`) and a retry after the restart can be launched with the same one. -/
structure KleisToken where
  token : String
  revocationId : String
  expires : Nat
deriving Repr, BEq, Inhabited

/-- Everything a daemon needs to carry a task on. See the module doc for what is deliberately
    *not* here. -/
structure RunState where
  /-- The step the task is at. -/
  phase : Phase
  /-- The attempt the task is at (`Phase.attempt`), or the last one it made (later phases), from
      zero. -/
  attempt : Nat := 0
  /-- The execution backend's name, as `execution.backend` names it: a daemon configured with a
      different one leaves the task to the restart resume. -/
  backend : String
  /-- What the backend needs to open the environment again (`Exec.Session.snapshot`). -/
  session : Json
  /-- The daemon-side checkout the task was opened on: what its repository configuration and hook
      scripts are read from, and what the session's paths are relative to. -/
  repoPath : String
  /-- The port the task's MCP server listens on. The agent's relay dials it again after a restart
      (`McpEndpoint.relayScript`), so the next daemon listens on it again. -/
  mcpPort : Nat
  /-- The token the agent presents to it. -/
  mcpToken : Option String := none
  /-- The task's kleis token, when kleis holds its GitHub credentials. -/
  kleis : Option KleisToken := none
  /-- The authentication source the task resolved to. The agent in the pod is already running on
      it, and a retry resumes its conversation, which only that account has seen. -/
  authLabel : Option String := none
deriving BEq, Inhabited

/-- The record as JSON, which is what goes in the `state` column. -/
def toJson (s : RunState) : Json :=
  Json.mkObj ([
    ("version", Json.num 1),
    ("phase", Json.str s.phase.toString),
    ("attempt", Lean.toJson s.attempt),
    ("backend", Json.str s.backend),
    ("session", s.session),
    ("repo_path", Json.str s.repoPath),
    ("mcp_port", Lean.toJson s.mcpPort)]
    ++ (match s.mcpToken with | some t => [("mcp_token", Json.str t)] | none => [])
    ++ (match s.kleis with
        | some k => [("kleis", Json.mkObj [("token", .str k.token),
                        ("revocation_id", .str k.revocationId),
                        ("expires", Lean.toJson k.expires)])]
        | none => [])
    ++ (match s.authLabel with | some l => [("auth_label", Json.str l)] | none => []))

/-- Read a record back, or say why it cannot be. Strict: a record this build does not fully
    understand is one it should not try to carry a task on from — the task is resumed the old way
    instead, which loses the step in flight and nothing else. -/
def ofJson? (j : Json) : Except String RunState := do
  let version ← j.getObjValAs? Nat "version"
  unless version == 1 do throw s!"run state version {version} is not one this build reads"
  let phaseStr ← j.getObjValAs? String "phase"
  let some phase := Phase.ofString? phaseStr | throw s!"unknown phase '{phaseStr}'"
  let attempt ← j.getObjValAs? Nat "attempt"
  let backend ← j.getObjValAs? String "backend"
  let session ← j.getObjVal? "session"
  let repoPath ← j.getObjValAs? String "repo_path"
  let mcpPort ← j.getObjValAs? Nat "mcp_port"
  if mcpPort == 0 || mcpPort > 65535 then throw s!"mcp_port {mcpPort} is not a port"
  let mcpToken := (j.getObjValAs? String "mcp_token").toOption
  let kleis ← match j.getObjVal? "kleis" with
    | .error _ => pure none
    | .ok k => do
      pure (some { token := ← k.getObjValAs? String "token"
                   revocationId := ← k.getObjValAs? String "revocation_id"
                   expires := ← k.getObjValAs? Nat "expires" : KleisToken })
  let authLabel := (j.getObjValAs? String "auth_label").toOption
  return { phase, attempt, backend, session, repoPath, mcpPort, mcpToken, kleis, authLabel }

-- Storage

open Db.Query.DSL in
/-- Record where task `taskId` is, replacing what was recorded before. -/
def save (taskId : String) (s : RunState) : IO Unit := do
  let row : Store.TaskRunStateRow := {
    task_id := taskId
    updated_at := ← TaskStore.currentIso8601
    phase := s.phase.toString
    attempt := s.attempt
    state := (toJson s).compress }
  Store.run <| HasModel.save row

open Db.Query.DSL in
/-- What is recorded for task `taskId`: `none` when nothing is, `some (.error _)` when something is
    and this build cannot read it. -/
def load (taskId : String) : IO (Option (Except String RunState)) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let r ← from Store.TaskRunStateRow
    guard r.task_id = taskId
    select r
  return rows[0]?.map fun r =>
    match Json.parse r.state with
    | .ok j => ofJson? j
    | .error e => .error s!"run state is not JSON: {e}"

open Db.Query.DSL in
/-- Forget task `taskId`'s record: the task has ended, or will not be picked up again. -/
def delete (taskId : String) : IO Unit := do
  let _ ← Store.run <| HasModel.delete (α := Store.TaskRunStateRow)
    (.eq (.var Store.TaskRunStateRowIndex.task_id .text) (.text taskId))


open Db.Query.DSL in
/-- Every task with something on record, for a starting daemon to sweep what no running entry
    explains any more. -/
def allTaskIds : IO (Array String) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let r ← from Store.TaskRunStateRow
    select r
  return rows.map (·.task_id)

end Orchestra.RunState
