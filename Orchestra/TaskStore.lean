import Lean.Data.Json
import Orchestra.Config
import Orchestra.Store
import Orchestra.Utils.Time
import Init.Data.String.Basic

/-!
# Task records

What orchestra remembers of a run once the daemon has started it: the prompt, the repositories,
the backend and identity it ran under, and how it ended. Rows of the `task` table of
`<data>/orchestra.db` (`Orchestra.Store`), keyed by the id `generateId` mints.

A record is the unit a continuation is built from, so every field a continuation inherits is on
it, and a series pointer — a row of `series` — is how `--series` finds the run to continue.
-/

open Lean (Json FromJson ToJson)

namespace Orchestra.TaskStore

-- Types

inductive TaskStatus where
  | running
  | completed
  | failed
  /-- The agent run was interrupted (e.g. usage limit hit or daemon stopped). -/
  | unfinished
  /-- The agent run was cancelled by the user via `queue cancel`. -/
  | cancelled
deriving Repr, BEq

instance : ToJson TaskStatus where
  toJson
    | .running    => "running"
    | .completed  => "completed"
    | .failed     => "failed"
    | .unfinished => "unfinished"
    | .cancelled  => "cancelled"

instance : FromJson TaskStatus where
  fromJson?
    | .str "running"    => .ok .running
    | .str "completed"  => .ok .completed
    | .str "failed"     => .ok .failed
    | .str "unfinished" => .ok .unfinished
    | .str "cancelled"  => .ok .cancelled
    | j => .error s!"expected task status string, got {j}"

structure TaskRecord where
  id            : String
  createdAt     : String
  /-- Repositories this run worked on, or `none` for a repository-independent run
      (see `IOTask.repo`). Inherited by continuations, like everything else here. -/
  repo          : Option RepoPair
  mode          : TaskMode      := .fork
  prompt        : String
  /-- Condition this run was held to (mirrors `IOTask.goal`). Inherited by continuations, so
      resuming a task keeps the bar it was launched against. -/
  goal          : Option String := none
  sessionId     : Option String := none
  status        : TaskStatus    := .running
  continuesFrom : Option String := none
  series        : Option String := none
  /-- Agent backend used for this run (e.g. "claude", "vibe"). Inherited by continuations. -/
  backend       : Option String := none
  /-- Model override used for this run. Inherited by continuations. -/
  model         : Option String := none
  /-- Sub-agent name used for this run. Inherited by continuations. -/
  agent         : Option String := none
  /-- System prompt file name used for this run. Inherited by continuations. -/
  systemPrompt  : Option String := none
  /-- Prepend prompt file name used for this run. Inherited by continuations. -/
  prependPrompt : Option String := none
  /-- Maximum spend in USD used for this run. -/
  budget        : Option Float  := none
  /-- Priority used for queue ordering. Defaults to 10. -/
  priority      : Nat           := 10
  /-- Orchestra project this task belongs to (optional). -/
  projectId     : Option Taxis.IssueId := none
  /-- Orchestra issue this task is/was working on (optional). -/
  issueId       : Option Taxis.IssueId   := none
  /-- Optional role name. -/
  role          : Option String    := none
  /-- The identity this run was performed under (`Orchestra.Identity`). Inherited by
      continuations, which is what it is here for: a continuation built from this record is the
      same work picked up again, and it has to be picked up by the same somebody. Also the only
      thing that says, of a run that has since been pruned from the queue, who wrote the comments
      it left on the tracker. -/
  identity      : Option String    := none
deriving Repr

instance : ToJson TaskRecord where
  toJson r :=
    let base : List (String × Json) :=
      [("id",         Json.str r.id),
       ("created_at", Json.str r.createdAt)]
      ++ repoPairFields r.repo
      ++ [("mode",   ToJson.toJson r.mode),
          ("prompt", Json.str r.prompt),
          ("status", ToJson.toJson r.status)]
    let fields := base
    let fields := if let some s := r.goal          then fields ++ [("goal",           Json.str s)]      else fields
    let fields := if let some s := r.sessionId     then fields ++ [("session_id",    Json.str s)]      else fields
    let fields := if let some s := r.continuesFrom then fields ++ [("continues_from", Json.str s)]     else fields
    let fields := if let some s := r.series        then fields ++ [("series",         Json.str s)]     else fields
    let fields := if let some s := r.backend       then fields ++ [("backend",        Json.str s)]     else fields
    let fields := if let some s := r.model         then fields ++ [("model",          Json.str s)]     else fields
    let fields := if let some s := r.agent         then fields ++ [("agent",          Json.str s)]     else fields
    let fields := if let some s := r.systemPrompt  then fields ++ [("system_prompt",  Json.str s)]     else fields
    let fields := if let some s := r.prependPrompt   then fields ++ [("prepend_prompt",  Json.str s)]     else fields
    let fields := if let some b := r.budget        then fields ++ [("budget",         ToJson.toJson b)] else fields
    let fields := if r.priority != 10           then fields ++ [("priority",         Json.num r.priority)] else fields
    let fields := if let some p := r.projectId   then fields ++ [("project_id",       ToJson.toJson p)]    else fields
    let fields := if let some i := r.issueId     then fields ++ [("issue_id",         ToJson.toJson i)]    else fields
    let fields := if let some s := r.role        then fields ++ [("role",             Json.str s)]         else fields
    let fields := if let some s := r.identity    then fields ++ [("identity",         Json.str s)]         else fields
    Json.mkObj fields

instance : FromJson TaskRecord where
  fromJson? j := do
    let id           ← j.getObjValAs? String "id"
    let createdAt    ← j.getObjValAs? String "created_at"
    let repo         ← parseRepoPair? j
    let mode         ← parseTaskMode? j
    let prompt       ← j.getObjValAs? String "prompt"
    let status       ← j.getObjValAs? TaskStatus "status"
    let goal          := j.getObjValAs? String "goal"           |>.toOption
    let sessionId     := j.getObjValAs? String "session_id"     |>.toOption
    let continuesFrom := j.getObjValAs? String "continues_from" |>.toOption
    let series        := j.getObjValAs? String "series"         |>.toOption
    let backend       := j.getObjValAs? String "backend"        |>.toOption
    let model         := j.getObjValAs? String "model"          |>.toOption
    let agent         := j.getObjValAs? String "agent"          |>.toOption
    let systemPrompt  := j.getObjValAs? String "system_prompt"  |>.toOption
    let prependPrompt   := j.getObjValAs? String "prepend_prompt"  |>.toOption
    let budget        := j.getObjValAs? Float  "budget"         |>.toOption
    let priority      := j.getObjValAs? Nat   "priority"      |>.toOption |>.getD 10
    let projectId     := j.getObjValAs? Taxis.IssueId "project_id" |>.toOption
    let issueId       := j.getObjValAs? Taxis.IssueId   "issue_id"   |>.toOption
    let role          := j.getObjValAs? String    "role"       |>.toOption
    let identity      := j.getObjValAs? String    "identity"   |>.toOption
    return { id, createdAt, repo, mode, prompt, goal, status, sessionId,
             continuesFrom, series, backend, model, agent, systemPrompt, prependPrompt, budget, priority,
             projectId, issueId, role, identity }

-- Directories

/-- `<data>/tasks`, which is no longer where task records live — they are rows in the `task`
    table. What is still written here is the per-run debug transcript `--debug` asks for
    (`TaskRunner`), which is a stream of text and not a record. -/
def tasksDir : IO System.FilePath :=
  return (← Dirs.dataBase) / "tasks"

/-- Where the JSON task records lived before the database. Read once, by
    `Orchestra.Store.Import`, and then left alone: the import copies, it does not delete. -/
def legacyTasksDir : IO System.FilePath := tasksDir

/-- Where the JSON series pointers lived before the database. As `legacyTasksDir`. -/
def legacySeriesDir : IO System.FilePath :=
  return (← Dirs.dataBase) / "series"

-- ID generation: the nanosecond monotonic clock plus a counter, so that two tasks starting
-- in the same nanosecond on different daemon workers cannot land on the same record.

def generateId : IO String := uniqueToken

-- Timestamp

def currentIso8601 : IO String := do
  let child ← IO.Process.spawn {
    cmd    := "date"
    args   := #["-u", "+%Y-%m-%dT%H:%M:%SZ"]
    stdout := .piped
    stderr := .null
    stdin  := .null
  }
  let out ← child.stdout.readToEnd
  let _   ← child.wait
  return out.trimAscii.toString

-- The row a task record is stored as

/-- The record as a row of the `task` table.

    Total: every field of the record has a column, and the ones that are not scalars go through
    the spelling their JSON instances already use — the status name, `"owner/repo"`, the decimal
    form of a taxis id — so that a row is readable and the legacy import can build one out of a
    file it has parsed without going through the record at all. -/
def TaskRecord.toRow (r : TaskRecord) : Store.TaskRow :=
  let (upstream, fork) := Store.repoColumns r.repo
  { id             := r.id
    created_at     := r.createdAt
    upstream       := upstream
    fork           := fork
    mode           := Store.enumColumn r.mode
    prompt         := r.prompt
    goal           := r.goal
    session_id     := r.sessionId
    status         := Store.enumColumn r.status
    continues_from := r.continuesFrom
    series         := r.series
    backend        := r.backend
    model          := r.model
    agent          := r.agent
    system_prompt  := r.systemPrompt
    prepend_prompt := r.prependPrompt
    budget         := r.budget
    priority       := Int.ofNat r.priority
    project_id     := Store.issueIdColumn r.projectId
    issue_id       := Store.issueIdColumn r.issueId
    role           := r.role
    identity       := r.identity }

/-- The record a row holds, or why this build cannot read it.

    Fails rather than guessing: a status name orchestra does not know is a record written by
    something else, and a listing that silently called it `failed` would be lying about a task
    that may well still be running. The callers report and skip. -/
def TaskRecord.ofRow? (row : Store.TaskRow) : Except String TaskRecord := do
  let repo      ← Store.repoOfColumns? row.upstream row.fork
  let mode      ← Store.enumOfColumn? "mode" row.mode
  let status    ← Store.enumOfColumn? "status" row.status
  let projectId ← Store.issueIdOfColumn? "project_id" row.project_id
  let issueId   ← Store.issueIdOfColumn? "issue_id" row.issue_id
  return { id            := row.id
           createdAt     := row.created_at
           repo          := repo
           mode          := mode
           prompt        := row.prompt
           goal          := row.goal
           sessionId     := row.session_id
           status        := status
           continuesFrom := row.continues_from
           series        := row.series
           backend       := row.backend
           model         := row.model
           agent         := row.agent
           systemPrompt  := row.system_prompt
           prependPrompt := row.prepend_prompt
           budget        := row.budget
           priority      := row.priority.toNat
           projectId     := projectId
           issueId       := issueId
           role          := row.role
           identity      := row.identity }

-- Storage

open Db.Query.DSL in
/-- Write the record, replacing whatever is under its id.

    One statement, deliberately: a task record is saved from several threads of the daemon and
    from the CLI at once, and a transaction around a single write would only lengthen the window
    the others wait on. -/
def saveTask (record : TaskRecord) : IO Unit :=
  Store.run <| HasModel.save record.toRow

open Db.Query.DSL in
def loadTask (id : String) : IO (Option TaskRecord) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let t ← from Store.TaskRow
    guard t.id = id
    select t
  return (← Store.keepConvertible "task" (·.id) TaskRecord.ofRow? rows)[0]?

open Db.Query.DSL in
/-- Load all task records, newest first.

    Ordered by `created_at`, not by id: ids come from a clock that restarts at boot, so after a
    reboot they sort every older record above every newer one. The id only breaks ties, which is
    what `Time.sortNewestFirst` did in memory and what the `(created_at, id)` index is for. -/
def loadAllTasks : IO (Array TaskRecord) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let t ← from Store.TaskRow
    select t
    order_by_desc t.created_at
    order_by_desc t.id
  Store.keepConvertible "task" (·.id) TaskRecord.ofRow? rows

/-! ## Query-shaped access

The history is the collection that grows without bound, and every one of these used to be a read
of all of it followed by a `filter` or a `take`. Each is now one statement inside one
`Store.run`, against an index `Store.target` declares for it. -/

open Db.Query.DSL in
/-- How many task records there are, counted by the database. -/
def count : IO Nat := do
  let n ← Store.run <| HasModel.count (QuerySet.all (α := Store.TaskRow))
  return n.toNat

open Db.Query.DSL in
/-- The `n` most recent task records, newest first.

    The overview shows ten of them. It used to read the whole history to find those ten. -/
def recent (n : Nat) : IO (Array TaskRecord) := do
  let rows ← Store.run <| HasModel.fetch <| (query% do
    let t ← from Store.TaskRow
    select t
    order_by_desc t.created_at
    order_by_desc t.id).limit n
  Store.keepConvertible "task" (·.id) TaskRecord.ofRow? rows

open Db.Query.DSL in
/-- One page of the history, newest first, and how many records the filter matched.

    `since?` is epoch seconds and keeps the records created at or after it; `skip` and `take` are
    the window. The total counts everything `since?` matched, before the window — which is what
    lets the dashboard say "50 of 812" rather than "the last 50 that exist". -/
def page (since? : Option Int) (skip take : Nat) : IO (Array TaskRecord × Nat) := do
  let bound := Store.sinceBound since?
  let matching : QuerySet Store.TaskRow := query% do
    let t ← from Store.TaskRow
    guard t.created_at ≥ bound
    select t
    order_by_desc t.created_at
    order_by_desc t.id
  let (rows, total) ← Store.run do
    let rows ← HasModel.fetch (matching.offset skip |>.limit take)
    let total ← HasModel.count matching
    pure (rows, total)
  return (← Store.keepConvertible "task" (·.id) TaskRecord.ofRow? rows, total.toNat)

open Db.Query.DSL in
/-- Every task recorded against an issue, newest first.

    The issue detail and `orchestra issue tasks` both want exactly this, and an issue's tasks are
    a handful out of a history of thousands. -/
def tasksForIssue (issueId : Taxis.IssueId) : IO (Array TaskRecord) := do
  let wanted := issueId.toString
  let rows ← Store.run <| HasModel.fetch <| query% do
    let t ← from Store.TaskRow
    guard t.issue_id = some wanted
    select t
    order_by_desc t.created_at
    order_by_desc t.id
  Store.keepConvertible "task" (·.id) TaskRecord.ofRow? rows

open Db.Query.DSL in
/-- Every task in a named series, newest first. -/
def tasksInSeries (seriesName : String) : IO (Array TaskRecord) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let t ← from Store.TaskRow
    guard t.series = some seriesName
    select t
    order_by_desc t.created_at
    order_by_desc t.id
  Store.keepConvertible "task" (·.id) TaskRecord.ofRow? rows

-- Series pointers

open Db.Query.DSL in
def latestInSeries (seriesName : String) : IO (Option String) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let s ← from Store.SeriesRow
    guard s.name = seriesName
    select s
  return rows[0]?.map (·.latest_task_id)

def updateSeriesPointer (seriesName taskId : String) : IO Unit :=
  Store.run <| HasModel.save ({ name := seriesName, latest_task_id := taskId } : Store.SeriesRow)

open Db.Query.DSL in
/-- Every series and the task it last pointed at, by name.

    New with the database. `orchestra series` used to answer this by listing the directory the
    pointers were files in; there is no directory to list any more, and one query is what the
    listing costs now. -/
def allSeries : IO (Array (String × String)) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let s ← from Store.SeriesRow
    select s
    order_by s.name
  return rows.map fun r => (r.name, r.latest_task_id)

end Orchestra.TaskStore
