import Lean.Data.Json
import Orchestra.Config
import Orchestra.Store
import Orchestra.TaskStore
import Orchestra.Utils.Time

/-!
# The queue

What is waiting to run, what is running, and what a run of a concert workflow has got to. Rows of
the `queue_entry` and `concert_run` tables of the record database (`Orchestra.Store`); what is
left in `<data>/queue` is the daemon's pid file, its socket and its log.

An entry is a task's whole launch configuration, written once when it is queued and written back
when it is claimed — the slot it got, the authentication source it drew, the task it became. Three
processes write here at once (the daemon's workers, its claim loop, a CLI enqueueing), which is
what the busy timeout on every connection is for; every save is a single statement.

Below the storage sit the pure decisions the daemon makes out of these records — the claim order,
the slot choice, the reaping rules — which are ordinary functions over the entries and are tested
as such.
-/

open Lean (Json FromJson ToJson)

namespace Orchestra.Queue

-- Types

inductive QueueStatus where
  | pending
  | running
  | done
  | failed
  /-- The agent run was interrupted (usage limit hit or daemon stopped). May be retried. -/
  | unfinished
  /-- Cancelled because a dependency or same-backend task hit the usage limit. -/
  | cancelled
deriving Repr, BEq

instance : ToJson QueueStatus where
  toJson
    | .pending    => "pending"
    | .running    => "running"
    | .done       => "done"
    | .failed     => "failed"
    | .unfinished => "unfinished"
    | .cancelled  => "cancelled"

instance : FromJson QueueStatus where
  fromJson?
    | .str "pending"    => .ok .pending
    | .str "running"    => .ok .running
    | .str "done"       => .ok .done
    | .str "failed"     => .ok .failed
    | .str "unfinished" => .ok .unfinished
    | .str "cancelled"  => .ok .cancelled
    | j => .error s!"expected queue status string, got {j}"

-- Concert run tracking

inductive ConcertStatus where
  | running | done | failed | cancelled
  deriving Repr, BEq

instance : ToJson ConcertStatus where
  toJson | .running => "running" | .done => "done" | .failed => "failed" | .cancelled => "cancelled"

instance : FromJson ConcertStatus where
  fromJson?
    | .str "running"   => .ok .running
    | .str "done"      => .ok .done
    | .str "failed"    => .ok .failed
    | .str "cancelled" => .ok .cancelled
    | j => .error s!"expected concert status, got {j}"

structure ConcertRun where
  id           : String
  startedAt    : String
  status       : ConcertStatus := .running
  name         : Option String := none
  workflowFile : Option String := none
  finishedAt   : Option String := none

instance : ToJson ConcertRun where
  toJson r :=
    let fields : List (String × Json) := [("id", r.id), ("started_at", r.startedAt),
      ("status", ToJson.toJson r.status)]
    let fields := if let some n := r.name         then fields ++ [("name",          Json.str n)] else fields
    let fields := if let some f := r.workflowFile then fields ++ [("workflow_file", Json.str f)] else fields
    let fields := if let some t := r.finishedAt   then fields ++ [("finished_at",   Json.str t)] else fields
    Json.mkObj fields

instance : FromJson ConcertRun where
  fromJson? j := do
    let id          ← j.getObjValAs? String "id"
    let startedAt   ← j.getObjValAs? String "started_at"
    let status      ← j.getObjValAs? ConcertStatus "status"
    let name         := j.getObjValAs? String "name"          |>.toOption
    let workflowFile := j.getObjValAs? String "workflow_file" |>.toOption
    let finishedAt   := j.getObjValAs? String "finished_at"   |>.toOption
    return { id, startedAt, status, name, workflowFile, finishedAt }

-- Queue entries

structure QueueEntry where
  id            : String
  createdAt     : String
  status        : QueueStatus   := .pending
  /-- Repositories this entry works on, or `none` for a repository-independent entry that runs
      in a scratch workspace instead of a clone slot (see `IOTask.repo`). -/
  repo          : Option RepoPair
  mode          : TaskMode      := .fork
  prompt        : String
  /-- Condition the run is held to (mirrors `IOTask.goal`). Set from the bound issue's taxis
      `goal` field when the entry is built for one, so it survives enqueue → dequeue → run. -/
  goal          : Option String := none
  agent         : Option String := none
  systemPrompt  : Option String := none
  prependPrompt : Option String := none
  backend       : Option String := none
  model         : Option String := none
  continuesFrom : Option String := none
  series        : Option String := none
  taskId        : Option String := none
  /-- Index of the per-repo clone slot this entry ran in, recorded when it is claimed.

      Persisted rather than kept in daemon memory so that a continuation entry can find the
      workspace its predecessor left behind even across a daemon restart: the successor looks
      up the entry whose `taskId` it continues from and asks for that same slot. -/
  slot          : Option Nat    := none
  configPath    : Option String := none
  /-- Maximum spend in USD. Defaults to 4.0 if not set. -/
  budget        : Option Float  := none
  /-- Which memory directories to make available to the agent. Defaults to `both`. -/
  memory        : MemoryMode    := .both
  /-- The identity this entry is to be run under, by name (`Orchestra.Identity`).

      Resolved when the entry runs rather than when it is queued, like `authSources` and for a
      similar reason: an entry can wait hours for a slot, and the record is configuration that
      may have been rewritten in between. -/
  identity      : Option String := none
  /-- Label of the authentication source to use. Must match a label in the backend's `auth_sources`.

      What whoever queued the entry asked for, and nothing else: the daemon's own choice goes to
      `resolvedAuthSource`. It used to be written back here, and an entry put back to `pending`
      after a failed run then waited for the account that run had landed on, as if pinned to it. -/
  authSource    : Option String := none
  /-- The source the daemon resolved this entry to when it claimed it: which account ran it. -/
  resolvedAuthSource : Option String := none
  /-- Candidate authentication sources for this entry, tried according to `authMode`.

      Resolved when the entry is claimed rather than when it is created: an entry may sit pending
      for hours, and the source that was free when a listener queued it may be exhausted by the
      time a worker is ready for it. -/
  authSources   : List String := []
  /-- How to choose among the candidates. Absent defers to the backend's `default_auth_mode`,
      which is what a pooled `default_auth_source` is walked with. -/
  authMode      : Option AuthMode := none
  /-- Optional tools to enable beyond the always-available ones.
      When absent, allowed tools are derived from `mode` for backwards compatibility. -/
  tools         : Option (List String) := none
  /-- If true, the project folder is mounted read-only in the sandbox. -/
  readOnly      : Bool := false
  /-- Priority of this queue entry. Natural number; higher = more important.
      Defaults to 10 if not set. -/
  priority      : Nat := 10
  /-- Key linking this entry to a suspended concert fiber. When set, the queue
      daemon signals the ConcertManager with the task output after completion. -/
  concertStepKey : Option String := none
  /-- ID of the concert run that created this step entry. -/
  concertId     : Option String := none
  /-- Input type for the task. Controls which MCP tools are exposed. -/
  inputType     : ResultType     := .unit
  /-- Output type for the task. Controls which MCP tools are exposed. -/
  outputType    : ResultType     := .unit
  /-- Serialized task input, delivered via the `get_task_input` MCP tool. -/
  inputJson     : Option Json    := none
  /-- Serialized task output, written by the daemon after the task completes. -/
  outputJson    : Option Json    := none
  /-- Issue or PR number this task was launched from. Enables the `comment` tool. -/
  issueNumber : Option Nat := none
  /-- Orchestra project this entry belongs to (optional).
      Distinct from `issueNumber` (a GitHub issue number). -/
  projectId : Option Taxis.IssueId := none
  /-- Orchestra issue this entry is working on (optional). -/
  issueId : Option Taxis.IssueId := none
  /-- Optional role name (mirrors `IOTask.role`). Used by the dispatcher to
      count per-role active tasks unambiguously. -/
  role : Option String := none
  /-- Labels to apply automatically to every PR created via `create_pr`. -/
  prLabels : List String := []
  /-- Labels to add to the issue or PR when using the `triage` backend. -/
  triageAddLabels : List String := []
  /-- Labels to remove from the issue or PR when using the `triage` backend. -/
  triageRemoveLabels : List String := []
  /-- Name of the listener that created this entry, if any. -/
  listenerName : Option String := none
  /-- What the task this entry becomes may itself put on the queue (`Orchestra.Spawn`).
      `none` means nothing, and the `queue_task` tool is not offered to it.

      Deliberately not inherited by a continuation, unlike the fields `TaskStore.TaskRecord`
      carries across one: `max_tasks` is counted per spawning task id, so a continuation that
      carried the policy would take a fresh allowance under its new id — the exact reset that
      counting over the queue rather than in memory exists to prevent. A continuation that should
      queue work is one to give a policy of its own. -/
  spawnPolicy : Option SpawnPolicy := none
  /-- The id of the task that queued this entry through `queue_task`, when one did.

      Provenance, and the counter behind `SpawnPolicy.maxTasks`: the ceiling is enforced by
      counting the entries carrying a task's id rather than by a tally in memory, so a daemon
      restart mid-run cannot hand an agent its whole allowance a second time. -/
  spawnedBy : Option String := none
  /-- The subtree this entry's task may write at or below, overriding what it would derive from
      its own issue and project (`Config.IOTask.scopeRoot`). Set only on an entry `queue_task`
      created, where it holds the queueing task's own scope. -/
  scopeRoot : Option Taxis.IssueId := none

/-- The pool this entry draws its workspace slot from.

    One pool per fork repository, plus a single shared pool for repository-independent entries —
    which therefore share the `--parallel-per-repo` budget with each other, and with nothing
    else. Spelled without a `/` so it can never collide with an `owner/repo`. -/
def QueueEntry.slotKey (e : QueueEntry) : String :=
  match e.repo with
  | some r => r.fork.toString
  | none   => "(no repository)"

instance : ToJson QueueEntry where
  toJson e :=
    let fields : List (String × Json) :=
      [("id",     Json.str e.id),
       ("created_at", Json.str e.createdAt),
       ("status", ToJson.toJson e.status)]
      ++ repoPairFields e.repo
      ++ [("mode",   ToJson.toJson e.mode),
          ("prompt", Json.str e.prompt)]
    let fields := if let some s := e.goal          then fields ++ [("goal",            Json.str s)]      else fields
    let fields := if let some s := e.agent         then fields ++ [("agent",           Json.str s)]      else fields
    let fields := if let some s := e.systemPrompt  then fields ++ [("system_prompt",   Json.str s)]      else fields
    let fields := if let some s := e.prependPrompt   then fields ++ [("prepend_prompt",   Json.str s)]      else fields
    let fields := if let some s := e.backend       then fields ++ [("backend",         Json.str s)]      else fields
    let fields := if let some s := e.model         then fields ++ [("model",           Json.str s)]      else fields
    let fields := if let some s := e.continuesFrom then fields ++ [("continues_from",  Json.str s)]      else fields
    let fields := if let some s := e.series        then fields ++ [("series",          Json.str s)]      else fields
    let fields := if let some s := e.taskId        then fields ++ [("task_id",         Json.str s)]      else fields
    let fields := if let some s := e.configPath    then fields ++ [("config_path",     Json.str s)]      else fields
    let fields := if let some b := e.budget        then fields ++ [("budget",          ToJson.toJson b)] else fields
    let fields := fields ++ [("memory", ToJson.toJson e.memory)]
    let fields := if let some s := e.identity      then fields ++ [("identity",        Json.str s)]      else fields
    let fields := if let some s := e.authSource    then fields ++ [("auth_source",     Json.str s)]      else fields
    let fields := if let some s := e.resolvedAuthSource then fields ++ [("resolved_auth_source", Json.str s)] else fields
    let fields := if !e.authSources.isEmpty        then fields ++ [("auth_sources",    ToJson.toJson e.authSources)] else fields
    let fields := if let some m := e.authMode      then fields ++ [("auth_mode",       ToJson.toJson m)]             else fields
    let fields := if let some t := e.tools         then fields ++ [("tools",           ToJson.toJson t)] else fields
    let fields := if e.readOnly                    then fields ++ [("read_only",        Json.bool true)]  else fields
    let fields := if e.priority != 10              then fields ++ [("priority",         Json.num e.priority)]           else fields
    let fields := if let some s := e.concertStepKey then fields ++ [("concert_step_key", Json.str s)]                  else fields
    let fields := if let some s := e.concertId      then fields ++ [("concert_id",       Json.str s)]                  else fields
    let fields := if e.inputType != .unit           then fields ++ [("input_type",       ToJson.toJson e.inputType)]   else fields
    let fields := if e.outputType != .unit          then fields ++ [("output_type",      ToJson.toJson e.outputType)]  else fields
    let fields := if let some j := e.inputJson       then fields ++ [("input_json",          j)]                           else fields
    let fields := if let some j := e.outputJson      then fields ++ [("output_json",         j)]                           else fields
    let fields := if let some n := e.issueNumber then fields ++ [("issue_number", Json.num n)] else fields
    let fields := if let some p := e.projectId then fields ++ [("project_id", ToJson.toJson p)] else fields
    let fields := if let some i := e.issueId   then fields ++ [("issue_id",   ToJson.toJson i)] else fields
    let fields := if let some r := e.role         then fields ++ [("role",          Json.str r)]      else fields
    let fields := if !e.prLabels.isEmpty          then fields ++ [("pr_labels",            ToJson.toJson e.prLabels)]         else fields
    let fields := if !e.triageAddLabels.isEmpty   then fields ++ [("triage_add_labels",    ToJson.toJson e.triageAddLabels)]   else fields
    let fields := if !e.triageRemoveLabels.isEmpty then fields ++ [("triage_remove_labels", ToJson.toJson e.triageRemoveLabels)] else fields
    let fields := if let some s := e.listenerName then fields ++ [("listener_name", Json.str s)]      else fields
    let fields := if let some p := e.spawnPolicy  then fields ++ [("spawn_policy",  ToJson.toJson p)] else fields
    let fields := if let some s := e.spawnedBy    then fields ++ [("spawned_by",    Json.str s)]      else fields
    let fields := if let some r := e.scopeRoot    then fields ++ [("scope_root",    ToJson.toJson r)]  else fields
    Json.mkObj fields

instance : FromJson QueueEntry where
  fromJson? j := do
    let id           ← j.getObjValAs? String "id"
    let createdAt    ← j.getObjValAs? String "created_at"
    let status       ← j.getObjValAs? QueueStatus "status"
    let repo         ← parseRepoPair? j
    let mode         ← parseTaskMode? j
    let prompt       ← j.getObjValAs? String "prompt"
    let goal          := j.getObjValAs? String "goal"           |>.toOption
    let agent         := j.getObjValAs? String "agent"          |>.toOption
    let systemPrompt  := j.getObjValAs? String "system_prompt"  |>.toOption
    let prependPrompt   := j.getObjValAs? String "prepend_prompt"  |>.toOption
    let backend       := j.getObjValAs? String "backend"        |>.toOption
    let model         := j.getObjValAs? String "model"          |>.toOption
    let continuesFrom := j.getObjValAs? String "continues_from" |>.toOption
    let series        := j.getObjValAs? String "series"         |>.toOption
    let taskId        := j.getObjValAs? String "task_id"        |>.toOption
    let configPath    := j.getObjValAs? String "config_path"    |>.toOption
    let budget        := j.getObjValAs? Float      "budget"  |>.toOption
    let memory        := j.getObjValAs? MemoryMode "memory"  |>.toOption |>.getD .both
    let identity      := j.getObjValAs? String "identity"    |>.toOption
    let authSource    := j.getObjValAs? String "auth_source" |>.toOption
    let resolvedAuthSource := j.getObjValAs? String "resolved_auth_source" |>.toOption
    let authSources   := j.getObjValAs? (List String) "auth_sources" |>.toOption |>.getD []
    let authMode      := j.getObjValAs? AuthMode "auth_mode" |>.toOption
    let tools         := j.getObjValAs? (List String) "tools" |>.toOption
    let readOnly      := j.getObjValAs? Bool "read_only" |>.toOption |>.getD false
    let priority       := j.getObjValAs? Nat        "priority"         |>.toOption |>.getD 10
    let concertStepKey := j.getObjValAs? String    "concert_step_key" |>.toOption
    let concertId      := j.getObjValAs? String    "concert_id"       |>.toOption
    let inputType      := j.getObjValAs? ResultType "input_type"      |>.toOption |>.getD .unit
    let outputType     := j.getObjValAs? ResultType "output_type"     |>.toOption |>.getD .unit
    let inputJson        := j.getObjVal?   "input_json"          |>.toOption
    let outputJson       := j.getObjVal?   "output_json"         |>.toOption
    let issueNumber := j.getObjValAs? Nat "issue_number" |>.toOption
    let projectId   := j.getObjValAs? Taxis.IssueId "project_id" |>.toOption
    let issueId     := j.getObjValAs? Taxis.IssueId   "issue_id"   |>.toOption
    let role         := j.getObjValAs? String    "role"          |>.toOption
    let prLabels          := j.getObjValAs? (List String) "pr_labels"           |>.toOption |>.getD []
    let triageAddLabels    := j.getObjValAs? (List String) "triage_add_labels"    |>.toOption |>.getD []
    let triageRemoveLabels := j.getObjValAs? (List String) "triage_remove_labels" |>.toOption |>.getD []
    let listenerName := j.getObjValAs? String "listener_name"    |>.toOption
    -- Lenient, unlike the role and listener documents that carry the same field. Those are
    -- written by hand, so a typo there is worth refusing; a queue entry is written by orchestra
    -- itself, so nothing here can be typed wrong — and `loadEntry` turns *any* decode failure
    -- into "no such entry", which for an entry holding a pre-claimed issue would mean a task
    -- that never runs, never appears in the queue, and a claim nobody ever releases.
    let spawnPolicy  := (parseSpawnPolicy? j).toOption.getD none
    let spawnedBy    := j.getObjValAs? String "spawned_by"       |>.toOption
    let scopeRoot    := j.getObjValAs? Taxis.IssueId "scope_root" |>.toOption
    return { id, createdAt, status, repo, mode, prompt, goal,
             agent, systemPrompt, prependPrompt, backend, model, continuesFrom, series, taskId, configPath,
             budget, memory, identity, authSource, resolvedAuthSource, authSources, authMode, tools,
             readOnly, priority, concertStepKey, concertId, inputType, outputType, inputJson, outputJson,
             issueNumber, projectId, issueId, role, prLabels, triageAddLabels, triageRemoveLabels,
             listenerName, spawnPolicy, spawnedBy, scopeRoot }

-- Directories and paths

/-- `<data>/queue`, which no longer holds the entries — those are rows in the `queue_entry`
    table. What is still here is the daemon's own furniture: its pid file, its socket and its
    log, none of which is a record. -/
def queueDir : IO System.FilePath :=
  return (← Dirs.dataBase) / "queue"

def pidFile : IO System.FilePath :=
  return (← queueDir) / "daemon.pid"

def socketFile : IO System.FilePath :=
  return (← queueDir) / "daemon.sock"

def daemonLogFile : IO System.FilePath :=
  return (← queueDir) / "daemon.log"

/-- Where the JSON concert runs lived before the database. Read once, by
    `Orchestra.Store.Import`, and then left alone. The entries' own legacy directory is
    `queueDir` itself, which is why that one has no `legacy` twin. -/
def legacyConcertsDir : IO System.FilePath :=
  return (← Dirs.dataBase) / "concerts"

-- The rows an entry and a concert run are stored as

/-- The entry as a row of the `queue_entry` table.

    Everything that is not a scalar goes into a `text` column as the compressed JSON its own
    instance writes — the label lists, the declared input and output types, the spawn policy, and
    the task input and output, which are whatever those types say and so have nothing to be
    decoded into here. An empty list is `[]` rather than NULL: `tools` is the one field where
    absent and empty differ, and it is the one that is nullable. -/
def QueueEntry.toRow (e : QueueEntry) : Store.QueueEntryRow :=
  let (upstream, fork) := Store.repoColumns e.repo
  { id                   := e.id
    created_at           := e.createdAt
    status               := Store.enumColumn e.status
    upstream             := upstream
    fork                 := fork
    mode                 := Store.enumColumn e.mode
    prompt               := e.prompt
    goal                 := e.goal
    agent                := e.agent
    system_prompt        := e.systemPrompt
    prepend_prompt       := e.prependPrompt
    backend              := e.backend
    model                := e.model
    continues_from       := e.continuesFrom
    series               := e.series
    task_id              := e.taskId
    slot                 := e.slot.map Store.natColumn
    config_path          := e.configPath
    -- NaN and the infinities have no SQL literal; an unusable budget is stored as no budget.
    budget               := Store.optFloatColumn e.budget
    memory               := Store.enumColumn e.memory
    identity             := e.identity
    auth_source          := e.authSource
    resolved_auth_source := e.resolvedAuthSource
    auth_sources         := Store.jsonColumn e.authSources
    auth_mode            := e.authMode.map Store.enumColumn
    tools                := e.tools.map Store.jsonColumn
    read_only            := e.readOnly
    priority             := Store.natColumn e.priority
    concert_step_key     := e.concertStepKey
    concert_id           := e.concertId
    input_type           := Store.jsonColumn e.inputType
    output_type          := Store.jsonColumn e.outputType
    input_json           := Store.rawJsonColumn e.inputJson
    output_json          := Store.rawJsonColumn e.outputJson
    issue_number         := e.issueNumber.map Store.natColumn
    project_id           := Store.issueIdColumn e.projectId
    issue_id             := Store.issueIdColumn e.issueId
    role                 := e.role
    pr_labels            := Store.jsonColumn e.prLabels
    triage_add_labels    := Store.jsonColumn e.triageAddLabels
    triage_remove_labels := Store.jsonColumn e.triageRemoveLabels
    listener_name        := e.listenerName
    spawn_policy         := e.spawnPolicy.map Store.jsonColumn
    spawned_by           := e.spawnedBy
    scope_root           := Store.issueIdColumn e.scopeRoot }

/-- The entry a row holds, or why this build cannot read it.

    Lenient column by column exactly where `FromJson QueueEntry` is lenient about the same field,
    and for the reason that instance gives: `loadEntry` turns *any* failure here into "no such
    entry", and for an entry holding a pre-claimed issue that means a task that never runs and a
    claim nobody ever releases. So a value this build cannot read in one of those columns costs
    that column — it falls back to the default the JSON instance would have given it — rather
    than the whole entry. The id, the timestamp, the status, the mode, the repository pair and
    the memory mode are the strict ones, there as in the JSON. -/
def QueueEntry.ofRow? (row : Store.QueueEntryRow) : Except String QueueEntry := do
  let repo               ← Store.repoOfColumns? row.upstream row.fork
  let status             ← Store.enumOfColumn? "status" row.status
  let mode               ← Store.enumOfColumn? "mode" row.mode
  let memory             ← Store.enumOfColumn? "memory" row.memory
  let authMode           ← row.auth_mode.mapM (Store.enumOfColumn? (α := AuthMode) "auth_mode")
  let authSources        := (Store.jsonOfColumn? (α := List String) "auth_sources"
                              row.auth_sources).toOption.getD []
  let tools              := row.tools.bind fun s =>
                              (Store.jsonOfColumn? (α := List String) "tools" s).toOption
  let inputType          := (Store.jsonOfColumn? (α := ResultType) "input_type"
                              row.input_type).toOption.getD .unit
  let outputType         := (Store.jsonOfColumn? (α := ResultType) "output_type"
                              row.output_type).toOption.getD .unit
  let inputJson          ← Store.rawJsonOfColumn? "input_json" row.input_json
  let outputJson         ← Store.rawJsonOfColumn? "output_json" row.output_json
  let projectId          := (Store.issueIdOfColumn? "project_id" row.project_id).toOption.getD none
  let issueId            := (Store.issueIdOfColumn? "issue_id" row.issue_id).toOption.getD none
  let scopeRoot          := (Store.issueIdOfColumn? "scope_root" row.scope_root).toOption.getD none
  let prLabels           := (Store.jsonOfColumn? (α := List String) "pr_labels"
                              row.pr_labels).toOption.getD []
  let triageAddLabels    := (Store.jsonOfColumn? (α := List String) "triage_add_labels"
                              row.triage_add_labels).toOption.getD []
  let triageRemoveLabels := (Store.jsonOfColumn? (α := List String) "triage_remove_labels"
                              row.triage_remove_labels).toOption.getD []
  let spawnPolicy        := row.spawn_policy.bind fun s =>
                              (Store.jsonOfColumn? (α := SpawnPolicy) "spawn_policy" s).toOption
  return { id := row.id
           createdAt := row.created_at
           status, repo, mode
           prompt := row.prompt
           goal := row.goal
           agent := row.agent
           systemPrompt := row.system_prompt
           prependPrompt := row.prepend_prompt
           backend := row.backend
           model := row.model
           continuesFrom := row.continues_from
           series := row.series
           taskId := row.task_id
           slot := row.slot.map Int.toNat
           configPath := row.config_path
           budget := row.budget
           memory
           identity := row.identity
           authSource := row.auth_source
           resolvedAuthSource := row.resolved_auth_source
           authSources, authMode, tools
           readOnly := row.read_only
           priority := row.priority.toNat
           concertStepKey := row.concert_step_key
           concertId := row.concert_id
           inputType, outputType, inputJson, outputJson
           issueNumber := row.issue_number.map Int.toNat
           projectId, issueId
           role := row.role
           prLabels, triageAddLabels, triageRemoveLabels
           listenerName := row.listener_name
           spawnPolicy
           spawnedBy := row.spawned_by
           scopeRoot }

/-- The concert run as a row of the `concert_run` table. -/
def ConcertRun.toRow (r : ConcertRun) : Store.ConcertRunRow :=
  { id            := r.id
    started_at    := r.startedAt
    status        := Store.enumColumn r.status
    name          := r.name
    workflow_file := r.workflowFile
    finished_at   := r.finishedAt }

/-- The concert run a row holds, or why this build cannot read it. -/
def ConcertRun.ofRow? (row : Store.ConcertRunRow) : Except String ConcertRun := do
  let status ← Store.enumOfColumn? "status" row.status
  return { id           := row.id
           startedAt    := row.started_at
           status       := status
           name         := row.name
           workflowFile := row.workflow_file
           finishedAt   := row.finished_at }

-- Storage

open Db.Query.DSL in
/-- Write the entry, replacing whatever is under its id.

    One statement. Entries are written by the daemon's workers, by its claim loop and by a CLI
    enqueueing, all at once; the busy timeout on each connection is what orders them, and a
    transaction around a single write would only make the others wait longer. -/
def saveEntry (entry : QueueEntry) : IO Unit :=
  Store.run <| HasModel.save entry.toRow

open Db.Query.DSL in
def loadEntry (id : String) : IO (Option QueueEntry) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let e ← from Store.QueueEntryRow
    guard e.id = id
    select e
  return (← Store.keepConvertible "queue entry" (·.id) QueueEntry.ofRow? rows)[0]?

open Db.Query.DSL in
/-- Load all queue entries, newest first.

    By `created_at` rather than id, for the reason `TaskStore.loadAllTasks` gives. -/
def loadAllEntries : IO (Array QueueEntry) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let e ← from Store.QueueEntryRow
    select e
    order_by_desc e.created_at
    order_by_desc e.id
  Store.keepConvertible "queue entry" (·.id) QueueEntry.ofRow? rows

/-- Entries in the order the daemon tries them: priority first (higher wins), then oldest.

    Oldest by `created_at`, not by id. Ids are minted from a clock that restarts at boot, so
    across a reboot the smaller id is the *newer* entry: an entry left pending from before the
    reboot sorted last and waited behind everything enqueued since, indefinitely.

    Separate from `pendingCandidates` because `orchestra status` wants the order without the
    slot-availability filter, and reaching it through that filter meant passing a `perRepoLimit`
    chosen to be inert — which stops being inert the moment the argument grows a second use.
    Filtering and ordering are two questions; this answers one of them. -/
def claimOrder (entries : Array QueueEntry) : Array QueueEntry :=
  let keyed := entries.map fun e => (Time.ageKey e.createdAt e.id, e)
  (keyed.qsort fun a b =>
    if a.2.priority != b.2.priority then a.2.priority > b.2.priority
    else Time.AgeKey.before (newest := false) a.1 b.1).map (·.2)

/-- Every pending entry that may start now, in the order the daemon should try them.

    `activePerRepo` maps a slot-pool key (`QueueEntry.slotKey`) to the number of tasks currently
    running in that pool; entries whose pool is already at `perRepoLimit` are excluded.
    Ordered by `claimOrder`.

    A list rather than a single entry, because an entry can turn out to be unclaimable for a
    reason only the caller knows — a continuation whose predecessor's slot is still busy, or a
    backend that needs the daemon to itself. Returning just the best candidate would let one
    such entry stall every other repository behind it for as long as its blocker runs. -/
def pendingCandidates (all : Array QueueEntry) (activePerRepo : Std.HashMap String Nat)
    (perRepoLimit : Nat) : Array QueueEntry :=
  let pending := all.filter (fun e =>
    e.status == .pending &&
    activePerRepo.getD e.slotKey 0 < perRepoLimit)
  claimOrder pending

/-! ## Query-shaped access

Everything below is a question the callers used to answer by loading the whole queue and
filtering the array: what is pending, what is active, whose entry a task is, how many entries a
task has spawned. Each is one statement against an index `Store.target` declares for exactly it,
inside one `Store.run`.

They sit here rather than up with `saveEntry` because the first of them hands its rows back in
`claimOrder`, the pure ordering just above. -/

open Db.Query.DSL in
/-- Every pending entry, in the order the daemon should try them.

    The order is `claimOrder`'s and is computed here rather than asked of the database: it turns
    on `Time.ageKey`, which decides what an unparseable timestamp is worth, and a record store
    that ordered one way in SQL and another way in memory would be two orders to keep in step.
    The query's business is the filter, which is what used to cost a pass over every entry. -/
def pendingEntries : IO (Array QueueEntry) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let e ← from Store.QueueEntryRow
    guard e.status = Store.enumColumn QueueStatus.pending
    select e
  return claimOrder (← Store.keepConvertible "queue entry" (·.id) QueueEntry.ofRow? rows)

open Db.Query.DSL in
/-- Every entry that is pending or running, newest first.

    "Active" in the sense every caller of it means: work the daemon either holds or still owes.
    The listener caps, `orchestra status` and the project health check all count over exactly
    this set. -/
def activeEntries : IO (Array QueueEntry) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let e ← from Store.QueueEntryRow
    guard e.status = Store.enumColumn QueueStatus.pending
       ∨ e.status = Store.enumColumn QueueStatus.running
    select e
    order_by_desc e.created_at
    order_by_desc e.id
  Store.keepConvertible "queue entry" (·.id) QueueEntry.ofRow? rows

open Db.Query.DSL in
/-- Every entry the queue calls `running`, newest first. What the task reaper sweeps. -/
def runningEntries : IO (Array QueueEntry) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let e ← from Store.QueueEntryRow
    guard e.status = Store.enumColumn QueueStatus.running
    select e
    order_by_desc e.created_at
    order_by_desc e.id
  Store.keepConvertible "queue entry" (·.id) QueueEntry.ofRow? rows

/-- How many running entries each authentication source has, as a lookup.

    Exact for the daemon's own claims, including ones made a moment ago: a claim writes the entry
    `running`, with the source it resolved, before it releases the claim mutex, so the next
    resolution under that mutex already counts it. Not keyed by backend, because labels are what
    a pool names and two backends sharing a label would be sharing an account. -/
def runningPerAuthSource : IO (String → Nat) := do
  let mut counts : Std.HashMap String Nat := {}
  for e in ← runningEntries do
    if let some l := e.resolvedAuthSource.orElse (fun _ => e.authSource) then
      counts := counts.insert l (counts.getD l 0 + 1)
  return fun l => counts.getD l 0

open Db.Query.DSL in
/-- The entry whose run became task `taskId`, if there is one.

    A task and the entry it came from are numbered separately, so this is how a continuation
    finds the entry its predecessor was — the lookup the claim loop used to do with
    `all.find? (·.taskId == some tid)` over every entry in the queue.

    Ordered, like `findEntry`, so that the one it answers with is the newest of however many
    entries name that task rather than whichever the database happened to hand back first. -/
def entryForTask (taskId : String) : IO (Option QueueEntry) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let e ← from Store.QueueEntryRow
    guard e.task_id = some taskId
    select e
    order_by_desc e.created_at
    order_by_desc e.id
  return (← Store.keepConvertible "queue entry" (·.id) QueueEntry.ofRow? rows)[0]?

open Db.Query.DSL in
/-- The entry addressed by either of the two ids it answers to: its own, or the task its run
    became.

    The dashboard's task detail and its cancel button are both reached from a link that may carry
    either id, and neither knows which it has. One query over both columns rather than a load of
    the whole queue and a `find?` over it. -/
def findEntry (id : String) : IO (Option QueueEntry) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let e ← from Store.QueueEntryRow
    guard e.id = id ∨ e.task_id = some id
    select e
    order_by_desc e.created_at
    order_by_desc e.id
  let entries ← Store.keepConvertible "queue entry" (·.id) QueueEntry.ofRow? rows
  -- The entry's own id wins: an entry can carry a task id that is some *other* entry's id only
  -- by accident, and a caller that named an entry meant that entry.
  return entries.find? (·.id == id) |>.orElse fun _ => entries[0]?

open Db.Query.DSL in
/-- How many entries task `taskId` has put on the queue.

    Counted by the database. Terminal entries count too: the spawn policy's ceiling is on how
    much work a task may create, not on how much of it is still running. -/
def countSpawnedBy (taskId : String) : IO Nat := do
  let n ← Store.run <| HasModel.count <| query% do
    let e ← from Store.QueueEntryRow
    guard e.spawned_by = some taskId
    select e
  return n.toNat

open Db.Query.DSL in
/-- The steps of one concert run, newest first. -/
def entriesOfConcert (concertId : String) : IO (Array QueueEntry) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let e ← from Store.QueueEntryRow
    guard e.concert_id = some concertId
    select e
    order_by_desc e.created_at
    order_by_desc e.id
  Store.keepConvertible "queue entry" (·.id) QueueEntry.ofRow? rows

/-- Every status the queue has, which is what `countByStatus` counts over. -/
def allStatuses : List QueueStatus :=
  [.pending, .running, .done, .failed, .unfinished, .cancelled]

open Db.Query.DSL in
/-- How many entries are in each status.

    Six counts on one connection, which is what the overview needs and all it needs: it prints
    three of them and a total, and used to read every entry in the queue to get them. Returned as
    a function rather than a record so that a status added later is a case here and nowhere
    else. -/
def countByStatus : IO (QueueStatus → Nat) := do
  let counts ← Store.run do
    let mut acc : Std.HashMap String Nat := {}
    for status in allStatuses do
      let name := Store.enumColumn status
      let n ← HasModel.count <| query% do
        let e ← from Store.QueueEntryRow
        guard e.status = name
        select e
      acc := acc.insert name n.toNat
    pure acc
  return fun status => counts.getD (Store.enumColumn status) 0

open Db.Query.DSL in
/-- One page of the queue, newest first, and how many entries the filter matched.

    `since?` is epoch seconds and keeps the entries created at or after it; `skip` and `take` are
    the window. The total is counted *before* the window, which is the arithmetic the dashboard's
    collection envelope reports and the only thing that makes an offset usable. -/
def entriesPage (since? : Option Int) (skip take : Nat) :
    IO (Array QueueEntry × Nat) := do
  let bound := Store.sinceBound since?
  let matching : QuerySet Store.QueueEntryRow := query% do
    let e ← from Store.QueueEntryRow
    guard e.created_at ≥ bound
    select e
    order_by_desc e.created_at
    order_by_desc e.id
  let (rows, total) ← Store.run do
    let rows ← HasModel.fetch (matching.offset skip |>.limit take)
    let total ← HasModel.count matching
    pure (rows, total)
  return (← Store.keepConvertible "queue entry" (·.id) QueueEntry.ofRow? rows, total.toNat)

/-- Choose the per-repo clone slot a freshly claimed entry should run in.

    `occupied` are the slot indices currently in use for that repository, `perRepoLimit` is
    the configured `--parallel-per-repo`, and `preferred` is the slot recorded on the entry
    this one continues from, if any.

    Returns `(slot, reuseTree)`, where `reuseTree` asks the caller to leave the working tree
    exactly as the previous task left it, or `none` when the entry cannot start right now and
    should stay pending.

    The three cases that matter:
    * A continuation whose predecessor's slot is free goes back to it and keeps the tree.
      `--resume` restores the conversation but not the filesystem, so anywhere else the agent
      wakes up to a tree missing every edit its context refers to.
    * A continuation whose predecessor's slot is busy **waits** rather than taking a fresh one.
      Starting it elsewhere would silently discard the work it was queued to build on; the
      occupying task will finish, so waiting cannot deadlock.
    * A continuation whose recorded slot is beyond the current limit (the daemon was restarted
      with a smaller `--parallel-per-repo`) falls back to a free slot and resets it, since the
      tree it wanted is not reachable any more.

    `preferred` means "the predecessor's slot, *and* its tree is still sitting there". The
    caller is responsible for that second half — see `claimDecision`. Passing a slot whose
    tree has since been reset by an unrelated task would make this function wait for a
    workspace that no longer exists. -/
def chooseSlot (occupied : Array Nat) (perRepoLimit : Nat) (preferred : Option Nat)
    : Option (Nat × Bool) :=
  let firstFree := (List.range perRepoLimit).find? (!occupied.contains ·)
  match preferred with
  | some p =>
    if occupied.contains p then none
    else if p < perRepoLimit then some (p, true)
    else firstFree.map (·, false)
  | none => firstFree.map (·, false)

/-- What the auth resolver decided about an entry.

    `wait` is deliberately not a failure: an entry whose only authentication source is out of
    quota is not broken, it is early. Cancelling it would throw away work that will be perfectly
    runnable once the window resets, so it stays pending and the daemon moves on to something
    else. -/
inductive AuthDecision where
  /-- Claimable, running on this source (`none` on the legacy config with no named sources). -/
  | use (label : Option String)
  /-- Not claimable right now; every candidate source is usage-limited. -/
  | wait (reason : String)
deriving Repr, Inhabited

/-- Everything the daemon knows about its own occupancy when it goes to claim an entry.

    Bundled into a structure so that the decision below is an ordinary function of its inputs
    rather than a closure over the daemon's `IO.Ref`s, which makes the interesting cases —
    exclusive backends, blocked continuations, per-repo limits — reachable from a test. -/
structure ClaimContext where
  /-- Slot-pool key (`QueueEntry.slotKey`) → slot indices currently in use in that pool. -/
  occupiedSlots : Std.HashMap String (Array Nat)
  /-- Number of tasks running across all repositories. -/
  total : Nat
  /-- Set while a backend that needs the daemon to itself is running. -/
  exclusiveActive : Bool
  /-- `--parallel`. -/
  parallelLimit : Nat
  /-- `--parallel-per-repo`. -/
  perRepoLimit : Nat
  /-- Whether a backend tolerates another task running beside it. Passed in rather than
      imported: `TaskRunner` depends on this module, so the dependency cannot run the other
      way, and the queue has no business knowing about agent backends anyway. -/
  parallelSafe : Option String → Bool
  /-- Which authentication source an entry may run on right now.

      Passed in for the same reason as `parallelSafe`: the queue has no business knowing about
      subscriptions, and injecting it keeps the usage-limited cases reachable from a test with
      no config and no network. Defaults to "always claimable on no particular source", which is
      what every caller that predates multiple auth sources wants. -/
  resolveAuth : QueueEntry → IO AuthDecision := fun _ => pure (.use none)
  /-- Whether a slot still holds the tree the last task in it left. True for the backends that run
      in the daemon's own clone slots. False for one that keeps each task's workspace itself
      (`Exec.Backend.persistentWorkspaces`): there a continuation is handed its predecessor's
      workspace by the backend, wherever it runs, so the slot is only a count of how many tasks a
      repository has running and a continuation neither waits for nor prefers one. -/
  slotsHoldTrees : Bool := true

/-- The outcome of a successful claim. -/
structure Claim where
  entry : QueueEntry
  /-- Slot the entry will run in. -/
  slot : Nat
  /-- Authentication source resolved for this entry, stamped onto it before it launches.

      Resolved here rather than when the entry was created because an entry can sit pending for
      hours: the account that was free when a listener queued it may be out of quota by the time
      a worker picks it up. -/
  authSource : Option String := none
  /-- Set when the entry is a continuation that got its predecessor's workspace back, naming
      that predecessor. The slot must then be left exactly as it was. -/
  resumeFrom : Option String := none

/-- Pick the entry the daemon should start next, or `none` if nothing can start right now.

    `pending` is the entries that are waiting — `pendingEntries` for the daemon, a literal array
    for a test; the per-repo limit is applied to them here, since only the context knows it.

    `predecessorOf tid` is the entry whose run became task `tid`, which is what a continuation
    has to find before it can ask for its workspace back. A lookup rather than the whole queue:
    the daemon passes `entryForTask`, one indexed query for the one continuation being
    considered, where reading every entry to answer it was most of what a claim used to cost.

    `slotOccupant fork slot` reports which entry's working tree currently sits in a slot — of
    the pool `fork` names, or of the shared repository-independent pool when it is `none`. It
    is consulted for continuations only, and it is what makes resuming safe: a predecessor's
    slot being *free* does not mean the predecessor's tree is still in it, because slots are
    pooled and an unrelated task may have taken it and reset it in between. Resuming onto that
    tree would silently hand the agent someone else's branch and edits while its restored
    conversation describes work that is gone — so when the occupant does not match, the
    continuation is treated as an ordinary entry that resets whatever slot it lands in. -/
def claimDecision (ctx : ClaimContext) (pending : Array QueueEntry)
    (predecessorOf : String → IO (Option QueueEntry))
    (slotOccupant : Option Repository → Nat → IO (Option String)) : IO (Option Claim) := do
  if ctx.total >= ctx.parallelLimit then return none
  -- Once a task on a backend that needs the daemon to itself is running, nothing else may
  -- start alongside it.
  if ctx.exclusiveActive then return none
  let counts := ctx.occupiedSlots.fold (fun m k slots => m.insert k slots.size)
    ({} : Std.HashMap String Nat)
  let candidates := pendingCandidates pending counts ctx.perRepoLimit
  for e in candidates do
    -- A backend that keeps per-run state at a fixed global path only starts when nothing else
    -- is running. `total == 0` means the daemon is idle.
    if !ctx.parallelSafe e.backend && ctx.total > 0 then continue
    -- The predecessor entry, and the slot it recorded — the workspace this entry was queued
    -- to build on.
    let predecessor ← match e.continuesFrom with
      | none     => pure none
      | some tid => predecessorOf tid
    -- Where slots hold no trees, nothing above made a continuation of a running task wait (its
    -- predecessor's slot being busy was the wait). Its workspace is still in use, and the backend
    -- would refuse it; waiting is the right answer, and the predecessor will finish.
    if !ctx.slotsHoldTrees && predecessor.any (·.status == .running) then continue
    let preferred ← match (if ctx.slotsHoldTrees then predecessor else none).bind
        (fun p => p.slot.map (p.id, ·)) with
      | none => pure none
      | some (predId, predSlot) =>
        -- Confirm the predecessor's tree is still there before asking to wait for its slot.
        if (← slotOccupant (e.repo.map (·.fork)) predSlot) == some predId then pure (some predSlot)
        else pure none
    let occupied := ctx.occupiedSlots.getD e.slotKey #[]
    let some (slot, reuseTree) := chooseSlot occupied ctx.perRepoLimit preferred | continue
    -- Last, because it is the only check that can cost a network round trip: an entry ruled out
    -- by occupancy never reaches the usage monitor.
    let .use authSource ← ctx.resolveAuth e | continue
    return some {
      entry := e
      slot
      resumeFrom := if reuseTree then predecessor.map (·.id) else none
      authSource
    }
  return none

/-- Return true if any entry created by `name` is currently pending or running. -/
def hasActiveEntryForListener (name : String) : IO Bool := do
  return (← activeEntries).any (·.listenerName == some name)

-- PID file management

def writePid (pid : UInt32) : IO Unit := do
  let dir ← queueDir
  IO.FS.createDirAll dir
  IO.FS.writeFile (← pidFile) (toString pid)

def readPid : IO (Option UInt32) := do
  let path ← pidFile
  if !(← path.pathExists) then return none
  let s ← IO.FS.readFile path
  return s.trimAscii.toString.toNat? |>.map (·.toUInt32)

def deletePid : IO Unit :=
  try IO.FS.removeFile (← pidFile) catch _ => pure ()

/-- This process's own PID. -/
def ownPid : IO UInt32 := do
  let stat ← IO.FS.readFile (System.FilePath.mk "/proc/self/stat")
  match stat.splitOn " " with
  | pid :: _ => return (pid.toNat?.getD 0).toUInt32
  | _        => return 0

/-- Process names a live daemon can be running under.

    `orchestrad` is the backend binary that holds the queue; `orchestra` is there because a daemon
    started before the CLI/backend split still runs under the old name, and an upgraded client must
    not conclude that such a daemon is dead and start a second one beside it. -/
private def daemonProcessNames : Array String := #["orchestrad", "orchestra"]

/-- Return true if a daemon process with the stored PID is still alive.

    "The PID is in `/proc`" is not sufficient on its own, for two reasons:

    * **It can be us.** The PID file outlives the process that wrote it, and in a container the
      daemon is PID 1 — so after a restart the next container's PID 1 is the very process running
      this check, which would otherwise conclude a daemon is already running and refuse to start,
      on every restart, forever.
    * **PIDs are recycled.** A daemon that died without cleaning up leaves a PID that some
      unrelated process may later take, wedging `queue start` until the file is deleted by hand.

    An unreadable `comm` (hardened `/proc`, different user) is treated as "running": refusing to
    start is recoverable, whereas two daemons racing on the same queue directory is not. -/
def daemonRunning : IO Bool := do
  match ← readPid with
  | none => return false
  | some pid =>
    if pid == (← ownPid) then return false
    if !(← (System.FilePath.mk s!"/proc/{pid}").pathExists) then return false
    let comm ← try some <$> IO.FS.readFile s!"/proc/{pid}/comm" catch _ => pure none
    match comm with
    | none => return true
    | some c => return daemonProcessNames.contains c.trimAscii.toString

-- Cascade cancellation

/-- Cancel `id` if it is *still* pending, and report whether it was.

    The re-read is what makes cascade cancellation safe under a parallel daemon. The caller
    iterates over a snapshot from `pendingEntries`, and a worker can claim and start any entry in
    that snapshot while the loop is still running; writing the snapshot's version back would
    stamp `cancelled` onto an entry that is at that moment executing. -/
private def cancelIfStillPending (id : String) : IO Bool := do
  let some cur ← loadEntry id | return false
  if cur.status != .pending then return false
  saveEntry { cur with status := .cancelled }
  return true

/-- Cancel all pending entries that have continuesFrom = taskId, then recurse. -/
partial def cancelDependents (taskId : String) : IO Unit := do
  for entry in ← pendingEntries do
    if entry.continuesFrom == some taskId then
      if ← cancelIfStillPending entry.id then
        -- If this entry already ran and has a taskId, cascade further
        if let some tid := entry.taskId then
          cancelDependents tid

/-- Mark the task record behind a dead run `unfinished`, and say whether that changed anything.

    A `TaskRecord` and a `QueueEntry` are two stores describing one run, and only one of them
    ever gets repaired. The record is stamped `running` the moment the run starts and terminal
    only when it lands (`TaskRunner`, step 7), so a worker that dies in between leaves the
    record at `running` for good — which is what the overview reads, while the queue page reads
    the entry beside it and says something else entirely.

    Only a record still sitting at `running` is touched. Anything already terminal is a run that
    landed, and a second writer arriving late must not overwrite the verdict it wrote. -/
def markTaskUnfinished (taskId : String) : IO Bool := do
  let some record ← TaskStore.loadTask taskId | return false
  if record.status == .running then
    TaskStore.saveTask { record with status := .unfinished }
    return true
  else
    return false

/-- Should this entry be reaped: does the queue call it `running` while no worker holds it?

    The daemon's table of in-flight tasks supplies `liveEntryIds` and is the authority on the
    second half; this is the rule that table is read through. Split out as a function of its
    inputs, in the way `claimDecision` is, so that the case which matters most — a live run,
    which must never be reaped — can be pinned by a test without standing a daemon up. -/
def shouldReap (liveEntryIds : Array String) (entry : QueueEntry) : Bool :=
  entry.status == .running && !liveEntryIds.contains entry.id

/-- Where the ids of the entries a startup sweep interrupted are kept until the daemon has decided
    whether to resume them (`resumeInterrupted`). One id per line.

    A file beside the pid file rather than anything in the entry: an entry swept here and an entry
    a usage limit or an exhausted budget left `unfinished` look exactly alike in the database —
    both `unfinished`, both with a session id — and only the first may be resumed, since resuming
    the second would spend the budget it ran out of again. Written before the entries are, so a
    daemon that dies between the sweep and the resume still leaves the next one the list. -/
def interruptedFile : IO System.FilePath :=
  return (← queueDir) / "interrupted"

/-- The entry ids `markStaleRunningAsUnfinished` has recorded and nobody has dealt with yet. -/
def loadInterrupted : IO (Array String) := do
  let f ← interruptedFile
  unless ← f.pathExists do return #[]
  let lines := (← IO.FS.readFile f).splitOn "\n" |>.map (·.trimAscii.toString)
  return (lines.filter (!·.isEmpty)).toArray

/-- Forget the recorded ids, once every one of them has been decided. -/
def clearInterrupted : IO Unit := do
  try IO.FS.removeFile (← interruptedFile) catch _ => pure ()

/-- On daemon startup, mark any entries stuck in 'running' state as unfinished, and answer them
    (as now written). These are left over from a previous daemon that was killed mid-task. Their
    ids are added to `interruptedFile` first, for `resumeInterrupted`: an entry this sweep did not
    touch was not interrupted by a restart and is not a restart's to pick up. -/
def markStaleRunningAsUnfinished : IO (Array QueueEntry) := do
  let stale ← runningEntries
  -- Never at the cost of the sweep or the start: a list that could not be written means these
  -- runs are not resumed automatically, which is what every daemon before this did anyway.
  unless stale.isEmpty do
    try
      IO.FS.createDirAll (← queueDir)
      let h ← IO.FS.Handle.mk (← interruptedFile) .append
      for e in stale do h.putStrLn e.id
      h.flush
    catch e =>
      IO.eprintln s!"Could not record the interrupted entries; they will not be resumed: {e}"
  let mut swept := #[]
  for entry in stale do
    let e := { entry with status := .unfinished }
    saveEntry e
    swept := swept.push e
  return swept

/-! ## Resuming after a restart

A deploy drains the daemon and then kills it, and a task still running at that moment loses its
agent mid-sentence. With a backend that keeps workspaces nothing else is lost: the tree, `$HOME`
and the agent's conversation are on the task's volume, and the session id is on the task record
from the moment the agent announced it. So the new daemon queues the continuation a person would
have queued — same work, same settings, `continuesFrom` the dead run — with a prompt that tells
the agent what happened. -/

/-- The first line of every prompt a restart resume is queued with. It is how a resume is told
    apart from a continuation somebody asked for (`restartResumeRun`), so it is never reworded
    without keeping the old spelling recognised. -/
def restartResumeMarker : String := "[orchestra: resumed after a daemon restart]"

/-- The prompt a restart resume is queued with: the next user message in the resumed
    conversation. It says nothing about the task itself, which the conversation already holds. -/
def restartResumePrompt : String :=
  restartResumeMarker ++ "\n\n" ++
  "The orchestra daemon was restarted while you were working on this task, and your previous \
run was stopped part-way through — possibly in the middle of a command or an edit. This is the \
same conversation, in the same working tree and home directory as before.\n\n\
Before carrying on, check where things actually stand rather than relying on what you last \
remember: look at the working tree (`git status`, `git log`, any half-written files), whether a \
command you started may have been cut off, and whether a pull request or comment you meant to \
create already exists. Then continue where you left off and finish the task."

/-- Whether `prompt` is one a restart resume was queued with. -/
def isRestartResumePrompt (prompt : String) : Bool :=
  prompt.startsWith restartResumeMarker

/-- How many restart resumes in a row a chain may get. A task that is killed by this many
    restarts running is more likely the cause than the victim — one that hangs every drain, or
    whose agent brings the daemon down — and resuming it at every start would keep that going
    forever. Past this it is left `unfinished` for somebody to look at. -/
def maxRestartResumes : Nat := 3

/-- How many restart resumes end a chain whose prompts, newest run first, are `prompts`: the
    unbroken run of them at the head. A continuation somebody queued by hand breaks the run, and
    the count starts again behind it — that person has looked at the task. -/
def restartResumeRun (prompts : List String) : Nat :=
  (prompts.takeWhile isRestartResumePrompt).length

/-- The prompts of the chain ending in task `taskId`, newest first, following `continuesFrom`
    through the task records. Stops after `limit` records (only the head of the chain is ever
    asked about) and at a record that is missing or already seen. -/
def chainPrompts (taskId : String) (limit : Nat := maxRestartResumes + 1) : IO (List String) := do
  let mut out : Array String := #[]
  let mut seen : Array String := #[]
  let mut cur := some taskId
  for _ in [0:limit] do
    let some tid := cur | break
    if seen.contains tid then break
    seen := seen.push tid
    let some r ← TaskStore.loadTask tid | break
    out := out.push r.prompt
    cur := r.continuesFrom
  return out.toList

/-- The session a continuation of task `taskId` resumes: that run's own, or — when it is a restart
    resume that was killed before its agent named a conversation — the one it was itself resuming,
    and so on back along the unbroken run of restart resumes. Such a run added nothing to the
    conversation, so the one before it is still the conversation to pick up; without this, a resume
    killed early would end the chain silently. A run somebody queued by hand that has no session is
    a fresh start and gets none: nothing of it can be resumed. Bounded like `chainPrompts`.

    Answers the task the session belongs to as well as the session: when that is not `taskId`
    itself, the dead resume between may never have reached its workspace either, and the
    workspace is then found under the owner's label (`SessionSpec.workspaceFallback`). -/
def sessionOwner (taskId : String) (limit : Nat := maxRestartResumes + 1)
    : IO (Option (String × String)) := do
  let mut cur := some taskId
  for _ in [0:limit] do
    let some tid := cur | return none
    let some r ← TaskStore.loadTask tid | return none
    if let some sid := r.sessionId then return some (tid, sid)
    unless isRestartResumePrompt r.prompt do return none
    cur := r.continuesFrom
  return none

/-- The session alone (`sessionOwner`). -/
def sessionFor (taskId : String) (limit : Nat := maxRestartResumes + 1) : IO (Option String) :=
  return (← sessionOwner taskId limit).map (·.2)

/-- Whether some entry already carries the work of task `taskId` on: one that continues it and is
    waiting, running or has landed. A cancelled or failed continuation does not count — it carried
    nothing on, and the task is still the one to pick up. Read by the restart resume, which must
    not queue a second agent onto a conversation a series follow-up or a hand-queued continuation
    already took, and by `orchestra queue retry` (`retryCandidates`). -/
def hasLiveContinuation (all : Array QueueEntry) (taskId : String) : Bool :=
  all.any fun e =>
    e.continuesFrom == some taskId
      && (e.status == .pending || e.status == .running || e.status == .done)

/-- What `orchestra queue retry` re-queues, oldest first: every `unfinished` or `cancelled` entry
    (in `series`, if given), except an `unfinished` one whose run something already carries on —
    the daemon's restart resume, typically, which leaves the interrupted entry `unfinished` beside
    it. Retrying that as well would put two agents on one conversation.

    Wider than `hasLiveContinuation`: an `unfinished` continuation hides its predecessor too, so
    only the newest link of a chain is offered — in E → R (unfinished) → R2 (pending) neither E nor
    R is, and in E → R (unfinished) only R is. Offering E there would continue a conversation R
    already moved on, in a workspace R took. Only a cancelled or failed continuation leaves its
    predecessor to be picked up: it carried nothing on.

    A retry of a restart resume copies its marker prompt, and so is itself recognised as one: it
    counts toward `maxRestartResumes`. That is harmless. -/
def retryCandidates (all : Array QueueEntry) (series : Option String := none) : List QueueEntry :=
  let continued : Std.HashSet String := all.foldl (init := {}) fun s e =>
    match e.continuesFrom with
    | some t =>
      if e.status == .cancelled || e.status == .failed then s else s.insert t
    | none => s
  (all.filter fun e =>
    (e.status == .unfinished || e.status == .cancelled)
      && !(e.status == .unfinished && e.taskId.any continued.contains)
      && (series.isNone || e.series == series)).toList.reverse

/-- How far back a restart resume reaches: a run started longer ago than this is left
    `unfinished`. A daemon that was down for days comes back to a world that has moved on — the
    pull request reviewed, the issue closed — and a person is better placed than a prompt to say
    whether the work is still wanted. -/
def resumeHorizonSeconds : Int := 24 * 3600

/-- Everything about an interrupted entry that decides whether it is resumed, other than the entry
    and its run: gathered by `resumeInterrupted`, decided over by `resumeEntryFor`. -/
structure ResumeFacts where
  /-- A conversation to resume (`sessionFor`). -/
  hasSession : Bool
  /-- Some entry already carries the work on (`hasLiveContinuation`). -/
  continued : Bool := false
  /-- `restartResumeRun` of the chain ending in the run, counting the run itself. -/
  run : Nat := 0
  /-- The run started within `resumeHorizonSeconds`. -/
  recent : Bool := true
  /-- The entry runs under the daemon's own execution configuration. An entry with a
      `configPath` of its own may name another backend, one whose workspaces were neither kept
      nor reclaimed here. -/
  sameExecution : Bool := true

/-- The continuation to queue for `entry`, interrupted by a restart, whose run is `record` — or why
    there is none.

    Resumed only when everything a resume needs is there: the entry is `unfinished` and its run is
    `record` and was left `unfinished` too; there is a conversation to resume; the entry is not a
    concert step, whose fiber died with the daemon and which the startup sweep has already
    cancelled; nothing else carries the task on already; the run is recent and on the daemon's own
    execution backend; and the chain has not been resumed `maxRestartResumes` times in a row.

    The new entry is the same work by the same somebody, as `orchestra queue retry` builds it, and
    a little more of it — the issue, project, role and tool settings too, since a resume is not a
    new decision about any of them. `authSource` is carried, because it is what whoever queued the
    work asked for; `resolvedAuthSource`, the account the daemon picked for the dead run, is not,
    so a resume is not pinned to an account that may be at its limit now. `spawnPolicy` is carried
    too: an orchestrating agent resumed without `queue_task` could not finish what it was doing.
    Its `max_tasks` is counted per spawning task id, so the resume — a new task id — starts a fresh
    allowance; at most `maxRestartResumes` of them a chain, which is the bound on that.
    Deliberately not carried:

    * `prependPrompt`. It was prepended to the first message of this conversation already;
      sending it again in the middle would read as a new instruction.
    * `spawnedBy`: the resume was queued by the daemon, not by the spawning task, and carrying it
      would spend that task's allowance a second time for the same piece of work.
    * The concert fields, the slot, the task id and the output, which belong to the dead run. -/
def resumeEntryFor (entry : QueueEntry) (record : TaskStore.TaskRecord) (facts : ResumeFacts)
    (id createdAt : String) : Except String QueueEntry := do
  unless entry.status == .unfinished do throw "the entry is no longer unfinished"
  unless entry.taskId == some record.id do throw "the entry points at another run"
  unless record.status == .unfinished do throw "its run is not unfinished"
  unless entry.concertStepKey.isNone do throw "it is a concert step"
  unless facts.hasSession do throw "its agent never started a conversation to resume"
  if facts.continued then throw "another entry already continues it"
  unless facts.recent do
    throw s!"it started more than {resumeHorizonSeconds / 3600} hours ago"
  unless facts.sameExecution do
    throw "its config_path names a different execution backend than the daemon's"
  if facts.run ≥ maxRestartResumes then
    throw s!"it was already resumed after {facts.run} restarts in a row"
  return {
    id, createdAt
    repo          := entry.repo
    mode          := entry.mode
    prompt        := restartResumePrompt
    goal          := entry.goal
    agent         := entry.agent
    systemPrompt  := entry.systemPrompt
    backend       := entry.backend
    model         := entry.model
    continuesFrom := some record.id
    series        := entry.series
    configPath    := entry.configPath
    budget        := entry.budget
    memory        := entry.memory
    identity      := entry.identity
    authSource    := entry.authSource
    authSources   := entry.authSources
    authMode      := entry.authMode
    tools         := entry.tools
    readOnly      := entry.readOnly
    priority      := entry.priority
    inputType     := entry.inputType
    outputType    := entry.outputType
    inputJson     := entry.inputJson
    issueNumber   := entry.issueNumber
    projectId     := entry.projectId
    issueId       := entry.issueId
    role          := entry.role
    prLabels      := entry.prLabels
    triageAddLabels    := entry.triageAddLabels
    triageRemoveLabels := entry.triageRemoveLabels
    listenerName  := entry.listenerName
    spawnPolicy   := entry.spawnPolicy
    scopeRoot     := entry.scopeRoot }

/-- Queue a restart resume (`resumeEntryFor`) for each entry recorded in `interruptedFile`, and
    answer how many were queued. The caller decides whether to call this at all: only with
    `queue.resume_after_restart` on and a backend that keeps workspaces, since anywhere else the
    tree the conversation is about is not guaranteed to be there.

    Decided from the database, not from what the sweep handed back, so that a daemon that died
    after its sweep and before this leaves nothing behind: the next start reads the same list and
    the same records. Running it twice is harmless — a resume already queued is a pending
    continuation, which `hasLiveContinuation` refuses a second of. The list is cleared once every
    id on it has been decided; one whose entry cannot be read is decided by being skipped.

    `execution` is the daemon's own (`ResumeFacts.sameExecution`), `now` the time in epoch
    seconds. Every skip is logged with its reason. -/
def resumeInterrupted (execution : ExecutionConfig) (now : Int) : IO Nat := do
  let ids ← loadInterrupted
  if ids.isEmpty then return 0
  let all ← loadAllEntries
  let mut queued := 0
  -- Ids whose decision threw — a database that was briefly locked, a record that could not be
  -- read — are kept for the next start rather than dropped with the rest: a skip is a decision,
  -- an error is not one.
  let mut undecided : Array String := #[]
  for entryId in ids.toList.eraseDups do
    try
      let some entry := all.find? (·.id == entryId) | continue
      let some tid := entry.taskId
        | IO.println s!"  Not resuming entry {entryId}: it never started a run"
      let some record ← TaskStore.loadTask tid
        | IO.println s!"  Not resuming entry {entryId}: its run {tid} has no record"
      let sameExecution ← match entry.configPath with
        | none    => pure true
        | some cp =>
          try
            let c ← loadAppConfig (some (System.FilePath.mk cp))
            pure (c.execution.backend == execution.backend && c.execution.options == execution.options)
          catch _ => pure false
      let facts : ResumeFacts := {
        hasSession := (← sessionFor tid).isSome
        continued  := hasLiveContinuation all tid
        run        := restartResumeRun (← chainPrompts tid)
        recent     := match Time.parseIso8601 record.createdAt with
          | some t => now - t ≤ resumeHorizonSeconds
          | none   => false
        sameExecution }
      match resumeEntryFor entry record facts (← TaskStore.generateId) (← TaskStore.currentIso8601) with
      | .ok e =>
        saveEntry e
        queued := queued + 1
        IO.println s!"  Resuming interrupted entry {entryId} (task {tid}) as {e.id}"
      | .error why =>
        IO.println s!"  Not resuming entry {entryId} (task {tid}): {why}"
    catch e =>
      IO.eprintln s!"  Could not resume interrupted entry {entryId}, keeping it for the next start: {e}"
      undecided := undecided.push entryId
  if undecided.isEmpty then clearInterrupted
  else
    try IO.FS.writeFile (← interruptedFile) (String.intercalate "\n" undecided.toList ++ "\n")
    catch e => IO.eprintln s!"  Could not rewrite the list of interrupted entries: {e}"
  return queued

/-- Bring task records back in line with the entries that own them, and answer how many needed it.

    An entry that is not `running` is proof that no worker is on that task, which makes a
    `running` record beside it stale. One pass over that rule repairs both the entries this
    startup just swept and every record left behind by a daemon that died before any of this
    existed, which is why there is no separate backfill.

    Records no entry points at are deliberately left alone. Tasks also run outside the daemon —
    `orchestra run` calls `TaskRunner.runTask` directly and never touches the queue — and from
    here a live foreground run is indistinguishable from an abandoned one. Stamping those would
    be this repair inventing exactly the disagreement it exists to remove. -/
def reconcileStaleTaskRecords : IO Nat := do
  let entries ← loadAllEntries
  let mut repaired := 0
  for entry in entries do
    if entry.status != .running then
      if let some taskId := entry.taskId then
        if ← markTaskUnfinished taskId then
          repaired := repaired + 1
  return repaired

/-- On daemon startup, cancel any unfinished concert-linked entries.
    Their concert fibers died with the previous daemon and can never be resumed. -/
def cancelStaleConcertEntries : IO Unit := do
  let all ← loadAllEntries
  for entry in all do
    if entry.status == .unfinished && entry.concertStepKey.isSome then
      saveEntry { entry with status := .cancelled }

-- Concert run persistence

open Db.Query.DSL in
def saveConcertRun (run : ConcertRun) : IO Unit :=
  Store.run <| HasModel.save run.toRow

open Db.Query.DSL in
def loadConcertRun (id : String) : IO (Option ConcertRun) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let c ← from Store.ConcertRunRow
    guard c.id = id
    select c
  return (← Store.keepConvertible "concert run" (·.id) ConcertRun.ofRow? rows)[0]?

open Db.Query.DSL in
/-- Load all concert runs, newest first.

    By `started_at` rather than id, for the reason `TaskStore.loadAllTasks` gives. -/
def loadAllConcertRuns : IO (Array ConcertRun) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let c ← from Store.ConcertRunRow
    select c
    order_by_desc c.started_at
    order_by_desc c.id
  Store.keepConvertible "concert run" (·.id) ConcertRun.ofRow? rows

open Db.Query.DSL in
/-- One page of the concert history, newest first, and how many runs the filter matched.

    As `entriesPage`, over `started_at` — which is the column a concert run is ordered by and the
    one the dashboard's `since` is meant to compare against. -/
def concertRunsPage (since? : Option Int) (skip take : Nat) :
    IO (Array ConcertRun × Nat) := do
  let bound := Store.sinceBound since?
  let matching : QuerySet Store.ConcertRunRow := query% do
    let c ← from Store.ConcertRunRow
    guard c.started_at ≥ bound
    select c
    order_by_desc c.started_at
    order_by_desc c.id
  let (rows, total) ← Store.run do
    let rows ← HasModel.fetch (matching.offset skip |>.limit take)
    let total ← HasModel.count matching
    pure (rows, total)
  return (← Store.keepConvertible "concert run" (·.id) ConcertRun.ofRow? rows, total.toNat)

/-- On daemon startup, mark any running concert runs as cancelled (the fibers died). -/
def cancelStaleRunningConcerts : IO Unit := do
  let all ← loadAllConcertRuns
  for run in all do
    if run.status == .running then
      saveConcertRun { run with status := .cancelled }

end Orchestra.Queue
