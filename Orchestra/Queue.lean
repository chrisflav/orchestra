import Lean.Data.Json
import Orchestra.Config
import Orchestra.TaskStore

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
  upstream      : Repository
  fork          : Repository
  mode          : TaskMode
  prompt        : String
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
  /-- Label of the authentication source to use. Must match a label in the backend's `auth_sources`. -/
  authSource    : Option String := none
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

instance : ToJson QueueEntry where
  toJson e :=
    let fields : List (String × Json) := [
      ("id",         e.id),
      ("created_at", e.createdAt),
      ("status",     ToJson.toJson e.status),
      ("upstream",   ToJson.toJson e.upstream),
      ("fork",       ToJson.toJson e.fork),
      ("mode",       ToJson.toJson e.mode),
      ("prompt",     e.prompt)
    ]
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
    let fields := if let some s := e.authSource    then fields ++ [("auth_source",     Json.str s)]      else fields
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
    Json.mkObj fields

instance : FromJson QueueEntry where
  fromJson? j := do
    let id           ← j.getObjValAs? String "id"
    let createdAt    ← j.getObjValAs? String "created_at"
    let status       ← j.getObjValAs? QueueStatus "status"
    let upstream     ← j.getObjValAs? Repository "upstream"
    let fork         ← j.getObjValAs? Repository "fork"
    let mode         ← j.getObjValAs? TaskMode "mode"
    let prompt       ← j.getObjValAs? String "prompt"
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
    let authSource    := j.getObjValAs? String "auth_source" |>.toOption
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
    return { id, createdAt, status, upstream, fork, mode, prompt,
             agent, systemPrompt, prependPrompt, backend, model, continuesFrom, series, taskId, configPath,
             budget, memory, authSource, tools, readOnly, priority,
             concertStepKey, concertId, inputType, outputType, inputJson, outputJson,
             issueNumber, projectId, issueId, role, prLabels, triageAddLabels, triageRemoveLabels,
             listenerName }

-- Directories and paths

def queueDir : IO System.FilePath :=
  return (← Dirs.dataBase) / "queue"

def pidFile : IO System.FilePath :=
  return (← queueDir) / "daemon.pid"

def socketFile : IO System.FilePath :=
  return (← queueDir) / "daemon.sock"

def daemonLogFile : IO System.FilePath :=
  return (← queueDir) / "daemon.log"

-- Storage

def saveEntry (entry : QueueEntry) : IO Unit := do
  let dir ← queueDir
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / s!"{entry.id}.json") (Lean.Json.compress (ToJson.toJson entry))

def loadEntry (id : String) : IO (Option QueueEntry) := do
  let path := (← queueDir) / s!"{id}.json"
  if !(← path.pathExists) then return none
  let contents ← IO.FS.readFile path
  match Json.parse contents with
  | .error _ => return none
  | .ok j    =>
    match FromJson.fromJson? j with
    | .error _ => return none
    | .ok e    => return some e

private def stripJsonExt (name : String) : Option String :=
  if name.endsWith ".json" then
    some (name.dropEnd ".json".length).toString
  else
    none

/-- Load all queue entries, sorted by ID descending (newest first). -/
def loadAllEntries : IO (Array QueueEntry) := do
  let dir ← queueDir
  if !(← dir.pathExists) then return #[]
  let entries ← System.FilePath.readDir dir
  let mut result : Array QueueEntry := #[]
  for entry in entries do
    if let some id := stripJsonExt entry.fileName then
      if let some e ← loadEntry id then
        result := result.push e
  return result.qsort (fun a b => a.id > b.id)

/-- Every pending entry that may start now, in the order the daemon should try them.

    `activePerRepo` maps a fork key (owner/name) to the number of tasks currently running on
    that repository; entries for a repository already at `perRepoLimit` are excluded.
    Ordering is by priority (higher first), then oldest first — ids are monotonic, so the
    smaller id is the older entry.

    A list rather than a single entry, because an entry can turn out to be unclaimable for a
    reason only the caller knows — a continuation whose predecessor's slot is still busy, or a
    backend that needs the daemon to itself. Returning just the best candidate would let one
    such entry stall every other repository behind it for as long as its blocker runs. -/
def pendingCandidates (all : Array QueueEntry) (activePerRepo : Std.HashMap String Nat)
    (perRepoLimit : Nat) : Array QueueEntry :=
  let pending := all.filter (fun e =>
    e.status == .pending &&
    activePerRepo.getD e.fork.toString 0 < perRepoLimit)
  pending.qsort (fun a b =>
    if a.priority != b.priority then a.priority > b.priority else a.id < b.id)

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

/-- Everything the daemon knows about its own occupancy when it goes to claim an entry.

    Bundled into a structure so that the decision below is an ordinary function of its inputs
    rather than a closure over the daemon's `IO.Ref`s, which makes the interesting cases —
    exclusive backends, blocked continuations, per-repo limits — reachable from a test. -/
structure ClaimContext where
  /-- Fork key (owner/name) → slot indices currently in use for that repository. -/
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

/-- The outcome of a successful claim. -/
structure Claim where
  entry : QueueEntry
  /-- Slot the entry will run in. -/
  slot : Nat
  /-- Set when the entry is a continuation that got its predecessor's workspace back, naming
      that predecessor. The slot must then be left exactly as it was. -/
  resumeFrom : Option String := none

/-- Pick the entry the daemon should start next, or `none` if nothing can start right now.

    `slotOccupant fork slot` reports which entry's working tree currently sits in a slot. It
    is consulted for continuations only, and it is what makes resuming safe: a predecessor's
    slot being *free* does not mean the predecessor's tree is still in it, because slots are
    pooled and an unrelated task may have taken it and reset it in between. Resuming onto that
    tree would silently hand the agent someone else's branch and edits while its restored
    conversation describes work that is gone — so when the occupant does not match, the
    continuation is treated as an ordinary entry that resets whatever slot it lands in. -/
def claimDecision (ctx : ClaimContext) (all : Array QueueEntry)
    (slotOccupant : Repository → Nat → IO (Option String)) : IO (Option Claim) := do
  if ctx.total >= ctx.parallelLimit then return none
  -- Once a task on a backend that needs the daemon to itself is running, nothing else may
  -- start alongside it.
  if ctx.exclusiveActive then return none
  let counts := ctx.occupiedSlots.fold (fun m k v => m.insert k v.size)
    ({} : Std.HashMap String Nat)
  let candidates := pendingCandidates all counts ctx.perRepoLimit
  for e in candidates do
    -- A backend that keeps per-run state at a fixed global path only starts when nothing else
    -- is running. `total == 0` means the daemon is idle.
    if !ctx.parallelSafe e.backend && ctx.total > 0 then continue
    -- The predecessor entry, and the slot it recorded — the workspace this entry was queued
    -- to build on.
    let predecessor := e.continuesFrom.bind fun tid => all.find? (·.taskId == some tid)
    let preferred ← match predecessor.bind (fun p => p.slot.map (p.id, ·)) with
      | none => pure none
      | some (predId, predSlot) =>
        -- Confirm the predecessor's tree is still there before asking to wait for its slot.
        if (← slotOccupant e.fork predSlot) == some predId then pure (some predSlot)
        else pure none
    let occupied := ctx.occupiedSlots.getD e.fork.toString #[]
    let some (slot, reuseTree) := chooseSlot occupied ctx.perRepoLimit preferred | continue
    return some {
      entry := e
      slot
      resumeFrom := if reuseTree then predecessor.map (·.id) else none
    }
  return none

/-- Return true if any entry created by `name` is currently pending or running. -/
def hasActiveEntryForListener (name : String) : IO Bool := do
  let entries ← loadAllEntries
  return entries.any fun e =>
    (e.status == .pending || e.status == .running) && e.listenerName == some name

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
    | some c => return c.trimAscii.toString == "orchestra"

-- Cascade cancellation

/-- Normalize backend: treat `none` and `some "claude"` as the same backend. -/
private def effectiveBackend (backend : Option String) : String :=
  backend.getD "claude"

/-- Cancel `id` if it is *still* pending, and report whether it was.

    The re-read is what makes cascade cancellation safe under a parallel daemon. Both callers
    below iterate over a snapshot from `loadAllEntries`, and a worker can claim and start any
    entry in that snapshot while the loop is still running. Writing the snapshot's version back
    would stamp `cancelled` onto an entry that is at that moment executing — and since
    `cancelPendingByBackend` matches on the *backend*, with `claude` as the default, that is
    almost every pending entry rather than a narrow window. -/
private def cancelIfStillPending (id : String) : IO Bool := do
  let some cur ← loadEntry id | return false
  if cur.status != .pending then return false
  saveEntry { cur with status := .cancelled }
  return true

/-- Cancel all pending entries that have continuesFrom = taskId, then recurse. -/
partial def cancelDependents (taskId : String) : IO Unit := do
  let all ← loadAllEntries
  for entry in all do
    if entry.status == .pending && entry.continuesFrom == some taskId then
      if ← cancelIfStillPending entry.id then
        -- If this entry already ran and has a taskId, cascade further
        if let some tid := entry.taskId then
          cancelDependents tid

/-- Cancel all pending entries with the same backend as the given one, except the
    entry identified by exceptId (the one that just became unfinished). -/
def cancelPendingByBackend (backend : Option String) (exceptId : String) : IO Unit := do
  let target := effectiveBackend backend
  let all ← loadAllEntries
  for entry in all do
    if entry.status == .pending && entry.id != exceptId &&
       effectiveBackend entry.backend == target then
      if ← cancelIfStillPending entry.id then
        -- Cascade from any taskId this cancelled entry had (rare for pending, but safe)
        if let some tid := entry.taskId then
          cancelDependents tid

/-- On daemon startup, mark any entries stuck in 'running' state as unfinished.
    These are left over from a previous daemon that was killed mid-task. -/
def markStaleRunningAsUnfinished : IO Unit := do
  let all ← loadAllEntries
  for entry in all do
    if entry.status == .running then
      saveEntry { entry with status := .unfinished }

/-- On daemon startup, cancel any unfinished concert-linked entries.
    Their concert fibers died with the previous daemon and can never be resumed. -/
def cancelStaleConcertEntries : IO Unit := do
  let all ← loadAllEntries
  for entry in all do
    if entry.status == .unfinished && entry.concertStepKey.isSome then
      saveEntry { entry with status := .cancelled }

-- Concert run persistence

def concertsDir : IO System.FilePath :=
  return (← Dirs.dataBase) / "concerts"

def saveConcertRun (run : ConcertRun) : IO Unit := do
  let dir ← concertsDir
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / s!"{run.id}.json") (Lean.Json.compress (ToJson.toJson run))

def loadConcertRun (id : String) : IO (Option ConcertRun) := do
  let path := (← concertsDir) / s!"{id}.json"
  if !(← path.pathExists) then return none
  let contents ← IO.FS.readFile path
  match Json.parse contents with
  | .error _ => return none
  | .ok j    =>
    match FromJson.fromJson? j with
    | .error _ => return none
    | .ok r    => return some r

/-- Load all concert runs, sorted by ID descending (newest first). -/
def loadAllConcertRuns : IO (Array ConcertRun) := do
  let dir ← concertsDir
  if !(← dir.pathExists) then return #[]
  let entries ← System.FilePath.readDir dir
  let mut result : Array ConcertRun := #[]
  for entry in entries do
    if let some id := stripJsonExt entry.fileName then
      if let some r ← loadConcertRun id then
        result := result.push r
  return result.qsort (fun a b => a.id > b.id)

/-- On daemon startup, mark any running concert runs as cancelled (the fibers died). -/
def cancelStaleRunningConcerts : IO Unit := do
  let all ← loadAllConcertRuns
  for run in all do
    if run.status == .running then
      saveConcertRun { run with status := .cancelled }

end Orchestra.Queue
