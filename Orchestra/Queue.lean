import Lean.Data.Json
import Orchestra.Config

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

structure QueueEntry where
  id            : String
  createdAt     : String
  status        : QueueStatus   := .pending
  upstream      : String
  fork          : String
  mode          : TaskMode
  prompt        : String
  agent         : Option String := none
  systemPrompt  : Option String := none
  backend       : Option String := none
  model         : Option String := none
  continuesFrom : Option String := none
  series        : Option String := none
  taskId        : Option String := none
  configPath    : Option String := none
deriving Repr

instance : ToJson QueueEntry where
  toJson e :=
    let base : List (String × Json) := [
      ("id",         e.id),
      ("created_at", e.createdAt),
      ("status",     ToJson.toJson e.status),
      ("upstream",   e.upstream),
      ("fork",       e.fork),
      ("mode",       ToJson.toJson e.mode),
      ("prompt",     e.prompt)
    ]
    let fields := base
      |> fun acc => match e.agent with
          | none => acc | some s => acc ++ [("agent", Json.str s)]
      |> fun acc => match e.systemPrompt with
          | none => acc | some s => acc ++ [("system_prompt", Json.str s)]
      |> fun acc => match e.backend with
          | none => acc | some s => acc ++ [("backend", Json.str s)]
      |> fun acc => match e.model with
          | none => acc | some s => acc ++ [("model", Json.str s)]
      |> fun acc => match e.continuesFrom with
          | none => acc | some s => acc ++ [("continues_from", Json.str s)]
      |> fun acc => match e.series with
          | none => acc | some s => acc ++ [("series", Json.str s)]
      |> fun acc => match e.taskId with
          | none => acc | some s => acc ++ [("task_id", Json.str s)]
      |> fun acc => match e.configPath with
          | none => acc | some s => acc ++ [("config_path", Json.str s)]
    Json.mkObj fields

instance : FromJson QueueEntry where
  fromJson? j := do
    let id           ← j.getObjValAs? String "id"
    let createdAt    ← j.getObjValAs? String "created_at"
    let status       ← j.getObjValAs? QueueStatus "status"
    let upstream     ← j.getObjValAs? String "upstream"
    let fork         ← j.getObjValAs? String "fork"
    let mode         ← j.getObjValAs? TaskMode "mode"
    let prompt       ← j.getObjValAs? String "prompt"
    let agent         := j.getObjValAs? String "agent"          |>.toOption
    let systemPrompt  := j.getObjValAs? String "system_prompt"  |>.toOption
    let backend       := j.getObjValAs? String "backend"        |>.toOption
    let model         := j.getObjValAs? String "model"          |>.toOption
    let continuesFrom := j.getObjValAs? String "continues_from" |>.toOption
    let series        := j.getObjValAs? String "series"         |>.toOption
    let taskId        := j.getObjValAs? String "task_id"        |>.toOption
    let configPath    := j.getObjValAs? String "config_path"    |>.toOption
    return { id, createdAt, status, upstream, fork, mode, prompt,
             agent, systemPrompt, backend, model, continuesFrom, series, taskId, configPath }

-- Directories and paths

def queueDir : IO System.FilePath := do
  match ← IO.getEnv "HOME" with
  | some h => return System.FilePath.mk h / ".agent" / "queue"
  | none   => throw (.userError "HOME not set")

def pidFile : IO System.FilePath :=
  return (← queueDir) / "daemon.pid"

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

/-- Return the oldest pending entry, or none.
    loadAllEntries returns newest-first, so back? gives the oldest. -/
def nextPending : IO (Option QueueEntry) := do
  let all ← loadAllEntries
  return (all.filter (fun e => e.status == .pending)).back?

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

/-- Return true if a daemon process with the stored PID is still alive. -/
def daemonRunning : IO Bool := do
  match ← readPid with
  | none => return false
  | some pid =>
    return ← (System.FilePath.mk s!"/proc/{pid}").pathExists

-- Cascade cancellation

/-- Normalize backend: treat `none` and `some "claude"` as the same backend. -/
private def effectiveBackend (backend : Option String) : String :=
  backend.getD "claude"

/-- Cancel all pending entries that have continuesFrom = taskId, then recurse. -/
partial def cancelDependents (taskId : String) : IO Unit := do
  let all ← loadAllEntries
  for entry in all do
    if entry.status == .pending && entry.continuesFrom == some taskId then
      saveEntry { entry with status := .cancelled }
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
      saveEntry { entry with status := .cancelled }
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

end Orchestra.Queue
