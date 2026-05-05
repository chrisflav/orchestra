import Lean.Data.Json
import Orchestra.TaskStore

open Lean (Json FromJson ToJson)

namespace Orchestra.UsageLimits

/-- Records that a (backend, authSource) pair has hit its usage limit. -/
structure BlockedSource where
  /-- The agent backend (e.g., "claude", "vibe"). -/
  backend    : String
  /-- The auth source label. `none` means the default/unspecified source. -/
  authSource : Option String
  /-- When the limit was hit (ISO 8601 UTC). -/
  blockedAt  : String
  /-- When the limit is expected to reset (ISO 8601 UTC).
      If `none`, the source stays blocked until cleared manually or daemon restart. -/
  resetAt    : Option String
deriving Repr

instance : ToJson BlockedSource where
  toJson s :=
    let fields : List (String × Json) := [
      ("backend",    s.backend),
      ("blocked_at", s.blockedAt)
    ]
    let fields := if let some a := s.authSource then fields ++ [("auth_source", Json.str a)] else fields
    let fields := if let some r := s.resetAt    then fields ++ [("reset_at",    Json.str r)] else fields
    Json.mkObj fields

instance : FromJson BlockedSource where
  fromJson? j := do
    let backend   ← j.getObjValAs? String "backend"
    let blockedAt ← j.getObjValAs? String "blocked_at"
    let authSource := j.getObjValAs? String "auth_source" |>.toOption
    let resetAt    := j.getObjValAs? String "reset_at"    |>.toOption
    return { backend, authSource, blockedAt, resetAt }

private def storePath : IO System.FilePath := do
  match ← IO.getEnv "HOME" with
  | some h => return System.FilePath.mk h / ".agent" / "queue" / "usage_limits.json"
  | none   => throw (.userError "HOME not set")

def loadBlocked : IO (Array BlockedSource) := do
  let path ← storePath
  if !(← path.pathExists) then return #[]
  let contents ← IO.FS.readFile path
  match Json.parse contents with
  | .error _ => return #[]
  | .ok j =>
    match FromJson.fromJson? j with
    | .error _ => return #[]
    | .ok entries => return entries

private def saveBlocked (entries : Array BlockedSource) : IO Unit := do
  let path ← storePath
  let dir := path.parent.getD (System.FilePath.mk ".")
  IO.FS.createDirAll dir
  IO.FS.writeFile path (Lean.Json.compress (ToJson.toJson entries))

/-- Estimate a reset time 24 hours from now. -/
def estimateResetTime : IO String := do
  let child ← IO.Process.spawn {
    cmd    := "sh"
    args   := #["-c", "date -u -d @$(( $(date +%s) + 86400 )) '+%Y-%m-%dT%H:%M:%SZ'"]
    stdout := .piped
    stderr := .null
    stdin  := .null
  }
  let out ← child.stdout.readToEnd
  let _   ← child.wait
  let result := out.trimAscii.toString
  if result.isEmpty then TaskStore.currentIso8601
  else return result

/-- Block the given (backend, authSource) pair until resetAt.
    If resetAt is `none`, uses an estimated 24-hour window. -/
def blockSource (backend : String) (authSource : Option String)
    (resetAt : Option String) : IO Unit := do
  let now ← TaskStore.currentIso8601
  let resetTime ← match resetAt with
    | some t => pure t
    | none   => estimateResetTime
  let entry : BlockedSource := {
    backend, authSource
    blockedAt := now
    resetAt   := some resetTime
  }
  let existing ← loadBlocked
  let updated :=
    if existing.any (fun e => e.backend == backend && e.authSource == authSource) then
      existing.map fun e =>
        if e.backend == backend && e.authSource == authSource then entry else e
    else
      existing.push entry
  saveBlocked updated

/-- Remove all blocked sources whose reset time has passed. -/
def clearExpiredBlocks : IO Unit := do
  let now ← TaskStore.currentIso8601
  let existing ← loadBlocked
  let active := existing.filter fun e =>
    match e.resetAt with
    | none   => true
    | some t => t > now
  if active.size != existing.size then
    saveBlocked active

/-- Return all currently active (not yet expired) blocked (backend, authSource) pairs. -/
def blockedSources : IO (Array (String × Option String)) := do
  let now ← TaskStore.currentIso8601
  let entries ← loadBlocked
  return entries.filterMap fun e =>
    match e.resetAt with
    | none   => some (e.backend, e.authSource)
    | some t => if t > now then some (e.backend, e.authSource) else none

/-- Return the reset time for a currently blocked (backend, authSource) pair,
    or `none` if not blocked. -/
def resetTimeFor (backend : String) (authSource : Option String) : IO (Option String) := do
  let now ← TaskStore.currentIso8601
  let entries ← loadBlocked
  return entries.findSome? fun e =>
    if e.backend == backend && e.authSource == authSource then
      match e.resetAt with
      | none   => some "unknown"
      | some t => if t > now then some t else none
    else none

/-- Clear the blocked state on daemon startup so stale blocks don't prevent work.
    Expired blocks are removed; unexpired ones are kept so tasks are still delayed. -/
def clearExpiredBlocksOnStartup : IO Unit :=
  clearExpiredBlocks

end Orchestra.UsageLimits
