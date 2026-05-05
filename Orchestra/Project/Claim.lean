import Lean.Data.Json
import Std.Sync
import Orchestra.Project.Basic

open Lean (Json FromJson ToJson)

namespace Orchestra.Project

/-! # Issue claim locks

An open issue may only be worked on by one task at a time. The claim is
recorded as a separate file `~/.agent/projects/<pid>/claims/<iid>.json` so
that the issue record itself stays read-mostly. Decision **C1** of the plan:
file-create-as-lock guarded by a daemon-wide mutex.

The mutex protects against intra-process races (two MCP requests handled by
the daemon at once). The on-disk file is what survives daemon restarts and
what other processes (CLI, future external tools) read to learn that an
issue is taken. -/

structure Claim where
  taskId    : String
  series    : Option String := none
  /-- Backend identifier of the claiming agent (e.g. `"claude"`), recorded
      so cascade-cancel can target the right work later. -/
  agent     : String
  claimedAt : String
deriving Repr, Inhabited

instance : ToJson Claim where
  toJson c :=
    let base : List (String × Json) :=
      [ ("task_id",    Json.str c.taskId)
      , ("agent",      Json.str c.agent)
      , ("claimed_at", Json.str c.claimedAt) ]
    let fields := if let some s := c.series then base ++ [("series", Json.str s)] else base
    Json.mkObj fields

instance : FromJson Claim where
  fromJson? j := do
    let taskId    ← j.getObjValAs? String "task_id"
    let agent     ← j.getObjValAs? String "agent"
    let claimedAt ← j.getObjValAs? String "claimed_at"
    let series    := j.getObjValAs? String "series" |>.toOption
    return { taskId, series, agent, claimedAt }

/-! ## Manager -/

/-- In-process serialiser for claim acquire/release. One instance lives in
    the daemon for the duration of its run; the on-disk files are the source
    of truth across restarts. -/
structure ClaimManager where
  private mutex : Std.BaseMutex
deriving Nonempty

def ClaimManager.new : IO ClaimManager := do
  return { mutex := ← Std.BaseMutex.new }

/-- Outcome of a `tryClaim` attempt. -/
inductive ClaimResult where
  /-- Lock acquired; the recorded `Claim` is now on disk. -/
  | acquired (claim : Claim)
  /-- Issue is already claimed; the existing `Claim` is returned for context. -/
  | alreadyClaimed (existing : Claim)
  /-- Issue does not exist or is not in `.open` status. -/
  | invalid (reason : String)
deriving Repr

/-! ## Disk helpers -/

/-- Read the claim file for an issue, if any. Tolerates malformed JSON by
    returning `none` (the daemon will treat such files as missing). -/
def loadClaim (pid : ProjectId) (iid : IssueId) : IO (Option Claim) := do
  let path ← claimFile pid iid
  if !(← path.pathExists) then return none
  let contents ← IO.FS.readFile path
  match Json.parse contents with
  | .error _ => return none
  | .ok j    =>
    match FromJson.fromJson? j with
    | .error _ => return none
    | .ok c    => return some c

/-- All claims currently held within a project. -/
def loadClaims (pid : ProjectId) : IO (Array (IssueId × Claim)) := do
  let dir ← claimsDir pid
  if !(← dir.pathExists) then return #[]
  let entries ← System.FilePath.readDir dir
  let mut out : Array (IssueId × Claim) := #[]
  for entry in entries do
    let name := entry.fileName
    let ext := ".json"
    if name.endsWith ext then
      let idStr := (name.dropEnd ext.length).toString
      if let some c ← loadClaim pid ⟨idStr⟩ then
        out := out.push (⟨idStr⟩, c)
  return out

private def writeClaim (pid : ProjectId) (iid : IssueId) (c : Claim) : IO Unit := do
  IO.FS.createDirAll (← claimsDir pid)
  IO.FS.writeFile (← claimFile pid iid) (Json.compress (ToJson.toJson c))

private def deleteClaimFile (pid : ProjectId) (iid : IssueId) : IO Unit := do
  let path ← claimFile pid iid
  if ← path.pathExists then IO.FS.removeFile path

/-! ## Operations

All public operations take the manager and serialise on its mutex. The
critical section is small (one read of the claim file, one write of the
claim + issue files) so contention is not a concern at orchestra's scale. -/

/-- Attempt to claim `iid` for `taskId`. On success the issue's status moves
    to `.claimed` and a claim file is written. On failure the issue is left
    untouched and the existing claim (if any) is returned. -/
def tryClaim (mgr : ClaimManager) (pid : ProjectId) (iid : IssueId)
    (taskId : String) (agent : String) (nowIso : String)
    (series : Option String := none) : IO ClaimResult := do
  mgr.mutex.lock
  try
    let some issue ← loadIssue pid iid
      | return .invalid s!"issue {iid.value} not found"
    if let some existing ← loadClaim pid iid then
      return .alreadyClaimed existing
    if issue.status != .open then
      return .invalid s!"issue {iid.value} is not open (status={repr issue.status})"
    let claim : Claim := { taskId, agent, series, claimedAt := nowIso }
    writeClaim pid iid claim
    saveIssue { issue with status := .claimed, updatedAt := nowIso }
    return .acquired claim
  finally
    mgr.mutex.unlock

/-- Release a claim, moving the issue to `newStatus` (typically `.open` on
    failure / rejection, or `.inReview` once a PR is attached). The caller
    is expected to choose the right next status; this function does not
    enforce a transition graph. Returns `true` if a claim was actually
    released. -/
def release (mgr : ClaimManager) (pid : ProjectId) (iid : IssueId)
    (newStatus : IssueStatus) (nowIso : String) : IO Bool := do
  mgr.mutex.lock
  try
    let hadClaim := (← loadClaim pid iid).isSome
    deleteClaimFile pid iid
    if let some issue ← loadIssue pid iid then
      saveIssue { issue with status := newStatus, updatedAt := nowIso }
    return hadClaim
  finally
    mgr.mutex.unlock

/-- Force-release without changing the issue status. Useful for daemon
    shutdown / orphan cleanup where we don't want to overwrite the current
    issue state. -/
def forceRelease (mgr : ClaimManager) (pid : ProjectId) (iid : IssueId) : IO Bool := do
  mgr.mutex.lock
  try
    let hadClaim := (← loadClaim pid iid).isSome
    deleteClaimFile pid iid
    return hadClaim
  finally
    mgr.mutex.unlock

end Orchestra.Project
