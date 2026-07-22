import Lean.Data.Json
import Std.Sync
import Orchestra.Project.Basic
import Orchestra.Taxis

open Lean (Json FromJson ToJson)

namespace Orchestra.Project

/-! # Issue claim locks

An open issue may only be worked on by one task at a time. The claim is recorded as a taxis
`session`-kind artifact attached to the issue (see the taxis-side "add `session` artifact kind for
claim tracking" tracking issue) — "claimed" = the issue has at least one `session` artifact.
Decision **C1** of the original plan (file-create-as-lock guarded by a daemon-wide mutex) carries
over unchanged in spirit: taxis has no compare-and-swap primitive, so the `Std.BaseMutex` below is
still what prevents two MCP requests handled by the same daemon process from racing each other
between the "is it claimed" read and the "attach the claim" write. Cross-process races (a second
orchestra daemon, or a human using the taxis UI directly) are not protected by the mutex either
way — same limitation the pre-migration file-lock design already had (file-create isn't atomic
across NFS etc. either), not a regression. -/

structure Claim where
  taskId    : String
  series    : Option String := none
  /-- Backend identifier of the claiming agent (e.g. `"claude"`), recorded so cascade-cancel can
      target the right work later. -/
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
    the daemon for the duration of its run; taxis is the source of truth
    across restarts. -/
structure ClaimManager where
  private mutex : Std.BaseMutex
deriving Nonempty

def ClaimManager.new : IO ClaimManager := do
  return { mutex := ← Std.BaseMutex.new }

/-- Outcome of a `tryClaim` attempt. -/
inductive ClaimResult where
  /-- Lock acquired; the recorded `Claim` is now attached to the issue. -/
  | acquired (claim : Claim)
  /-- Issue is already claimed; the existing `Claim` is returned for context. -/
  | alreadyClaimed (existing : Claim)
  /-- Issue does not exist or is not in `.open` status. -/
  | invalid (reason : String)
deriving Repr

/-! ## Reads -/

/-- The `session`-kind artifact on `artifacts`, decoded, if any — at most one is ever created
    (`tryClaim` checks first), but if several somehow exist the first is used. -/
private def sessionArtifact? (artifacts : Array Orchestra.Taxis.ArtifactView) :
    Option (Orchestra.Taxis.ArtifactView × Claim) :=
  artifacts.findSome? fun a =>
    if a.kind == "session" then
      match (FromJson.fromJson? a.payload : Except String Claim) with
      | .ok c => some (a, c)
      | .error _ => none
    else none

/-- Read the claim on an issue, if any. Tolerates a malformed/unreachable issue by returning
    `none` (the caller treats that the same as "not claimed"). -/
def loadClaim (iid : Taxis.IssueId) : IO (Option Claim) := do
  let cfg ← Orchestra.Taxis.getConfig
  match ← Orchestra.Taxis.getIssueDetail cfg iid with
  | .error _ => return none
  | .ok detail => return (sessionArtifact? detail.attachedArtifacts).map (·.2)

/-- All claims currently held within a project. taxis has no bulk "claims across these issues"
    endpoint, so this is one `GET /issues/:id` per issue (via `loadClaim`) after the lightweight
    project-wide list — the same shape `Project.loadIssues` already accepts as a cost of not
    having a server-side descendants filter. -/
def loadClaims (pid : Taxis.IssueId) : IO (Array (Taxis.IssueId × Claim)) := do
  let issues ← loadIssues pid
  let mut out : Array (Taxis.IssueId × Claim) := #[]
  for i in issues do
    if let some c ← loadClaim i.id then
      out := out.push (i.id, c)
  return out

/-! ## Operations

All public operations take the manager and serialise on its mutex. The critical section is one or
two HTTP calls (read the issue's artifacts, then attach/remove one and patch the status label) so
contention is not a concern at orchestra's scale. -/

/-- Attempt to claim `iid` for `taskId`. On success the issue's status moves
    to `.claimed` and a `session` artifact is attached. On failure the issue is left
    untouched and the existing claim (if any) is returned. -/
def tryClaim (mgr : ClaimManager) (pid : Taxis.IssueId) (iid : Taxis.IssueId)
    (taskId : String) (agent : String) (nowIso : String)
    (series : Option String := none) : IO ClaimResult := do
  mgr.mutex.lock
  try
    let some issue ← loadIssue pid iid
      | return .invalid s!"issue {iid.toString} not found"
    -- For terminal statuses a stale orphan claim must not mask the real reason the issue is
    -- unavailable. Check status first so callers see .invalid rather than .alreadyClaimed for
    -- completed/abandoned issues.
    match issue.status with
    | .completed | .abandoned =>
      return .invalid s!"issue {iid.toString} is not open (status={repr issue.status})"
    | _ => pure ()
    if let some existing ← loadClaim iid then
      return .alreadyClaimed existing
    if issue.status != .open then
      return .invalid s!"issue {iid.toString} is not open (status={repr issue.status})"
    let claim : Claim := { taskId, agent, series, claimedAt := nowIso }
    let cfg ← Orchestra.Taxis.getConfig
    match ← Orchestra.Taxis.createArtifact cfg iid "session" (ToJson.toJson claim) with
    | .error e => return .invalid s!"failed to record claim: {e}"
    | .ok _ =>
      saveIssue { issue with status := .claimed, updatedAt := nowIso }
      return .acquired claim
  finally
    mgr.mutex.unlock

/-- Release a claim, moving the issue to `newStatus` (typically `.open` on
    failure / rejection, or `.inReview` once a PR is attached). The caller
    is expected to choose the right next status; this function does not
    enforce a transition graph. Returns `true` if a claim was actually
    released. -/
def release (mgr : ClaimManager) (pid : Taxis.IssueId) (iid : Taxis.IssueId)
    (newStatus : IssueStatus) (nowIso : String) : IO Bool := do
  mgr.mutex.lock
  try
    let cfg ← Orchestra.Taxis.getConfig
    let hadClaim ← match ← Orchestra.Taxis.getIssueDetail cfg iid with
      | .error _ => pure false
      | .ok detail =>
        match sessionArtifact? detail.attachedArtifacts with
        | none => pure false
        | some (art, _) => do
          let _ ← Orchestra.Taxis.deleteArtifact cfg art.id
          pure true
    if let some issue ← loadIssue pid iid then
      saveIssue { issue with status := newStatus, updatedAt := nowIso }
    return hadClaim
  finally
    mgr.mutex.unlock

/-- Retag an existing claim with a new task ID. Called when a pre-claimed issue
    is picked up by a freshly-generated session: the claim was written with the
    queue-entry ID, but the running task has a different generated ID. Artifacts are immutable
    (there's no `PATCH /artifacts/:id`), so "update" is delete-then-recreate — the artifact gets a
    new id, which is fine since callers only ever look it up by `kind`, never by stored id. -/
def updateClaimTaskId (mgr : ClaimManager) (iid : Taxis.IssueId)
    (newTaskId : String) : IO Unit := do
  mgr.mutex.lock
  try
    let cfg ← Orchestra.Taxis.getConfig
    match ← Orchestra.Taxis.getIssueDetail cfg iid with
    | .error _ => pure ()
    | .ok detail =>
      match sessionArtifact? detail.attachedArtifacts with
      | none => pure ()
      | some (art, claim) =>
        let _ ← Orchestra.Taxis.deleteArtifact cfg art.id
        let _ ← Orchestra.Taxis.createArtifact cfg iid "session"
          (ToJson.toJson { claim with taskId := newTaskId })
        pure ()
  finally
    mgr.mutex.unlock

/-- Force-release without choosing a new status: the issue keeps its taxis `state`, so a
    completed or abandoned issue stays that way. Useful where the caller has no business
    deciding what happens next — daemon shutdown, orphan cleanup, a worker that finished and
    left a pull request behind for review.

    "Without changing the status" still has to clear the `o-claimed` label, because that label
    *is* `.claimed` (`statusOf`) — an issue whose claim artifact is gone but whose label
    remains reads as claimed forever and is skipped by everything that gates on `.open`,
    including the reviewer sweep. The label is cleared even when no artifact was found, since
    that combination is exactly the state older releases left behind.

    Returns whether a claim artifact was actually deleted. -/
def forceRelease (mgr : ClaimManager) (iid : Taxis.IssueId) : IO Bool := do
  mgr.mutex.lock
  try
    let cfg ← Orchestra.Taxis.getConfig
    let hadClaim ← match ← Orchestra.Taxis.getIssueDetail cfg iid with
      | .error _ => pure false
      | .ok detail =>
        match sessionArtifact? detail.attachedArtifacts with
        | none => pure false
        | some (art, _) => do
          let _ ← Orchestra.Taxis.deleteArtifact cfg art.id
          pure true
    -- Best-effort: the claim is already gone by this point, and throwing here would turn a
    -- released lock into a failed task on paths that only call this to clean up.
    try clearClaimedLabel iid
    catch e =>
      IO.eprintln s!"  Warning: released the claim on {iid.toString} but could not clear its \
o-claimed label: {e}"
    return hadClaim
  finally
    mgr.mutex.unlock

end Orchestra.Project
