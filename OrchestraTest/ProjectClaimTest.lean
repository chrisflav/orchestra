import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Project

namespace OrchestraTest.ProjectClaim

/-! Claims are taxis `session` artifacts now (see the "Migrate Claim.lean onto taxis session
    artifacts" tracking issue), so these tests need a real taxis instance — see
    `Orchestra.ensureTestTaxisConfigured`'s docs for how to point them at one. Each test skips
    itself (prints a note, no assertions) when taxis isn't configured, rather than failing. -/

private def setupProjectWithIssue : IO (Project × Issue) := do
  let project ← createProject "test"
  let issue ← createIssue project.id "do the thing" "details"
  return (project, issue)

private def cleanup (project : Project) (issue : Issue) : IO Unit := do
  deleteIssue issue.id
  deleteProject project.id

@[test]
def claimAcquiresAndPersists : Test := do
  unless ← ensureTestTaxisConfigured do
    TestM.skip "ORCHESTRA_TEST_TAXIS_URL/TOKEN not set"; return
  let (project, issue) ← setupProjectWithIssue
  let ok ← (do
    let mgr ← ClaimManager.new
    let now ← TaskStore.currentIso8601
    match ← tryClaim mgr project.id issue.id "task-1" "claude" now with
    | .acquired c =>
      let onDisk ← loadClaim issue.id
      let reloadedIssue ← loadIssue project.id issue.id
      return c.taskId == "task-1"
          && (onDisk.map (·.taskId) == some "task-1")
          && (reloadedIssue.map (·.status) == some .claimed)
    | .alreadyClaimed _ | .invalid _ => return false)
  cleanup project issue
  TestM.assert ok "first claim should be acquired and persisted"

@[test]
def secondClaimerIsRejected : Test := do
  unless ← ensureTestTaxisConfigured do
    TestM.skip "ORCHESTRA_TEST_TAXIS_URL/TOKEN not set"; return
  let (project, issue) ← setupProjectWithIssue
  let outcome ← (do
    let mgr ← ClaimManager.new
    let now ← TaskStore.currentIso8601
    let _ ← tryClaim mgr project.id issue.id "task-1" "claude" now
    tryClaim mgr project.id issue.id "task-2" "vibe" now)
  cleanup project issue
  match outcome with
  | .alreadyClaimed existing =>
    TestM.assertEqual existing.taskId "task-1"
      (msg := "rejected claim should report the original holder")
  | .acquired _    => TestM.fail "second claim should not be acquired"
  | .invalid r     => TestM.fail s!"second claim returned invalid: {r}"

@[test]
def releaseAllowsReclaim : Test := do
  unless ← ensureTestTaxisConfigured do
    TestM.skip "ORCHESTRA_TEST_TAXIS_URL/TOKEN not set"; return
  let (project, issue) ← setupProjectWithIssue
  let outcome ← (do
    let mgr ← ClaimManager.new
    let now ← TaskStore.currentIso8601
    let _ ← tryClaim mgr project.id issue.id "task-1" "claude" now
    let released ← release mgr project.id issue.id .open now
    let res ← tryClaim mgr project.id issue.id "task-2" "vibe" now
    return (released, res))
  cleanup project issue
  match outcome with
  | (true, .acquired c) =>
    TestM.assertEqual c.taskId "task-2" (msg := "reclaim should record new holder")
  | (false, _)          => TestM.fail "release should report a held claim"
  | (_, other)          => TestM.fail s!"reclaim outcome was not acquired: {repr other}"

@[test]
def forceReleaseClearsClaimedLabel : Test := do
  unless ← ensureTestTaxisConfigured do
    TestM.skip "ORCHESTRA_TEST_TAXIS_URL/TOKEN not set"; return
  -- The regression this guards: `forceRelease` deleted the session artifact but left the
  -- `o-claimed` label, and that label *is* `.claimed` (`statusOf`) — so a worker that finished
  -- and left a PR for review dropped its lock while the issue went on reading as claimed
  -- forever, invisible to the reviewer sweep and to the dispatcher, both of which gate on
  -- `.open`.
  let (project, issue) ← setupProjectWithIssue
  let outcome ← (do
    let mgr ← ClaimManager.new
    let now ← TaskStore.currentIso8601
    let _ ← tryClaim mgr project.id issue.id "task-1" "claude" now
    let hadClaim ← forceRelease mgr issue.id
    let claim ← loadClaim issue.id
    let reloaded ← loadIssue project.id issue.id
    -- Reclaiming has to work too: `tryClaim` refuses anything that is not `.open`, so a
    -- lingering label would also make the issue permanently unclaimable.
    let reclaim ← tryClaim mgr project.id issue.id "task-2" "vibe" now
    return (hadClaim, claim, reloaded.map (·.status), reclaim))
  cleanup project issue
  let (hadClaim, claim, status, reclaim) := outcome
  TestM.assert hadClaim "forceRelease should report that it deleted a claim"
  TestM.assertEqual (claim.map (·.taskId)) none (msg := "the session artifact is gone")
  TestM.assertEqual status (some IssueStatus.open)
    (msg := "and the issue reads as open, not stuck in claimed")
  match reclaim with
  | .acquired c => TestM.assertEqual c.taskId "task-2" (msg := "the issue is claimable again")
  | other       => TestM.fail s!"reclaim after forceRelease failed: {repr other}"

@[test]
def forceReleaseKeepsTerminalState : Test := do
  unless ← ensureTestTaxisConfigured do
    TestM.skip "ORCHESTRA_TEST_TAXIS_URL/TOKEN not set"; return
  -- Clearing the label must not become "reopen the issue": `forceRelease` leaves the taxis
  -- `state` alone, which is what lets orphan cleanup run over completed issues safely.
  let (project, issue) ← setupProjectWithIssue
  let status ← (do
    let mgr ← ClaimManager.new
    let now ← TaskStore.currentIso8601
    let _ ← tryClaim mgr project.id issue.id "task-1" "claude" now
    let _ ← release mgr project.id issue.id .completed now
    let _ ← forceRelease mgr issue.id
    return (← loadIssue project.id issue.id).map (·.status))
  cleanup project issue
  TestM.assertEqual status (some IssueStatus.completed)
    (msg := "a completed issue stays completed")

@[test]
def cannotClaimNonOpenIssue : Test := do
  unless ← ensureTestTaxisConfigured do
    TestM.skip "ORCHESTRA_TEST_TAXIS_URL/TOKEN not set"; return
  let (project, issue) ← setupProjectWithIssue
  let outcome ← (do
    let mgr ← ClaimManager.new
    let now ← TaskStore.currentIso8601
    -- Manually move issue out of `.open` (e.g. completed by review).
    saveIssue { issue with status := .completed, updatedAt := now }
    tryClaim mgr project.id issue.id "task-1" "claude" now)
  cleanup project issue
  match outcome with
  | .invalid _   => TestM.assert true "non-open issue rejected"
  | .acquired _  => TestM.fail "claiming a completed issue must not succeed"
  | .alreadyClaimed _ =>
    TestM.fail "completed-issue claim should be invalid, not already-claimed"

end OrchestraTest.ProjectClaim
