import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Project

namespace OrchestraTest.ProjectClaim

/-- Run `act` with the projects directory pointed at a fresh temp dir. -/
private def withTempHome (act : IO α) : IO α := do
  let tmpRoot : System.FilePath :=
    System.FilePath.mk "/tmp" / s!"orchestra-test-{← IO.monoNanosNow}"
  IO.FS.createDirAll tmpRoot
  setProjectsDirOverride (some tmpRoot)
  try
    act
  finally
    setProjectsDirOverride none

private def setupProjectWithIssue : IO (Project × Issue) := do
  let pid ← freshProjectId
  let now ← TaskStore.currentIso8601
  let project : Project := { id := pid, name := "test", createdAt := now }
  saveProject project
  let iid ← freshIssueId
  let issue : Issue :=
    { id := iid, projectId := pid, title := "do the thing"
    , description := "details", createdAt := now, updatedAt := now }
  saveIssue issue
  return (project, issue)

@[test]
def claimAcquiresAndPersists : Test := do
  let ok ← (do
    withTempHome do
      let mgr ← ClaimManager.new
      let (project, issue) ← setupProjectWithIssue
      let now ← TaskStore.currentIso8601
      match ← tryClaim mgr project.id issue.id "task-1" "claude" now with
      | .acquired c =>
        let onDisk ← loadClaim project.id issue.id
        let reloadedIssue ← loadIssue project.id issue.id
        return c.taskId == "task-1"
            && (onDisk.map (·.taskId) == some "task-1")
            && (reloadedIssue.map (·.status) == some .claimed)
      | .alreadyClaimed _ | .invalid _ => return false)
  TestM.assert ok "first claim should be acquired and persisted on disk"

@[test]
def secondClaimerIsRejected : Test := do
  let outcome ← (do
    withTempHome do
      let mgr ← ClaimManager.new
      let (project, issue) ← setupProjectWithIssue
      let now ← TaskStore.currentIso8601
      let _ ← tryClaim mgr project.id issue.id "task-1" "claude" now
      tryClaim mgr project.id issue.id "task-2" "vibe" now)
  match outcome with
  | .alreadyClaimed existing =>
    TestM.assertEqual existing.taskId "task-1"
      (msg := "rejected claim should report the original holder")
  | .acquired _    => TestM.fail "second claim should not be acquired"
  | .invalid r     => TestM.fail s!"second claim returned invalid: {r}"

@[test]
def releaseAllowsReclaim : Test := do
  let outcome ← (do
    withTempHome do
      let mgr ← ClaimManager.new
      let (project, issue) ← setupProjectWithIssue
      let now ← TaskStore.currentIso8601
      let _ ← tryClaim mgr project.id issue.id "task-1" "claude" now
      let released ← release mgr project.id issue.id .open now
      let res ← tryClaim mgr project.id issue.id "task-2" "vibe" now
      return (released, res))
  match outcome with
  | (true, .acquired c) =>
    TestM.assertEqual c.taskId "task-2" (msg := "reclaim should record new holder")
  | (false, _)          => TestM.fail "release should report a held claim"
  | (_, other)          => TestM.fail s!"reclaim outcome was not acquired: {repr other}"

@[test]
def cannotClaimNonOpenIssue : Test := do
  let outcome ← (do
    withTempHome do
      let mgr ← ClaimManager.new
      let (project, issue) ← setupProjectWithIssue
      let now ← TaskStore.currentIso8601
      -- Manually move issue out of `.open` (e.g. completed by review).
      saveIssue { issue with status := .completed, updatedAt := now }
      tryClaim mgr project.id issue.id "task-1" "claude" now)
  match outcome with
  | .invalid _   => TestM.assert true "non-open issue rejected"
  | .acquired _  => TestM.fail "claiming a completed issue must not succeed"
  | .alreadyClaimed _ =>
    TestM.fail "completed-issue claim should be invalid, not already-claimed"

end OrchestraTest.ProjectClaim
