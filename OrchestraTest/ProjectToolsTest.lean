import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Project
open Orchestra.Project.Tools

namespace OrchestraTest.ProjectTools

/-- Run `act` against a fresh temp `~/.agent/projects` dir. -/
private def withTempHome (act : IO α) : IO α := do
  let tmpRoot : System.FilePath :=
    System.FilePath.mk "/tmp" / s!"orchestra-tools-test-{← IO.monoNanosNow}"
  IO.FS.createDirAll tmpRoot
  setProjectsDirOverride (some tmpRoot)
  try act finally setProjectsDirOverride none

private def setupProject (defaultTarget : Option RepoTarget := none) : IO Project := do
  let pid ← freshProjectId
  let now ← TaskStore.currentIso8601
  let project : Project :=
    { id := pid, name := "test-project", createdAt := now, defaultTarget }
  saveProject project
  return project

private def addOpenIssue (pid : ProjectId) (title : String := "i") : IO Issue := do
  let now ← TaskStore.currentIso8601
  let iid ← freshIssueId
  let issue : Issue :=
    { id := iid, projectId := pid, title, description := "x"
    , target := some { repo := { owner := "o", name := "r" }, branch := "main" }
    , createdAt := now, updatedAt := now }
  saveIssue issue
  return issue

private def baseEnv (perms : List String) (mgr : Option ClaimManager := none)
    (taskId : String := "task-x") : Env :=
  { claimManager := mgr, allowedTools := perms, taskId := some taskId
  , agentBackend := "claude" }

/-- Extract the inner `text` payload from a tool-content JSON envelope. -/
private def textOf (j : Json) : String :=
  let arr? := j.getObjVal? "content" |>.toOption |>.bind (·.getArr? |>.toOption)
  let first? := arr?.bind (·[0]?)
  (first?.bind (·.getObjValAs? String "text" |>.toOption)).getD ""

private def jsonContains (j : Json) (needle : String) : Bool :=
  (textOf j).containsSubstr needle

@[test]
def parseListIssuesWithStatus : Test := do
  let args := Json.mkObj [("project_id", "p"), ("status", "in_review")]
  match tryParseToolCall "list_issues" args with
  | some (.ok (.listIssues pid sf _)) =>
    TestM.assertEqual pid.value "p" (msg := "project id")
    TestM.assert (sf == some .inReview) "status filter parsed"
  | other => TestM.fail s!"unexpected parse: {repr other}"

@[test]
def parseAttachPrFailsWithoutFields : Test := do
  match tryParseToolCall "attach_pr" (Json.mkObj [("issue_id", "x")]) with
  | some (.error _) => TestM.assert true "missing-field error reported"
  | other => TestM.fail s!"expected error, got {repr other}"

@[test]
def parseUnknownToolReturnsNone : Test := do
  match tryParseToolCall "totally_made_up" (Json.mkObj []) with
  | none => TestM.assert true "unknown tool falls through"
  | _    => TestM.fail "should have returned none"

@[test]
def listProjectsRequiresPerm : Test := do
  let result ← (withTempHome do
    evalProjectTool (baseEnv []) .listProjects)
  TestM.assert (jsonContains result "not authorized")
    "missing manage_issues should be denied"

@[test]
def createIssueWithDefaultTargetSucceeds : Test := do
  let (msgOk, listOk) ← (withTempHome do
    let target : RepoTarget := { repo := { owner := "o", name := "r" }, branch := "main" }
    let project ← setupProject (defaultTarget := some target)
    let env := baseEnv [manageIssuesPerm]
    let r1 ← evalProjectTool env (.createIssue project.id "t" "d" none none)
    let r2 ← evalProjectTool env (.listIssues project.id none none)
    return (jsonContains r1 "created issue", jsonContains r2 "[open]"))
  TestM.assert msgOk "create_issue should report success"
  TestM.assert listOk "list_issues should show the new open issue"

@[test]
def createIssueWithoutTargetIsRejected : Test := do
  let denied ← (withTempHome do
    let project ← setupProject  -- no default target
    let env := baseEnv [manageIssuesPerm]
    let r ← evalProjectTool env (.createIssue project.id "t" "d" none none)
    return jsonContains r "no default target")
  TestM.assert denied "issue without effective target should be rejected"

@[test]
def claimAndListOpenInteract : Test := do
  let outcome ← (withTempHome do
    let project ← setupProject
    let issue ← addOpenIssue project.id "claim me"
    let mgr ← ClaimManager.new
    let env := baseEnv [workIssuesPerm] (mgr := some mgr) (taskId := "T1")
    let claimRes ← evalProjectTool env (.claimIssue issue.id)
    let listRes ← evalProjectTool env (.listOpenIssues project.id none)
    let secondClaim ← evalProjectTool (baseEnv [workIssuesPerm] (mgr := some mgr) (taskId := "T2"))
                        (.claimIssue issue.id)
    return ( jsonContains claimRes "\"ok\":true"
           , jsonContains listRes "No open"
           , jsonContains secondClaim "already_claimed"))
  let (firstOk, listEmpty, secondRejected) := outcome
  TestM.assert firstOk            "first claim should succeed"
  TestM.assert listEmpty          "claimed issue should disappear from list_open_issues"
  TestM.assert secondRejected     "second claim should report already_claimed"

@[test]
def attachPrMovesToInReview : Test := do
  let (msg, status) ← (withTempHome do
    let project ← setupProject
    let issue ← addOpenIssue project.id
    let mgr ← ClaimManager.new
    let env := baseEnv [workIssuesPerm] (mgr := some mgr) (taskId := "T1")
    let _ ← evalProjectTool env (.claimIssue issue.id)
    let r ← evalProjectTool env
              (.attachPr issue.id { owner := "o", name := "r" } 42 "feature/x")
    let updated ← loadIssue project.id issue.id
    return (jsonContains r "moved to in_review", updated.map (·.status))
  )
  TestM.assert msg "attach_pr should mention in_review transition"
  TestM.assertEqual status (some .inReview) (msg := "issue status flips to in_review")

@[test]
def decideRejectClearsClaim : Test := do
  let outcome ← (withTempHome do
    let project ← setupProject
    let issue ← addOpenIssue project.id
    let mgr ← ClaimManager.new
    let workEnv := baseEnv [workIssuesPerm, reviewIssuesPerm] (mgr := some mgr) (taskId := "T1")
    let _ ← evalProjectTool workEnv (.claimIssue issue.id)
    let _ ← evalProjectTool workEnv
              (.attachPr issue.id { owner := "o", name := "r" } 7 "br")
    let r ← evalProjectTool workEnv (.decideIssue issue.id .reject "no thanks")
    let updated ← loadIssue project.id issue.id
    return (jsonContains r "rejected", updated.map (·.status)))
  let (rejected, status) := outcome
  TestM.assert rejected "decide_issue reject should report rejection"
  TestM.assertEqual status (some .open) (msg := "rejected issue returns to open")

@[test]
def decideApproveCallsHook : Test := do
  let (calledRef : IO.Ref Bool) ← IO.mkRef false
  let (msgOk, wasCalled) ← (withTempHome do
    let project ← setupProject
    let issue ← addOpenIssue project.id
    let mgr ← ClaimManager.new
    let env : Env :=
      { claimManager := some mgr
      , allowedTools := [workIssuesPerm, reviewIssuesPerm]
      , taskId := some "T1"
      , agentBackend := "claude"
      , enqueueMerger := some (fun _ _ _ => do
          calledRef.set true; return "merger-task-id") }
    let _ ← evalProjectTool env (.claimIssue issue.id)
    let _ ← evalProjectTool env
              (.attachPr issue.id { owner := "o", name := "r" } 9 "br")
    let r ← evalProjectTool env (.decideIssue issue.id .approve "lgtm")
    return (jsonContains r "merger task merger-task-id", ← calledRef.get))
  TestM.assert msgOk     "decide_issue approve should report enqueued merger"
  TestM.assert wasCalled "enqueueMerger hook should have been invoked"

@[test]
def attachPrRequiresOwnership : Test := do
  let denied ← (withTempHome do
    let project ← setupProject
    let issue ← addOpenIssue project.id
    let mgr ← ClaimManager.new
    let _ ← evalProjectTool (baseEnv [workIssuesPerm] (mgr := some mgr) (taskId := "T1"))
              (.claimIssue issue.id)
    let r ← evalProjectTool (baseEnv [workIssuesPerm] (mgr := some mgr) (taskId := "T2"))
              (.attachPr issue.id { owner := "o", name := "r" } 5 "br")
    return jsonContains r "held by task T1")
  TestM.assert denied "non-holder must be rejected from attach_pr"

@[test]
def parseSplitIssueRejectsEmptyChildren : Test := do
  let args := Json.mkObj
    [ ("parent_id", "p"), ("reason", "too big"), ("children", Json.arr #[]) ]
  match tryParseToolCall "split_issue" args with
  | some (.error _) => TestM.assert true "empty children rejected"
  | other => TestM.fail s!"expected error, got {repr other}"

@[test]
def splitIssueHappyPath : Test := do
  let outcome ← (withTempHome do
    let project ← setupProject
    let parent ← addOpenIssue project.id "big task"
    let mgr ← ClaimManager.new
    let env := baseEnv [workIssuesPerm] (mgr := some mgr) (taskId := "T1")
    let _ ← evalProjectTool env (.claimIssue parent.id)
    let children : Array NewSubissueSpec := #[
      { title := "part 1", description := "do x" },
      { title := "part 2", description := "do y" } ]
    let r ← evalProjectTool env (.splitIssue parent.id children "too big for one go")
    let updatedParent ← loadIssue project.id parent.id
    let allIssues ← loadIssues project.id
    let claimAfter ← loadClaim project.id parent.id
    return ( jsonContains r "\"ok\":true"
           , updatedParent.map (·.status)
           , allIssues.size
           , claimAfter.isNone))
  let (ok, parentStatus, count, claimGone) := outcome
  TestM.assert ok                   "split_issue should report success"
  TestM.assertEqual parentStatus (some .blocked) (msg := "parent moves to blocked")
  TestM.assertEqual count 3         (msg := "parent + 2 children = 3 issues")
  TestM.assert claimGone            "claim file should be deleted after split"

@[test]
def releaseClaimAfterAttachPrPreservesInReview : Test := do
  let status ← (withTempHome do
    let project ← setupProject
    let issue ← addOpenIssue project.id
    let mgr ← ClaimManager.new
    let env := baseEnv [workIssuesPerm] (mgr := some mgr) (taskId := "T1")
    let _ ← evalProjectTool env (.claimIssue issue.id)
    let _ ← evalProjectTool env
              (.attachPr issue.id { owner := "o", name := "r" } 42 "feature/x")
    let _ ← evalProjectTool env (.releaseClaim issue.id "done for now")
    let updated ← loadIssue project.id issue.id
    return updated.map (·.status))
  TestM.assertEqual status (some .inReview)
    (msg := "releasing claim after attach_pr should keep status in_review")

@[test]
def splitIssueRequiresOwnership : Test := do
  let denied ← (withTempHome do
    let project ← setupProject
    let parent ← addOpenIssue project.id
    let mgr ← ClaimManager.new
    -- T1 claims, but T2 tries to split.
    let _ ← evalProjectTool (baseEnv [workIssuesPerm] (mgr := some mgr) (taskId := "T1"))
              (.claimIssue parent.id)
    let r ← evalProjectTool (baseEnv [workIssuesPerm] (mgr := some mgr) (taskId := "T2"))
              (.splitIssue parent.id
                #[{ title := "x", description := "y" }] "stealing")
    return jsonContains r "held by task T1")
  TestM.assert denied "non-holder must be rejected"

end OrchestraTest.ProjectTools
