import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Project
open Orchestra.Project.Tools

namespace OrchestraTest.ProjectTools

/-! Most of these exercise `evalProjectTool` end-to-end against real taxis-backed issues, so they
    need a real taxis instance — see `Orchestra.ensureTestTaxisConfigured`'s docs for how to point
    them at one. Each such test skips itself (prints a note, no assertions) when taxis isn't
    configured. The pure-parsing tests (`tryParseToolCall`, no `evalProjectTool` call) don't need
    it and always run. -/

private def setupProject (defaultTarget : Option RepoTarget := none) : IO Project :=
  createProject "test-project" (defaultTarget := defaultTarget)

private def addOpenIssue (pid : Taxis.IssueId) (title : String := "i") : IO Issue :=
  createIssue pid title "x" (target := some { repo := { owner := "o", name := "r" }, branch := "main" })

/-- Delete every issue created during a test, then its project. Issues first since taxis's
    `parent_id` FK is `ON DELETE SET NULL`, not cascading — deleting the project alone would leave
    orphans rather than cleaning them up. -/
private def cleanup (project : Project) (issues : Array Issue) : IO Unit := do
  for i in issues do
    deleteIssue i.id
  deleteProject project.id

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

/-! ## Parsing (pure — no taxis needed) -/

@[test]
def parseListIssuesWithStatus : Test := do
  let args := Json.mkObj [("project_id", Json.num 7), ("status", "claimed")]
  match tryParseToolCall "list_issues" args with
  | some (.ok (.listIssues pid sf _)) =>
    TestM.assertEqual pid.val (7 : Int64) (msg := "project id")
    TestM.assert (sf == some .claimed) "status filter parsed"
  | other => TestM.fail s!"unexpected parse: {repr other}"

@[test]
def parseAttachPrFailsWithoutFields : Test := do
  match tryParseToolCall "attach_pr" (Json.mkObj [("issue_id", Json.num 1)]) with
  | some (.error _) => TestM.assert true "missing-field error reported"
  | other => TestM.fail s!"expected error, got {repr other}"

@[test]
def parseUnknownToolReturnsNone : Test := do
  match tryParseToolCall "totally_made_up" (Json.mkObj []) with
  | none => TestM.assert true "unknown tool falls through"
  | _    => TestM.fail "should have returned none"

@[test]
def parseSplitIssueRejectsEmptyChildren : Test := do
  let args := Json.mkObj
    [ ("parent_id", Json.num 1), ("reason", "too big"), ("children", Json.arr #[]) ]
  match tryParseToolCall "split_issue" args with
  | some (.error _) => TestM.assert true "empty children rejected"
  | other => TestM.fail s!"expected error, got {repr other}"

/-! ## Evaluation (needs a real taxis instance) -/

@[test]
def listProjectsRequiresPerm : Test := do
  unless ← ensureTestTaxisConfigured do
    TestM.skip "ORCHESTRA_TEST_TAXIS_URL/TOKEN not set"; return
  let result ← evalProjectTool (baseEnv []) .listProjects
  TestM.assert (jsonContains result "not authorized")
    "missing manage_issues should be denied"

@[test]
def createIssueWithDefaultTargetSucceeds : Test := do
  unless ← ensureTestTaxisConfigured do
    TestM.skip "ORCHESTRA_TEST_TAXIS_URL/TOKEN not set"; return
  let target : RepoTarget := { repo := { owner := "o", name := "r" }, branch := "main" }
  let project ← setupProject (defaultTarget := some target)
  -- Scoped to the project: `manage_issues` writes are confined to the task's own subtree, and an
  -- env naming neither project nor issue has no subtree at all, so every write is refused. The
  -- sibling test below never reaches that check — the missing-target rejection short-circuits
  -- first — which is why only this one noticed.
  let env := { baseEnv [manageIssuesPerm] with projectId := some project.id }
  let r1 ← evalProjectTool env (.createIssue project.id "t" "d" none none)
  let r2 ← evalProjectTool env (.listIssues project.id none none)
  let issues ← loadIssues project.id
  cleanup project issues
  TestM.assert (jsonContains r1 "created issue") "create_issue should report success"
  TestM.assert (jsonContains r2 "[open]") "list_issues should show the new open issue"

@[test]
def createIssueWithoutTargetIsRejected : Test := do
  unless ← ensureTestTaxisConfigured do
    TestM.skip "ORCHESTRA_TEST_TAXIS_URL/TOKEN not set"; return
  let project ← setupProject  -- no default target
  let env := baseEnv [manageIssuesPerm]
  let r ← evalProjectTool env (.createIssue project.id "t" "d" none none)
  cleanup project #[]
  TestM.assert (jsonContains r "no default target") "issue without effective target should be rejected"

@[test]
def claimAndListOpenInteract : Test := do
  unless ← ensureTestTaxisConfigured do
    TestM.skip "ORCHESTRA_TEST_TAXIS_URL/TOKEN not set"; return
  let project ← setupProject
  let issue ← addOpenIssue project.id "claim me"
  let mgr ← ClaimManager.new
  let env := baseEnv [workIssuesPerm] (mgr := some mgr) (taskId := "T1")
  let claimRes ← evalProjectTool env (.claimIssue issue.id)
  let listRes ← evalProjectTool env (.listOpenIssues project.id none)
  let secondClaim ← evalProjectTool (baseEnv [workIssuesPerm] (mgr := some mgr) (taskId := "T2"))
                      (.claimIssue issue.id)
  cleanup project #[issue]
  TestM.assert (jsonContains claimRes "\"ok\":true") "first claim should succeed"
  TestM.assert (jsonContains listRes "No open") "claimed issue should disappear from list_open_issues"
  TestM.assert (jsonContains secondClaim "already_claimed") "second claim should report already_claimed"

@[test]
def attachPrMovesToInReview : Test := do
  unless ← ensureTestTaxisConfigured do
    TestM.skip "ORCHESTRA_TEST_TAXIS_URL/TOKEN not set"; return
  let project ← setupProject
  let issue ← addOpenIssue project.id
  let mgr ← ClaimManager.new
  let env := baseEnv [workIssuesPerm] (mgr := some mgr) (taskId := "T1")
  let _ ← evalProjectTool env (.claimIssue issue.id)
  let r ← evalProjectTool env
            (.attachPr issue.id { owner := "o", name := "r" } 42 "feature/x")
  let updated ← loadIssue project.id issue.id
  cleanup project #[issue]
  TestM.assert (jsonContains r "awaiting review") "attach_pr should say the issue awaits review"
  -- No status flip any more: an issue awaits review because it has an unmerged PR attached, not
  -- because a status says so. "Unchanged" means unchanged from `.claimed`, though — the worker
  -- still holds its claim here and may push more commits; the issue only becomes `.open` when
  -- the worker finishes and releases (see `releaseClaimAfterAttachPrPreservesInReview`).
  TestM.assertEqual (updated.map (·.status)) (some .claimed) (msg := "status is unchanged")
  TestM.assertEqual (updated.map (·.attachedPRs.size)) (some 1) (msg := "the PR is attached")

@[test]
def decideRejectClearsClaim : Test := do
  unless ← ensureTestTaxisConfigured do
    TestM.skip "ORCHESTRA_TEST_TAXIS_URL/TOKEN not set"; return
  let project ← setupProject
  let issue ← addOpenIssue project.id
  let mgr ← ClaimManager.new
  let workEnv := baseEnv [workIssuesPerm, reviewIssuesPerm] (mgr := some mgr) (taskId := "T1")
  let _ ← evalProjectTool workEnv (.claimIssue issue.id)
  let _ ← evalProjectTool workEnv
            (.attachPr issue.id { owner := "o", name := "r" } 7 "br")
  let r ← evalProjectTool workEnv (.decideIssue issue.id .reject "no thanks")
  let updated ← loadIssue project.id issue.id
  cleanup project #[issue]
  TestM.assert (jsonContains r "rejected") "decide_issue reject should report rejection"
  TestM.assertEqual (updated.map (·.status)) (some .open) (msg := "rejected issue returns to open")

@[test]
def decideApproveCallsHook : Test := do
  unless ← ensureTestTaxisConfigured do
    TestM.skip "ORCHESTRA_TEST_TAXIS_URL/TOKEN not set"; return
  let calledRef ← IO.mkRef false
  let project ← setupProject
  let issue ← addOpenIssue project.id
  let mgr ← ClaimManager.new
  let env : Env :=
    { claimManager := some mgr
    , allowedTools := [workIssuesPerm, reviewIssuesPerm]
    , taskId := some "T1"
    , agentBackend := "claude"
    , enqueueMerger := some (fun _ _ _ => do
        calledRef.set true; return .ok "merger-task-id") }
  let _ ← evalProjectTool env (.claimIssue issue.id)
  let _ ← evalProjectTool env
            (.attachPr issue.id { owner := "o", name := "r" } 9 "br")
  let r ← evalProjectTool env (.decideIssue issue.id .approve "lgtm")
  let wasCalled ← calledRef.get
  cleanup project #[issue]
  TestM.assert (jsonContains r "merger task merger-task-id") "decide_issue approve should report enqueued merger"
  TestM.assert wasCalled "enqueueMerger hook should have been invoked"

@[test]
def attachPrRequiresOwnership : Test := do
  unless ← ensureTestTaxisConfigured do
    TestM.skip "ORCHESTRA_TEST_TAXIS_URL/TOKEN not set"; return
  let project ← setupProject
  let issue ← addOpenIssue project.id
  let mgr ← ClaimManager.new
  let _ ← evalProjectTool (baseEnv [workIssuesPerm] (mgr := some mgr) (taskId := "T1"))
            (.claimIssue issue.id)
  let r ← evalProjectTool (baseEnv [workIssuesPerm] (mgr := some mgr) (taskId := "T2"))
            (.attachPr issue.id { owner := "o", name := "r" } 5 "br")
  cleanup project #[issue]
  TestM.assert (jsonContains r "held by task T1") "non-holder must be rejected from attach_pr"

@[test]
def splitIssueHappyPath : Test := do
  unless ← ensureTestTaxisConfigured do
    TestM.skip "ORCHESTRA_TEST_TAXIS_URL/TOKEN not set"; return
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
  let claimAfter ← loadClaim parent.id
  cleanup project allIssues
  TestM.assert (jsonContains r "\"ok\":true") "split_issue should report success"
  -- The parent keeps its status: what makes it a container now is having open children, which
  -- `Project.dispatchCandidates` reads off the tree rather than off a label.
  TestM.assertEqual (updatedParent.map (·.status)) (some .open)
    (msg := "split leaves the parent's own status alone")
  TestM.assertEqual allIssues.size 3 (msg := "parent + 2 children = 3 issues")
  TestM.assert claimAfter.isNone "claim should be cleared after split"

@[test]
def releaseClaimAfterAttachPrPreservesInReview : Test := do
  unless ← ensureTestTaxisConfigured do
    TestM.skip "ORCHESTRA_TEST_TAXIS_URL/TOKEN not set"; return
  let project ← setupProject
  let issue ← addOpenIssue project.id
  let mgr ← ClaimManager.new
  let env := baseEnv [workIssuesPerm] (mgr := some mgr) (taskId := "T1")
  let _ ← evalProjectTool env (.claimIssue issue.id)
  let _ ← evalProjectTool env
            (.attachPr issue.id { owner := "o", name := "r" } 42 "feature/x")
  let _ ← evalProjectTool env (.releaseClaim issue.id "done for now")
  let updated ← loadIssue project.id issue.id
  cleanup project #[issue]
  TestM.assertEqual (updated.map (·.status)) (some .open)
    (msg := "releasing a claim returns the issue to open; its attached PR is what marks it for review")

@[test]
def splitIssueRequiresOwnership : Test := do
  unless ← ensureTestTaxisConfigured do
    TestM.skip "ORCHESTRA_TEST_TAXIS_URL/TOKEN not set"; return
  let project ← setupProject
  let parent ← addOpenIssue project.id
  let mgr ← ClaimManager.new
  -- T1 claims, but T2 tries to split.
  let _ ← evalProjectTool (baseEnv [workIssuesPerm] (mgr := some mgr) (taskId := "T1"))
            (.claimIssue parent.id)
  let r ← evalProjectTool (baseEnv [workIssuesPerm] (mgr := some mgr) (taskId := "T2"))
            (.splitIssue parent.id
              #[{ title := "x", description := "y" }] "stealing")
  cleanup project #[parent]
  TestM.assert (jsonContains r "held by task T1") "non-holder must be rejected"

end OrchestraTest.ProjectTools

/-- `list_issue_comments` is offered under three permission groups and `comment_issue` under two,
    so a task holding more than one would be handed the same tool twice without the dedupe in
    `Server.toolsList` — a malformed tools/list the agent sees, not a server-side error. -/
@[test]
def commentToolsAreListedOncePerGroupSet : Test := do
  let names := toolDefs.map (fun (_, name, _) => name)
  TestM.assert ((names.filter (· == "list_issue_comments")).length > 1)
    "list_issue_comments is deliberately registered under several groups"
  let forWorker := toolDefs.filterMap fun (perm, name, _) =>
    if perm == workIssuesPerm || perm == reviewIssuesPerm then some name else none
  let deduped := forWorker.foldl (fun acc n => if acc.contains n then acc else acc ++ [n]) []
  TestM.assert (forWorker.length > deduped.length)
    "a worker+reviewer task sees duplicates before dedupe, which is what Server.toolsList removes"

@[test]
def commentToolsParse : Test := do
  match tryParseToolCall "comment_issue"
      (Json.mkObj [("issue_id", Json.num 57), ("body", Json.str "looks good")]) with
  | some (.ok (.commentIssue iid body)) =>
    TestM.assertEqual iid.toString "57"
    TestM.assertEqual body "looks good"
  | _ => TestM.fail "comment_issue should parse"
  match tryParseToolCall "list_issue_comments" (Json.mkObj [("issue_id", Json.num 57)]) with
  | some (.ok (.listIssueComments iid)) => TestM.assertEqual iid.toString "57"
  | _ => TestM.fail "list_issue_comments should parse"
