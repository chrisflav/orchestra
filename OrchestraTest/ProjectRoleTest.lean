import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Project
open Orchestra.Listener

namespace OrchestraTest.ProjectRole

/-! Roles stay file-based even after the taxis migration (see the "Migrate Project/Issue data
    layer" tracking issue — out of scope there), so `projectRolesDir`/`globalRolesDir` isolation
    via `withTempHome` still applies unchanged. The dispatcher tests below don't touch taxis at
    all: `dispatcherTick` is a pure function over `Project`/`Issue` values, so fixtures are built
    as plain record literals with fabricated ids rather than round-tripped through a real
    tracker — there's nothing here for a taxis instance to add. -/

private def withTempHome (act : IO α) : IO α := do
  let tmpRoot : System.FilePath :=
    System.FilePath.mk "/tmp" / s!"orchestra-role-test-{← IO.monoNanosNow}"
  let projectsRoot := tmpRoot / "projects"
  let globalRoles  := tmpRoot / "global-roles"
  IO.FS.createDirAll projectsRoot
  IO.FS.createDirAll globalRoles
  setProjectsDirOverride (some projectsRoot)
  setGlobalRolesDirOverride (some globalRoles)
  try act
  finally
    setProjectsDirOverride none
    setGlobalRolesDirOverride none

private def writeRole (dir : System.FilePath) (name : String) (json : String) : IO Unit := do
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / s!"{name}.json") json

/-- A fixture project, not backed by any real tracker — just enough of a `Project` value for
    `projectRolesDir`/`dispatcherTick` to work with. -/
private def fixtureProject (id : Int64 := 1) : Project :=
  { id := ⟨id⟩, name := "demo", createdAt := "2026-01-01T00:00:00Z" }

/-- A fixture issue belonging to `fixtureProject`, not backed by any real tracker. -/
private def fixtureIssue (id : Int64) (project : Project) (status : IssueStatus := .open)
    (title : String := "i") (dependencies : Array Taxis.IssueId := #[]) : Issue :=
  { id := ⟨id⟩, projectId := project.id, title, description := "x", status, dependencies
  , target := some { repo := { owner := "o", name := "r" }, branch := "main" }
  , createdAt := "2026-01-01T00:00:00Z", updatedAt := "2026-01-01T00:00:00Z" }

@[test]
def renderSubstitutesPlaceholders : Test := do
  let v : RenderVars :=
    { projectId := "p1", projectName := "API",
      instructions := "go", issueId := some "i1",
      issueTitle := some "do x",
      targetRepo := some "o/r", targetBranch := some "main" }
  let out := render
    "[{{project_name}} / {{issue_id}}] {{issue_title}} on {{target_repo}}@{{target_branch}}: {{instructions}}"
    v
  TestM.assertEqual out "[API / i1] do x on o/r@main: go" (msg := "render substitution")

/-- The issue body has to reach the agent through the prompt: `get_issue` is the only tool that
    renders it and it needs `manage_issues`, which worker roles don't have. Before
    `{{issue_description}}` existed, a template asking for it got an empty string and the agent
    was left with only a title — so this pins both the substitution and that the shipped
    implementor template actually uses it. -/
@[test]
def renderSubstitutesIssueDescription : Test := do
  let v : RenderVars :=
    { projectId := "9", projectName := "Formal schemes",
      instructions := "", issueId := some "9",
      issueTitle := some "Formal schemes",
      issueDescription := some "Suggested reference EGA I.",
      targetRepo := some "o/r", targetBranch := some "master" }
  TestM.assertEqual (render "{{issue_description}}" v) "Suggested reference EGA I."
    (msg := "issue_description substitution")
  -- An issue-less render (planner-style) leaves it empty rather than failing.
  let noIssue : RenderVars := { projectId := "1", projectName := "P", instructions := "" }
  TestM.assertEqual (render "[{{issue_description}}]" noIssue) "[]"
    (msg := "absent description renders empty")

@[test]
def shippedImplementorTemplateCarriesTheIssueBody : Test := do
  let raw ← IO.FS.readFile "examples/projects/roles/implementor.json"
  match Json.parse raw >>= FromJson.fromJson? (α := Role) with
  | .error e => TestM.fail s!"implementor.json: {e}"
  | .ok role =>
    TestM.assert ((role.promptTemplate.splitOn "{{issue_description}}").length > 1)
      "the shipped implementor template must include {{issue_description}}"

@[test]
def projectRoleOverridesGlobal : Test := do
  let outcome ← (withTempHome do
    let project := fixtureProject
    let global ← globalRolesDir
    writeRole global "worker" r#"{"name":"worker","permissions":["a"],"prompt_template":"GLOBAL"}"#
    let pdir ← projectRolesDir project.id
    writeRole pdir "worker" r#"{"name":"worker","permissions":["a","b"],"prompt_template":"PROJECT"}"#
    let r ← loadRole project.id "worker"
    return r.map (fun r => (r.promptTemplate, r.permissions)))
  TestM.assertEqual outcome (some ("PROJECT", ["a","b"])) (msg := "project file wins")

@[test]
def loadAllRolesMergesGlobalsThatArentShadowed : Test := do
  let names ← (withTempHome do
    let project := fixtureProject
    let global ← globalRolesDir
    let pdir ← projectRolesDir project.id
    writeRole global "planner"  r#"{"name":"planner","permissions":["manage_issues"],"prompt_template":"P"}"#
    writeRole global "reviewer" r#"{"name":"reviewer","permissions":["review_issues"],"prompt_template":"R"}"#
    -- Project shadows planner with a different impl, leaves reviewer untouched.
    writeRole pdir   "planner"  r#"{"name":"planner","permissions":["manage_issues","comment"],"prompt_template":"PP"}"#
    let rs ← loadAllRoles project.id
    return (rs.map (·.name)).qsort (· < ·))
  TestM.assertEqual names #["planner", "reviewer"] (msg := "merged set")

@[test]
def dispatcherSpawnsWorkerWhenOpenIssueAndCapAllows : Test := do
  let project := fixtureProject
  let issue := fixtureIssue 101 project (status := .open) (title := "do it")
  let role : Role :=
    { name := "implementor", permissions := ["work_issues", "create_pr"]
    , promptTemplate := "implement"
    , dispatch := some { trigger := .hasOpenIssues, max := 1, preClaim := true } }
  let result := dispatcherTick
    { activeByRole := {}, issues := #[issue], caps := [("implementor", 2)], roles := #[role] }
  TestM.assertEqual result.size 1 (msg := "exactly one spawn")
  match result[0]? with
  | some s =>
    TestM.assertEqual s.roleName "implementor" (msg := "role name")
    TestM.assert s.issueId.isSome  "spawn must bind to an issue"
  | none => TestM.fail "expected one spawn"

/-- A reviewer spawn has to carry the issue it is reviewing. The label-dispatcher takes an entry's
    repository and branch from the issue's own artifacts, so an unbound spawn has no target and is
    dropped — which silently meant reviewers were never dispatched there. -/
@[test]
def dispatcherBindsReviewerToInReviewIssue : Test := do
  let project := fixtureProject
  let issues := #[fixtureIssue 101 project .open]
  -- Awaiting review is no longer a status: the caller resolves it (open issue, unmerged PR
  -- attached) and hands the result in as `reviewable`.
  let reviewable := #[fixtureIssue 102 project .open]
  let role : Role :=
    { name := "reviewer", permissions := ["review_issues"]
    , promptTemplate := "review"
    , dispatch := some { trigger := .hasInReviewIssues, max := 1, preClaim := false } }
  let result := dispatcherTick
    { activeByRole := {}, issues, reviewable, caps := [("reviewer", 1)], roles := #[role] }
  TestM.assertEqual result.size 1 (msg := "exactly one reviewer spawn")
  match result[0]? with
  | some s =>
    TestM.assert s.issueId.isSome "reviewer spawn must bind to an issue"
    TestM.assertEqual (s.issueId.map (·.toString)) (some "102")
      (msg := "bound to the reviewable issue, not the one a worker would take")
  | none => TestM.fail "expected one spawn"

/-- No in-review issue means no reviewer, bound or otherwise. -/
@[test]
def dispatcherSkipsReviewerWithoutInReviewIssues : Test := do
  let project := fixtureProject
  let role : Role :=
    { name := "reviewer", permissions := ["review_issues"]
    , promptTemplate := "review"
    , dispatch := some { trigger := .hasInReviewIssues, max := 1 } }
  let result := dispatcherTick
    { activeByRole := {}, issues := #[fixtureIssue 101 project .open]
    , reviewable := #[], caps := [("reviewer", 1)], roles := #[role] }
  TestM.assert result.isEmpty "nothing awaiting review, so no reviewer"

@[test]
def dispatcherRespectsCap : Test := do
  let project := fixtureProject
  let issues := #[fixtureIssue 101 project .open, fixtureIssue 102 project .open]
  let role : Role :=
    { name := "implementor", permissions := []
    , promptTemplate := "x"
    , dispatch := some { trigger := .hasOpenIssues, max := 5 } }
  let active : Std.HashMap String Nat := ({} : Std.HashMap String Nat).insert "implementor" 2
  let count := (dispatcherTick
    { activeByRole := active, issues, caps := [("implementor", 2)], roles := #[role] }).size
  TestM.assertEqual count 0 (msg := "active==cap blocks spawn")

@[test]
def dispatcherIdleTriggerOnlyWhenNoWork : Test := do
  let project := fixtureProject
  let role : Role :=
    { name := "planner", permissions := ["manage_issues"]
    , promptTemplate := "plan"
    , dispatch := some { trigger := .idle, max := 1 } }
  let resEmpty := dispatcherTick
    { activeByRole := {}, issues := #[], caps := [("planner", 1)], roles := #[role] }
  let resWithOpen := dispatcherTick
    { activeByRole := {}, issues := #[fixtureIssue 101 project .open]
    , caps := [("planner", 1)], roles := #[role] }
  TestM.assertEqual resEmpty.size 1     (msg := "idle role spawns when no work")
  TestM.assertEqual resWithOpen.size 0  (msg := "idle role suppressed when open issues exist")

@[test]
def dispatcherEmitsAtMostOnePerRolePerTick : Test := do
  let project := fixtureProject
  let issues := #[fixtureIssue 101 project .open, fixtureIssue 102 project .open, fixtureIssue 103 project .open]
  let role : Role :=
    { name := "implementor", permissions := []
    , promptTemplate := "x"
    , dispatch := some { trigger := .hasOpenIssues, max := 10 } }
  let count := (dispatcherTick
    { activeByRole := {}, issues, caps := [("implementor", 10)], roles := #[role] }).size
  TestM.assertEqual count 1 (msg := "≤1 spawn per role per tick (gradual ramp)")


/-- The comment thread reaches an agent only through the prompt for worker roles: reading it needs
    a tool call the agent has to know to make, and a rejection lives nowhere else now that there
    is no rejected status. -/
@[test]
def renderSubstitutesIssueComments : Test := do
  let v : RenderVars :=
    { projectId := "9", projectName := "P", instructions := ""
    , issueId := some "57", issueTitle := some "t"
    , issueComments := some "  reviewer at 2026-01-01 [review: requestChanges]\n    fix the proof" }
  let out := render "thread:\n{{issue_comments}}" v
  TestM.assert ((out.splitOn "fix the proof").length > 1) "comment body substituted"
  -- Absent thread renders empty rather than leaving the placeholder visible.
  let noComments : RenderVars := { projectId := "1", projectName := "P", instructions := "" }
  TestM.assertEqual (render "[{{issue_comments}}]" noComments) "[]"
    (msg := "no thread renders empty")

@[test]
def shippedWorkerTemplatesCarryTheThread : Test := do
  for name in ["implementor", "reviewer"] do
    let raw ← IO.FS.readFile s!"examples/projects/roles/{name}.json"
    match Json.parse raw >>= FromJson.fromJson? (α := Role) with
    | .error e => TestM.fail s!"{name}.json: {e}"
    | .ok role =>
      TestM.assert ((role.promptTemplate.splitOn "{{issue_comments}}").length > 1)
        s!"the shipped {name} template must carry the issue comment thread"

/-! ## When an issue can be worked on

    `workableIssues` is the whole rule for the project-dispatcher: open, no open children, no open
    dependencies. It is applied over every issue in the project, because deciding it from a set
    already narrowed to open issues cannot tell "done" from "absent" — which is exactly how issues
    with dependencies came to be blocked forever. -/

private def workableIds (issues : Array Issue) : List String :=
  ((workableIssues issues).map (·.id.toString)).toList

@[test]
def openDependencyBlocksWork : Test := do
  let p := fixtureProject
  let issues := #[fixtureIssue 101 p .open, fixtureIssue 102 p .open (dependencies := #[⟨101⟩])]
  TestM.assertEqual (workableIds issues) ["101"]
    (msg := "102 waits on an open 101")

@[test]
def closedDependencyReleasesWork : Test := do
  let p := fixtureProject
  let done := #[fixtureIssue 101 p .completed, fixtureIssue 102 p .open (dependencies := #[⟨101⟩])]
  TestM.assertEqual (workableIds done) ["102"] (msg := "a completed dependency releases")
  -- Abandoning is a decision the work will not happen, so it must not strand what follows.
  let dropped := #[fixtureIssue 101 p .abandoned, fixtureIssue 102 p .open (dependencies := #[⟨101⟩])]
  TestM.assertEqual (workableIds dropped) ["102"] (msg := "an abandoned dependency releases")

@[test]
def openChildBlocksWork : Test := do
  let p := fixtureProject
  let parent := fixtureIssue 101 p .open
  let child  := { fixtureIssue 102 p .open with parentId := some ⟨101⟩ }
  TestM.assertEqual (workableIds #[parent, child]) ["102"]
    (msg := "the parent is a container while its child is open")
  let doneChild := { fixtureIssue 102 p .completed with parentId := some ⟨101⟩ }
  TestM.assertEqual (workableIds #[parent, doneChild]) ["101"]
    (msg := "and becomes workable once the child closes")

/-- Both conditions at once, which is the rule as stated: workable means no open children *and*
    no open dependencies. -/
@[test]
def bothConditionsMustHold : Test := do
  let p := fixtureProject
  let blocker := fixtureIssue 100 p .open
  let parent  := fixtureIssue 101 p .open (dependencies := #[⟨100⟩])
  let child   := { fixtureIssue 102 p .open with parentId := some ⟨101⟩ }
  -- 101 has both an open child and an open dependency; only the leaf 102 and the leaf 100 qualify.
  TestM.assertEqual (workableIds #[blocker, parent, child]) ["100", "102"]
    (msg := "only issues clear on both counts")

/-- A dependency the caller cannot see does not block. Blocking on an unknown id would strand the
    dependent permanently, which is the worse failure. -/
@[test]
def invisibleDependencyDoesNotBlock : Test := do
  let p := fixtureProject
  TestM.assertEqual (workableIds #[fixtureIssue 102 p .open (dependencies := #[⟨999⟩])]) ["102"]
    (msg := "an id outside the set is treated as satisfied")

/-! ## Decision reporting

    `dispatcherDecisions` reports a verdict for every role the listener names, and `dispatcherTick`
    is just the spawns among them — so the log lines and the behaviour cannot drift apart. -/

@[test]
def everyNamedRoleGetsAVerdict : Test := do
  let p := fixtureProject
  let reviewer : Role :=
    { name := "reviewer", permissions := [], promptTemplate := "r"
    , dispatch := some { trigger := .hasInReviewIssues, max := 1 } }
  let ds := dispatcherDecisions
    { activeByRole := {}, issues := #[], reviewable := #[]
    , caps := [("reviewer", 1), ("ghost", 1), ("implementor", 0)], roles := #[reviewer] }
  TestM.assertEqual (ds.map (·.roleName)).toList ["reviewer", "ghost", "implementor"]
    (msg := "a verdict per named role, including ones that cannot run")
  TestM.assert (ds.any fun d => match d.outcome with | .nothingToReview => true | _ => false)
    "reviewer with nothing to review reports why"
  TestM.assert (ds.any fun d => match d.outcome with | .roleMissing => true | _ => false)
    "a cap naming a role with no file reports that"
  TestM.assert (ds.any fun d => match d.outcome with | .notEnabled => true | _ => false)
    "a zero cap reports that auto-dispatch is off"

/-- A reviewer is dispatched from `reviewable`, never from `issues`. This is the wiring that broke
    twice: a caller that builds the input without `reviewable` silently gets no reviewers, because
    the field defaults to empty and nothing else complains. -/
@[test]
def reviewerComesFromReviewableNotIssues : Test := do
  let p := fixtureProject
  let reviewer : Role :=
    { name := "reviewer", permissions := [], promptTemplate := "r"
    , dispatch := some { trigger := .hasInReviewIssues, max := 1 } }
  let withReviewable := dispatcherTick
    { activeByRole := {}, issues := #[fixtureIssue 101 p .open]
    , reviewable := #[fixtureIssue 102 p .open], caps := [("reviewer", 1)], roles := #[reviewer] }
  TestM.assertEqual (withReviewable.map (·.issueId.map (·.toString))).toList [some "102"]
    (msg := "bound to the reviewable issue")
  -- Omitting `reviewable` must yield nothing, not fall back to `issues`.
  let withoutIt := dispatcherTick
    { activeByRole := {}, issues := #[fixtureIssue 101 p .open]
    , caps := [("reviewer", 1)], roles := #[reviewer] }
  TestM.assert withoutIt.isEmpty "no reviewable set means no reviewer, never a worker issue"

end OrchestraTest.ProjectRole
