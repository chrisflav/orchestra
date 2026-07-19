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
  let issues := #[fixtureIssue 101 project .open, fixtureIssue 102 project .inReview]
  let role : Role :=
    { name := "reviewer", permissions := ["review_issues"]
    , promptTemplate := "review"
    , dispatch := some { trigger := .hasInReviewIssues, max := 1, preClaim := false } }
  let result := dispatcherTick
    { activeByRole := {}, issues, caps := [("reviewer", 1)], roles := #[role] }
  TestM.assertEqual result.size 1 (msg := "exactly one reviewer spawn")
  match result[0]? with
  | some s =>
    TestM.assert s.issueId.isSome "reviewer spawn must bind to an issue"
    TestM.assertEqual (s.issueId.map (·.toString)) (some "102")
      (msg := "bound to the in-review issue, not the open one")
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
    , caps := [("reviewer", 1)], roles := #[role] }
  TestM.assert result.isEmpty "nothing in review, so no reviewer"

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

end OrchestraTest.ProjectRole
