import OrchestraTest.TestM
import Orchestra.Server
import Orchestra.Workflow
import Orchestra.Project.Role

open Lean (Json)
open Orchestra
open Orchestra.Server

namespace OrchestraTest.CreateRepository

/-!
# The `create_repository` tool

Creating a repository is the other tool that cannot be taken back, and it is the only one whose
destination the agent does not name: it always writes to `default_organization`, the organisation
tasks are forked into. So what is worth pinning down here is the refusals — a task that was not
granted the tool, and a daemon with no organisation configured — and the argument handling around
them, since a defaulted argument is what an agent gets when it says nothing.

The creation itself needs a network and a GitHub App, so nothing below reaches `curl`: the
refusals return before any token is minted, and the rest is parsing plus `GitHub.repoNameError?`,
the name check that stops a name GitHub would have quietly renamed.
-/

/-! ## Permission gating -/

private def state (tools : List String) (org : Option String := some "fork-org")
    (repo : Option RepoPair := some { upstream := { owner := "up",   name := "repo" }
                                    , fork     := { owner := "fork", name := "repo" } })
    : State :=
  { repo
  , allowedTools := tools
  , appId := 0
  , privateKeyPath := ""
  , installationId := some 0
  , pat := "pat"
  , defaultOrganization := org }

/-- Extract the inner `text` payload from a tool-content JSON envelope. -/
private def textOf (j : Json) : String :=
  let arr? := j.getObjVal? "content" |>.toOption |>.bind (·.getArr? |>.toOption)
  let first? := arr?.bind (·[0]?)
  (first?.bind (·.getObjValAs? String "text" |>.toOption)).getD ""

private def isError (j : Json) : Bool :=
  j.getObjValAs? Bool "isError" |>.toOption |>.getD false

private def call : ToolCall := .createRepository "new-repo" "" true false

@[test]
def createRepository_deniedWithoutThePermission : Test := do
  let result ← evalToolCall (state []) call
  TestM.assert (isError result) "creating a repository the task was not granted is an error"
  TestM.assert ((textOf result).contains "not enabled for this task")
    "the refusal says the tool is not enabled, not that the creation failed"

@[test]
def createRepository_deniedWhenOnlyOtherToolsAreGranted : Test := do
  -- The neighbouring repository-writing tools. Neither carries the right to make a new
  -- repository: the tools a task holds are the ones it was given, not a category.
  let result ← evalToolCall (state ["create_pr", "merge_pr", "comment"]) call
  TestM.assert (isError result) "create_pr and merge_pr do not imply create_repository"

@[test]
def createRepository_survivesATaskWithNoRepository : Test := do
  -- The one repository-writing tool a repository-independent task keeps. It creates a repository
  -- in `default_organization` rather than acting on one the task named, so nothing about it needs
  -- an `upstream`/`fork` pair — which is why it is not in `repoScopedTools`. The refusal below is
  -- the destination check, not the "acts on a repository, and this task runs without one" one,
  -- and that is the whole point: it got past the gate the others do not.
  let result ← evalToolCall (state ["create_repository"] (org := none) (repo := none)) call
  let text := textOf result
  TestM.assert (!text.contains "runs without one")
    "create_repository is not refused for want of a repository the task never needed"
  TestM.assert (text.contains "default_organization")
    "it reaches the destination check like any other task's would"

@[test]
def createRepository_grantedButNoOrganisationIsReported : Test := do
  -- Past the gate, and the next thing checked is the destination. With no `default_organization`
  -- there is nowhere the tool is willing to write, and it must say so rather than fall back to
  -- some other owner.
  let result ← evalToolCall (state ["create_repository"] (org := none)) call
  TestM.assert (isError result) "a missing default_organization is an error"
  TestM.assert ((textOf result).contains "default_organization")
    "the refusal names the config field to set"

/-! ## Argument parsing -/

@[test]
def parseCreateRepository_defaults : Test := do
  match parseToolCall "create_repository" (Json.mkObj [("name", .str "widget")]) with
  | .createRepository name description isPrivate autoInit =>
    TestM.assertEqual name "widget" (msg := "name")
    TestM.assertEqual description "" (msg := "description defaults to empty")
    -- Both defaults are the recoverable answer: a private, empty repository can be published and
    -- filled afterwards, while publishing cannot be undone and an initial commit is a history a
    -- first push then has to be reconciled with.
    TestM.assertEqual isPrivate true (msg := "private defaults to true")
    TestM.assertEqual autoInit false (msg := "auto_init defaults to false")
  | _ => TestM.fail "expected .createRepository"

@[test]
def parseCreateRepository_explicitArguments : Test := do
  let args := Json.mkObj [
    ("name",        .str "widget"),
    ("description", .str "a widget"),
    ("private",     .bool false),
    ("auto_init",   .bool true)
  ]
  match parseToolCall "create_repository" args with
  | .createRepository name description isPrivate autoInit =>
    TestM.assertEqual name "widget" (msg := "name")
    TestM.assertEqual description "a widget" (msg := "description is honoured")
    TestM.assertEqual isPrivate false (msg := "private is honoured")
    TestM.assertEqual autoInit true (msg := "auto_init is honoured")
  | _ => TestM.fail "expected .createRepository"

@[test]
def parseCreateRepository_missingName : Test := do
  match parseToolCall "create_repository" (Json.mkObj []) with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for a missing name"

@[test]
def parseCreateRepository_nameIsTrimmed : Test := do
  -- Surrounding whitespace is a typo, not a different repository; GitHub would refuse it.
  match parseToolCall "create_repository" (Json.mkObj [("name", .str "  widget  ")]) with
  | .createRepository name _ _ _ => TestM.assertEqual name "widget" (msg := "name is trimmed")
  | _ => TestM.fail "expected .createRepository"

@[test]
def parseCreateRepository_rejectsAnOwnerQualifiedName : Test := do
  -- The most likely mistake, and the one worth catching here: an agent naming `org/repo` is
  -- asking for a destination this tool does not offer, and `/` is not a name GitHub accepts.
  match parseToolCall "create_repository" (Json.mkObj [("name", .str "other-org/widget")]) with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for an owner-qualified name"

@[test]
def parseCreateRepository_rejectsAnEmptyName : Test := do
  match parseToolCall "create_repository" (Json.mkObj [("name", .str "   ")]) with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for a blank name"

/-! ## Names GitHub will and will not take -/

@[test]
def repoNameError_acceptsTheUsualNames : Test := do
  TestM.assertEqual (GitHub.repoNameError? "orchestra") none (msg := "plain name")
  TestM.assertEqual (GitHub.repoNameError? "my-repo_2.0") none
    (msg := "hyphens, underscores and dots are accepted")

@[test]
def repoNameError_refusesWhatGitHubWouldRenameOrReject : Test := do
  -- A name with a space is the dangerous one: GitHub accepts it and hyphenates it, so the
  -- repository that comes back is not the one that was asked for.
  TestM.assert (GitHub.repoNameError? "my repo" |>.isSome) "a space is refused"
  TestM.assert (GitHub.repoNameError? "org/repo" |>.isSome) "a slash is refused"
  TestM.assert (GitHub.repoNameError? "" |>.isSome) "an empty name is refused"
  TestM.assert (GitHub.repoNameError? "." |>.isSome) "'.' is not a repository"
  TestM.assert (GitHub.repoNameError? ".." |>.isSome) "'..' is not a repository"
  TestM.assert (GitHub.repoNameError? (String.ofList (List.replicate 101 'a')) |>.isSome)
    "a name over 100 characters is refused"

/-! ## Where the tool is offered -/

/-- The names in `tools/list`. -/
private def offeredTools (st : State) : List String :=
  match (toolsList st).getObjVal? "tools" |>.toOption |>.bind (·.getArr? |>.toOption) with
  | none      => []
  | some defs => defs.toList.filterMap (·.getObjValAs? String "name" |>.toOption)

@[test]
def createRepository_isOfferedUnderTheNameItIsGatedOn : Test := do
  -- The name the tool is listed under and the name `evalToolCall` checks come from two lists
  -- written out separately. If they disagree the tool is either offered and always refused, or
  -- gated and never offered, and neither shows up until an agent tries to use it.
  TestM.assert ((offeredTools (state ["create_repository"])).contains "create_repository")
    "a task granted the tool is offered it"
  TestM.assert (!(offeredTools (state ["create_pr"])).contains "create_repository")
    "a task that was not granted it is not offered it"

@[test]
def createRepository_isAKnownToolName : Test := do
  -- The name the server gates on has to be one a task file, workflow step or `--tools` list can
  -- actually carry; a tool nobody can name is a tool nobody has.
  TestM.assert (TaskSpec.knownTools.contains "create_repository")
    "a workflow step may ask for create_repository"
  -- Roles are templates dispatched at whatever issue comes along, and creating repositories is
  -- not something to grant a whole class of tasks — the same reason `merge_pr` is absent there.
  TestM.assert (!Project.Role.knownPermissions.contains "create_repository")
    "a role does not grant create_repository"

end OrchestraTest.CreateRepository
