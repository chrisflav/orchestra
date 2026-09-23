import OrchestraTest.TestM
import Orchestra.GitHub

open Lean (Json FromJson)
open Orchestra

namespace OrchestraTest.ForkResolve

/-!
# Fork / upstream resolution for project-based tasks

Project/role-based tasks name a target repository; the agent works on a `fork` it can push to.
When the GitHub App can already push to the target the fork is the target itself, otherwise the
target is forked into `default_organization`. The decision hinges on reading the App's `contents`
permission out of a `GET /repos/{owner}/{repo}/installation` response, on the branch table that
turns that answer into a repository to push to, and on parsing the config option that names the org
to fork into. All three are covered here — the branch table through `resolveForkWith`, which takes
the probe and the fork step as arguments precisely so it can be driven by stubs. What is not
covered is the code that actually talks to GitHub.

`existingForkDecision` is covered here too: it is the rule that decides whether the repository
already sitting at the fork's path is the fork, which is what lets a fork that exists be reused
without the source-side installation the fork endpoint would demand.
-/

-- Orchestra.GitHub.installationWriteDecision

/-- An installation holding `contents: write` on the repo can push. -/
@[test]
def access_contentsWriteIsWritable : Test := do
  let body := "{\"id\":1,\"permissions\":{\"contents\":\"write\",\"metadata\":\"read\"}}"
  TestM.assertEqual (GitHub.installationWriteDecision 200 body) (some true)
    (msg := "contents:write ⇒ writable")

/-- `contents: read`, or a `permissions` object that omits `contents` altogether (GitHub leaves
    ungranted permissions out), is a definitive "cannot push". -/
@[test]
def access_contentsNotWriteIsNotWritable : Test := do
  TestM.assertEqual
    (GitHub.installationWriteDecision 200 "{\"permissions\":{\"contents\":\"read\"}}") (some false)
    (msg := "contents:read ⇒ not writable")
  TestM.assertEqual
    (GitHub.installationWriteDecision 200 "{\"permissions\":{\"metadata\":\"read\"}}") (some false)
    (msg := "no contents permission ⇒ not writable")

/-- The repository's *own* `permissions` block — a user's role, all-false under an installation
    token even where the App holds `contents: write` — must not be what settles this. A body
    shaped like that response is a body with no installation `permissions` in it: inconclusive,
    never "can push". -/
@[test]
def access_repoRolePermissionsAreNotTheSignal : Test := do
  let body := "{\"full_name\":\"org/repo\",\
    \"permissions\":{\"admin\":false,\"push\":false,\"pull\":false}}"
  TestM.assertEqual (GitHub.installationWriteDecision 200 body) (some false)
    (msg := "a role block has no contents key ⇒ not writable, not silently writable")

/-- A 404 is no installation of the App reaching the repo — a definitive "cannot push", not an
    inconclusive result, so the caller forks rather than retrying forever. -/
@[test]
def access_notFoundIsNotWritable : Test := do
  TestM.assertEqual (GitHub.installationWriteDecision 404 "{\"message\":\"Not Found\"}")
    (some false) (msg := "404 ⇒ cannot push")

/-- A 403 is *not* a definitive "cannot push": a repo the App cannot see answers 404, so a 403 is
    a rate limit or a blocked App. Reading it as "cannot push" would fork over a rate limit. -/
@[test]
def access_forbiddenIsInconclusive : Test := do
  TestM.assertEqual
    (GitHub.installationWriteDecision 403 "{\"message\":\"API rate limit exceeded\"}") none
    (msg := "403 rate limit ⇒ inconclusive")
  TestM.assertEqual (GitHub.installationWriteDecision 403 "{\"message\":\"Forbidden\"}") none
    (msg := "403 ⇒ inconclusive")

/-- A suspended installation still reports its permissions but cannot use them; that needs a human,
    not a fork, so it is inconclusive. -/
@[test]
def access_suspendedIsInconclusive : Test := do
  let body := "{\"suspended_at\":\"2026-01-01T00:00:00Z\",\"permissions\":{\"contents\":\"write\"}}"
  TestM.assertEqual (GitHub.installationWriteDecision 200 body) none
    (msg := "suspended installation ⇒ inconclusive")
  let live := "{\"suspended_at\":null,\"permissions\":{\"contents\":\"write\"}}"
  TestM.assertEqual (GitHub.installationWriteDecision 200 live) (some true)
    (msg := "suspended_at:null ⇒ not suspended")

/-- A 5xx, an unparseable body, or a 2xx without a `permissions` object is inconclusive (`none`):
    the answer is unknown and the caller should retry rather than assume either way. -/
@[test]
def access_inconclusiveCases : Test := do
  TestM.assertEqual (GitHub.installationWriteDecision 500 "oops") none
    (msg := "5xx ⇒ inconclusive")
  TestM.assertEqual (GitHub.installationWriteDecision 200 "<html>not json</html>") none
    (msg := "unparseable 2xx ⇒ inconclusive")
  TestM.assertEqual (GitHub.installationWriteDecision 200 "{\"id\":1}") none
    (msg := "2xx without permissions ⇒ inconclusive")

-- Orchestra.GitHub.resolveForkWith

private def target : Repository := { owner := "upstream-org", name := "widget" }

/-- `Repository` derives `BEq` but not `DecidableEq`, so `TestM.assertEqual` does not apply to it;
    this renders the two sides itself. -/
private def showRepo : Option Repository → String
  | some r => r.toString
  | none   => "«no repository»"

private def assertRepo (actual expected : Option Repository) (msg : String) : TestM Unit :=
  TestM.assert (actual == expected)
    s!"{msg}: expected {showRepo expected}, got {showRepo actual}"

/-- A `mkFork` that records nothing and answers `org/{target.name}`, for the branches where the
    fork is expected to be created. -/
private def stubFork (t : Repository) (org : String) : IO Repository :=
  return { owner := org, name := t.name }

/-- A `mkFork` that must never be reached; reaching it fails the test by throwing. -/
private def noFork (t : Repository) (org : String) : IO Repository :=
  throw (.userError s!"mkFork should not have been called ({t} into '{org}')")

/-- A writable target is worked on directly: the fork is the target, and nothing is created. -/
@[test]
def resolve_writableTargetIsUsedDirectly : Test := do
  let r ← GitHub.resolveForkWith (fun _ => pure (some true)) noFork (some "my-org") target
  assertRepo r (some target) "writable ⇒ the target itself"

/-- An unwritable target with an org configured is forked into it. -/
@[test]
def resolve_unwritableTargetIsForked : Test := do
  let r ← GitHub.resolveForkWith (fun _ => pure (some false)) stubFork (some "my-org") target
  assertRepo r (some { owner := "my-org", name := "widget" })
    "unwritable + org ⇒ fork in that org"

/-- An unwritable target with no org configured has nowhere to go, so the task is skipped. -/
@[test]
def resolve_unwritableWithoutOrgIsSkipped : Test := do
  let r ← GitHub.resolveForkWith (fun _ => pure (some false)) noFork none target
  assertRepo r none "unwritable + no org ⇒ skip"

/-- An inconclusive probe skips rather than guessing, and in particular does not fork — that is
    what keeps a rate-limited or unreachable GitHub from creating repositories. -/
@[test]
def resolve_inconclusiveProbeIsSkipped : Test := do
  let r ← GitHub.resolveForkWith (fun _ => pure none) noFork (some "my-org") target
  assertRepo r none "inconclusive ⇒ skip without forking"

/-- A fork that fails is a skip, not an exception: callers have one "cannot dispatch" branch. -/
@[test]
def resolve_failedForkIsSkippedNotThrown : Test := do
  let failing := fun (_ : Repository) (_ : String) =>
    (throw (.userError "boom") : IO Repository)
  let r ← GitHub.resolveForkWith (fun _ => pure (some false)) failing (some "my-org") target
  assertRepo r none "mkFork throws ⇒ skip"

-- Orchestra.GitHub.existingForkDecision

/-- The lookup is a `GET` on `org`'s own side, so these bodies are what that request answers with;
    `org` is the organisation the caller asked about. -/
private def org : String := "my-org"

/-- The steady state: `org` holds a repository whose `parent` is the target, so it is the fork and
    no fork has to be created — the case that works with the App installed on `org` alone. -/
@[test]
def existing_parentMatchingTargetIsTheFork : Test := do
  let body := "{\"full_name\":\"my-org/widget\",\"fork\":true,\
    \"parent\":{\"full_name\":\"upstream-org/widget\"}}"
  assertRepo (GitHub.existingForkDecision 200 body org target)
    (some { owner := "my-org", name := "widget" }) "parent matches ⇒ that repository is the fork"

/-- A body naming a repository other than the one the `GET` asked for is GitHub answering about
    something else, and is refused however good its `parent` looks. Unlike the POST — where GitHub
    may legitimately rename the fork it creates and the name must be read back — a 2xx here
    describes the path that was requested, so a different name can only mean a different
    repository, and dispatching an agent at it is the failure this whole function exists to
    prevent. -/
@[test]
def existing_answerAboutAnotherRepositoryIsRefused : Test := do
  let renamed := "{\"full_name\":\"my-org/widget-1\",\
    \"parent\":{\"full_name\":\"upstream-org/widget\"}}"
  assertRepo (GitHub.existingForkDecision 200 renamed org target) none
    "a different name under org ⇒ not the repository that was asked for"
  let elsewhere := "{\"full_name\":\"other-org/widget\",\
    \"parent\":{\"full_name\":\"upstream-org/widget\"}}"
  assertRepo (GitHub.existingForkDecision 200 elsewhere org target) none
    "a fork in another organisation ⇒ refused"

/-- A repository sitting at the fork's path whose parent is something else is somebody else's
    repository that shares a name. Using it would push a branch and open a pull request at a
    repository nobody nominated, so it is not a fork answer. -/
@[test]
def existing_differentParentIsNotTheFork : Test := do
  let body := "{\"full_name\":\"my-org/widget\",\
    \"parent\":{\"full_name\":\"someone-else/widget\"}}"
  assertRepo (GitHub.existingForkDecision 200 body org target) none
    "a fork of a different upstream ⇒ not our fork"

/-- A repository that is not a fork at all carries no `parent`, and a name collision is exactly
    what that looks like. A `parent` explicitly `null` is the same answer. -/
@[test]
def existing_noParentIsNotTheFork : Test := do
  assertRepo (GitHub.existingForkDecision 200 "{\"full_name\":\"my-org/widget\"}" org target) none
    "no parent ⇒ not a fork"
  assertRepo (GitHub.existingForkDecision 200
    "{\"full_name\":\"my-org/widget\",\"parent\":null}" org target) none
    "parent:null ⇒ not a fork"

/-- Owner and repository names are compared case-insensitively, because GitHub's are and both the
    target and the organisation are written by hand — in a config file or a taxis `repository`
    artifact. -/
@[test]
def existing_comparisonIsCaseInsensitive : Test := do
  let body := "{\"full_name\":\"My-Org/Widget\",\
    \"parent\":{\"full_name\":\"Upstream-Org/Widget\"}}"
  assertRepo (GitHub.existingForkDecision 200 body org target)
    (some { owner := "My-Org", name := "Widget" })
    "case differences still match, and GitHub's spelling is what is returned"

/-- Nothing at the path (404), a repository that has since been renamed (301, which
    `curlWithStatus` does not follow), and anything else that does not describe a repository all
    send the caller on to the fork endpoint rather than answering. -/
@[test]
def existing_absentOrUnreadableIsNoAnswer : Test := do
  assertRepo (GitHub.existingForkDecision 404 "{\"message\":\"Not Found\"}" org target) none
    "404 ⇒ nothing to reuse"
  assertRepo (GitHub.existingForkDecision 301 "{\"message\":\"Moved Permanently\"}" org target) none
    "301 ⇒ no answer; the redirect is not followed"
  assertRepo (GitHub.existingForkDecision 200 "<html>not json</html>" org target) none
    "unparseable 2xx ⇒ no answer"
  assertRepo (GitHub.existingForkDecision 500 "oops" org target) none
    "5xx ⇒ no answer"
  assertRepo (GitHub.existingForkDecision 200
    "{\"parent\":{\"full_name\":\"upstream-org/widget\"}}" org target) none
    "no full_name ⇒ the fork cannot be identified"

-- Orchestra.AppConfig default_organization parsing

private def parseConfig (s : String) : Except String AppConfig := do
  match Json.parse s with
  | .error e => .error e
  | .ok j    => FromJson.fromJson? j

@[test]
def config_defaultOrganizationParsed : Test := do
  let cfg := parseConfig "{\"github_app\":{\"app_id\":1,\"private_key_path\":\"/k\"},\
    \"default_organization\":\"my-org\"}"
  match cfg with
  | .ok c  => TestM.assertEqual c.defaultOrganization (some "my-org")
                (msg := "default_organization is read")
  | .error e => TestM.fail s!"config failed to parse: {e}"

@[test]
def config_defaultOrganizationDefaultsToNone : Test := do
  let cfg := parseConfig "{\"github_app\":{\"app_id\":1,\"private_key_path\":\"/k\"}}"
  match cfg with
  | .ok c  => TestM.assertEqual c.defaultOrganization none
                (msg := "absent default_organization ⇒ none")
  | .error e => TestM.fail s!"config failed to parse: {e}"

end OrchestraTest.ForkResolve
