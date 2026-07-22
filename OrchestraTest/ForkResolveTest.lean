import OrchestraTest.TestM
import Orchestra.GitHub

open Lean (Json FromJson)
open Orchestra

namespace OrchestraTest.ForkResolve

/-!
# Fork / upstream resolution for project-based tasks

Project/role-based tasks name a target repository; the agent works on a `fork` it can push to.
When the GitHub App can already push to the target the fork is the target itself, otherwise the
target is forked into `default_organization`. The decision hinges on reading the App's push
permission out of a `GET /repos/{owner}/{repo}` response, and on parsing the config option that
names the org to fork into. Both pure pieces are covered here; the network halves are not.
-/

-- Orchestra.GitHub.repoAccessDecision

/-- A 2xx whose `permissions.push` is true is a definitive "can push". -/
@[test]
def access_pushTrueIsWritable : Test := do
  let body := "{\"name\":\"repo\",\"permissions\":{\"admin\":false,\"push\":true,\"pull\":true}}"
  TestM.assertEqual (GitHub.repoAccessDecision 200 body) (some true)
    (msg := "push:true ⇒ writable")

/-- A 2xx whose `permissions.push` is false is a definitive "cannot push". -/
@[test]
def access_pushFalseIsNotWritable : Test := do
  let body := "{\"permissions\":{\"admin\":false,\"push\":false,\"pull\":true}}"
  TestM.assertEqual (GitHub.repoAccessDecision 200 body) (some false)
    (msg := "push:false ⇒ not writable")

/-- 404 and 403 are the App having no visibility of the repo — a definitive "cannot push", not an
    inconclusive result, so the caller forks rather than retrying forever. -/
@[test]
def access_notFoundAndForbiddenAreNotWritable : Test := do
  TestM.assertEqual (GitHub.repoAccessDecision 404 "{\"message\":\"Not Found\"}") (some false)
    (msg := "404 ⇒ cannot push")
  TestM.assertEqual (GitHub.repoAccessDecision 403 "{\"message\":\"Forbidden\"}") (some false)
    (msg := "403 ⇒ cannot push")

/-- A 5xx, an unparseable body, or a 2xx without a `permissions` field is inconclusive (`none`):
    the answer is unknown and the caller should retry rather than assume either way. -/
@[test]
def access_inconclusiveCases : Test := do
  TestM.assertEqual (GitHub.repoAccessDecision 500 "oops") none
    (msg := "5xx ⇒ inconclusive")
  TestM.assertEqual (GitHub.repoAccessDecision 200 "<html>not json</html>") none
    (msg := "unparseable 2xx ⇒ inconclusive")
  TestM.assertEqual (GitHub.repoAccessDecision 200 "{\"name\":\"repo\"}") none
    (msg := "2xx without permissions ⇒ inconclusive")

-- Orchestra.AppConfig default_organization parsing

private def parseConfig (s : String) : Except String AppConfig := do
  match Json.parse s with
  | .error e => .error e
  | .ok j    => FromJson.fromJson? j

@[test]
def config_defaultOrganizationParsed : Test := do
  let cfg := parseConfig
    "{\"github_app\":{\"app_id\":1,\"private_key_path\":\"/k\"},\"default_organization\":\"my-org\"}"
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
