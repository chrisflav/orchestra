import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.ApiClient

namespace OrchestraTest.ApiClient

/-! What an API client may queue over HTTP (`Orchestra.ApiClient`): reading its file, reading a
    request, checking one against the other, and the entry that comes out. All of that is pure;
    the store's half — finding an entry by its `external_id`, and refusing a second one — runs
    against the test database. -/

private def token : String := "0123456789abcdef0123456789abcdef"

private def secrets : List (String × String) := [("review_token", token)]

private def parseJson (s : String) : Json :=
  (Json.parse s).toOption.getD Json.null

private def reviewerFile : String :=
  "{\"token\": \"{{review_token}}\", \"backend\": \"claude\", \"model\": \"sonnet\",
    \"read_only\": true, \"kleis_spec\": \"reviewer\", \"kleis_narrow\": [\"github: only read\"],
    \"queue_policy\": {\"repos\": [\"acme/widgets\"], \"models\": [\"opus\"],
                       \"max_budget\": 3.0, \"priority\": 20},
    \"max_active\": 2}"

private def clientOf (body : String) : Except String ApiClient :=
  parse "reviews" (parseJson body) secrets

private def reviewer : ApiClient :=
  match clientOf reviewerFile with
  | .ok c    => c
  | .error _ => { name := "?", token := "", defaults := {}, policy := {} }

private def requestOf (body : String) : Except String QueueRequest :=
  parseRequest (parseJson body)

/-- `msg` mentions `needle`. -/
private def mentions (msg needle : String) : Bool :=
  (msg.splitOn needle).length > 1

/-! ## The client file -/

@[test]
def aClientFileIsReadWithItsTokenResolved : Test := do
  match clientOf reviewerFile with
  | .error e => TestM.fail s!"the reviewer client should parse: {e}"
  | .ok c =>
    TestM.assertEqual c.token token (msg := "the token comes from secrets.json")
    TestM.assertEqual c.defaults.model (some "sonnet")
    TestM.assertEqual c.defaults.kleisSpec (some "reviewer")
    TestM.assert c.defaults.readOnly "read_only is the client's"
    TestM.assertEqual c.policy.priority 20
    TestM.assertEqual c.maxActive 2

@[test]
def aLiteralTokenIsRefused : Test := do
  match clientOf ("{\"token\": \"" ++ token ++ "\"}") with
  | .ok _ => TestM.fail "a token written into the client file should be refused"
  | .error e => TestM.assert (mentions e "reference to a secret") e

@[test]
def aTokenNamingAMissingSecretIsRefused : Test := do
  match clientOf "{\"token\": \"{{nope}}\"}" with
  | .ok _ => TestM.fail "a reference to a secret that is not defined should be refused"
  | .error e => TestM.assert (mentions e "'nope'") e

@[test]
def aShortTokenIsRefused : Test := do
  match parse "reviews" (parseJson "{\"token\": \"{{t}}\"}") [("t", "short")] with
  | .ok _ => TestM.fail "a token of five characters should be refused"
  | .error e => TestM.assert (mentions e "32 characters") e

@[test]
def anUnknownFieldInAClientFileIsRefused : Test := do
  match clientOf "{\"token\": \"{{review_token}}\", \"queuePolicy\": {}}" with
  | .ok _ => TestM.fail "a misspelt 'queue_policy' should be refused, not read as absent"
  | .error e => TestM.assert (mentions e "queuePolicy") e

@[test]
def maxTasksIsRefusedInAClientPolicy : Test := do
  match clientOf "{\"token\": \"{{review_token}}\", \"queue_policy\": {\"max_tasks\": 5}}" with
  | .ok _ => TestM.fail "max_tasks means nothing for a client and should be refused"
  | .error e => TestM.assert (mentions e "max_active") e

@[test]
def allowPreClaimIsRefusedInAClientPolicy : Test := do
  match clientOf
      "{\"token\": \"{{review_token}}\", \"queue_policy\": {\"allow_pre_claim\": true}}" with
  | .ok _ => TestM.fail "a client cannot bind an issue, so allow_pre_claim should be refused"
  | .error e => TestM.assert (mentions e "allow_pre_claim") e

@[test]
def anUnknownToolIsRefused : Test := do
  match clientOf "{\"token\": \"{{review_token}}\", \"tools\": [\"rm_rf\"]}" with
  | .ok _ => TestM.fail "a tool no role could hold should be refused"
  | .error e => TestM.assert (mentions e "rm_rf") e

/-! ## The request -/

@[test]
def aRequestNeedsAPrompt : Test := do
  match requestOf "{\"upstream\": \"acme/widgets\"}" with
  | .ok _ => TestM.fail "a request with no prompt should be refused"
  | .error e => TestM.assert (mentions e "prompt") e

@[test]
def anUnknownRequestFieldIsRefused : Test := do
  match requestOf "{\"prompt\": \"review\", \"priority\": 100}" with
  | .ok _ => TestM.fail "a priority the client cannot set should be refused, not ignored"
  | .error e => TestM.assert (mentions e "priority") e

@[test]
def anIssueNumberNeedsAnUpstream : Test := do
  match requestOf "{\"prompt\": \"review\", \"issue_number\": 7}" with
  | .ok _ => TestM.fail "an issue number without a repository names nothing"
  | .error e => TestM.assert (mentions e "upstream") e

@[test]
def anUpstreamThatIsAPathIsRefused : Test := do
  match requestOf "{\"prompt\": \"review\", \"upstream\": \"../widgets\"}" with
  | .ok _ => TestM.fail "'..' becomes a clone path component and should be refused"
  | .error _ => TestM.assert true

/-! ## The policy -/

@[test]
def aRequestInheritsTheClientsDefaults : Test := do
  match requestOf "{\"prompt\": \"review #7\", \"upstream\": \"acme/widgets\",
                    \"issue_number\": 7, \"external_id\": \"job-1\"}" with
  | .error e => TestM.fail s!"the request should parse: {e}"
  | .ok r =>
    match reviewer.resolve r with
    | .error e => TestM.fail s!"the request should be allowed: {e}"
    | .ok res =>
      TestM.assertEqual res.model (some "sonnet") (msg := "model inherited")
      TestM.assert (res.budget == some 3.0) "max_budget is the default"
      TestM.assertEqual res.priority 20 (msg := "priority is the policy's")
      TestM.assert res.readOnly "read_only inherited"
      TestM.assertEqual res.kleisSpec (some "reviewer") (msg := "the client's kleis source")

@[test]
def aRepositoryOutsideThePolicyIsRefused : Test := do
  match requestOf "{\"prompt\": \"review\", \"upstream\": \"evil/repo\"}" with
  | .error e => TestM.fail s!"the request should parse: {e}"
  | .ok r =>
    match reviewer.resolve r with
    | .ok _ => TestM.fail "evil/repo is not in the client's policy"
    | .error e => TestM.assert (mentions e "client 'reviews'") e

@[test]
def aBudgetOverTheCeilingIsRefused : Test := do
  match requestOf "{\"prompt\": \"review\", \"upstream\": \"acme/widgets\", \"budget\": 50}" with
  | .error e => TestM.fail s!"the request should parse: {e}"
  | .ok r =>
    match reviewer.resolve r with
    | .ok _ => TestM.fail "50 USD is over the client's 3 USD ceiling"
    | .error e => TestM.assert (mentions e "3") e

@[test]
def aRequestMayNarrowButNotRenameItsKleisSource : Test := do
  match requestOf "{\"prompt\": \"review\", \"kleis_narrow\": [\"github: none\"]}" with
  | .error e => TestM.fail s!"the request should parse: {e}"
  | .ok r =>
    match reviewer.resolve r with
    | .error e => TestM.fail s!"narrowing should be allowed: {e}"
    | .ok res =>
      TestM.assertEqual res.kleisNarrow ["github: only read", "github: none"]
        (msg := "the client's narrowings, then the request's")
  match requestOf "{\"prompt\": \"review\", \"kleis_spec\": \"worker\"}" with
  | .ok _ => TestM.fail "a request naming a kleis spec should be refused"
  | .error e => TestM.assert (mentions e "kleis_spec") e

/-! ## The entry -/

private def resolvedEntry : Except String Queue.QueueEntry := do
  let r ← requestOf "{\"prompt\": \"review #7\", \"upstream\": \"acme/widgets\",
                      \"issue_number\": 7, \"external_id\": \"job-1\"}"
  let res ← reviewer.resolve r
  return reviewer.entry r res none "e1" "2026-10-24T00:00:00Z"

@[test]
def aReadOnlyEntryWorksInTheUpstream : Test := do
  match resolvedEntry with
  | .error e => TestM.fail e
  | .ok e =>
    TestM.assertEqual (e.repo.map (·.fork.toString)) (some "acme/widgets")
      (msg := "no fork for a read-only task")
    TestM.assertEqual e.apiClient (some "reviews")
    TestM.assertEqual e.externalId (some "job-1")
    TestM.assertEqual e.issueNumber (some 7)
    TestM.assertEqual e.tools (some []) (msg := "an explicit empty list, never a fallback")
    TestM.assert e.spawnPolicy.isNone "what a client queues cannot queue more"

@[test]
def anEntryRoundTripsItsClientFields : Test := do
  match resolvedEntry with
  | .error e => TestM.fail e
  | .ok e =>
    match (FromJson.fromJson? (ToJson.toJson e) : Except String Queue.QueueEntry) with
    | .error err => TestM.fail s!"the entry should read back: {err}"
    | .ok back =>
      TestM.assertEqual back.apiClient e.apiClient
      TestM.assertEqual back.externalId e.externalId

/-! ## The routes a client reaches -/

@[test]
def aClientReachesItsRoutesAndNothingElse : Test := do
  TestM.assertEqual (Dashboard.clientRoute? Std.Http.Method.post "/api/v1/queue") (some none)
  TestM.assertEqual (Dashboard.clientRoute? Std.Http.Method.get "/api/v1/queue/e1") (some (some "e1"))
  TestM.assertEqual (Dashboard.clientRoute? Std.Http.Method.get "/sse/v1/queue/e1") (some (some "e1"))
  TestM.assertEqual (Dashboard.clientRoute? Std.Http.Method.post "/api/v1/queue/e1/cancel") (some (some "e1"))
  TestM.assertEqual (Dashboard.clientRoute? Std.Http.Method.get "/api/v1/queue") none
    (msg := "the whole queue holds other clients' prompts")
  TestM.assertEqual (Dashboard.clientRoute? Std.Http.Method.get "/api/v1/tasks/e1") none
  TestM.assertEqual (Dashboard.clientRoute? Std.Http.Method.put "/api/v1/listeners/x") none
  TestM.assertEqual (Dashboard.clientRoute? Std.Http.Method.post "/api/v1/interactive") none
  TestM.assertEqual (Dashboard.clientRoute? Std.Http.Method.post "/sse/v1/queue") none
  TestM.assertEqual (Dashboard.clientRoute? Std.Http.Method.get "/api/v1/queue/../tasks") none

/-! ## The store -/

@[test]
def anExternalIdIsUniquePerClient : Test := do
  let outcome ← Orchestra.withTempData "api-client" do
    let base : Queue.QueueEntry :=
      { id := "a", createdAt := "2026-10-24T00:00:00Z", repo := none, prompt := "p"
      , apiClient := some "reviews", externalId := some "job-1" }
    Queue.saveEntry base
    -- Another client may use the same id for its own work.
    Queue.saveEntry { base with id := "b", apiClient := some "other" }
    -- And entries with none never collide.
    Queue.saveEntry { base with id := "c", apiClient := none, externalId := none }
    Queue.saveEntry { base with id := "d", apiClient := none, externalId := none }
    let second ← try Queue.saveEntry { base with id := "e" }; pure true
      catch _ => pure false
    let found ← Queue.findByExternalId "reviews" "job-1"
    let active ← Queue.countActiveOfClient "reviews"
    return (second, found.map (·.id), active)
  let (second, found, active) := outcome
  TestM.assert (!second) "a second entry under the same client and external_id must be refused"
  TestM.assertEqual found (some "a")
  TestM.assertEqual active 1

end OrchestraTest.ApiClient