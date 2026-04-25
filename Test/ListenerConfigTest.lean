import Test.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Listener

@[test]
def repoEntryRoundTrip : Test := do
  let entry : RepoEntry := { upstream := "org/repo", fork := "my-org/fork" }
  let entryJson := ToJson.toJson entry
  match FromJson.fromJson? entryJson (α := RepoEntry) with
  | .error e => TestM.fail s!"RepoEntry round-trip parse: {e}"
  | .ok got =>
    TestM.assertEqual got.upstream "org/repo"
      (msg := "RepoEntry.upstream round-trip")
    TestM.assertEqual got.fork "my-org/fork"
      (msg := "RepoEntry.fork round-trip")

@[test]
def githubCommentsNewFormat : Test := do
  let newFormat := r#"
    {"type": "github-comments",
     "repos": [{"upstream": "org/repo", "fork": "my-org/fork"}],
     "trigger": "@bot",
     "authorized_users": ["alice", "bob"]}
  "#
  match Json.parse newFormat >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => TestM.fail s!"new repos format parse: {e}"
  | .ok (.githubComments repos _labels trigger authorizedUsers) =>
    TestM.assertEqual repos.length 1 (msg := "repos length")
    match repos with
    | [r] =>
      TestM.assertEqual r.upstream "org/repo" (msg := "repos[0].upstream")
      TestM.assertEqual r.fork "my-org/fork" (msg := "repos[0].fork")
    | _ => TestM.fail "repos not a singleton"
    TestM.assertEqual trigger "@bot" (msg := "trigger")
    TestM.assertEqual authorizedUsers ["alice", "bob"]
      (msg := "authorized_users")
  | .ok _ => TestM.fail "unexpected SourceConfig variant"

@[test]
def githubCommentsBackwardCompat : Test := do
  let oldFormat :=
    r#"{"type": "github-comments", "fork": "org/repo", "trigger": "@bot"}"#
  match Json.parse oldFormat >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => TestM.fail s!"backward compat parse: {e}"
  | .ok (.githubComments repos _labels _trigger authorizedUsers) =>
    TestM.assertEqual repos.length 1 (msg := "repos length")
    match repos with
    | [r] =>
      TestM.assertEqual r.upstream "org/repo" (msg := "upstream")
      TestM.assertEqual r.fork "org/repo" (msg := "fork")
    | _ => TestM.fail "repos not a singleton"
    TestM.assertEqual authorizedUsers ([] : List String)
      (msg := "authorized_users empty")
  | .ok _ => TestM.fail "unexpected SourceConfig variant"

@[test]
def githubIssuesNewFormat : Test := do
  let issuesFormat := r#"
    {"type": "github-issues",
     "repos": [{"upstream": "org/repo", "fork": "my-org/fork"},
               {"upstream": "org/other", "fork": "my-org/other"}],
     "labels": ["bug"],
     "authorized_users": ["carol"]}
  "#
  match Json.parse issuesFormat >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => TestM.fail s!"github-issues new format: {e}"
  | .ok (.githubIssues repos labels trigger authorizedUsers) =>
    TestM.assertEqual repos.length 2 (msg := "repos length")
    TestM.assertEqual labels ["bug"] (msg := "labels")
    TestM.assertEqual trigger "" (msg := "trigger default")
    TestM.assertEqual authorizedUsers ["carol"] (msg := "authorized_users")
  | .ok _ => TestM.fail "unexpected SourceConfig variant"

@[test]
def githubIssuesWithTrigger : Test := do
  let json := r#"
    {"type": "github-issues",
     "repos": [{"upstream": "org/repo", "fork": "my-org/fork"}],
     "trigger": "@bot",
     "authorized_users": []}
  "#
  match Json.parse json >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => TestM.fail s!"github-issues with trigger: {e}"
  | .ok (.githubIssues _repos _labels trigger _authorizedUsers) =>
    TestM.assertEqual trigger "@bot" (msg := "trigger")
  | .ok _ => TestM.fail "unexpected SourceConfig variant"

@[test]
def githubPrReviewsWithTrigger : Test := do
  let json := r#"
    {"type": "github-pr-reviews",
     "repos": [{"upstream": "org/repo", "fork": "my-org/fork"}],
     "trigger": "@orchestra",
     "authorized_users": []}
  "#
  match Json.parse json >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => TestM.fail s!"github-pr-reviews with trigger: {e}"
  | .ok (.githubPrReviews _repos _labels trigger _authorizedUsers) =>
    TestM.assertEqual trigger "@orchestra" (msg := "trigger")
  | .ok _ => TestM.fail "unexpected SourceConfig variant"

@[test]
def githubPrReviewsNewFormat : Test := do
  let json := r#"
    {"type": "github-pr-reviews",
     "repos": [{"upstream": "org/repo", "fork": "my-org/fork"}],
     "labels": []}
  "#
  match Json.parse json >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => TestM.fail s!"github-pr-reviews new format: {e}"
  | .ok (.githubPrReviews repos _labels trigger authorizedUsers) =>
    TestM.assertEqual repos.length 1 (msg := "repos length")
    TestM.assertEqual trigger "" (msg := "trigger default")
    TestM.assertEqual authorizedUsers ([] : List String)
      (msg := "authorized_users")
  | .ok _ => TestM.fail "unexpected SourceConfig variant"

@[test]
def sourceConfigRoundTrip : Test := do
  let sc : SourceConfig := .githubComments
    [{ upstream := "pimotte-agents/orchestra"
       fork := "my-fork/orchestra" }]
    ["agent"] "@orchestra" ["dave"]
  match FromJson.fromJson? (ToJson.toJson sc) (α := SourceConfig) with
  | .error e => TestM.fail s!"SourceConfig round-trip: {e}"
  | .ok (.githubComments repos _labels trigger authorizedUsers) =>
    TestM.assertEqual repos.length 1 (msg := "repos length")
    TestM.assertEqual trigger "@orchestra" (msg := "trigger")
    TestM.assertEqual authorizedUsers ["dave"] (msg := "authorized_users")
    match repos with
    | [r] =>
      TestM.assertEqual r.upstream "pimotte-agents/orchestra"
        (msg := "upstream")
      TestM.assertEqual r.fork "my-fork/orchestra" (msg := "fork")
    | _ => TestM.fail "repos not a singleton"
  | .ok _ => TestM.fail "wrong variant"

@[test]
def appConfigAuthorizedUsers : Test := do
  let json := r#"
    {"github_app": {"app_id": 42, "private_key_path": "/key"},
     "authorized_users": ["alice", "bob"]}
  "#
  match Json.parse json >>= FromJson.fromJson? (α := AppConfig) with
  | .error e => TestM.fail s!"AppConfig authorized_users parse: {e}"
  | .ok cfg =>
    TestM.assertEqual cfg.authorizedUsers ["alice", "bob"]
      (msg := "authorizedUsers")

@[test]
def appConfigDefaultAuthorizedUsers : Test := do
  let json := r#"
    {"github_app": {"app_id": 42, "private_key_path": "/key"}}
  "#
  match Json.parse json >>= FromJson.fromJson? (α := AppConfig) with
  | .error e => TestM.fail s!"AppConfig no authorized_users: {e}"
  | .ok cfg =>
    TestM.assertEqual cfg.authorizedUsers ([] : List String)
      (msg := "authorizedUsers default empty")

@[test]
def agentAuthConfigExtraPorts : Test := do
  let json := r#"
    {"name": "claude", "extra_ports": [8080, 9090]}
  "#
  match Json.parse json >>= FromJson.fromJson? (α := AgentAuthConfig) with
  | .error e => TestM.fail s!"AgentAuthConfig extra_ports parse: {e}"
  | .ok cfg =>
    TestM.assertEqual cfg.extraPorts #[8080, 9090] (msg := "extraPorts")

@[test]
def agentAuthConfigExtraPortsDefault : Test := do
  let json := r#"
    {"name": "claude"}
  "#
  match Json.parse json >>= FromJson.fromJson? (α := AgentAuthConfig) with
  | .error e => TestM.fail s!"AgentAuthConfig no extra_ports: {e}"
  | .ok cfg =>
    TestM.assertEqual cfg.extraPorts (#[] : Array Nat) (msg := "extraPorts default empty")
