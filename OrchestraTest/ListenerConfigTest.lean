import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Listener

@[test]
def repoEntryRoundTrip : Test := do
  let entry : RepoEntry := { upstream := { owner := "org", name := "repo" },
                             fork := { owner := "my-org", name := "fork" } }
  let entryJson := ToJson.toJson entry
  match FromJson.fromJson? entryJson (α := RepoEntry) with
  | .error e => TestM.fail s!"RepoEntry round-trip parse: {e}"
  | .ok got =>
    TestM.assertEqual got.upstream.toString "org/repo"
      (msg := "RepoEntry.upstream round-trip")
    TestM.assertEqual got.fork.toString "my-org/fork"
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
      TestM.assertEqual r.upstream.toString "org/repo" (msg := "repos[0].upstream")
      TestM.assertEqual r.fork.toString "my-org/fork" (msg := "repos[0].fork")
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
      TestM.assertEqual r.upstream.toString "org/repo" (msg := "upstream")
      TestM.assertEqual r.fork.toString "org/repo" (msg := "fork")
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
    [{ upstream := { owner := "pimotte-agents", name := "orchestra" }
       fork := { owner := "my-fork", name := "orchestra" } }]
    ["agent"] "@orchestra" ["dave"]
  match FromJson.fromJson? (ToJson.toJson sc) (α := SourceConfig) with
  | .error e => TestM.fail s!"SourceConfig round-trip: {e}"
  | .ok (.githubComments repos _labels trigger authorizedUsers) =>
    TestM.assertEqual repos.length 1 (msg := "repos length")
    TestM.assertEqual trigger "@orchestra" (msg := "trigger")
    TestM.assertEqual authorizedUsers ["dave"] (msg := "authorized_users")
    match repos with
    | [r] =>
      TestM.assertEqual r.upstream.toString "pimotte-agents/orchestra"
        (msg := "upstream")
      TestM.assertEqual r.fork.toString "my-fork/orchestra" (msg := "fork")
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

@[test]
def githubLabelCountBasic : Test := do
  let json := r#"
    {"type": "github-label-count",
     "repos": [{"upstream": "org/repo", "fork": "my-org/fork"}],
     "labels": ["needs-review"],
     "max": 5}
  "#
  match Json.parse json >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => TestM.fail s!"github-label-count parse: {e}"
  | .ok (.githubLabelCount repos labels max kind) =>
    TestM.assertEqual repos.length 1 (msg := "repos length")
    TestM.assertEqual labels ["needs-review"] (msg := "labels")
    TestM.assertEqual max 5 (msg := "max")
    TestM.assertEqual kind "issues" (msg := "kind default")
    match repos with
    | [r] =>
      TestM.assertEqual r.upstream.toString "org/repo" (msg := "upstream")
      TestM.assertEqual r.fork.toString "my-org/fork" (msg := "fork")
    | _ => TestM.fail "repos not a singleton"
  | .ok _ => TestM.fail "unexpected SourceConfig variant"

@[test]
def githubLabelCountKindPulls : Test := do
  let json := r#"
    {"type": "github-label-count",
     "repos": [{"upstream": "org/repo", "fork": "my-org/fork"}],
     "labels": ["ready-for-review"],
     "max": 3,
     "kind": "pulls"}
  "#
  match Json.parse json >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => TestM.fail s!"github-label-count pulls parse: {e}"
  | .ok (.githubLabelCount _repos _labels max kind) =>
    TestM.assertEqual max 3 (msg := "max")
    TestM.assertEqual kind "pulls" (msg := "kind")
  | .ok _ => TestM.fail "unexpected SourceConfig variant"

@[test]
def githubLabelCountRoundTrip : Test := do
  let sc : SourceConfig := .githubLabelCount
    [{ upstream := { owner := "org", name := "repo" }
       fork := { owner := "my-org", name := "repo" } }]
    ["bug", "help-wanted"] 10 "all"
  match FromJson.fromJson? (ToJson.toJson sc) (α := SourceConfig) with
  | .error e => TestM.fail s!"github-label-count round-trip: {e}"
  | .ok (.githubLabelCount repos labels max kind) =>
    TestM.assertEqual repos.length 1 (msg := "repos length")
    TestM.assertEqual labels ["bug", "help-wanted"] (msg := "labels")
    TestM.assertEqual max 10 (msg := "max")
    TestM.assertEqual kind "all" (msg := "kind")
  | .ok _ => TestM.fail "wrong variant"

@[test]
def githubLabelsBasic : Test := do
  let json := r#"
    {"type": "github-labels",
     "repos": [{"upstream": "org/repo", "fork": "my-org/fork"}],
     "labels": ["needs-review", "help-wanted"],
     "authorized_users": ["alice"]}
  "#
  match Json.parse json >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => TestM.fail s!"github-labels parse: {e}"
  | .ok (.githubLabels repos labels kind authorizedUsers) =>
    TestM.assertEqual repos.length 1 (msg := "repos length")
    TestM.assertEqual labels ["needs-review", "help-wanted"] (msg := "labels")
    TestM.assertEqual kind "all" (msg := "kind default")
    TestM.assertEqual authorizedUsers ["alice"] (msg := "authorized_users")
    match repos with
    | [r] =>
      TestM.assertEqual r.upstream.toString "org/repo" (msg := "upstream")
      TestM.assertEqual r.fork.toString "my-org/fork" (msg := "fork")
    | _ => TestM.fail "repos not a singleton"
  | .ok _ => TestM.fail "unexpected SourceConfig variant"

@[test]
def githubLabelsKindIssues : Test := do
  let json := r#"
    {"type": "github-labels",
     "repos": [{"upstream": "org/repo", "fork": "my-org/fork"}],
     "labels": ["bug"],
     "kind": "issues"}
  "#
  match Json.parse json >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => TestM.fail s!"github-labels kind=issues parse: {e}"
  | .ok (.githubLabels _repos _labels kind _authorizedUsers) =>
    TestM.assertEqual kind "issues" (msg := "kind")
  | .ok _ => TestM.fail "unexpected SourceConfig variant"

@[test]
def githubLabelsRoundTrip : Test := do
  let sc : SourceConfig := .githubLabels
    [{ upstream := { owner := "org", name := "repo" }
       fork := { owner := "my-org", name := "repo" } }]
    ["triage", "needs-work"] "pulls" ["carol", "dave"]
  match FromJson.fromJson? (ToJson.toJson sc) (α := SourceConfig) with
  | .error e => TestM.fail s!"github-labels round-trip: {e}"
  | .ok (.githubLabels repos labels kind authorizedUsers) =>
    TestM.assertEqual repos.length 1 (msg := "repos length")
    TestM.assertEqual labels ["triage", "needs-work"] (msg := "labels")
    TestM.assertEqual kind "pulls" (msg := "kind")
    TestM.assertEqual authorizedUsers ["carol", "dave"] (msg := "authorized_users")
  | .ok _ => TestM.fail "wrong variant"
