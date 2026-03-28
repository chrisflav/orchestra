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
def listenerConfigSingleActionBackwardCompat : Test := do
  -- Old format with a single `action` field should still parse correctly.
  let json := r#"
    {"name": "my-listener",
     "source": {"type": "github-comments", "fork": "org/repo", "trigger": "@bot"},
     "action": {"mode": "fork", "prompt_template": "do something"}}
  "#
  match Json.parse json >>= FromJson.fromJson? (α := ListenerConfig) with
  | .error e => TestM.fail s!"single action backward compat: {e}"
  | .ok cfg =>
    TestM.assertEqual cfg.name "my-listener" (msg := "name")
    TestM.assertEqual cfg.actions.length 1 (msg := "actions length")
    match cfg.actions with
    | [a] => TestM.assertEqual a.promptTemplate "do something" (msg := "promptTemplate")
    | _   => TestM.fail "expected exactly one action"

@[test]
def listenerConfigActionsListNewFormat : Test := do
  -- New format with an `actions` list should parse all entries.
  let json := r#"
    {"name": "multi-action",
     "source": {"type": "github-comments", "fork": "org/repo", "trigger": "@bot"},
     "actions": [
       {"mode": "fork", "prompt_template": "implement {{body}}"},
       {"mode": "fork", "prompt_template": "review the changes", "series": "review-{{issue_number}}"}
     ]}
  "#
  match Json.parse json >>= FromJson.fromJson? (α := ListenerConfig) with
  | .error e => TestM.fail s!"actions list new format: {e}"
  | .ok cfg =>
    TestM.assertEqual cfg.name "multi-action" (msg := "name")
    TestM.assertEqual cfg.actions.length 2 (msg := "actions length")
    match cfg.actions with
    | [a1, a2] =>
      TestM.assertEqual a1.promptTemplate "implement {{body}}" (msg := "actions[0].promptTemplate")
      TestM.assertEqual a2.promptTemplate "review the changes" (msg := "actions[1].promptTemplate")
      TestM.assertEqual a2.series (some "review-{{issue_number}}") (msg := "actions[1].series")
    | _ => TestM.fail "expected exactly two actions"

@[test]
def listenerConfigActionsRoundTrip : Test := do
  -- Round-trip: toJson >> fromJson should preserve the actions list.
  let cfg : ListenerConfig := {
    name   := "rt-listener"
    source := .githubComments [{ upstream := "org/repo", fork := "org/repo" }] [] "@bot" []
    actions := [
      { mode := .fork, promptTemplate := "first task" },
      { mode := .fork, promptTemplate := "second task", series := some "s-{{issue_number}}" }
    ]
  }
  match FromJson.fromJson? (ToJson.toJson cfg) (α := ListenerConfig) with
  | .error e => TestM.fail s!"ListenerConfig round-trip: {e}"
  | .ok got =>
    TestM.assertEqual got.name "rt-listener" (msg := "name")
    TestM.assertEqual got.actions.length 2 (msg := "actions length")
    match got.actions with
    | [a1, a2] =>
      TestM.assertEqual a1.promptTemplate "first task" (msg := "actions[0].promptTemplate")
      TestM.assertEqual a2.promptTemplate "second task" (msg := "actions[1].promptTemplate")
      TestM.assertEqual a2.series (some "s-{{issue_number}}") (msg := "actions[1].series")
    | _ => TestM.fail "expected exactly two actions"
