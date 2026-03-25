import Lean.Data.Json
import Orchestra.Listener
import Orchestra.Config

open Lean (Json FromJson ToJson)
open Orchestra.Listener

private def fail (msg : String) : IO α := do
  IO.eprintln s!"FAIL: {msg}"
  IO.Process.exit 1

private def passTest (msg : String) : IO Unit :=
  IO.println s!"PASS: {msg}"

private def expectEq [BEq α] [Repr α] (msg : String) (expected : α) (got : α) : IO Unit := do
  if got != expected then
    fail s!"{msg}: expected {repr expected}, got {repr got}"
  else
    passTest msg

def main : IO Unit := do

  -- Test 1: RepoEntry round-trip via JSON
  let entry : RepoEntry := { upstream := "org/repo", fork := "my-org/fork" }
  let entryJson := ToJson.toJson entry
  match FromJson.fromJson? entryJson (α := RepoEntry) with
  | .error e => fail s!"RepoEntry round-trip parse: {e}"
  | .ok got  =>
    expectEq "RepoEntry.upstream round-trip" "org/repo"     got.upstream
    expectEq "RepoEntry.fork round-trip"     "my-org/fork"  got.fork

  -- Test 2: Parse github-comments with new `repos` array format
  let newFormat := r#"
    {"type": "github-comments",
     "repos": [{"upstream": "org/repo", "fork": "my-org/fork"}],
     "trigger": "@bot",
     "authorized_users": ["alice", "bob"]}
  "#
  match Json.parse newFormat >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => fail s!"new repos format parse: {e}"
  | .ok (.githubComments repos _labels trigger authorizedUsers) =>
    expectEq "github-comments repos length"         1          repos.length
    match repos with
    | [r] =>
      expectEq "github-comments repos[0].upstream"  "org/repo"     r.upstream
      expectEq "github-comments repos[0].fork"      "my-org/fork"  r.fork
    | _ => fail "github-comments repos not a singleton"
    expectEq "github-comments trigger"              "@bot"         trigger
    expectEq "github-comments authorized_users"     ["alice", "bob"] authorizedUsers
  | .ok _ => fail "github-comments: unexpected SourceConfig variant"

  -- Test 3: Backward compat — single `fork` string for github-comments
  let oldFormat := r#"{"type": "github-comments", "fork": "org/repo", "trigger": "@bot"}"#
  match Json.parse oldFormat >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => fail s!"backward compat parse: {e}"
  | .ok (.githubComments repos _labels _trigger authorizedUsers) =>
    expectEq "backward compat repos length"   1         repos.length
    match repos with
    | [r] =>
      expectEq "backward compat upstream"     "org/repo" r.upstream
      expectEq "backward compat fork"         "org/repo" r.fork
    | _ => fail "backward compat repos not a singleton"
    expectEq "backward compat authorized_users empty" [] authorizedUsers
  | .ok _ => fail "backward compat: unexpected SourceConfig variant"

  -- Test 4: github-issues with new repos format and authorized_users
  let issuesFormat := r#"
    {"type": "github-issues",
     "repos": [{"upstream": "org/repo", "fork": "my-org/fork"},
               {"upstream": "org/other", "fork": "my-org/other"}],
     "labels": ["bug"],
     "authorized_users": ["carol"]}
  "#
  match Json.parse issuesFormat >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => fail s!"github-issues new format: {e}"
  | .ok (.githubIssues repos labels trigger authorizedUsers) =>
    expectEq "github-issues repos length"      2         repos.length
    expectEq "github-issues labels"            ["bug"]   labels
    expectEq "github-issues trigger default"   ""        trigger
    expectEq "github-issues authorized_users"  ["carol"] authorizedUsers
  | .ok _ => fail "github-issues: unexpected SourceConfig variant"

  -- Test 4b: github-issues with trigger
  let issuesWithTriggerFormat := r#"
    {"type": "github-issues",
     "repos": [{"upstream": "org/repo", "fork": "my-org/fork"}],
     "trigger": "@bot",
     "authorized_users": []}
  "#
  match Json.parse issuesWithTriggerFormat >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => fail s!"github-issues with trigger: {e}"
  | .ok (.githubIssues _repos _labels trigger _authorizedUsers) =>
    expectEq "github-issues trigger"  "@bot" trigger
  | .ok _ => fail "github-issues with trigger: unexpected SourceConfig variant"

  -- Test 4c: github-pr-reviews with trigger
  let reviewsWithTriggerFormat := r#"
    {"type": "github-pr-reviews",
     "repos": [{"upstream": "org/repo", "fork": "my-org/fork"}],
     "trigger": "@orchestra",
     "authorized_users": []}
  "#
  match Json.parse reviewsWithTriggerFormat >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => fail s!"github-pr-reviews with trigger: {e}"
  | .ok (.githubPrReviews _repos _labels trigger _authorizedUsers) =>
    expectEq "github-pr-reviews trigger"  "@orchestra" trigger
  | .ok _ => fail "github-pr-reviews with trigger: unexpected SourceConfig variant"

  -- Test 5: github-pr-reviews with new repos format
  let reviewsFormat := r#"
    {"type": "github-pr-reviews",
     "repos": [{"upstream": "org/repo", "fork": "my-org/fork"}],
     "labels": []}
  "#
  match Json.parse reviewsFormat >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => fail s!"github-pr-reviews new format: {e}"
  | .ok (.githubPrReviews repos _labels trigger authorizedUsers) =>
    expectEq "github-pr-reviews repos length"      1  repos.length
    expectEq "github-pr-reviews trigger default"   "" trigger
    expectEq "github-pr-reviews authorized_users"  [] authorizedUsers
  | .ok _ => fail "github-pr-reviews: unexpected SourceConfig variant"

  -- Test 6: SourceConfig round-trip (ToJson → FromJson) for github-comments
  let sc : SourceConfig := .githubComments
    [{ upstream := "pimotte-agents/orchestra", fork := "my-fork/orchestra" }]
    ["agent"] "@orchestra" ["dave"]
  match FromJson.fromJson? (ToJson.toJson sc) (α := SourceConfig) with
  | .error e => fail s!"SourceConfig round-trip: {e}"
  | .ok (.githubComments repos _labels trigger authorizedUsers) =>
    expectEq "round-trip repos length"      1                          repos.length
    expectEq "round-trip trigger"           "@orchestra"               trigger
    expectEq "round-trip authorized_users"  ["dave"]                   authorizedUsers
    match repos with
    | [r] =>
      expectEq "round-trip upstream"  "pimotte-agents/orchestra"  r.upstream
      expectEq "round-trip fork"      "my-fork/orchestra"         r.fork
    | _ => fail "round-trip repos not a singleton"
  | .ok _ => fail "SourceConfig round-trip: wrong variant"

  -- Test 7: AppConfig authorized_users field
  let appCfgJson := r#"
    {"github_app": {"app_id": 42, "private_key_path": "/key"},
     "authorized_users": ["alice", "bob"]}
  "#
  match Json.parse appCfgJson >>= FromJson.fromJson? (α := Orchestra.AppConfig) with
  | .error e => fail s!"AppConfig authorized_users parse: {e}"
  | .ok cfg  =>
    expectEq "AppConfig.authorizedUsers" ["alice", "bob"] cfg.authorizedUsers

  -- Test 8: AppConfig without authorized_users defaults to empty
  let appCfgJsonNoAuth := r#"
    {"github_app": {"app_id": 42, "private_key_path": "/key"}}
  "#
  match Json.parse appCfgJsonNoAuth >>= FromJson.fromJson? (α := Orchestra.AppConfig) with
  | .error e => fail s!"AppConfig no authorized_users: {e}"
  | .ok cfg  =>
    expectEq "AppConfig.authorizedUsers default empty" [] cfg.authorizedUsers

  IO.println "\nAll tests passed!"
