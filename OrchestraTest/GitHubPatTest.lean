import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson)
open Orchestra

namespace OrchestraTest.GitHubPat

/-!
# Per-repository GitHub PATs

`github.pats` is how one orchestra reaches repositories no single personal access token can see.
The whole mechanism is a lookup — `AppConfig.patFor` — so what matters is that the lookup is
unambiguous and that a config which would make it ambiguous does not load at all.

The second half is the point. A PAT that resolves to the wrong account does not announce itself:
GitHub answers a read the token is not entitled to with a 404, which is also its answer for a
repository that does not exist, so the symptom of a misrouted token is "no such repository" on a
repository that plainly exists. Everything that could produce that quietly — a swallowed parse
error, an unsubstituted secret, a pattern that looks like a glob and is not — is rejected at load
time here instead.

Nothing below reaches the network: `patFor` is pure, and the parse tests run `FromJson` on a JSON
literal.
-/

private def repo (owner name : String) : Repository := { owner, name }

/-- A config carrying `pat` as its fallback and `entries` as its per-repository sources. -/
private def cfg (pat : String) (entries : Array GitHubAuth) : AppConfig :=
  { appId := 0, privateKeyPath := "", pat, patEntries := entries }

/-! ## Resolution -/

@[test]
def patFor_fallsBackToTheGlobalPat : Test := do
  -- The single-token install, which is every config that predates `github.pats`.
  let c := cfg "global" #[]
  TestM.assertEqual (c.patFor (repo "acme" "widgets")) "global"
  TestM.assertEqual (c.patLabelFor (repo "acme" "widgets")) "github.pat"

@[test]
def patFor_ownerWildcardCoversTheAccount : Test := do
  let c := cfg "global" #[{ label := "work", token := "work-tok", repos := ["acme/*"] }]
  TestM.assertEqual (c.patFor (repo "acme" "widgets")) "work-tok"
  TestM.assertEqual (c.patFor (repo "acme" "anything-else")) "work-tok"
  TestM.assertEqual (c.patLabelFor (repo "acme" "widgets")) "work"

@[test]
def patFor_repositoryOutsideEveryPatternTakesTheFallback : Test := do
  -- The case that decides whether adding a second token can break the first: a repository no
  -- entry names must still resolve to `github.pat` rather than to whichever entry was written
  -- last.
  let c := cfg "global" #[{ label := "work", token := "work-tok", repos := ["acme/*"] }]
  TestM.assertEqual (c.patFor (repo "someone-else" "widgets")) "global"
  TestM.assertEqual (c.patLabelFor (repo "someone-else" "widgets")) "github.pat"

@[test]
def patFor_exactMatchBeatsAWildcardWrittenAbove : Test := do
  -- Specificity rather than file order, so a config can read top-down: the broad line for the
  -- account, then the individual repositories that are exceptions to it. Under first-match the
  -- exception below would never be reached and would fail silently.
  let c := cfg "global"
    #[ { label := "work",     token := "work-tok", repos := ["acme/*"] }
     , { label := "consult",  token := "consult-tok", repos := ["acme/client-work"] } ]
  TestM.assertEqual (c.patFor (repo "acme" "client-work")) "consult-tok"
  TestM.assertEqual (c.patFor (repo "acme" "widgets")) "work-tok"

@[test]
def patFor_exactMatchBeatsAWildcardWrittenBelow : Test := do
  -- And the same the other way round, so the answer does not depend on which order the two
  -- lines happen to be in.
  let c := cfg "global"
    #[ { label := "consult", token := "consult-tok", repos := ["acme/client-work"] }
     , { label := "work",    token := "work-tok",    repos := ["acme/*"] } ]
  TestM.assertEqual (c.patFor (repo "acme" "client-work")) "consult-tok"
  TestM.assertEqual (c.patFor (repo "acme" "widgets")) "work-tok"

@[test]
def patFor_ownerWildcardBeatsTheCatchAll : Test := do
  let c := cfg "global"
    #[ { label := "everything", token := "any-tok",  repos := ["*"] }
     , { label := "work",       token := "work-tok", repos := ["acme/*"] } ]
  TestM.assertEqual (c.patFor (repo "acme" "widgets")) "work-tok"
  -- And the catch-all takes everything the wildcard does not, in place of `github.pat`.
  TestM.assertEqual (c.patFor (repo "someone-else" "widgets")) "any-tok"

@[test]
def patFor_tiesGoToTheEntryWrittenFirst : Test := do
  -- Two entries claiming the same repository at the same specificity is a config mistake, but it
  -- must still resolve the same way on every run — a lookup that depended on iteration order
  -- would send the same call to different accounts on different days.
  let c := cfg "global"
    #[ { label := "first",  token := "first-tok",  repos := ["acme/*"] }
     , { label := "second", token := "second-tok", repos := ["acme/*"] } ]
  TestM.assertEqual (c.patFor (repo "acme" "widgets")) "first-tok"

@[test]
def patFor_matchesRegardlessOfCase : Test := do
  -- GitHub's own owner and repository names are case-insensitive, so a pattern written `Acme/*`
  -- and a taxis artifact reading `acme/widgets` are the same account.
  let c := cfg "global" #[{ label := "work", token := "work-tok", repos := ["Acme/Widgets"] }]
  TestM.assertEqual (c.patFor (repo "acme" "widgets")) "work-tok"
  TestM.assertEqual (c.patFor (repo "ACME" "WIDGETS")) "work-tok"

@[test]
def patFor_takesTheBestPatternWithinOneEntry : Test := do
  -- An entry's `repos` is a list, and the entry's claim on a repository is its strongest one.
  let c := cfg "global"
    #[ { label := "broad", token := "broad-tok", repos := ["*"] }
     , { label := "work",  token := "work-tok",  repos := ["other/thing", "acme/widgets"] } ]
  TestM.assertEqual (c.patFor (repo "acme" "widgets")) "work-tok"

/-! ## Patterns

`repoPatternError?` is the gate: a pattern it accepts is one `matchRepoPattern?` understands, and
anything else is refused at load rather than silently matching nothing. -/

@[test]
def patterns_theThreeSupportedFormsAreAccepted : Test := do
  TestM.assert (repoPatternError? "*").isNone "'*' is the catch-all"
  TestM.assert (repoPatternError? "acme/*").isNone "'owner/*' covers an account"
  TestM.assert (repoPatternError? "acme/widgets").isNone "'owner/name' covers one repository"

@[test]
def patterns_partialGlobsAreRefused : Test := do
  -- These are the shapes someone reaches for expecting glob semantics. Accepting them as literals
  -- would match nothing and route the repository to the fallback token without saying so.
  TestM.assert (repoPatternError? "acme-*/widgets").isSome "a globbed owner is not supported"
  TestM.assert (repoPatternError? "acme/widget*").isSome "a globbed name is not supported"
  TestM.assert (repoPatternError? "*/widgets").isSome "wildcarding only the owner is not supported"

@[test]
def patterns_malformedSlugsAreRefused : Test := do
  TestM.assert (repoPatternError? "widgets").isSome "a bare name names no owner"
  TestM.assert (repoPatternError? "acme/team/widgets").isSome "three components is not a slug"
  TestM.assert (repoPatternError? "acme/").isSome "an empty half is not a slug"
  TestM.assert (repoPatternError? "/widgets").isSome "an empty half is not a slug"

/-! ## Parsing

Everything here is about failing loudly. A `github.pats` block that does not load the way it reads
is worse than no block at all, because the daemon keeps running on the fallback token. -/

private def isErr (r : Except String AppConfig) : Bool :=
  match r with | .error _ => true | .ok _ => false

private def parseConfig (githubBlock : String) : Except String AppConfig :=
  let text := "{\"github_app\": {\"app_id\": 1, \"private_key_path\": \"/k.pem\"}, \
               \"github\": " ++ githubBlock ++ "}"
  match Json.parse text with
  | .error e => .error s!"test fixture is not JSON: {e}"
  | .ok j    => FromJson.fromJson? j

@[test]
def parse_readsEntriesAndKeepsTheFallback : Test := do
  match parseConfig "{\"pat\": \"global\", \"pats\": [
      {\"label\": \"work\", \"token\": \"work-tok\", \"repos\": [\"acme/*\"]}]}" with
  | .error e => TestM.fail s!"a well-formed github.pats block should load: {e}"
  | .ok c =>
    TestM.assertEqual c.pat "global"
    TestM.assertEqual c.patEntries.size 1
    TestM.assertEqual (c.patFor (repo "acme" "widgets")) "work-tok"

@[test]
def parse_anAbsentPatsBlockIsTheOrdinaryConfig : Test := do
  -- The single-token install must keep loading untouched; `github.pats` is an addition, not a
  -- migration.
  match parseConfig "{\"pat\": \"global\"}" with
  | .error e => TestM.fail s!"a config without github.pats should still load: {e}"
  | .ok c =>
    TestM.assert c.patEntries.isEmpty "no entries were configured"
    TestM.assertEqual (c.patFor (repo "acme" "widgets")) "global"

@[test]
def parse_aMalformedEntryFailsTheWholeConfig : Test := do
  -- Strict when present, for the same reason the `agents` block is: an entry dropped for being
  -- unreadable does not stop anything, it quietly moves that repository's pull requests, reviews
  -- and comments onto whichever account `github.pat` holds.
  let r := parseConfig "{\"pat\": \"global\", \"pats\": [{\"label\": \"work\"}]}"
  TestM.assert (isErr r) "an entry missing its token must not be skipped over"

@[test]
def parse_anEmptyTokenIsRefused : Test := do
  let r := parseConfig "{\"pats\": [
      {\"label\": \"work\", \"token\": \"  \", \"repos\": [\"acme/*\"]}]}"
  TestM.assert (isErr r) "an entry with a blank token authenticates as nobody"

@[test]
def parse_anUnsubstitutedSecretIsRefused : Test := do
  -- `{{github_pat}}` survives verbatim when secrets.json does not define it. Left alone it is a
  -- non-empty, token-shaped string that authenticates as nobody, and every repository the entry
  -- covers then reports a 404 — the failure this whole feature is most likely to produce, caught
  -- at the one moment it is still legible.
  let r := parseConfig "{\"pats\": [
      {\"label\": \"work\", \"token\": \"{{work_pat}}\", \"repos\": [\"acme/*\"]}]}"
  TestM.assert (isErr r) "an unresolved secret placeholder is not a token"

@[test]
def parse_anEntryCoveringNothingIsRefused : Test := do
  let r := parseConfig "{\"pats\": [{\"label\": \"work\", \"token\": \"t\", \"repos\": []}]}"
  TestM.assert (isErr r) "an entry with no patterns would never be reached"

@[test]
def parse_aBadPatternIsRefused : Test := do
  let r := parseConfig "{\"pats\": [
      {\"label\": \"work\", \"token\": \"t\", \"repos\": [\"acme/widget*\"]}]}"
  TestM.assert (isErr r) "a pattern that matches nothing must not load as one that does"

@[test]
def parse_duplicateLabelsAreRefused : Test := do
  -- Labels are what the startup coverage line and the diagnostics name a source by, and two
  -- sources answering to one name make those unreadable exactly when they are being read.
  let r := parseConfig "{\"pats\": [
      {\"label\": \"work\", \"token\": \"a\", \"repos\": [\"acme/*\"]},
      {\"label\": \"work\", \"token\": \"b\", \"repos\": [\"other/*\"]}]}"
  TestM.assert (isErr r) "two entries may not share a label"

/-! ## Coverage reporting -/

@[test]
def coverage_namesLabelsAndPatternsAndNeverTokens : Test := do
  let c := cfg "global"
    #[ { label := "work",     token := "work-tok",     repos := ["acme/*", "acme-labs/tooling"] }
     , { label := "personal", token := "personal-tok", repos := ["me/*"] } ]
  let lines := c.patCoverage
  TestM.assertEqual lines.size 2
  TestM.assert (lines[0]!.contains "work" && lines[0]!.contains "acme/*"
                && lines[0]!.contains "acme-labs/tooling")
    "the line names the source and everything it covers"
  -- The line is printed at daemon startup, where it would otherwise be the one place a token
  -- could reach a log.
  TestM.assert (lines.all fun l => !l.contains "-tok") "a coverage line never carries a token"

end OrchestraTest.GitHubPat
