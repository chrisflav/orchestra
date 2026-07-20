import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Listener

namespace OrchestraTest.LabelDispatcher

/-! Tests for the project-independent dispatcher (`SourceConfig.labelDispatcher`).

    Its issue set and target resolution both need a live taxis instance, so those are covered by
    the opt-in integration tests. What is pure — the config round-trip and the repository-URL
    parsing that turns a `repository` artifact into a `Repository` — is covered here. -/

@[test]
def configRoundTrips : Test := do
  let src := SourceConfig.labelDispatcher "ready" [("implementor", 2), ("reviewer", 1)]
  match FromJson.fromJson? (ToJson.toJson src) (α := SourceConfig) with
  | .error e => TestM.fail s!"label-dispatcher round-trip: {e}"
  | .ok got =>
    match got with
    | .labelDispatcher label caps =>
      TestM.assertEqual label "ready"
      TestM.assertEqual (caps.lookup "implementor") (some 2)
      TestM.assertEqual (caps.lookup "reviewer") (some 1)
    | _ => TestM.fail "round-trip produced a different source type"

@[test]
def configParsesFromJson : Test := do
  let raw := r#"{"type": "label-dispatcher", "label": "agent-ready", "caps": {"implementor": 3}}"#
  match Json.parse raw >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => TestM.fail s!"label-dispatcher parse: {e}"
  | .ok (.labelDispatcher label caps) =>
    TestM.assertEqual label "agent-ready"
    TestM.assertEqual (caps.lookup "implementor") (some 3)
  | .ok _ => TestM.fail "parsed as the wrong source type"

@[test]
def configRequiresLabel : Test := do
  let raw := r#"{"type": "label-dispatcher", "caps": {"implementor": 1}}"#
  match Json.parse raw >>= FromJson.fromJson? (α := SourceConfig) with
  | .error _ => TestM.assert true
  | .ok _ => TestM.fail "a label-dispatcher without a label should not parse"

/-! ## Repository URL parsing

    The `repository` artifact stores a URL (`Taxis.Plugins.Standard`), but a queue entry needs an
    `owner/repo` pair. -/

private def parsed (url : String) : Option String :=
  (Project.repositoryOfArtifactUrl url).map (·.toString)

@[test]
def parsesGithubUrls : Test := do
  TestM.assertEqual (parsed "https://github.com/leanprover/lean4") (some "leanprover/lean4")
  TestM.assertEqual (parsed "http://github.com/leanprover/lean4") (some "leanprover/lean4")
  TestM.assertEqual (parsed "https://github.com/leanprover/lean4/") (some "leanprover/lean4")

/-- A `.git` suffix and deeper paths both appear in URLs people paste in; neither should end up
    in the repository name. -/
@[test]
def parsesUrlVariants : Test := do
  TestM.assertEqual (parsed "https://github.com/leanprover/lean4.git") (some "leanprover/lean4")
  TestM.assertEqual (parsed "https://github.com/leanprover/lean4/tree/master")
    (some "leanprover/lean4")
  -- Self-hosted forges have the same owner/repo shape.
  TestM.assertEqual (parsed "https://git.example.com/team/tool") (some "team/tool")

@[test]
def rejectsUrlsWithoutOwnerAndRepo : Test := do
  TestM.assert (parsed "https://github.com/leanprover").isNone "owner alone is not a repository"
  TestM.assert (parsed "https://github.com").isNone "host alone is not a repository"
  TestM.assert (parsed "").isNone "empty url is not a repository"

/-! ## Unbound (`always`) roles

    This dispatcher places an unbound role per *labelled root* rather than per issue, so its caps
    are counted differently from every other role's. Selecting the issues and resolving the roots
    needs a tracker, but the two pieces that decide how many spawn are pure. -/

private def mkRole (name : String) (trigger : Project.RoleTrigger) : Project.Role :=
  { name, permissions := [], promptTemplate := "x"
  , dispatch := some { trigger, max := 1 } }

private def mkUnboundEntry (id : String) (role : String) (project : Option Taxis.IssueId)
    (issue : Option Taxis.IssueId := none)
    (status : Queue.QueueStatus := .running) : Queue.QueueEntry :=
  let repo : Repository := { owner := "o", name := "r" }
  { id, createdAt := "2026-01-01T00:00:00Z", status
  , upstream := some repo, fork := some repo, mode := some .pr, prompt := ""
  , role := some role, projectId := project, issueId := issue }

@[test]
def splitCapsSeparatesAlwaysRolesFromBoundOnes : Test := do
  let roles := #[ mkRole "maintainer" .always
                , mkRole "implementor" .hasOpenIssues
                , mkRole "reviewer" .hasInReviewIssues ]
  let caps := [("implementor", 2), ("maintainer", 3), ("reviewer", 1)]
  let (bound, unbound) := splitCapsByBinding roles caps
  TestM.assertEqual (bound.map (·.1)) ["implementor", "reviewer"] (msg := "bound roles")
  TestM.assertEqual (unbound.map (·.1)) ["maintainer"] (msg := "unbound roles")

/-- A capped name with no role file behind it has to stay on the bound side: that is where
    `dispatcherDecisions` turns it into a `roleMissing` verdict. Treating it as unbound would
    hide the typo instead of reporting it. -/
@[test]
def splitCapsKeepsUnknownRolesOnTheBoundSide : Test := do
  let (bound, unbound) := splitCapsByBinding #[] [("typoed", 1)]
  TestM.assertEqual (bound.map (·.1)) ["typoed"] (msg := "unknown role stays bound")
  TestM.assert unbound.isEmpty "unknown role is not treated as unbound"

/-- The runaway this guards: the issue-bound tally skips entries without an `issueId`, so if
    unbound entries were counted there the cap would never be reached and the dispatcher would
    spawn a maintainer on every tick forever. -/
@[test]
def unboundTallyCountsThisRootsUnboundEntries : Test := do
  let root : Taxis.IssueId := ⟨42⟩
  let other : Taxis.IssueId := ⟨99⟩
  let entries := #[ mkUnboundEntry "1" "maintainer" (some root)
                  , mkUnboundEntry "2" "maintainer" (some root)
                  , mkUnboundEntry "3" "maintainer" (some other)        -- another root
                  , mkUnboundEntry "4" "maintainer" (some root) (issue := some ⟨7⟩)  -- bound
                  , mkUnboundEntry "5" "maintainer" none                -- no project
                  , mkUnboundEntry "6" "maintainer" (some root) (status := .done)
                  , mkUnboundEntry "7" "implementor" (some root) ]
  let tally := unboundActiveByRole entries root
  TestM.assertEqual (tally.getD "maintainer" 0) 2
    (msg := "only active, unbound, this-root maintainer entries")
  TestM.assertEqual (tally.getD "implementor" 0) 1 (msg := "other roles counted separately")
  TestM.assertEqual (tally.getD "absent" 0) 0 (msg := "unknown role is zero")

/-- Roots are independent: filling one root's cap must not stop another root's maintainer. -/
@[test]
def unboundTallyIsPerRoot : Test := do
  let a : Taxis.IssueId := ⟨1⟩
  let b : Taxis.IssueId := ⟨2⟩
  let roles := #[mkRole "maintainer" .always]
  let entries := #[mkUnboundEntry "1" "maintainer" (some a)]
  let spawnsFor (root : Taxis.IssueId) :=
    dispatcherTick
      { activeByRole := unboundActiveByRole entries root
      , issues := #[], reviewable := #[], caps := [("maintainer", 1)], roles }
  TestM.assert (spawnsFor a).isEmpty "root a is at its cap"
  TestM.assertEqual (spawnsFor b).size 1 (msg := "root b is untouched by root a's entry")
  match (spawnsFor b)[0]? with
  | some s => TestM.assertEqual s.issueId none (msg := "root spawn stays unbound")
  | none   => TestM.fail "expected a spawn for root b"

end OrchestraTest.LabelDispatcher
