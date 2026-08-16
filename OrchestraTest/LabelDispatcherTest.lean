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
  let src := SourceConfig.labelDispatcher "ready" [("implementor", 2), ("reviewer", 1)] true true
  match FromJson.fromJson? (ToJson.toJson src) (α := SourceConfig) with
  | .error e => TestM.fail s!"label-dispatcher round-trip: {e}"
  | .ok got =>
    match got with
    | .labelDispatcher label caps limitUnclaimed excludeRoots =>
      TestM.assertEqual label "ready"
      TestM.assertEqual (caps.lookup "implementor") (some 2)
      TestM.assertEqual (caps.lookup "reviewer") (some 1)
      TestM.assert limitUnclaimed "the unclaimed limit survives the round trip"
      TestM.assert excludeRoots "so does the root exclusion"
    | _ => TestM.fail "round-trip produced a different source type"

@[test]
def configParsesFromJson : Test := do
  let raw := r#"{"type": "label-dispatcher", "label": "agent-ready", "caps": {"implementor": 3}}"#
  match Json.parse raw >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => TestM.fail s!"label-dispatcher parse: {e}"
  | .ok (.labelDispatcher label caps limitUnclaimed excludeRoots) =>
    TestM.assertEqual label "agent-ready"
    TestM.assertEqual (caps.lookup "implementor") (some 3)
    TestM.assert (!limitUnclaimed)
      "the unclaimed limit lowers configured caps, so it has to be asked for"
    TestM.assert (!excludeRoots)
      "excluding roots stops a labelled leaf being worked, so it has to be asked for too"
  | .ok _ => TestM.fail "parsed as the wrong source type"

@[test]
def configParsesTheDispatchLimits : Test := do
  let raw := r#"{"type": "label-dispatcher", "label": "agent-ready", "caps": {"maintainer": 3},
                 "limit_unclaimed_to_open_issues": true, "exclude_root_issues": true}"#
  match Json.parse raw >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => TestM.fail s!"label-dispatcher parse: {e}"
  | .ok (.labelDispatcher _ _ limitUnclaimed excludeRoots) =>
    TestM.assert limitUnclaimed "limit_unclaimed_to_open_issues is read from the config"
    TestM.assert excludeRoots "exclude_root_issues is read from the config"
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

private def mkRole (name : String) (trigger : Project.RoleTrigger) (preClaim : Bool := false) :
    Project.Role :=
  { name, permissions := [], promptTemplate := "x"
  , dispatch := some { trigger, max := 1, preClaim } }

private def mkUnboundEntry (id : String) (role : String) (project : Option Taxis.IssueId)
    (issue : Option Taxis.IssueId := none)
    (status : Queue.QueueStatus := .running) : Queue.QueueEntry :=
  let repo : Repository := { owner := "o", name := "r" }
  { id, createdAt := "2026-01-01T00:00:00Z", status
  , upstream := repo, fork := repo, mode := .pr, prompt := ""
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

/-! ## Bounding the roles that do not pre-claim

    `limit_unclaimed_to_open_issues` lowers the caps of roles nothing arbitrates for. The
    arithmetic is pure; what it is applied to (the candidate sets, and each root's own subtree)
    needs a tracker and is covered by the integration tests. -/

@[test]
def capIsLoweredToTheWorkAvailable : Test := do
  TestM.assertEqual (capToAvailable 3 1) 1 (msg := "one issue, one agent")
  TestM.assertEqual (capToAvailable 3 5) 3 (msg := "more work than cap leaves the cap alone")
  TestM.assertEqual (capToAvailable 3 3) 3 (msg := "exactly as much work as cap")

/-- Zero available must not become a cap of zero: `dispatcherDecisions` reports that as "no cap
    configured, auto-dispatch is off", which is not what happened. The trigger's own verdict
    (`nothingToReview`) is the one that should be logged, and it only runs while the cap is
    non-zero. A configured zero still means off. -/
@[test]
def capFloorKeepsTheRealReasonVisible : Test := do
  TestM.assertEqual (capToAvailable 3 0) 1 (msg := "no work available, cap floors at one")
  TestM.assertEqual (capToAvailable 0 5) 0 (msg := "a configured zero stays off")
  let roles := #[mkRole "reviewer" .hasInReviewIssues]
  let caps := limitBoundCaps roles [("reviewer", 3)] (workable := 0) (reviewable := 0)
  let decisions := dispatcherDecisions { issues := #[], reviewable := #[], caps, roles }
  match decisions[0]? with
  | some d => match d.outcome with
    | .nothingToReview => TestM.assert true
    | o => TestM.fail s!"expected 'nothing to review', got {repr o}"
  | none => TestM.fail "expected a decision for the reviewer"

/-- The rule that answers the question this option exists for: with a single issue awaiting
    review, a cap of three reviewers is three agents on that one issue, because nothing claims it
    for them. An implementor that pre-claims keeps its cap — the claim is what keeps two of those
    apart, per issue rather than per tick. -/
@[test]
def boundCapsAreLoweredOnlyForRolesThatDoNotPreClaim : Test := do
  let roles := #[ mkRole "implementor" .hasOpenIssues (preClaim := true)
                , mkRole "checker" .hasOpenIssues
                , mkRole "reviewer" .hasInReviewIssues ]
  let caps := [("checker", 3), ("implementor", 3), ("reviewer", 3)]
  let limited := limitBoundCaps roles caps (workable := 2) (reviewable := 1)
  TestM.assertEqual (limited.lookup "implementor") (some 3) (msg := "a pre-claiming role")
  TestM.assertEqual (limited.lookup "checker") (some 2) (msg := "bounded by the workable issues")
  TestM.assertEqual (limited.lookup "reviewer") (some 1) (msg := "bounded by what awaits review")

/-- A capped name with no role file behind it keeps its cap here too: `dispatcherDecisions` is
    where that is reported, and lowering it first would change the verdict it reports. -/
@[test]
def boundCapsLeaveUnknownRolesAlone : Test := do
  let limited := limitBoundCaps #[] [("typoed", 3)] (workable := 0) (reviewable := 0)
  TestM.assertEqual (limited.lookup "typoed") (some 3)

/-- Unbound roles claim nothing at spawn, so the limit always applies to them — and it applies per
    root, since that is the scope their caps are counted in. -/
@[test]
def unboundCapsAreLoweredToTheRootsOwnWork : Test := do
  let caps := [("maintainer", 3)]
  TestM.assertEqual ((limitUnboundCaps caps 1).lookup "maintainer") (some 1)
    (msg := "a root that is a single open issue gets a single maintainer")
  TestM.assertEqual ((limitUnboundCaps caps 7).lookup "maintainer") (some 3)
    (msg := "a root with more work than the cap keeps the cap")

/-- End to end over the pure half: one open issue under a root, three configured, one dispatched.
    The second tick is what the limit is really for — the first maintainer is active by then, so
    without it the cap of three would keep spawning onto the same single issue. -/
@[test]
def oneOpenIssueDispatchesOneUnboundAgent : Test := do
  let root : Taxis.IssueId := ⟨42⟩
  let roles := #[mkRole "maintainer" .always]
  let caps := limitUnboundCaps [("maintainer", 3)] (openUnderRoot := 1)
  let spawnsGiven (entries : Array Queue.QueueEntry) :=
    dispatcherTick
      { activeByRole := unboundActiveByRole entries root
      , issues := #[], reviewable := #[], caps, roles }
  TestM.assertEqual (spawnsGiven #[]).size 1 (msg := "one agent for the one open issue")
  TestM.assert (spawnsGiven #[mkUnboundEntry "1" "maintainer" (some root)]).isEmpty
    "a second agent would duplicate the first one's work"

/-- The other half of "do not duplicate work": with two issues awaiting review and a cap of two,
    bounding the count alone still lets the second reviewer be handed the issue the first is on,
    since the within-tick `taken` array does not outlive the tick. -/
@[test]
def issuesAnAgentIsAlreadyOnLeaveTheSelection : Test := do
  let mkIssue (id : Int64) : Project.Issue :=
    { id := ⟨id⟩, projectId := ⟨1⟩, title := s!"issue {id}", description := ""
    , status := .open, createdAt := "", updatedAt := "" }
  let issues := #[mkIssue 7, mkIssue 8]
  let onSeven := mkUnboundEntry "1" "reviewer" (some ⟨1⟩) (issue := some ⟨7⟩)
  let left := unattendedIssues issues #[onSeven]
  TestM.assertEqual (left.map (·.id.val)).toList [(8 : Int64)]
    (msg := "the issue with an agent on it is not offered again")
  TestM.assertEqual (unattendedIssues issues #[]).size 2
    (msg := "with nothing active, everything is on offer")

/-- What the daemon prints when the limit bites. Only the caps actually lowered are reported:
    saying nothing about the ones left alone is what keeps the line worth reading. -/
@[test]
def loweredCapsAreReportable : Test := do
  let before := [("maintainer", 3), ("implementor", 2)]
  let after  := [("maintainer", 1), ("implementor", 2)]
  TestM.assertEqual (loweredCaps before after) [("maintainer", 3, 1)]

end OrchestraTest.LabelDispatcher
