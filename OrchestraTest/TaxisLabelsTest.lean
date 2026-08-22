import OrchestraTest.TestM
import Orchestra

open Orchestra
open Orchestra.Project

namespace OrchestraTest.TaxisLabels

/-! Tests for the two deltas a triaging agent applies to a taxis issue: its labels, which decide
    which worker the dispatcher offers it to, and its assignees, which is how a named human is put
    on the hook for one.

    Both are planned before they are written (`planTaxisLabels`, `planAssignees`) precisely so
    the deciding is pure and testable without a tracker — the rest of the taxis-backed subsystem
    is not. What is checked here is what those calls can and cannot do to an issue: the name
    arithmetic itself is `Utils.Labels.planLabelChange`'s and is covered in
    `OrchestraTest.LabelIssue`. -/

private def label (n : Int64) (name : String) : Orchestra.Taxis.Label :=
  { id := ⟨n⟩, name }

private def known : Array Orchestra.Taxis.Label :=
  #[label 1 "auto-manage", label 2 "auto-work-fable", label 3 "auto-work-opus",
    label 4 "o-claimed", label 5 "t-project"]

private def ids (ns : List Int64) : Array Orchestra.Taxis.LabelId := (ns.map (⟨·⟩)).toArray

/-! ## Labels -/

@[test]
def labels_addAndRemoveInOneSet : Test := do
  -- taxis takes the whole label set in one PATCH, so the point of the plan is the array on the
  -- right: what the delta comes to when applied to what the issue carries now.
  match planTaxisLabels known (ids [1, 2]) ["auto-work-opus"] ["auto-work-fable"] with
  | .error e => TestM.fail s!"expected a plan, got: {e}"
  | .ok (change, final) =>
    TestM.assertEqual change.add ["auto-work-opus"] (msg := "add")
    TestM.assertEqual change.remove ["auto-work-fable"] (msg := "remove")
    TestM.assertEqual final.toList (ids [1, 3]).toList (msg := "the set to write")

@[test]
def labels_leaveTheOnesNotNamedAlone : Test := do
  -- The reason the tool takes a delta at all. An agent re-routing an issue to another model must
  -- not drop the label that put the issue in the dispatcher's scope in the first place.
  match planTaxisLabels known (ids [1, 2]) ["auto-work-opus"] [] with
  | .error e => TestM.fail s!"expected a plan, got: {e}"
  | .ok (_, final) =>
    TestM.assert (final.contains ⟨1⟩) "the label that was not named is still there"

@[test]
def labels_refuseOrchestrasOwn : Test := do
  -- `o-claimed` *is* an issue's claim: an agent setting it by hand would leave the tracker
  -- reading as claimed with no task holding it, invisible to the dispatcher and to every review
  -- sweep. It is on the tracker's label list like any other, so only this rule keeps it out.
  match planTaxisLabels known (ids []) ["o-claimed"] [] with
  | .ok _ => TestM.fail "expected o-claimed to be refused"
  | .error e => TestM.assert (e.contains "o-claimed") "the refusal names the label"

@[test]
def labels_refuseRemovingOrchestrasOwn : Test := do
  match planTaxisLabels known (ids [4]) [] ["o-claimed"] with
  | .ok _ => TestM.fail "expected removing o-claimed to be refused too"
  | .error e => TestM.assert (e.contains "o-claimed") "the refusal names the label"

@[test]
def labels_refuseReservedInAnyCase : Test := do
  -- The check folds case because the tracker's spelling is not the agent's. Matching `o-claimed`
  -- exactly would leave `O-Claimed` to be resolved case-insensitively downstream and applied to
  -- the very label the check exists to protect.
  match planTaxisLabels known (ids []) ["O-Claimed"] [] with
  | .ok _ => TestM.fail "expected a case variant of o-claimed to be refused"
  | .error _ => TestM.assert true

@[test]
def labels_refuseWhenTheTrackerSpellsOneLabelTwoWays : Test := do
  -- taxis's uniqueness on label names is case-sensitive, so these are two labels with two ids,
  -- and a dispatcher configured for one of them watches one id. Resolving the request
  -- case-insensitively has to pick one, and picking wrong is silent: the plan reports against the
  -- id it chose and writes a set holding the other, so the call answers "removed auto-work-opus"
  -- having changed nothing at all.
  let twoWays := known.push (label 6 "Auto-Work-Opus")
  match planTaxisLabels twoWays (ids [3]) [] ["auto-work-opus"] with
  | .ok _ => TestM.fail "expected a case-ambiguous label to be refused rather than guessed at"
  | .error e =>
    TestM.assert (e.contains "auto-work-opus" && e.contains "Auto-Work-Opus")
      "the refusal shows both spellings, since fixing it means merging them in the tracker"

@[test]
def labels_refuseProjectMarker : Test := do
  match planTaxisLabels known (ids []) ["t-project"] [] with
  | .ok _ => TestM.fail "expected t-project to be refused"
  | .error e => TestM.assert (e.contains "t-project") "the refusal names the label"

@[test]
def labels_refuseOneTheTrackerDoesNotDefine : Test := do
  -- Routing labels are configuration: a listener names one, and a label the agent invented would
  -- dispatch nothing while looking exactly like one that does.
  match planTaxisLabels known (ids []) ["auto-work-sonnet"] [] with
  | .ok _ => TestM.fail "expected an unknown label to be refused"
  | .error e =>
    TestM.assert (e.contains "auto-work-sonnet") "the refusal names it"
    TestM.assert (e.contains "auto-work-opus") "and lists what the tracker does define"

@[test]
def labels_noOpWritesNothing : Test := do
  match planTaxisLabels known (ids [1]) ["auto-manage"] ["auto-work-opus"] with
  | .error e => TestM.fail s!"expected a plan, got: {e}"
  | .ok (change, final) =>
    TestM.assertEqual change.add ([] : List String) (msg := "already carried, not added again")
    TestM.assertEqual change.remove ([] : List String) (msg := "not carried, not removed")
    TestM.assertEqual final.toList (ids [1]).toList (msg := "the set is unchanged")

/-! ## Assignees -/

private def actor (n : Int64) (email name : String) (bot : Bool := false) : Orchestra.Taxis.Actor :=
  { id := ⟨n⟩, email, displayName := name, bot }

private def actors : Array Orchestra.Taxis.Actor :=
  #[actor 1 "chris@example.com" "Christian",
    actor 2 "jj@example.com" "JJ",
    actor 3 "fuxi@example.com" "JJ" (bot := true)]

private def actorIds (ns : List Int64) : Array Orchestra.Taxis.ActorId := (ns.map (⟨·⟩)).toArray

@[test]
def actors_foundByEmailOrDisplayName : Test := do
  -- The display name is what an agent will have read on the issue; the email is what is unique.
  match findActor? actors "chris@example.com", findActor? actors "Christian" with
  | .ok a, .ok b => TestM.assert (a.id == b.id && a.id == ⟨1⟩) "both keys reach the same actor"
  | _, _ => TestM.fail "expected both an email and a display name to resolve"

@[test]
def actors_matchedCaseInsensitively : Test := do
  match findActor? actors "CHRISTIAN" with
  | .ok a => TestM.assert (a.id == ⟨1⟩) "case is not part of the name"
  | .error e => TestM.fail s!"expected a match, got: {e}"

@[test]
def actors_ambiguousDisplayNameIsRefused : Test := do
  -- Assigning the wrong person is worse than saying which two were meant — and here one of them
  -- is a bot, so guessing would silently escalate an issue to nobody.
  match findActor? actors "JJ" with
  | .ok _ => TestM.fail "expected an ambiguous display name to be refused"
  | .error e =>
    TestM.assert (e.contains "jj@example.com" && e.contains "fuxi@example.com")
      "the refusal names both candidates by the key that tells them apart"

@[test]
def actors_unknownIsRefusedWithTheRoster : Test := do
  match findActor? actors "nobody@example.com" with
  | .ok _ => TestM.fail "expected an unknown actor to be refused"
  | .error e =>
    TestM.assert (e.contains "nobody@example.com") "the refusal names who was asked for"
    TestM.assert (e.contains "chris@example.com") "and who the tracker knows"

@[test]
def assignees_addAndRemoveInOneSet : Test := do
  match planAssignees actors (actorIds [1]) ["jj@example.com"] ["chris@example.com"] with
  | .error e => TestM.fail s!"expected a plan, got: {e}"
  | .ok (change, final) =>
    TestM.assertEqual change.add ["jj@example.com"] (msg := "assigned")
    TestM.assertEqual change.remove ["chris@example.com"] (msg := "unassigned")
    TestM.assertEqual final.toList (actorIds [2]).toList (msg := "the set to write")

@[test]
def assignees_reportWhatWasAlreadyTheCase : Test := do
  match planAssignees actors (actorIds [1]) ["Christian"] ["jj@example.com"] with
  | .error e => TestM.fail s!"expected a plan, got: {e}"
  | .ok (change, final) =>
    TestM.assertEqual change.add ([] : List String) (msg := "already assigned")
    TestM.assertEqual change.alreadyPresent ["chris@example.com"] (msg := "but reported")
    TestM.assertEqual change.notPresent ["jj@example.com"] (msg := "and so is the absent removal")
    TestM.assertEqual final.toList (actorIds [1]).toList (msg := "the set is unchanged")

@[test]
def assignees_oneActorNamedTwiceIsOneAssignment : Test := do
  -- Both keys reach the same actor, and `final` already treats that as one assignment. Without
  -- the same collapse on the report it reads "assigned chris@…, chris@…".
  match planAssignees actors (actorIds []) ["Christian", "chris@example.com"] [] with
  | .error e => TestM.fail s!"expected a plan, got: {e}"
  | .ok (change, final) =>
    TestM.assertEqual change.add ["chris@example.com"] (msg := "reported once")
    TestM.assertEqual final.toList (actorIds [1]).toList (msg := "and assigned once")

@[test]
def assignees_contradictoryRequestIsRefused : Test := do
  -- Serving this would come down to which half of the delta was applied last, so neither is.
  match planAssignees actors (actorIds []) ["Christian"] ["chris@example.com"] with
  | .ok _ => TestM.fail "expected assigning and unassigning the same actor to be refused"
  | .error e => TestM.assert (e.contains "chris@example.com") "the refusal names the actor"

end OrchestraTest.TaxisLabels
