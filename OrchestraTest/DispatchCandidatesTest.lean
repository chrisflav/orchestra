import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Project

namespace OrchestraTest.DispatchCandidates

/-! Selection policy for the project-independent dispatcher: the trigger label is inherited by
    descendants, and an issue is workable when it is open with no open children and no open
    dependencies. `dispatchCandidates` is pure, so unlike the rest of the label-dispatch path this
    runs without a taxis instance. -/

private def trigger : Taxis.LabelId := ⟨3⟩
private def tProject : Taxis.LabelId := ⟨2⟩

private def mk (id : Int64) (parent : Option Int64 := none)
    (labels : Array Taxis.LabelId := #[]) (state : Taxis.IssueState := .open)
    (dependencies : Array Taxis.IssueId := #[]) : Taxis.Issue :=
  { id := ⟨id⟩, title := s!"issue {id}", parent := parent.map (⟨·⟩), labels, state, dependencies
    createdAt := ⟨0⟩, updatedAt := ⟨0⟩ }

private def ids (issues : Array Taxis.Issue) : List Int64 :=
  (issues.map (·.id.val)).toList

@[test]
def labelledLeafDispatches : Test := do
  let all := #[mk 9 (parent := some 7) (labels := #[tProject, trigger]), mk 7]
  TestM.assertEqual (ids (dispatchCandidates all trigger)) [9]
    (msg := "a labelled leaf is dispatched; its unlabelled parent is not")

/-- The label is inherited, so labelling a project once opts its whole subtree in. -/
@[test]
def labelIsInheritedByDescendants : Test := do
  let all := #[
    mk 9 (labels := #[trigger]),
    mk 20 (parent := some 9),
    mk 21 (parent := some 20),
    mk 50]
  TestM.assertEqual (ids (dispatchCandidates all trigger)) [21]
    (msg := "only the deepest leaf under the labelled root")

/-- Once an issue has children it is a container: the children are the work, and dispatching the
    parent as well would put an agent on the whole while others work the parts. -/
@[test]
def containersAreNotDispatched : Test := do
  let leafOnly := #[mk 9 (labels := #[trigger])]
  TestM.assertEqual (ids (dispatchCandidates leafOnly trigger)) [9]
    (msg := "labelled issue with no children is work")
  let withChild := #[mk 9 (labels := #[trigger]), mk 20 (parent := some 9)]
  TestM.assertEqual (ids (dispatchCandidates withChild trigger)) [20]
    (msg := "the same issue becomes a container once it has a child")

/-- Only *open* children hold a parent back. This is what replaced the `o-blocked` label: a
    decomposed parent becomes workable again the moment its last open child closes, with nothing
    having to notice and clear a flag. -/
@[test]
def onlyOpenChildrenBlockAParent : Test := do
  let working := #[
    mk 9 (labels := #[trigger]),
    mk 20 (parent := some 9) (state := .completed),
    mk 21 (parent := some 9)]
  TestM.assertEqual (ids (dispatchCandidates working trigger)) [21]
    (msg := "one child still open, so the parent stays a container")
  let allDone := #[
    mk 9 (labels := #[trigger]),
    mk 20 (parent := some 9) (state := .completed),
    mk 21 (parent := some 9) (state := .closed)]
  TestM.assertEqual (ids (dispatchCandidates allDone trigger)) [9]
    (msg := "children completed and abandoned, so the parent is workable again")

@[test]
def unlabelledSubtreesAreIgnored : Test := do
  let all := #[mk 1, mk 2 (parent := some 1), mk 3 (parent := some 1) (labels := #[tProject])]
  TestM.assert (dispatchCandidates all trigger).isEmpty
    "t-project alone must not select anything — only the trigger label does"

/-- Mirrors a real tracker: one labelled leaf among many unlabelled issues, with `t-project`
    applied broadly (13 of 18 issues there) so it cannot be used to tell containers from work. -/
@[test]
def realTrackerShape : Test := do
  let all := #[
    mk 1, mk 2 (parent := some 1) (labels := #[tProject]),
    mk 3 (parent := some 1) (labels := #[tProject]),
    mk 4 (parent := some 3), mk 5 (parent := some 3), mk 6 (parent := some 3),
    mk 7 (labels := #[tProject]), mk 8 (labels := #[tProject]),
    mk 9 (parent := some 7) (labels := #[tProject, trigger]),
    mk 14 (parent := some 7) (labels := #[tProject]),
    mk 18 (parent := some 8) (labels := #[tProject])]
  TestM.assertEqual (ids (dispatchCandidates all trigger)) [9]
    (msg := "exactly the labelled leaf, nothing else in the tracker")

/-- A parent cycle must not hang the selection. -/
@[test]
def cyclesTerminate : Test := do
  let all := #[mk 1 (parent := some 2), mk 2 (parent := some 1)]
  TestM.assert (dispatchCandidates all trigger).isEmpty "a cycle with no label selects nothing"

/-- The label-dispatcher applies the same rule as the project one, over the raw tracker listing:
    open dependencies block, closed ones do not. It lists every issue, so unlike the project case
    a dependency in another project is still visible and still counts. -/
@[test]
def openDependencyBlocksCandidacy : Test := do
  let blocked := #[mk 9 (labels := #[trigger]),
                   mk 20 (parent := some 9) (dependencies := #[⟨21⟩]),
                   mk 21 (parent := some 9)]
  TestM.assertEqual (ids (dispatchCandidates blocked trigger)) [21]
    (msg := "20 waits on an open 21")
  let released := #[mk 9 (labels := #[trigger]),
                    mk 20 (parent := some 9) (dependencies := #[⟨21⟩]),
                    mk 21 (parent := some 9) (state := .completed)]
  TestM.assertEqual (ids (dispatchCandidates released trigger)) [20]
    (msg := "and runs once 21 is completed")

/-! ## Open issues per labelled root

    An unbound role is placed per labelled root, so bounding it by "the open issues in scope"
    means the ones that root owns (`openIssuesByRoot`). -/

private def countFor (all : Array Taxis.Issue) (root : Int64) : Nat :=
  (openIssuesByRoot all trigger).getD root 0

@[test]
def aRootCountsItselfAndItsSubtree : Test := do
  let lone := #[mk 9 (labels := #[trigger])]
  TestM.assertEqual (countFor lone 9) 1
    (msg := "a labelled issue with nothing under it is one open issue, not none")
  let subtree := #[mk 9 (labels := #[trigger]), mk 20 (parent := some 9),
                   mk 21 (parent := some 20), mk 50]
  TestM.assertEqual (countFor subtree 9) 3
    (msg := "the root plus its descendants; the unlabelled issue elsewhere is not in scope")

/-- Only open issues count: a completed child is work that has happened, and an agent dispatched
    for it would find nothing to do. -/
@[test]
def closedIssuesDoNotCount : Test := do
  let all := #[mk 9 (labels := #[trigger]),
               mk 20 (parent := some 9) (state := .completed),
               mk 21 (parent := some 9) (state := .closed),
               mk 22 (parent := some 9)]
  TestM.assertEqual (countFor all 9) 2 (msg := "the root and its one still-open child")

/-- Nested roots partition the set rather than each counting the whole subtree below them: an
    issue belongs to the nearest root above it, which is the one whose agents would pick it up. -/
@[test]
def nestedRootsSplitTheirSubtrees : Test := do
  let all := #[mk 1 (labels := #[trigger]),
               mk 2 (parent := some 1),
               mk 3 (parent := some 1) (labels := #[trigger]),
               mk 4 (parent := some 3),
               mk 5 (parent := some 3)]
  TestM.assertEqual (countFor all 1) 2 (msg := "the outer root and the child it still owns")
  TestM.assertEqual (countFor all 3) 3 (msg := "the inner root and its own two children")

/-- A labelled ancestor that has closed is not a root any more, so what is under it belongs to
    the next root still standing — otherwise those issues would be counted against a root nobody
    dispatches to, and the root that does get agents would look emptier than it is. -/
@[test]
def aClosedRootHandsItsWorkUpwards : Test := do
  let all := #[mk 1 (labels := #[trigger]),
               mk 3 (parent := some 1) (labels := #[trigger]) (state := .completed),
               mk 4 (parent := some 3),
               mk 5 (parent := some 3)]
  TestM.assertEqual (countFor all 1) 3 (msg := "the open root, counting the closed root's children")
  TestM.assertEqual (countFor all 3) 0 (msg := "a completed issue roots nothing")

/-! ## `exclude_root_issues`: the label marks the epic, not the work

    Same two functions, told that an issue carrying the label *directly* is a container by
    convention rather than a unit of work. -/

@[test]
def excludedRootsAreNotDispatchedOnto : Test := do
  let leafOnly := #[mk 9 (labels := #[trigger])]
  TestM.assert (dispatchCandidates leafOnly trigger (excludeRoots := true)).isEmpty
    "a labelled issue with nothing under it is an epic waiting to be decomposed, not work"
  let withChildren := #[mk 9 (labels := #[trigger]), mk 20 (parent := some 9),
                        mk 21 (parent := some 9)]
  TestM.assertEqual (ids (dispatchCandidates withChildren trigger (excludeRoots := true)))
    [20, 21] (msg := "what inherited the label is still the work")

/-- The root leaves the count as well, so the bound matches what can be dispatched. A nested root
    is excluded from its parent's count too: it is a root wherever it sits. -/
@[test]
def excludedRootsAreNotCounted : Test := do
  let all := #[mk 1 (labels := #[trigger]),
               mk 2 (parent := some 1),
               mk 3 (parent := some 1) (labels := #[trigger]),
               mk 4 (parent := some 3)]
  TestM.assertEqual ((openIssuesByRoot all trigger (excludeRoots := true)).getD 1 0) 1
    (msg := "the outer root counts its one plain child, not itself and not the inner root")
  TestM.assertEqual ((openIssuesByRoot all trigger (excludeRoots := true)).getD 3 0) 1
    (msg := "the inner root counts its own child only")

/-- A root with nothing under it counts zero here, which is a real answer and not a gap in the
    map. What keeps one agent on it to plan is the floor in `Listener.capToAvailable`, not this. -/
@[test]
def anEmptyExcludedRootCountsZero : Test := do
  let all := #[mk 9 (labels := #[trigger])]
  TestM.assert (openIssuesByRoot all trigger (excludeRoots := true)).isEmpty
    "nothing is counted against a root whose subtree is empty"

/-- The counts do not have to add up to the in-scope set. Scope is inherited from a labelled
    ancestor whatever state it is in, but only an *open* one is a root, so an issue under a
    completed epic is still dispatched onto and belongs to no root's tally. Nothing is placed
    against a root that does not exist, so there is nothing there to bound. -/
@[test]
def anIssueUnderACompletedEpicIsInScopeButUncounted : Test := do
  let all := #[mk 1 (labels := #[trigger]) (state := .completed),
               mk 2 (parent := some 1)]
  TestM.assertEqual (ids (dispatchCandidates all trigger)) [2]
    (msg := "still work: the label is inherited regardless of the epic's state")
  TestM.assert (openIssuesByRoot all trigger).isEmpty
    "no open labelled ancestor, so no root owns it"

@[test]
def rootCountingTerminatesOnCycles : Test := do
  let all := #[mk 1 (parent := some 2), mk 2 (parent := some 1)]
  TestM.assert (openIssuesByRoot all trigger).isEmpty "a cycle with no label has no roots"

end OrchestraTest.DispatchCandidates
