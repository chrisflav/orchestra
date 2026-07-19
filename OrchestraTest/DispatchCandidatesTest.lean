import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Project

namespace OrchestraTest.DispatchCandidates

/-! Selection policy for the project-independent dispatcher: the trigger label is inherited by
    descendants, and only leaves are dispatched. `dispatchCandidates` is pure, so unlike the rest
    of the label-dispatch path this runs without a taxis instance. -/

private def trigger : Taxis.LabelId := ⟨3⟩
private def tProject : Taxis.LabelId := ⟨2⟩

private def mk (id : Int64) (parent : Option Int64 := none)
    (labels : Array Taxis.LabelId := #[]) (state : Taxis.IssueState := .open) : Taxis.Issue :=
  { id := ⟨id⟩, title := s!"issue {id}", parent := parent.map (⟨·⟩), labels, state
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

end OrchestraTest.DispatchCandidates
