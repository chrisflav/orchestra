import OrchestraTest.TestM
import Orchestra

open Orchestra

/-!
# Records ordered by the wall clock, not by their ids

Ids come from `uniqueToken`, which reads `IO.monoNanosNow` — a clock that on Linux counts from
boot. Within one boot those ids ascend, and every store used to sort by them. A reboot restarts
the clock at zero, so an id minted a minute after a reboot is two orders of magnitude smaller
than one minted after three weeks of uptime, and an id sort puts every pre-reboot record above
every record made since.

What that cost, in the incident this file exists for: the dashboard's task list showed nothing
newer than the reboot, because the whole first page was the tail of the previous boot; and a
queue entry left pending across a reboot looked like the *newest* entry at its priority and was
tried last, behind everything enqueued after it.

So the stores sort by their recorded timestamp. The ids below are written in the shape the
clock actually produces — a large pre-reboot value, a small one from just after — so that a
regression to id ordering fails here rather than after the next reboot.
-/

namespace OrchestraTest.RebootOrdering

/-- Three weeks of uptime, then a reboot. `preReboot` is the *older* record with the *larger*
    id; `postReboot` is newer with a smaller one. -/
private def preRebootId  : String := "19113565611182650235"
private def postRebootId : String := "00000234013334180000"

private def preRebootAt  : String := "2026-08-29T20:03:47Z"
private def postRebootAt : String := "2026-08-29T20:05:49Z"

private def withTempData (act : IO α) : IO α := do
  let root := System.FilePath.mk "/tmp" / s!"orchestra-reboot-{← IO.monoNanosNow}"
  IO.FS.createDirAll root
  let previous ← Dirs.dataBaseOverride.get
  Dirs.setDataBaseOverride (some root)
  try act
  finally
    Dirs.setDataBaseOverride previous
    try IO.FS.removeDirAll root catch _ => pure ()

/-! ## The comparator -/

private def order (xs : List (String × String)) : List String :=
  (Time.sortNewestFirst (·.1) (·.2) xs.toArray).toList.map (·.2)

@[test]
def theLaterTimestampWinsHoweverTheIdsSort : Test := do
  TestM.assertEqual
    (order [(preRebootAt, preRebootId), (postRebootAt, postRebootId)])
    [postRebootId, preRebootId]
    (msg := "the record made after the reboot is the newer one, though its id is smaller")
  TestM.assertEqual
    (order [(postRebootAt, postRebootId), (preRebootAt, preRebootId)])
    [postRebootId, preRebootId]
    (msg := "and the answer does not depend on what order they were read off disk in")

@[test]
def theIdBreaksTiesWithinASecond : Test := do
  -- Timestamps are recorded to the second and a dispatcher enqueues several tasks inside one,
  -- so ties are the common case, not the corner. Within a second the id is monotone and is
  -- exactly the right tiebreaker.
  TestM.assertEqual
    (order [("2026-08-21T10:00:00Z", "b"), ("2026-08-21T10:00:00Z", "c"),
            ("2026-08-21T10:00:00Z", "a")])
    ["c", "b", "a"]
    (msg := "same second, so the id decides")

@[test]
def anUnreadableTimestampSortsOldest : Test := do
  -- Last rather than first: a record whose timestamp cannot be parsed must not displace the
  -- genuinely newest one at the head of a listing. It stays visible at the tail.
  TestM.assertEqual
    (order [("not a timestamp", "broken"), (preRebootAt, preRebootId),
            (postRebootAt, postRebootId)])
    [postRebootId, preRebootId, "broken"]
    (msg := "unparseable sorts oldest, and is still listed")

/-! ## The stores -/

private def aRecord (id createdAt : String) : TaskStore.TaskRecord := {
  id, createdAt
  repo   := none
  prompt := "look at the dashboard"
  status := .completed
}

@[test]
def theTaskListShowsWhatFinishedSinceTheReboot : Test := do
  let ids ← withTempData do
    TaskStore.saveTask (aRecord preRebootId preRebootAt)
    TaskStore.saveTask (aRecord postRebootId postRebootAt)
    pure ((← TaskStore.loadAllTasks).map (·.id))
  TestM.assertEqual ids.toList [postRebootId, preRebootId]
    (msg := "the run that finished after the reboot is the one at the top of the page")

private def anEntry (id createdAt : String) (priority : Nat := 10) : Queue.QueueEntry := {
  id, createdAt, priority
  status := .pending
  repo   := none
  prompt := "run something"
}

@[test]
def anEntryStrandedByTheRebootIsNotStarved : Test := do
  -- The daemon takes the head of this list. Ordered by id ascending — "oldest first" before
  -- this was fixed — the pre-reboot entry has the larger id and comes last, so it waits behind
  -- every entry enqueued since the machine came back, indefinitely.
  let picked := Queue.pendingCandidates
    #[anEntry postRebootId postRebootAt, anEntry preRebootId preRebootAt] {} 4
  TestM.assertEqual (picked.map (·.id)).toList [preRebootId, postRebootId]
    (msg := "the entry that has been waiting since before the reboot runs first")

@[test]
def priorityStillOutranksAge : Test := do
  let picked := Queue.pendingCandidates
    #[anEntry preRebootId preRebootAt (priority := 5),
      anEntry postRebootId postRebootAt (priority := 20)] {} 4
  TestM.assertEqual (picked.map (·.id)).toList [postRebootId, preRebootId]
    (msg := "age only orders entries that share a priority")

end OrchestraTest.RebootOrdering
