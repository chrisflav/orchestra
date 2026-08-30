import OrchestraTest.TestM
import Orchestra

open Orchestra

/-!
# Records ordered by the wall clock, not by their ids

Ids come from `uniqueToken`, which reads `IO.monoNanosNow` — a clock that on Linux counts from
boot. Within one boot those ids ascend, and every store used to sort by them. A reboot restarts
the clock at zero, so an id minted 23 seconds after a reboot is nearly five orders of magnitude
smaller than one minted after three weeks of uptime, and an id sort puts every pre-reboot record
above every record made since.

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

private def withTempData (act : IO α) : IO α := Orchestra.withTempData "reboot" act

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

private def oldestOrder (xs : List (String × String)) : List String :=
  (Time.sortOldestFirst (·.1) (·.2) xs.toArray).toList.map (·.2)

@[test]
def anUnreadableTimestampGoesLastInEitherDirection : Test := do
  -- Not "oldest" — last, whichever way the sort runs, because nothing rewrites a stored
  -- timestamp and either head is a place it must not permanently occupy. At the head of a
  -- listing it would read as the most recent record; at the head of the claim order it would
  -- preempt every entry that has genuinely been waiting longer.
  let broken := [("not a timestamp", "broken"), (preRebootAt, preRebootId),
                 (postRebootAt, postRebootId)]
  TestM.assertEqual (order broken) [postRebootId, preRebootId, "broken"]
    (msg := "newest first: last, and still listed rather than dropped")
  TestM.assertEqual (oldestOrder broken) [preRebootId, postRebootId, "broken"]
    (msg := "oldest first: still last, so it cannot jump the queue")

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

@[test]
def anEntryWithNoReadableTimestampDoesNotJumpTheQueue : Test := do
  -- `created_at` is required to load at all, so the reachable failure is an empty one:
  -- `TaskStore.currentIso8601` shells out to `date` and does not check that it produced
  -- anything. Ordering that entry as the oldest would put it at the head of its priority for
  -- good — the starvation this whole change exists to remove, re-introduced one entry at a time.
  let picked := Queue.claimOrder
    #[anEntry "00000000000000000009" "", anEntry postRebootId postRebootAt,
      anEntry preRebootId preRebootAt]
  TestM.assertEqual (picked.map (·.id)).toList
    [preRebootId, postRebootId, "00000000000000000009"]
    (msg := "an entry nobody can date waits behind every entry that can be dated")

end OrchestraTest.RebootOrdering
