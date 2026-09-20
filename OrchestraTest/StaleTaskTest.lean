import OrchestraTest.TestM
import Orchestra

open Orchestra

/-!
# Two stores, one run, and keeping them from disagreeing

A run is recorded twice: as a `QueueEntry`, which is the daemon's handle on a slot, and as a
`TaskStore.TaskRecord`, which is the run itself. Only the record is what the dashboard's overview
reads, and only the entry was ever repaired when a daemon died mid-task — so a killed worker left
the overview saying `running` and the queue page saying `unfinished` about the same work, forever.

What is tested here is the rule that closes that gap and, just as much, the two things it must
refuse to touch: a run that is genuinely still going, and a run this process cannot see at all.
-/

private def withTempData (act : IO α) : IO α := Orchestra.withTempData "stale" act

private def anEntry (id : String) (status : Queue.QueueStatus)
    (taskId : Option String) : Queue.QueueEntry := {
  id, createdAt := "2026-08-21T10:04:12Z", status, taskId
  repo   := none
  prompt := "look at the queue"
}

private def aRecord (id : String) (status : TaskStore.TaskStatus) : TaskStore.TaskRecord := {
  id, createdAt := "2026-08-21T10:04:12Z", status
  repo   := none
  prompt := "look at the queue"
}

private def statusOf (id : String) : IO (Option TaskStore.TaskStatus) :=
  return (← TaskStore.loadTask id).map (·.status)

/-- The case from the report: the entry was swept on a restart, the record was not. -/
@[test]
def aStrandedRecordIsBroughtInLine : Test := do
  let (repaired, status) ← withTempData do
    Queue.saveEntry (anEntry "q-1" .unfinished (some "t-1"))
    TaskStore.saveTask (aRecord "t-1" .running)
    let repaired ← Queue.reconcileStaleTaskRecords
    pure (repaired, ← statusOf "t-1")
  TestM.assertEqual repaired 1 (msg := "one record repaired")
  TestM.assert (status == some .unfinished) (msg := "the record follows its entry")

/-- A run that landed keeps the verdict its worker wrote. Reconciliation is repair, not a second
    opinion: `completed` here must survive a pass that is looking at a non-running entry. -/
@[test]
def aLandedRecordKeepsItsVerdict : Test := do
  let (repaired, status) ← withTempData do
    Queue.saveEntry (anEntry "q-1" .done (some "t-1"))
    TaskStore.saveTask (aRecord "t-1" .completed)
    let repaired ← Queue.reconcileStaleTaskRecords
    pure (repaired, ← statusOf "t-1")
  TestM.assertEqual repaired 0 (msg := "nothing to repair")
  TestM.assert (status == some .completed) (msg := "the verdict stands")

/-- An entry that is `running` is one the daemon is working on, and reconciliation must step over
    it. This pins the reconciliation half only — that a live *worker* is never reaped is
    `shouldReap` below, which is where that decision actually lives. -/
@[test]
def aRunningEntryIsSteppedOver : Test := do
  let (repaired, status) ← withTempData do
    Queue.saveEntry (anEntry "q-1" .running (some "t-1"))
    TaskStore.saveTask (aRecord "t-1" .running)
    let repaired ← Queue.reconcileStaleTaskRecords
    pure (repaired, ← statusOf "t-1")
  TestM.assertEqual repaired 0 (msg := "a running entry is nobody's to repair")
  TestM.assert (status == some .running) (msg := "still running")

/-- Tasks also run outside the daemon — `orchestra run` writes a record and no entry at all — and
    from here a live foreground run is indistinguishable from an abandoned one. Reaching records
    only through the entries that name them is what keeps this pass off them. -/
@[test]
def aRecordNoEntryNamesIsLeftAlone : Test := do
  let (repaired, status) ← withTempData do
    Queue.saveEntry (anEntry "q-1" .done (some "t-1"))
    TaskStore.saveTask (aRecord "t-1" .completed)
    TaskStore.saveTask (aRecord "t-loose" .running)
    let repaired ← Queue.reconcileStaleTaskRecords
    pure (repaired, ← statusOf "t-loose")
  TestM.assertEqual repaired 0 (msg := "an unreferenced record is not evidence of anything")
  TestM.assert (status == some .running) (msg := "left as it was found")

/-- An entry that never got as far as minting a task has nothing to reconcile against, and must
    not throw on the way past. -/
@[test]
def anEntryWithNoTaskIsSkipped : Test := do
  let repaired ← withTempData do
    Queue.saveEntry (anEntry "q-1" .cancelled none)
    Queue.reconcileStaleTaskRecords
  TestM.assertEqual repaired 0 (msg := "nothing to point at")

/-- `markTaskUnfinished` answers whether it changed anything, which is what the reaper and the
    cancel route report to a caller. It must say `false` for a record that had already landed and
    for one that does not exist. -/
@[test]
def markTaskUnfinishedMovesOnlyRunningRecords : Test := do
  let (movedRunning, movedDone, movedMissing, doneStatus) ← withTempData do
    TaskStore.saveTask (aRecord "t-running" .running)
    TaskStore.saveTask (aRecord "t-done" .completed)
    let a ← Queue.markTaskUnfinished "t-running"
    let b ← Queue.markTaskUnfinished "t-done"
    let c ← Queue.markTaskUnfinished "t-nothing"
    pure (a, b, c, ← statusOf "t-done")
  TestM.assert movedRunning (msg := "a running record moves")
  TestM.assert (!movedDone) (msg := "a landed record does not")
  TestM.assert (!movedMissing) (msg := "a record that is not there does not")
  TestM.assert (doneStatus == some .completed) (msg := "and is not rewritten")

/-- The rule the reaper reads its table through, and the half that would be a disaster to get
    wrong: an entry whose id is in the live set has a worker on it, whatever the queue file says,
    and must survive. The daemon's ordering is what makes the live set trustworthy here — a row
    goes up before its entry is written `running` and comes down after it stops being so — but
    given a set, this is the whole of the decision. -/
@[test]
def shouldReapSparesEveryEntryWithAWorker : Test := do
  let live := #["q-live"]
  TestM.assert (!Queue.shouldReap live (anEntry "q-live" .running (some "t-live")))
    (msg := "a running entry with a worker is not reaped")
  TestM.assert (Queue.shouldReap live (anEntry "q-dead" .running (some "t-dead")))
    (msg := "a running entry with no worker is")
  TestM.assert (!Queue.shouldReap live (anEntry "q-done" .done (some "t-done")))
    (msg := "an entry that already landed is not")
  TestM.assert (!Queue.shouldReap live (anEntry "q-pending" .pending none))
    (msg := "nor is one that has not started")
  TestM.assert (!Queue.shouldReap #[] (anEntry "q-done" .done (some "t-done")))
    (msg := "an empty table does not make a landed entry reapable")
  TestM.assert (Queue.shouldReap #[] (anEntry "q-dead" .running none))
    (msg := "an empty table is what a daemon with no workers looks like")
