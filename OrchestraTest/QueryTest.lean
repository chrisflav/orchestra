import OrchestraTest.TestM
import Orchestra

open Orchestra
open Lean (Json ToJson FromJson)

/-!
# The listings that ask the database a question

`StoreTest` pins what survives the round trip; this pins what comes back when the caller asks
for part of it. Every function here replaced a `loadAll…` followed by a `filter` or a `take`, and
the way one can go wrong is that the SQL says something slightly other than the array code did:
a status compared against the wrong spelling, a nullable column compared with `= NULL`, an order
that is the index's rather than the caller's, a `since` bound that is off by a row, a total
counted after the window instead of before it.

So each is exercised against a fixture with rows that must come back and rows that must not.
-/

namespace OrchestraTest.QueryTest

private def withTempData (act : IO α) : IO α := Orchestra.withTempData "query" act

/-! ## The fixture

Five entries, four tasks and three concert runs, stamped an hour apart so that a `since` has
something to cut between and the newest-first order has something to get wrong. -/

private def entry (id stamp : String) (status : Queue.QueueStatus := .pending)
    (priority : Nat := 10) : Queue.QueueEntry :=
  { id, createdAt := stamp, status, priority, repo := none, prompt := "p" }

private def task (id stamp : String) : TaskStore.TaskRecord :=
  { id, createdAt := stamp, repo := none, prompt := "p" }

/-- The queue as the tests below read it: one of each status that matters, a continuation's
    predecessor carrying a task id, two entries spawned by one task, and two steps of a concert
    run with a third step belonging to another. -/
private def writeEntries : IO Unit := do
  Queue.saveEntry { entry "q-pending-old" "2026-09-13T08:00:00Z" .pending (priority := 10) with
                    listenerName := some "labels" }
  Queue.saveEntry { entry "q-pending-new" "2026-09-13T11:00:00Z" .pending (priority := 50) with
                    spawnedBy := some "t-parent" }
  Queue.saveEntry { entry "q-running" "2026-09-13T09:00:00Z" .running with
                    taskId := some "t-run", concertId := some "c-1" }
  Queue.saveEntry { entry "q-done" "2026-09-13T10:00:00Z" .done with
                    concertId := some "c-1", spawnedBy := some "t-parent" }
  Queue.saveEntry { entry "q-failed" "2026-09-13T07:00:00Z" .failed with
                    concertId := some "c-2" }

private def writeTasks : IO Unit := do
  TaskStore.saveTask { task "t-1" "2026-09-13T08:00:00Z" with issueId := some ⟨34⟩ }
  TaskStore.saveTask { task "t-2" "2026-09-13T09:00:00Z" with series := some "parser" }
  TaskStore.saveTask { task "t-3" "2026-09-13T10:00:00Z" with
                       issueId := some ⟨34⟩, series := some "parser" }
  TaskStore.saveTask { task "t-4" "2026-09-13T11:00:00Z" with issueId := some ⟨99⟩ }

/-- `2026-09-13T09:30:00Z`, which falls between the third and the fourth of everything above. -/
private def halfPastNine : Int := 1789291800

/-! ## Filtering by status -/

@[test]
def pendingEntriesAreThePendingOnesInClaimOrder : Test := do
  let ids ← withTempData do
    writeEntries
    return (← Queue.pendingEntries).map (·.id)
  -- Higher priority first, and nothing that is not pending — the running entry is newer than
  -- one of these and would lead a newest-first listing.
  TestM.assertEqual ids.toList ["q-pending-new", "q-pending-old"]
    (msg := "the pending entries, in the order the daemon tries them")

@[test]
def activeEntriesArePendingAndRunning : Test := do
  let ids ← withTempData do
    writeEntries
    return (← Queue.activeEntries).map (·.id)
  TestM.assertEqual ids.toList ["q-pending-new", "q-running", "q-pending-old"]
    (msg := "pending and running, newest first, and nothing terminal")

@[test]
def runningEntriesAreOnlyTheRunningOnes : Test := do
  let ids ← withTempData do
    writeEntries
    return (← Queue.runningEntries).map (·.id)
  TestM.assertEqual ids.toList ["q-running"] (msg := "what the reaper sweeps")

@[test]
def countByStatusCountsEachStatusSeparately : Test := do
  let counts ← withTempData do
    writeEntries
    let byStatus ← Queue.countByStatus
    pure (byStatus .pending, byStatus .running, byStatus .done, byStatus .failed,
          byStatus .unfinished, byStatus .cancelled)
  TestM.assertEqual counts (2, 1, 1, 1, 0, 0)
    (msg := "one count per status, and zero for a status no entry is in")

/-! ## Finding one entry -/

@[test]
def entryForTaskFindsTheEntryThatBecameIt : Test := do
  let (found, missing) ← withTempData do
    writeEntries
    pure ((← Queue.entryForTask "t-run").map (·.id), ← Queue.entryForTask "t-nothing")
  TestM.assertEqual found (some "q-running") (msg := "the entry whose run became the task")
  TestM.assert missing.isNone
    "a task no entry points at is absent, not the first entry with no task id at all"

@[test]
def findEntryAnswersToEitherId : Test := do
  let (byOwn, byTask, missing) ← withTempData do
    writeEntries
    pure ((← Queue.findEntry "q-running").map (·.id),
          (← Queue.findEntry "t-run").map (·.id),
          ← Queue.findEntry "nonsense")
  TestM.assertEqual byOwn (some "q-running") (msg := "found by the entry's own id")
  TestM.assertEqual byTask (some "q-running") (msg := "and by the id of the task it became")
  TestM.assert missing.isNone "and neither id matching is absent"

@[test]
def countSpawnedByCountsTerminalEntriesToo : Test := do
  let (parent, none') ← withTempData do
    writeEntries
    pure (← Queue.countSpawnedBy "t-parent", ← Queue.countSpawnedBy "t-run")
  TestM.assertEqual parent 2
    (msg := "the ceiling is on work created, so the entry that has already finished counts")
  TestM.assertEqual none' 0 (msg := "a task that spawned nothing has spawned nothing")

@[test]
def entriesOfConcertAreThatConcertsSteps : Test := do
  let ids ← withTempData do
    writeEntries
    return (← Queue.entriesOfConcert "c-1").map (·.id)
  TestM.assertEqual ids.toList ["q-done", "q-running"]
    (msg := "one concert's steps, newest first, and not the other concert's")

/-! ## Paging

The window and the total are two different questions of the same filter: the total counts
everything `since` matched, *before* the window, which is what lets a page say "2 of 5". -/

@[test]
def entriesPageWindowsTheQueueAndCountsTheWhole : Test := do
  let (firstIds, firstTotal, skippedIds, skippedTotal) ← withTempData do
    writeEntries
    let (a, ta) ← Queue.entriesPage none 0 2
    let (b, tb) ← Queue.entriesPage none 2 2
    pure (a.map (·.id), ta, b.map (·.id), tb)
  TestM.assertEqual firstIds.toList ["q-pending-new", "q-done"] (msg := "the newest two")
  TestM.assertEqual firstTotal 5 (msg := "the total is the whole queue, not the window")
  TestM.assertEqual skippedIds.toList ["q-running", "q-pending-old"]
    (msg := "the offset skips before it takes")
  TestM.assertEqual skippedTotal 5 (msg := "and the total does not move with the window")

@[test]
def entriesPageSinceKeepsWhatIsAtOrAfterIt : Test := do
  let (ids, total) ← withTempData do
    writeEntries
    let (entries, total) ← Queue.entriesPage (some halfPastNine) 0 50
    pure (entries.map (·.id), total)
  TestM.assertEqual ids.toList ["q-pending-new", "q-done"]
    (msg := "only what was created at or after the bound")
  TestM.assertEqual total 2 (msg := "and the total counts what the bound matched")

@[test]
def concertRunsPageOrdersByStartedAt : Test := do
  let (ids, total, sinceIds, sinceTotal) ← withTempData do
    Queue.saveConcertRun { id := "c-a", startedAt := "2026-09-13T08:00:00Z" }
    Queue.saveConcertRun { id := "c-b", startedAt := "2026-09-13T10:00:00Z" }
    Queue.saveConcertRun { id := "c-c", startedAt := "2026-09-13T11:00:00Z" }
    let (all, total) ← Queue.concertRunsPage none 1 1
    let (recent, recentTotal) ← Queue.concertRunsPage (some halfPastNine) 0 50
    pure (all.map (·.id), total, recent.map (·.id), recentTotal)
  TestM.assertEqual ids.toList ["c-b"] (msg := "the second newest run, by started_at")
  TestM.assertEqual total 3 (msg := "out of three")
  TestM.assertEqual sinceIds.toList ["c-c", "c-b"] (msg := "and `since` cuts on started_at too")
  TestM.assertEqual sinceTotal 2 (msg := "two of them")

/-! ## The task history -/

@[test]
def countAndRecentReadOnlyWhatTheyNeed : Test := do
  let (total, recentIds) ← withTempData do
    writeTasks
    pure (← TaskStore.count, (← TaskStore.recent 2).map (·.id))
  TestM.assertEqual total 4 (msg := "every task record, counted by the database")
  TestM.assertEqual recentIds.toList ["t-4", "t-3"] (msg := "the newest two, newest first")

@[test]
def taskPageWindowsAndCountsTheWhole : Test := do
  let (ids, total, sinceIds, sinceTotal) ← withTempData do
    writeTasks
    let (page, total) ← TaskStore.page none 1 2
    let (recent, recentTotal) ← TaskStore.page (some halfPastNine) 0 50
    pure (page.map (·.id), total, recent.map (·.id), recentTotal)
  TestM.assertEqual ids.toList ["t-3", "t-2"] (msg := "one skipped, two taken, newest first")
  TestM.assertEqual total 4 (msg := "the total is the history, not the page")
  TestM.assertEqual sinceIds.toList ["t-4", "t-3"] (msg := "`since` keeps what is at or after it")
  TestM.assertEqual sinceTotal 2 (msg := "and counts exactly those")

@[test]
def tasksForIssueAreThatIssuesTasks : Test := do
  let (mine, other, none') ← withTempData do
    writeTasks
    pure ((← TaskStore.tasksForIssue ⟨34⟩).map (·.id),
          (← TaskStore.tasksForIssue ⟨99⟩).map (·.id),
          (← TaskStore.tasksForIssue ⟨1⟩).map (·.id))
  TestM.assertEqual mine.toList ["t-3", "t-1"] (msg := "both tasks on the issue, newest first")
  TestM.assertEqual other.toList ["t-4"] (msg := "and not another issue's")
  TestM.assertEqual none'.toList ([] : List String)
    (msg := "an issue with no tasks is empty, not every task with no issue at all")

@[test]
def tasksInSeriesAreThatSeriesTasks : Test := do
  let (parser, missing) ← withTempData do
    writeTasks
    pure ((← TaskStore.tasksInSeries "parser").map (·.id),
          (← TaskStore.tasksInSeries "nothing").map (·.id))
  TestM.assertEqual parser.toList ["t-3", "t-2"] (msg := "the series, newest first")
  TestM.assertEqual missing.toList ([] : List String)
    (msg := "and a series nothing is in is empty rather than every task outside a series")

/-! ## Interactive sessions -/

private def session (id stamp : String) : Interactive.SessionRecord :=
  { id, createdAt := stamp, lastActivityAt := stamp
    upstream := { owner := "acme", name := "widgets" }
    fork     := { owner := "bot",  name := "widgets" } }

@[test]
def sessionsPageWindowsAndCountsTheWhole : Test := do
  let (ids, total, sinceIds, sinceTotal) ← withTempData do
    Interactive.saveSession (session "i-1" "2026-09-13T08:00:00Z")
    Interactive.saveSession (session "i-2" "2026-09-13T10:00:00Z")
    Interactive.saveSession (session "i-3" "2026-09-13T11:00:00Z")
    let (page, total) ← Interactive.sessionsPage none 1 1
    let (recent, recentTotal) ← Interactive.sessionsPage (some halfPastNine) 0 50
    pure (page.map (·.id), total, recent.map (·.id), recentTotal)
  TestM.assertEqual ids.toList ["i-2"] (msg := "the second newest session")
  TestM.assertEqual total 3 (msg := "out of three")
  TestM.assertEqual sinceIds.toList ["i-3", "i-2"] (msg := "`since` keeps what is at or after it")
  TestM.assertEqual sinceTotal 2 (msg := "two of them")

end OrchestraTest.QueryTest
