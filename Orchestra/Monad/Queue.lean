import Orchestra.Queue

namespace Orchestra

open Queue (QueueEntry ConcertRun)

/--
Abstract interface for persisting and retrieving queue entries, concert runs,
and the daemon PID file.
-/
class MonadQueueStore (m : Type → Type) where
  saveEntry          : QueueEntry → m Unit
  loadEntry          : String → m (Option QueueEntry)
  loadAllEntries     : m (Array QueueEntry)
  saveConcertRun     : ConcertRun → m Unit
  loadConcertRun     : String → m (Option ConcertRun)
  loadAllConcertRuns : m (Array ConcertRun)
  writePid           : UInt32 → m Unit
  readPid            : m (Option UInt32)
  deletePid          : m Unit
  daemonRunning      : m Bool

instance : MonadQueueStore IO where
  saveEntry          := Orchestra.Queue.saveEntry
  loadEntry          := Orchestra.Queue.loadEntry
  loadAllEntries     := Orchestra.Queue.loadAllEntries
  saveConcertRun     := Orchestra.Queue.saveConcertRun
  loadConcertRun     := Orchestra.Queue.loadConcertRun
  loadAllConcertRuns := Orchestra.Queue.loadAllConcertRuns
  writePid           := Orchestra.Queue.writePid
  readPid            := Orchestra.Queue.readPid
  deletePid          := Orchestra.Queue.deletePid
  daemonRunning      := Orchestra.Queue.daemonRunning

end Orchestra
