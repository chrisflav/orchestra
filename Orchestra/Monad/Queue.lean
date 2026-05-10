import Orchestra.Queue

namespace Orchestra

/-- Typeclass for monads that can read and write queue entries.
    Covers task-queue persistence, PID-file management, and entry status updates. -/
class MonadQueue (m : Type → Type) where
  /-- Persist a queue entry to disk. -/
  saveEntry : Queue.QueueEntry → m Unit
  /-- Load a queue entry by ID, returning `none` if it does not exist. -/
  loadEntry : String → m (Option Queue.QueueEntry)
  /-- Load all queue entries, newest first. -/
  loadAllEntries : m (Array Queue.QueueEntry)
  /-- Return the next pending entry to run, ordered by priority then age. -/
  nextPending : m (Option Queue.QueueEntry)
  /-- Write the daemon PID file. -/
  writePid : UInt32 → m Unit
  /-- Read the daemon PID file, returning `none` if absent. -/
  readPid : m (Option UInt32)
  /-- Delete the daemon PID file. -/
  deletePid : m Unit

export MonadQueue (saveEntry loadEntry loadAllEntries nextPending writePid readPid deletePid)

instance : MonadQueue IO where
  saveEntry      := Queue.saveEntry
  loadEntry      := Queue.loadEntry
  loadAllEntries := Queue.loadAllEntries
  nextPending    := Queue.nextPending
  writePid       := Queue.writePid
  readPid        := Queue.readPid
  deletePid      := Queue.deletePid

end Orchestra
