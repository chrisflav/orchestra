import Orchestra.TaskStore

namespace Orchestra

/-- Typeclass for monads that can read and write task records.
    Covers both individual task persistence and series-pointer tracking. -/
class MonadTaskStore (m : Type → Type) where
  /-- Persist a task record to disk. -/
  saveTask : TaskStore.TaskRecord → m Unit
  /-- Load a task record by ID, returning `none` if it does not exist. -/
  loadTask : String → m (Option TaskStore.TaskRecord)
  /-- Load all task records, newest first. -/
  loadAllTasks : m (Array TaskStore.TaskRecord)
  /-- Return the latest task ID in a named series, or `none` if none exists. -/
  latestInSeries : String → m (Option String)
  /-- Update the series pointer to point to `taskId`. -/
  updateSeriesPointer : String → String → m Unit
  /-- Generate a fresh unique task ID. -/
  generateTaskId : m String
  /-- Return the current time as an ISO-8601 string. -/
  currentIso8601 : m String

export MonadTaskStore (saveTask loadTask loadAllTasks latestInSeries
  updateSeriesPointer generateTaskId currentIso8601)

instance : MonadTaskStore IO where
  saveTask            := TaskStore.saveTask
  loadTask            := TaskStore.loadTask
  loadAllTasks        := TaskStore.loadAllTasks
  latestInSeries      := TaskStore.latestInSeries
  updateSeriesPointer := TaskStore.updateSeriesPointer
  generateTaskId      := TaskStore.generateId
  currentIso8601      := TaskStore.currentIso8601

end Orchestra
