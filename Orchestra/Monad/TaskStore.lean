import Orchestra.TaskStore

namespace Orchestra

open TaskStore (TaskRecord)

/--
Abstract interface for persisting and retrieving task records and series pointers.
Also provides ID generation and timestamp utilities needed when creating records.
-/
class MonadTaskStore (m : Type → Type) where
  saveTask            : TaskRecord → m Unit
  loadTask            : String → m (Option TaskRecord)
  loadAllTasks        : m (Array TaskRecord)
  latestInSeries      : String → m (Option String)
  updateSeriesPointer : String → String → m Unit
  generateId          : m String
  currentIso8601      : m String

instance : MonadTaskStore IO where
  saveTask            := Orchestra.TaskStore.saveTask
  loadTask            := Orchestra.TaskStore.loadTask
  loadAllTasks        := Orchestra.TaskStore.loadAllTasks
  latestInSeries      := Orchestra.TaskStore.latestInSeries
  updateSeriesPointer := Orchestra.TaskStore.updateSeriesPointer
  generateId          := Orchestra.TaskStore.generateId
  currentIso8601      := Orchestra.TaskStore.currentIso8601

end Orchestra
