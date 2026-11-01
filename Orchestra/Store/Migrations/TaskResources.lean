import Db

/-!
# `0013_task_resources`

Adds `queue_entry.resources`: the container `resources` of the pod an entry's task runs in, when
its task file named them (`Config.IOTask.resources`), as JSON. Nullable and left NULL on existing
rows, which is what every row written before it meant: the pod gets the configured
`execution.options.resources`.

Not on `task`, for the reason `0011_execution_image` gives: a retry and a restart resume copy it
from entry to entry, and the continuations built from a task record read it from the entry that
ran the task (`Queue.carriedForContinuation`).
-/

namespace Orchestra.Store.Migrations

def migration_0013_task_resources : Db.Migration.Migration where
  name := "0013_task_resources"
  steps := [
    .addColumn "queue_entry" "resources" { type := .text, nullable := true }]

end Orchestra.Store.Migrations
