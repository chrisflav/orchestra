import Db

/-!
# `0011_execution_image`

Adds `queue_entry.execution_image`: the image an entry's task runs in, when whatever queued it
named one (`Config.IOTask.executionImage`). Nullable and left NULL on existing rows, which is what
every row written before it meant: the task runs wherever its repository and the operator's
configuration put it.

Not on `task` as well. The image is read at launch from the entry the task is dispatched from, and
a retry or a restart resume copies it from entry to entry; nothing reads it back off a task
record.
-/

namespace Orchestra.Store.Migrations

def migration_0011_execution_image : Db.Migration.Migration where
  name := "0011_execution_image"
  steps := [
    .addColumn "queue_entry" "execution_image" { type := .text, nullable := true }]

end Orchestra.Store.Migrations