import Db

/-!
# `0012_checkout`

Adds `queue_entry.checkout`: the commit an entry's task is checked out at, with the upstream ref it
is fetched from and whether the checkout is isolated (`Config.CheckoutSpec`), as JSON. Nullable and
left NULL on existing rows, which is what every row written before it meant: the task starts on the
default branch.

Not on `task`, for the reason `0011_execution_image` gives: a retry and a restart resume copy it
from entry to entry, and the continuations built from a task record read it from the entry that
ran the task (`Queue.carriedForContinuation`).
-/

namespace Orchestra.Store.Migrations

def migration_0012_checkout : Db.Migration.Migration where
  name := "0012_checkout"
  steps := [
    .addColumn "queue_entry" "checkout" { type := .text, nullable := true }]

end Orchestra.Store.Migrations