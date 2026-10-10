import Db

/-!
# `0003_resolved_auth_source`

Adds `queue_entry.resolved_auth_source`, the source the daemon chose when it claimed an entry. Until
now that choice was written over `auth_source`, which is also how an entry is pinned to a source, so
an entry put back to `pending` by hand after a failed run waited for whichever account the failed
run had landed on, as if someone had asked for it.

Backfilled from `auth_source` for every entry that has been claimed — it has a task, or it is no
longer pending — so that what ran each one stays on record in the column that now says so.
`auth_source` itself is left as it is: which of the old values were pins and which were claims
cannot be told apart from the row, so an old entry revived by hand may still be pinned to the
source its last run landed on.
-/

namespace Orchestra.Store.Migrations

def migration_0003_resolved_auth_source : Db.Migration.Migration where
  name := "0003_resolved_auth_source"
  steps := [
    .addColumn "queue_entry" "resolved_auth_source" { type := .text, nullable := true },
    .sql "UPDATE queue_entry SET resolved_auth_source = auth_source WHERE task_id IS NOT NULL OR status <> 'pending'"]

end Orchestra.Store.Migrations
