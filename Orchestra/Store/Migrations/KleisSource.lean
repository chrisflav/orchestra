import Db

/-!
# `0005_kleis_source`

Adds a task's kleis source and narrowings (`docs/kleis.md`) to `task` and `queue_entry`: the
`{{secret}}` reference of a token the task names (`kleis_token` — the reference, never a token),
the spec it names (`kleis_spec`), the spec of the role or listener action it was queued for
(`kleis_role_spec`) and its narrowings (`kleis_narrow`, a JSON array). All nullable and left
NULL on existing rows, which is what every row written before them meant: no source of the task's
own, and no narrowing.
-/

namespace Orchestra.Store.Migrations

def migration_0005_kleis_source : Db.Migration.Migration where
  name := "0005_kleis_source"
  steps := [
    .addColumn "task" "kleis_token" { type := .text, nullable := true },
    .addColumn "task" "kleis_spec" { type := .text, nullable := true },
    .addColumn "task" "kleis_role_spec" { type := .text, nullable := true },
    .addColumn "task" "kleis_narrow" { type := .text, nullable := true },
    .addColumn "queue_entry" "kleis_token" { type := .text, nullable := true },
    .addColumn "queue_entry" "kleis_spec" { type := .text, nullable := true },
    .addColumn "queue_entry" "kleis_role_spec" { type := .text, nullable := true },
    .addColumn "queue_entry" "kleis_narrow" { type := .text, nullable := true }]

end Orchestra.Store.Migrations
