import Db

/-!
# `0004_task_run_state`

Adds `task_run_state`: where each running task has got to — which step, which attempt, the pod and
the MCP endpoint it uses — so that a daemon that restarts can pick its running tasks up again
rather than resuming them from scratch (`Orchestra.RunState`). Empty to begin with; nothing that
ran before it is ever picked up.
-/

namespace Orchestra.Store.Migrations

def migration_0004_task_run_state : Db.Migration.Migration where
  name := "0004_task_run_state"
  steps := [
    .createTable "task_run_state"
      { columns := .ofList
          [("task_id", { type := .text, nullable := false }),
           ("updated_at", { type := .text, nullable := false }),
           ("phase", { type := .text, nullable := false }),
           ("attempt", { type := .int, nullable := false }),
           ("state", { type := .text, nullable := false })],
        primaryKey := ["task_id"] }]

end Orchestra.Store.Migrations
