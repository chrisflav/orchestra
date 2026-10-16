import Db

/-!
# `0006_resident_sessions`

Resident sessions (`docs/design/resident-sessions.md`): an interactive session gains the resident it
is (`resident`, unique, NULL for every session that is not one) and why it is paused
(`paused_reason`); `resident_inbox` holds what a resident is to be handed, deduplicated by
`(resident, source, event_id)`; `resident_spend` what it has spent per budget day. Existing rows are
sessions that are not residents, which is what NULL says.
-/

namespace Orchestra.Store.Migrations

def migration_0006_resident_sessions : Db.Migration.Migration where
  name := "0006_resident_sessions"
  steps := [
    .addColumn "interactive_session" "resident" { type := .text, nullable := true },
    .addColumn "interactive_session" "paused_reason" { type := .text, nullable := true },
    .createTable "resident_inbox"
      { columns := .ofList
          [("id", { type := .int, nullable := false, autoIncrement := true }),
           ("resident", { type := .text, nullable := false }),
           ("source", { type := .text, nullable := false }),
           ("event_id", { type := .text, nullable := false }),
           ("trusted", { type := .bool, nullable := false }),
           ("received_at", { type := .text, nullable := false }),
           ("payload", { type := .text, nullable := false }),
           ("sent_in", { type := .int, nullable := true }),
           ("sent_seq", { type := .int, nullable := true }),
           ("confirmed", { type := .bool, nullable := false })],
        primaryKey := ["id"] },
    .createTable "resident_spend"
      { columns := .ofList
          [("resident", { type := .text, nullable := false }),
           ("day", { type := .text, nullable := false }),
           ("usd", { type := .float, nullable := false })],
        primaryKey := ["resident", "day"] },
    .createIndex "interactive_session" { name := "idx_interactive_session_resident", keys := [{ column := "resident" }], unique := true },
    .createIndex "resident_inbox" { name := "idx_resident_inbox_key", keys := [{ column := "resident" }, { column := "source" }, { column := "event_id" }], unique := true },
    .createIndex "resident_inbox" { name := "idx_resident_inbox_resident_confirmed", keys := [{ column := "resident" }, { column := "confirmed" }] }]

end Orchestra.Store.Migrations
