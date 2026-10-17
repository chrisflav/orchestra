import Db

/-!
# `0007_resident_feeds`

A resident's feeds (`docs/design/resident-sessions.md` §5, §6): a long-poll listener keeps its
checkpoint, its last error and the revocation ids of the kleis tokens it holds on its
`listener_state` row (`checkpoint`, `error`, `revocations`, all NULL for every listener that is not
a feed); `resident_watch` holds the topics a resident asked to be told about, unique by
`(resident, feed, key)`.
-/

namespace Orchestra.Store.Migrations

def migration_0007_resident_feeds : Db.Migration.Migration where
  name := "0007_resident_feeds"
  steps := [
    .addColumn "listener_state" "checkpoint" { type := .text, nullable := true },
    .addColumn "listener_state" "error" { type := .text, nullable := true },
    .addColumn "listener_state" "revocations" { type := .text, nullable := true },
    .createTable "resident_watch"
      { columns := .ofList
          [("id", { type := .int, nullable := false, autoIncrement := true }),
           ("resident", { type := .text, nullable := false }),
           ("feed", { type := .text, nullable := false }),
           ("key", { type := .text, nullable := false }),
           ("note", { type := .text, nullable := true }),
           ("created_at", { type := .text, nullable := false }),
           ("expires_at", { type := .text, nullable := true })],
        primaryKey := ["id"] },
    .createIndex "resident_watch" { name := "idx_resident_watch_key", keys := [{ column := "resident" }, { column := "feed" }, { column := "key" }], unique := true }]

end Orchestra.Store.Migrations
