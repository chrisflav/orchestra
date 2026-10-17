import Db

/-!
# `0009_session_tokens`

The kleis tokens a session holds, by revocation id (`docs/design/resident-sessions.md` §7): written
when a session derives or mints one and deleted when it is revoked, so a daemon that died holding
some — a crash, a kill — has them revoked by the next one at its start, as a feed's are from its
listener's state. Only the revocation id is kept: it is what revoking needs, and it grants nothing.
-/

namespace Orchestra.Store.Migrations

def migration_0009_session_tokens : Db.Migration.Migration where
  name := "0009_session_tokens"
  steps := [
    .createTable "session_token"
      { columns := .ofList
          [("revocation_id", { type := .text, nullable := false }),
           ("session_id", { type := .text, nullable := false }),
           ("recorded_at", { type := .text, nullable := false })],
        primaryKey := ["revocation_id"] }]

end Orchestra.Store.Migrations
