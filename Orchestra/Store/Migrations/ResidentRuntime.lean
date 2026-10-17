import Db

/-!
# `0008_resident_runtime`

What the delivery loop knows about a resident beyond its session record
(`docs/design/resident-sessions.md` §7, §10): wake failures in a row and when it may be tried again
(the backoff, and the pause after `Resident.maxFailures`), when recent turns were handed over (for
`max_turns_per_hour`), and when the kleis token its agent process holds expires. Kept in the
database so that a daemon restart neither forgets a resident that keeps failing nor lets one past
its hourly limit. A row of its own rather than columns on `interactive_session`: the session record
is written whole from the copy the manager holds, which would put a stale count back.
-/

namespace Orchestra.Store.Migrations

def migration_0008_resident_runtime : Db.Migration.Migration where
  name := "0008_resident_runtime"
  steps := [
    .createTable "resident_runtime"
      { columns := .ofList
          [("resident", { type := .text, nullable := false }),
           ("failures", { type := .int, nullable := false }),
           ("retry_after", { type := .int, nullable := false }),
           ("turn_stamps", { type := .text, nullable := false }),
           ("token_expires", { type := .int, nullable := true })],
        primaryKey := ["resident"] }]

end Orchestra.Store.Migrations
