import Db
import Db.Postgres
import Orchestra.Store.Schema

/-!
# Getting at the database

One PostgreSQL database, and one way in: `Store.run`, which opens a connection, runs the
computation and closes it again.

**A connection per call, not a shared one.** The daemon runs its workers, the reaper, the
listeners and the HTTP server on separate threads, and one connection shared between them would
let their transactions interleave — a `withTransaction` on one thread would wrap whatever another
thread happened to be doing at the time. This costs more than it did against SQLite, where a
connection was an open file: a PostgreSQL connection is a TCP handshake and an authentication
round trip. It is still small against the work on either side of it at this volume — the busiest
thing here is a listener tick, and there are thirteen of those on intervals measured in minutes.
If it ever stops being small, the answer is a pooler in front of the server (pgbouncer in
transaction mode), not a connection shared between threads that must not share one.

**Where the server is.** `ORCHESTRA_DATABASE_URL`, or `setUrlOverride` for tests. There is no
default and no fallback: a wrong guess would silently write the records of a live install into
whatever database happened to answer, and there is no longer a file on disk whose absence would
make the mistake obvious. Absent, every command that touches a record fails with the name of the
variable to set, and every command that does not still works.

**The bounds a wedged query runs into.** `lock_timeout` and `statement_timeout`, set on each
connection because they are per-session settings and each call brings its own session. This is
the half of the move away from SQLite that was actually wanted: a `busy_timeout` could bound a
writer waiting for a lock and nothing else, so a statement that had *acquired* its locks and then
ran forever was unbounded and invisible. These bound both, and `pg_stat_activity` on the server
says which statement is in which state while it is happening rather than afterwards.

`idle_in_transaction_session_timeout` is the backstop under those two: `run` opens and closes a
connection around each computation, so a transaction cannot ordinarily outlive its caller — but a
caller that blocks between statements is exactly the failure this deployment has had, and this is
what stops such a caller from holding a row lock against every other process indefinitely.

**The schema, once per URL per process.** The first `run` against a given URL applies the declared
migrations. Remembered per URL rather than globally because `setUrlOverride` moves the database: a
test that switches to a fresh schema gets a fresh set of migrations applied, and production pays
for it once.

**Two processes migrating at the same time.** The library writes the migration's record first,
inside the migration's own transaction and under the primary key of the tracking table, so the
loser of that race does not apply a step twice — it blocks on the uncommitted row until the winner
commits and then fails with a unique-violation, or finds the row already there. That failure is
the daemon's first `Store.run`, after it has already written its pid file, so it is not one to let
escape: `ensureSchema` catches it and migrates once more. The second pass finds the migration
recorded and does nothing. A failure that is not the race fails the same way the second time, and
that one is rethrown.

This is also why the two containers can start simultaneously. Under SQLite the loser of that race
got `database is locked` as soon as the winner's transaction outlasted the busy timeout, because
the whole file was the unit of contention; here the unit is the row, and waiting for it is what
the loser is supposed to do.
-/

namespace Orchestra.Store

/-- The URL this process was told to use, overriding the environment.

    Set by tests, which give each case a schema of its own rather than a database of its own —
    see `OrchestraTest.TestM.withTempData`. Nothing in production sets it. -/
private initialize urlOverride : IO.Ref (Option String) ← IO.mkRef none

/-- Point the store at `u`, or back at the environment with `none`. Returns nothing: the caller
    that has to restore the previous value reads it with `getUrlOverride` first. -/
def setUrlOverride (u : Option String) : IO Unit :=
  urlOverride.set u

/-- What `setUrlOverride` was last given, so a nested override can put it back. -/
def getUrlOverride : IO (Option String) :=
  urlOverride.get

/-- Where the database is: the override if one is set, otherwise `ORCHESTRA_DATABASE_URL`.

    Throws rather than defaulting. See the module docs — a default here would be a silent write
    into the wrong database, and unlike a missing file there is nothing on disk to notice. -/
def url : IO String := do
  if let some u ← urlOverride.get then return u
  match ← IO.getEnv "ORCHESTRA_DATABASE_URL" with
  | some u => return u
  | none =>
    throw <| IO.userError
      "ORCHESTRA_DATABASE_URL is not set, and orchestra keeps its records in PostgreSQL. \
       Set it to a libpq connection string — postgresql://user:password@host/database — for \
       the database this install's daemon, dashboard and CLI all share."

/-- How long a statement may wait for a lock before giving up, in milliseconds.

    Generous, because the thing most worth waiting for is another process's migration or the
    one-time import of the JSON stores, both of which are a single transaction over every record
    there is. Ten seconds was the SQLite busy timeout and was *shorter than the import it had to
    outlast*, which is how two containers starting together each ended up reporting that the
    other had the database locked. -/
private def lockTimeoutMs : Nat := 60000

/-- How long a single statement may run before the server cancels it, in milliseconds.

    Above anything this schema asks for — the largest query here pages a few hundred rows out of
    a table of a few thousand — and far enough above it that reaching this bound means something
    is wrong rather than something is big. -/
private def statementTimeoutMs : Nat := 120000

/-- How long a session may sit inside an open transaction doing nothing, in milliseconds.

    The backstop described in the module docs: not a bound on work, a bound on a caller that has
    stopped doing any. -/
private def idleInTransactionTimeoutMs : Nat := 120000

/-- Open a connection to `u`, give it the timeouts, and run `x`.

    The one place a connection is opened. `ensureSchema` uses it too, which is why it is separate
    from `run`: going through `run` from inside the first-call guard would deadlock on the mutex
    that guard holds. -/
private def withConnection {α : Type} (u : String) (x : PostgreSQL.M α) : IO α := do
  let result ← PostgreSQL.runDB u do
    -- `SET`, not `SET LOCAL`: these are to hold for every statement the connection goes on to
    -- run, including the ones inside a `withTransaction`, and a `SET LOCAL` outside a
    -- transaction lasts exactly one statement.
    PostgreSQL.execIgnoring s!"SET lock_timeout = {lockTimeoutMs}"
    PostgreSQL.execIgnoring s!"SET statement_timeout = {statementTimeoutMs}"
    PostgreSQL.execIgnoring
      s!"SET idle_in_transaction_session_timeout = {idleInTransactionTimeoutMs}"
    x
  match result with
  | .ok a => return a
  | .error e => throw <| IO.userError (describe e)
where
  /-- The backend's error as something with the database in it. `Repr` alone would say
      `connectionError` and leave the reader to guess which database did not connect. -/
  describe : PostgreSQL.Exception → String
    | .connectionError =>
      s!"could not connect to the orchestra database at {redact u}"
    | .fatal => s!"the orchestra database at {redact u} reported a fatal error"
    | .decodeError m => s!"a value from the orchestra database could not be read: {m}"
    | .migrationError m => s!"the orchestra database schema could not be brought up to date: {m}"
    | .userError m => m
  /-- The URL with its password removed, for a message that may reach a log. libpq accepts
      several spellings; this handles the `scheme://user:password@host` one and leaves anything
      else alone rather than guessing at it. -/
  redact (s : String) : String :=
    match s.splitOn "://" with
    | [scheme, rest] =>
      match rest.splitOn "@" with
      | [userinfo, host] =>
        match userinfo.splitOn ":" with
        | [user, _] => s!"{scheme}://{user}:***@{host}"
        | _ => s
      | _ => s
    | _ => s

/-- The URLs whose schema this process has already brought up to date. -/
private initialize initialized : IO.Ref (Std.HashSet String) ← IO.mkRef {}

/-- Serialises the first-call work below. Held while the migrations run, so that two threads
    starting at once do not both migrate. -/
private initialize initMutex : Std.BaseMutex ← Std.BaseMutex.new

/-- Bring the database at `u` up to the declared schema, unless this process already has.

    Idempotent and cheap after the first call: a `Std.HashSet` lookup. The check inside the lock
    is the one that decides — the one outside it only keeps every later call from taking the
    lock at all.

    The mutex orders this process's own threads; another process starting at the same moment is
    ordered by the tracking table's primary key instead, and the loser of that race fails on it.
    A second pass finds the migration recorded and applies nothing, so that is what a failure
    gets — once. The path is marked as done only if one of the two passes succeeded; a `run` that
    throws here leaves the next one to try again. -/
private def ensureSchema (u : String) : IO Unit := do
  if (← initialized.get).contains u then return
  initMutex.lock
  try
    if (← initialized.get).contains u then return
    let now := (← Std.Time.Timestamp.now).toSecondsSinceUnixEpoch.val
    let bringUpToDate : IO Unit := withConnection u do
      let _ ← Db.Migration.migrate migrations now
    try
      bringUpToDate
    catch _ =>
      -- Most likely another process applying the same migration a moment earlier. Whatever it
      -- was, the second pass either finds nothing left to do or fails the same way again, and
      -- that failure is the caller's.
      bringUpToDate
    initialized.modify (·.insert u)
  finally
    initMutex.unlock

/-- Run a database computation: open a connection to the orchestra database, run `x`, close it. -/
def run {α : Type} (x : PostgreSQL.M α) : IO α := do
  let u ← url
  ensureSchema u
  withConnection u x

/-- Run a database computation as one transaction — all of it, or none of it.

    For the handful of writes that are more than one statement (the legacy import, replacing a
    source's usage history). A single `save` needs none of this: one statement is already atomic,
    and wrapping it would only lengthen the window another process waits on. -/
def transaction {α : Type} (x : PostgreSQL.M α) : IO α :=
  run (DBMonadTransactional.withTransaction x)

end Orchestra.Store
