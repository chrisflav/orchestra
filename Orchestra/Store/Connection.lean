import Db
import Orchestra.Dirs
import Orchestra.Store.Schema

/-!
# Getting at the database

One file, `<data>/orchestra.db`, and one way in: `Store.run`, which opens a connection, runs the
computation and closes it again.

**A connection per call, not a shared one.** The daemon runs its workers, the reaper, the
listeners and the HTTP server on separate threads, and one connection shared between them would
let their transactions interleave — a `withTransaction` on one thread would wrap whatever another
thread happened to be doing at the time. Opening a connection costs microseconds against a file
the operating system has cached; reading a directory of JSON files is what was slow, not this.

**A busy timeout instead of a lock.** Three processes touch the file — the daemon, the dashboard
(a second container in the compose deployment) and the `orchestra` CLI — and WAL mode plus a ten
second `busy_timeout` is the whole of what SQLite needs to let them. A writer that finds the file
locked waits rather than failing, so a CLI enqueueing while a worker saves a record is not an
error either of them has to handle.

**The schema, once per path per process.** The first `run` against a given path applies the
declared migrations and sets the two pragmas that persist in the file. Remembered per path rather
than globally because `Dirs.setDataBaseOverride` moves the database: a test that switches to a
fresh temporary directory gets a fresh schema, and production pays for it once.

**Two processes migrating at the same time.** The library writes the migration's record first,
inside the migration's own transaction and under the primary key of the tracking table, so the
loser of that race does not apply a step twice — it fails, with `UNIQUE constraint failed:
db_migrations.name`. That failure is the daemon's first `Store.run`, after it has already written
its pid file, so it is not one to let escape: `ensureSchema` catches it and migrates once more.
The second pass finds the migration recorded and does nothing. A failure that is not the race
fails the same way the second time, and that one is rethrown.
-/

namespace Orchestra.Store

/-- Where the database lives: under `Dirs.dataBase`, so `Dirs.setDataBaseOverride` redirects it
    along with everything else a test redirects. -/
def path : IO System.FilePath :=
  return (← Dirs.dataBase) / "orchestra.db"

/-- The paths whose schema this process has already brought up to date. -/
private initialize initialized : IO.Ref (Std.HashSet String) ← IO.mkRef {}

/-- Serialises the first-call work below. Held while the migrations run, so that two threads
    starting at once do not both migrate. -/
private initialize initMutex : Std.BaseMutex ← Std.BaseMutex.new

/-- Open a connection to `p`, give it the busy timeout, and run `x`.

    The one place a connection is opened. `ensureSchema` uses it too, which is why it is separate
    from `run`: going through `run` from inside the first-call guard would deadlock on the mutex
    that guard holds. -/
private def withConnection {α : Type} (p : System.FilePath) (x : Sqlite.M α) : IO α :=
  Sqlite.runDB p do
    let db ← read
    db.busyTimeout 10000
    x

/-- Bring the database at `p` up to the declared schema, unless this process already has.

    Idempotent and cheap after the first call: a `Std.HashSet` lookup. The check inside the lock
    is the one that decides — the one outside it only keeps every later call from taking the
    lock at all.

    The mutex orders this process's own threads; another process starting at the same moment on a
    fresh database is ordered by nothing, and the loser of that race fails on the tracking table's
    primary key. A second pass finds the migration recorded and applies nothing, so that is what a
    failure gets — once. The path is marked as done only if one of the two passes succeeded; a
    `run` that throws here leaves the next one to try again. -/
private def ensureSchema (p : System.FilePath) : IO Unit := do
  if (← initialized.get).contains p.toString then return
  initMutex.lock
  try
    if (← initialized.get).contains p.toString then return
    if let some parent := p.parent then
      IO.FS.createDirAll parent
    let now := (← Std.Time.Timestamp.now).toSecondsSinceUnixEpoch.val
    let bringUpToDate : IO Unit := withConnection p do
      let db ← read
      -- Both persist in the file; setting them on every open would be harmless but pointless.
      -- WAL is what lets a reader and a writer be in the file at the same time, which is the
      -- whole of what the daemon, the dashboard and the CLI need from each other.
      db.exec "PRAGMA journal_mode=WAL"
      db.exec "PRAGMA synchronous=NORMAL"
      let _ ← Db.Migration.migrate migrations now
    try
      bringUpToDate
    catch _ =>
      -- Most likely another process applying the same migration a moment earlier. Whatever it
      -- was, the second pass either finds nothing left to do or fails the same way again, and
      -- that failure is the caller's.
      bringUpToDate
    initialized.modify (·.insert p.toString)
  finally
    initMutex.unlock

/-- Run a database computation: open a connection to `<data>/orchestra.db`, run `x`, close it. -/
def run {α : Type} (x : Sqlite.M α) : IO α := do
  let p ← path
  ensureSchema p
  withConnection p x

/-- Run a database computation as one transaction — all of it, or none of it.

    For the handful of writes that are more than one statement (the legacy import, replacing a
    source's usage history). A single `save` needs none of this: one statement is already atomic,
    and wrapping it would only lengthen the window another process waits on. -/
def transaction {α : Type} (x : Sqlite.M α) : IO α :=
  run (DBMonadTransactional.withTransaction x)

end Orchestra.Store
