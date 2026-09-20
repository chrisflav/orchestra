import Db
import Orchestra.Store

/-!
# Carrying the SQLite database over to PostgreSQL, once

`orchestra-migrate-sqlite <path-to-orchestra.db>` reads every row of the record store out of the
SQLite file orchestra used to keep it in and writes it into the PostgreSQL database
`ORCHESTRA_DATABASE_URL` names. It is a one-off for an installation that predates the move, not
part of any command's normal path, which is why it is its own executable: nothing else has a
reason to link SQLite in.

**The order is deliberate.** `legacy_import` goes over with everything else, and that matters more
than its three columns suggest: it is the table saying which of the JSON record directories under
`<data>/` have already been carried into the database. An empty PostgreSQL database without it
would look to `Orchestra.Store.Import` like a fresh installation, and the daemon's next start
would import a thousand-odd stale JSON files on top of the rows this just wrote.

**It refuses a target that already holds rows.** Running it twice would insert every row a second
time, and for the tables whose primary key is a generated id that is not even a conflict — it is
silent duplication. `--force` is there for the case where you have deliberately emptied the target
and want to go again.

**What it does not do.** It does not read or write anything else under `<data>/`: task logs, the
cloned repositories, the dashboard secret and the JSON directories all stay exactly where they
are, and the SQLite file is opened read-only and left in place. Nothing here is destructive, so a
run that goes wrong is undone by emptying the PostgreSQL database and running it again.
-/

open Orchestra

namespace Orchestra.MigrateSqlite

/-- Copy every row of one table, and answer how many there were.

    Read from SQLite and written to PostgreSQL through the same `@[model]` structure, so the two
    sides cannot disagree about what a row is: a column that had drifted would fail to decode on
    the way out rather than arrive wrong.

    One transaction for the whole table. These are small enough — the largest here is a few
    thousand rows — that the simplicity is worth more than batching, and it means a table either
    arrives whole or not at all. -/
private def copyTable (α : Type) [HasModel α] (label : String) (sqlitePath : System.FilePath) :
    IO Nat := do
  let rows ← Sqlite.runDB sqlitePath <| HasModel.fetch (QuerySet.all (α := α))
  if rows.isEmpty then
    IO.println s!"  {label}: 0"
    return 0
  Store.transaction do
    for r in rows do
      HasModel.insert r
  IO.println s!"  {label}: {rows.size}"
  return rows.size

/-- How many rows the target already holds, across every table. Zero is what lets the copy run. -/
private def targetRowCount : IO Int := do
  let mut n : Int := 0
  n := n + (← Store.run <| HasModel.count (QuerySet.all (α := Store.TaskRow)))
  n := n + (← Store.run <| HasModel.count (QuerySet.all (α := Store.SeriesRow)))
  n := n + (← Store.run <| HasModel.count (QuerySet.all (α := Store.QueueEntryRow)))
  n := n + (← Store.run <| HasModel.count (QuerySet.all (α := Store.ConcertRunRow)))
  n := n + (← Store.run <| HasModel.count (QuerySet.all (α := Store.InteractiveSessionRow)))
  n := n + (← Store.run <| HasModel.count (QuerySet.all (α := Store.InteractiveEventRow)))
  n := n + (← Store.run <| HasModel.count (QuerySet.all (α := Store.UsageSourceRow)))
  n := n + (← Store.run <| HasModel.count (QuerySet.all (α := Store.UsageWindowRow)))
  n := n + (← Store.run <| HasModel.count (QuerySet.all (α := Store.ListenerStateRow)))
  n := n + (← Store.run <| HasModel.count (QuerySet.all (α := Store.LegacyImportRow)))
  return n

def run (sqlitePath : System.FilePath) (force : Bool) : IO UInt32 := do
  unless ← sqlitePath.pathExists do
    IO.eprintln s!"No such file: {sqlitePath}"
    return 1

  -- Applies the schema to the target if it is empty, which is the ordinary case here: the
  -- database this is pointed at was created a moment ago and has nothing in it.
  let existing ← targetRowCount
  if existing != 0 && !force then
    IO.eprintln s!"The target database already holds {existing} row(s). Copying into it would \
      duplicate them rather than merge — every table here is keyed by an id the source assigned, \
      so a second copy is a second row and not a conflict. Empty it and run again, or pass \
      --force if that is what you have already done."
    return 1

  IO.println s!"Copying {sqlitePath} into the PostgreSQL database."
  let mut total := 0
  -- `legacy_import` last: until it lands the target is not yet a faithful copy, and stopping
  -- short of it leaves a database that would re-import the JSON directories. Written last, it is
  -- the row that says the rest arrived.
  total := total + (← copyTable Store.TaskRow "task" sqlitePath)
  total := total + (← copyTable Store.SeriesRow "series" sqlitePath)
  total := total + (← copyTable Store.QueueEntryRow "queue_entry" sqlitePath)
  total := total + (← copyTable Store.ConcertRunRow "concert_run" sqlitePath)
  total := total + (← copyTable Store.InteractiveSessionRow "interactive_session" sqlitePath)
  total := total + (← copyTable Store.InteractiveEventRow "interactive_event" sqlitePath)
  total := total + (← copyTable Store.UsageSourceRow "usage_source" sqlitePath)
  total := total + (← copyTable Store.UsageWindowRow "usage_window" sqlitePath)
  total := total + (← copyTable Store.ListenerStateRow "listener_state" sqlitePath)
  total := total + (← copyTable Store.LegacyImportRow "legacy_import" sqlitePath)
  IO.println s!"Copied {total} row(s)."
  return 0

end Orchestra.MigrateSqlite

def main (args : List String) : IO UInt32 := do
  let force := args.contains "--force"
  let positional := args.filter (!·.startsWith "--")
  match positional with
  | [path] => Orchestra.MigrateSqlite.run (System.FilePath.mk path) force
  | _ =>
    IO.eprintln "usage: orchestra-migrate-sqlite [--force] <path-to-orchestra.db>\n\n\
      Copies orchestra's record store out of the SQLite file it used to live in and into the\n\
      PostgreSQL database ORCHESTRA_DATABASE_URL names. Reads the file, writes the database,\n\
      and touches nothing else under <data>/."
    return 1
