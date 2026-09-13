import Orchestra.Queue
import Orchestra.Store
import Orchestra.TaskStore

/-!
# Carrying the JSON files over, once

Every store used to be a directory of one JSON file per record. This reads those directories into
the database the first time a binary that has them starts, and then never looks at them again.

Sits above the stores rather than inside them because it imports all of them: it is the one place
that knows about every legacy directory at once, and each importer is written in terms of the
record's existing `FromJson` and the store's own `toRow`.

**How "once" is decided.** One row of `legacy_import` per store, written with `insertIfAbsent` at
the top of the transaction that does the import. `store` is that table's primary key, so a second
process starting at the same moment blocks on the uncommitted row and then finds it there — it
imports nothing, rather than importing everything a second time. The counts are written back to
the same row when the import finishes, so a marker is never a claim about a number that was not
actually stored.

**Rows are inserted directly, not through `saveEntry`/`saveTask`.** Those open a connection of
their own, which would put every record outside the transaction that is claiming the store.

Files that do not parse are reported and skipped, as their loaders did. The directories are left
exactly where they are: the import copies, and says so, so that a deployment that wants the disk
back deletes them when it is ready rather than having the decision made for it.

Later stages of the port add the interactive sessions and transcripts, the usage state and history
and the listener state; `run` below is where their importers go.
-/

open Lean (Json FromJson ToJson)

namespace Orchestra.Store.Import

/-- Every `<name>.json` in `dir`, as its file stem and parsed content. Anything else in the
    directory is ignored, and anything that does not parse is reported and skipped — which is
    exactly what the loaders these replace did with such a file. -/
private def legacyFiles (α : Type) [FromJson α] (what : String) (dir : System.FilePath) :
    IO (Array (String × α)) := do
  let mut out : Array (String × α) := #[]
  for entry in (← dir.readDir) do
    unless entry.fileName.endsWith ".json" do continue
    let stem := (entry.fileName.dropEnd ".json".length).toString
    let contents ← IO.FS.readFile entry.path
    match Json.parse contents >>= FromJson.fromJson? (α := α) with
    | .ok x    => out := out.push (stem, x)
    | .error e => IO.eprintln s!"[orchestra] legacy {what} '{entry.fileName}' did not parse: {e}"
  return out

/-- Import one store, if its directory is there and nothing has imported it yet.

    `act` does the reading and the inserting and answers how many records it stored; it runs
    inside the transaction that holds the marker, so a failure half way leaves neither rows nor a
    marker claiming they are there. -/
def importStore (store : String) (dir : System.FilePath) (act : Sqlite.M Nat) : IO Unit := do
  unless ← dir.pathExists do return
  let stamp ← TaskStore.currentIso8601
  let imported ← Orchestra.Store.transaction do
    let claimed ← HasModel.insertIfAbsent
      ({ store, imported_at := stamp, records := 0 } : Orchestra.Store.LegacyImportRow)
    -- Somebody else got here first — this process has nothing to do and nothing to say.
    if !claimed then return none
    let n ← act
    HasModel.save ({ store, imported_at := stamp, records := Int.ofNat n } :
      Orchestra.Store.LegacyImportRow)
    return some n
  if let some n := imported then
    IO.println s!"[orchestra] imported {n} {store} from {dir} into the database; \
      that directory is no longer read and can be deleted."

/-- Import the task records. -/
private def importTasks : IO Unit := do
  let dir ← TaskStore.legacyTasksDir
  importStore "tasks" dir do
    let records ← legacyFiles TaskStore.TaskRecord "task record" dir
    for (_, r) in records do
      HasModel.insert r.toRow
    return records.size

/-- Import the series pointers. The name is the file's stem, which is what the pointer was keyed
    by; the document holds only the id it points at. -/
private def importSeries : IO Unit := do
  let dir ← TaskStore.legacySeriesDir
  importStore "series" dir do
    let mut n := 0
    for entry in (← dir.readDir) do
      unless entry.fileName.endsWith ".json" do continue
      let name := (entry.fileName.dropEnd ".json".length).toString
      let contents ← IO.FS.readFile entry.path
      match Json.parse contents >>= (·.getObjValAs? String "latest_task_id") with
      | .ok latest =>
        HasModel.insert ({ name, latest_task_id := latest } : Orchestra.Store.SeriesRow)
        n := n + 1
      | .error e =>
        IO.eprintln s!"[orchestra] legacy series '{entry.fileName}' did not parse: {e}"
    return n

/-- Import the queue entries. Their directory is the daemon's own, so the pid file, the socket
    and the log are in it too; none of them ends in `.json`. -/
private def importQueueEntries : IO Unit := do
  let dir ← Queue.queueDir
  importStore "queue entries" dir do
    let entries ← legacyFiles Queue.QueueEntry "queue entry" dir
    for (_, e) in entries do
      HasModel.insert e.toRow
    return entries.size

/-- Import the concert runs. -/
private def importConcertRuns : IO Unit := do
  let dir ← Queue.legacyConcertsDir
  importStore "concert runs" dir do
    let runs ← legacyFiles Queue.ConcertRun "concert run" dir
    for (_, r) in runs do
      HasModel.insert r.toRow
    return runs.size

/-- Carry every legacy directory that is still there into the database.

    Called once at startup by both binaries, before anything else reads a record. Silent when
    there is nothing to do, which on a fresh installation and on every start after the first is
    what happens. -/
def run : IO Unit := do
  importTasks
  importSeries
  importQueueEntries
  importConcertRuns
  -- Later stages: the interactive sessions and their transcripts, the usage source state and
  -- history, and the listener state. Each is one `importStore` call, and each takes its own
  -- marker, so the stages can land one at a time on a database an earlier one has imported into.

end Orchestra.Store.Import
