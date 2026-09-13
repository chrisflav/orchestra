import Orchestra.Listener
import Orchestra.Queue
import Orchestra.Store
import Orchestra.TaskStore
import Orchestra.Usage

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

Later stages of the port add the interactive sessions and transcripts; `run` below is where their
importers go.
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

/-- Insert the windows of one legacy `<stem>.history.json`, in the order the file lists them.

    Order is the whole of what a window's `AutoKey` means — `loadHistory` reads them back by it —
    so they go in one at a time rather than in whatever order a fold might produce. A file that
    does not parse leaves the source with no history, which is what `loadHistory` made of an
    unreadable one before. -/
private def importUsageHistory (path : System.FilePath) (backend label : String) :
    Sqlite.M Unit := do
  let contents ← IO.FS.readFile path
  match Json.parse contents >>= (·.getObjValAs? (Array Usage.Window) "windows") with
  | .error e =>
    IO.eprintln s!"[orchestra] legacy usage history '{path}' did not parse: {e}"
  | .ok windows =>
    for w in windows do
      HasModel.insert (w.toRow backend label)

/-- Import the usage source state and the history beside it.

    One store rather than two: the state file and its `.history.json` are the same source seen
    from two sides, and a marker per table would let a restart between them import one and not
    the other.

    The document is authoritative about which source it is, not the path. A label is anything
    config says it is and the file name was that flattened to what a filename may hold, so
    `main (spare)` and `main-spare` are one stem and two sources; the `backend` and `label` the
    state carries are the ones they were flattened from. -/
private def importUsage : IO Unit := do
  let dir ← Usage.legacyUsageDir
  importStore "usage sources" dir do
    let mut sources := 0
    for backendEntry in (← dir.readDir) do
      unless ← backendEntry.path.isDir do continue
      let files ← backendEntry.path.readDir
      -- The stems a state file claims, so that the histories left over can be spotted below.
      let mut claimed : Std.HashSet String := {}
      for entry in files do
        unless entry.fileName.endsWith ".json" do continue
        if entry.fileName.endsWith ".history.json" then continue
        let stem := (entry.fileName.dropEnd ".json".length).toString
        claimed := claimed.insert stem
        let contents ← IO.FS.readFile entry.path
        match Json.parse contents >>= FromJson.fromJson? (α := Usage.SourceState) with
        | .error e =>
          IO.eprintln s!"[orchestra] legacy usage state '{entry.fileName}' did not parse: {e}"
        | .ok state =>
          HasModel.insert state.toRow
          sources := sources + 1
          let history := backendEntry.path / s!"{stem}.history.json"
          if ← history.pathExists then
            importUsageHistory history state.backend state.label
      -- A history whose state file is gone — deleted by hand, or never written because every
      -- poll so far failed. The label it belongs to is only recoverable as the stem, which is
      -- the flattened spelling and may not be the label config uses; said out loud, because a
      -- graph filed under a name nothing polls is a graph that stops growing.
      for entry in files do
        unless entry.fileName.endsWith ".history.json" do continue
        let stem := (entry.fileName.dropEnd ".history.json".length).toString
        if claimed.contains stem then continue
        IO.eprintln s!"[orchestra] legacy usage history '{backendEntry.fileName}/{entry.fileName}' \
          has no state file beside it; importing it under '{stem}', which is the file name rather \
          than necessarily the label."
        importUsageHistory entry.path backendEntry.fileName stem
    return sources

/-- Import the listener state.

    Keyed by the file's stem, which is the name a listener has: the config file names the
    listener, and `migrateListenerStateNames` — which carried state written under a config's old
    in-file `name` over to its file name — ran at every daemon start-up until this import
    replaced it. A stem that could not be a listener name is one no config file could match, so
    it is skipped rather than stored under a key nothing will ever look up. -/
private def importListenerState : IO Unit := do
  let dir ← Listener.legacyListenerStateDir
  importStore "listener states" dir do
    let states ← legacyFiles Listener.ListenerState "listener state" dir
    let mut n := 0
    for (name, state) in states do
      match Utils.checkConfigName "listener" name with
      | .error e =>
        IO.eprintln s!"[orchestra] legacy listener state '{name}.json' is not a listener name: {e}"
      | .ok _ =>
        HasModel.insert (state.toRow name)
        n := n + 1
    return n

/-- Carry every legacy directory that is still there into the database.

    Called once at startup by both binaries, before anything else reads a record. Silent when
    there is nothing to do, which on a fresh installation and on every start after the first is
    what happens. -/
def run : IO Unit := do
  importTasks
  importSeries
  importQueueEntries
  importConcertRuns
  importUsage
  importListenerState
  -- Later stages: the interactive sessions and their transcripts. Each is one `importStore`
  -- call, and each takes its own marker, so the stages can land one at a time on a database an
  -- earlier one has imported into.

end Orchestra.Store.Import
