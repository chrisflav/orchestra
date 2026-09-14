import Orchestra.Interactive.Store
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

**How "once" is decided.** One row of `legacy_import` per store. The marker is *read* first, on a
plain connection: on every start after the first there is one there, and this then costs one
indexed select rather than a write transaction per store for the life of the installation. Only
when it is absent is the transaction entered, and the write is still an `insertIfAbsent` — `store`
is that table's primary key, so a second process starting at the same moment blocks on the
uncommitted row and then finds it there, and imports nothing rather than importing everything a
second time. The counts are written back to the same row when the import finishes, so a marker is
never a claim about a number that was not actually stored.

**A marker is only claimed for a directory that held something.** Two of these directories are
live: `<data>/queue` is the daemon's own — its pid file, its socket, its log — and `<data>/tasks`
holds the `--debug` transcripts. On a fresh installation the daemon creates them, and an import
that claimed them would be saying it had carried over a store that never existed, and would then
not notice the legacy files a later `orchestra migrate` copies in. So an importer answers with
both numbers — how many candidate records it found, and how many of them it stored — and a store
that offered no candidates at all is left unclaimed for next time.

**Rows are inserted directly, not through `saveEntry`/`saveTask`.** Those open a connection of
their own, which would put every record outside the transaction that is claiming the store.

**Nothing here is allowed to take the rest of the import down.** A file that does not parse is
reported and skipped, as its loader did; so is one that cannot be read at all. A record whose id
is already in the database is reported and left alone — the database holds the newer truth, and
the file is what it was written from. And each store's import is wrapped on its own in `run`, so
that a directory this process cannot read costs that store and not the ones after it. The
alternative was what this replaced: one bad record, and every later store failed to import on
every single start, forever.

The directories are left exactly where they are: the import copies, and says so, so that a
deployment that wants the disk back deletes them when it is ready rather than having the decision
made for it.

Every store the port covers has its importer here, one `importStore` call each in `run` below.
-/

open Lean (Json FromJson ToJson)

namespace Orchestra.Store.Import

/-- Read a legacy file, reporting and skipping one that cannot be read at all.

    Separate from "did not parse": a directory carried over by hand can hold a file this process
    has no permission for, a dangling symlink, or a name that is itself a directory, and
    `IO.FS.readFile` throws on each. Unguarded, one such file used to abort the whole import —
    and with it every store after it — on every single start. -/
private def readFile? (what : String) (path : System.FilePath) : IO (Option String) := do
  try
    return some (← IO.FS.readFile path)
  catch e =>
    IO.eprintln s!"[orchestra] legacy {what} '{path}' could not be read: {e}"
    return none

/-- Every `<name>.json` in `dir`, as its file stem and parsed content, and how many such files
    there were.

    Anything else in the directory is ignored, and anything that does not read or does not parse
    is reported and skipped — which is exactly what the loaders these replace did with such a
    file. The count is of candidates rather than of successes: a directory holding only records
    this build cannot read still held records, and `importStore` marks it as dealt with rather
    than reporting the same broken files at every start until someone removes them. -/
private def legacyFiles (α : Type) [FromJson α] (what : String) (dir : System.FilePath) :
    IO (Nat × Array (String × α)) := do
  let mut candidates := 0
  let mut out : Array (String × α) := #[]
  for entry in (← dir.readDir) do
    unless entry.fileName.endsWith ".json" do continue
    candidates := candidates + 1
    let stem := (entry.fileName.dropEnd ".json".length).toString
    let some contents ← readFile? what entry.path | continue
    match Json.parse contents >>= FromJson.fromJson? (α := α) with
    | .ok x    => out := out.push (stem, x)
    | .error e => IO.eprintln s!"[orchestra] legacy {what} '{entry.fileName}' did not parse: {e}"
  return (candidates, out)

/-- Store one legacy record, unless the database already has one under its key.

    The database copy is the newer truth — it is what a running orchestra has been writing, where
    the file is what it was written from — so a collision leaves it alone and says so. Throwing
    instead, which is what a plain `insert` does, rolled the store back with no marker and left
    every later store unimported, on this start and on every one after it. -/
private def insertNew {α : Type} [HasModel α] (what id : String) (row : α) : PostgreSQL.M Bool := do
  if ← HasModel.insertIfAbsent row then
    return true
  IO.eprintln s!"[orchestra] legacy {what} '{id}' is already in the database; \
    leaving the stored record as it is."
  return false

open Db.Query.DSL in
/-- Import one store, if its directory is there and nothing has imported it yet.

    `act` does the reading and the inserting and answers with two numbers: how many candidate
    records the directory held, and how many of them are now rows. It runs inside the transaction
    that holds the marker, so a failure half way leaves neither rows nor a marker claiming they
    are there.

    The marker is read on a plain connection first, so that the start after the import — and
    every start after that, for the life of the installation — costs a select rather than a write
    lock per store. It is claimed only for a directory that held at least one candidate: the
    daemon's own `<data>/queue` and the `--debug` transcripts under `<data>/tasks` are directories
    that exist on a fresh installation and hold no records at all, and one that gains legacy files
    later (through `orchestra migrate`, say) is still there to be picked up. -/
def importStore (store : String) (dir : System.FilePath) (act : PostgreSQL.M (Nat × Nat)) :
    IO Unit := do
  unless ← dir.pathExists do return
  let marker ← Orchestra.Store.run <| HasModel.fetch <| query% do
    let r ← from Orchestra.Store.LegacyImportRow
    guard r.store = store
    select r
  unless marker.isEmpty do return
  let stamp ← TaskStore.currentIso8601
  let outcome ← Orchestra.Store.transaction do
    let claimed ← HasModel.insertIfAbsent
      ({ store, imported_at := stamp, records := 0 } : Orchestra.Store.LegacyImportRow)
    -- Somebody else got here first — this process has nothing to do and nothing to say.
    if !claimed then return none
    let (candidates, imported) ← act
    if candidates == 0 then
      -- Not a legacy directory, or not one yet. Give the marker back rather than claim a store
      -- that was never there and stop looking at a directory that may still fill up.
      let _ ← HasModel.delete (α := Orchestra.Store.LegacyImportRow)
        (.eq (.var Orchestra.Store.LegacyImportRowIndex.store .text) (.text store))
      return none
    HasModel.save ({ store, imported_at := stamp, records := Int.ofNat imported } :
      Orchestra.Store.LegacyImportRow)
    return some (candidates, imported)
  match outcome with
  | none => return
  | some (candidates, imported) =>
    if imported > 0 then
      IO.println s!"[orchestra] imported {imported} {store} from {dir} into the database; \
        the JSON records in that directory are no longer read."
    else
      IO.println s!"[orchestra] found {candidates} {store} in {dir} and imported none of them; \
        the JSON records in that directory are no longer read."

/-- Import the task records. The directory is the live one — the `--debug` transcripts are
    written beside them, under names that end in `.jsonl` rather than `.json`. -/
private def importTasks : IO Unit := do
  let dir ← TaskStore.legacyTasksDir
  importStore "tasks" dir do
    let (candidates, records) ← legacyFiles TaskStore.TaskRecord "task record" dir
    let mut n := 0
    for (_, r) in records do
      if ← insertNew "task record" r.id r.toRow then n := n + 1
    return (candidates, n)

/-- Import the series pointers. The name is the file's stem, which is what the pointer was keyed
    by; the document holds only the id it points at. -/
private def importSeries : IO Unit := do
  let dir ← TaskStore.legacySeriesDir
  importStore "series" dir do
    let mut candidates := 0
    let mut n := 0
    for entry in (← dir.readDir) do
      unless entry.fileName.endsWith ".json" do continue
      candidates := candidates + 1
      let name := (entry.fileName.dropEnd ".json".length).toString
      let some contents ← readFile? "series pointer" entry.path | continue
      match Json.parse contents >>= (·.getObjValAs? String "latest_task_id") with
      | .ok latest =>
        if ← insertNew "series pointer" name
            ({ name, latest_task_id := latest } : Orchestra.Store.SeriesRow) then
          n := n + 1
      | .error e =>
        IO.eprintln s!"[orchestra] legacy series '{entry.fileName}' did not parse: {e}"
    return (candidates, n)

/-- Import the queue entries. Their directory is the daemon's own, so the pid file, the socket
    and the log are in it too; none of them ends in `.json`. -/
private def importQueueEntries : IO Unit := do
  let dir ← Queue.queueDir
  importStore "queue entries" dir do
    let (candidates, entries) ← legacyFiles Queue.QueueEntry "queue entry" dir
    let mut n := 0
    for (_, e) in entries do
      if ← insertNew "queue entry" e.id e.toRow then n := n + 1
    return (candidates, n)

/-- Import the concert runs. -/
private def importConcertRuns : IO Unit := do
  let dir ← Queue.legacyConcertsDir
  importStore "concert runs" dir do
    let (candidates, runs) ← legacyFiles Queue.ConcertRun "concert run" dir
    let mut n := 0
    for (_, r) in runs do
      if ← insertNew "concert run" r.id r.toRow then n := n + 1
    return (candidates, n)

/-- Read a legacy transcript, tolerating a tail torn mid-character.

    `IO.FS.readFile` throws outright on invalid UTF-8, one level below the per-line recovery
    below, so it never gets the chance. A daemon killed in the middle of writing a multi-byte
    character (this repo's own tool output is full of `→` and `✓`) left a transcript that threw
    on *every* read: not one lost event, the whole conversation unreadable. A truncated code
    point is at most three bytes short, so trimming back to the last valid boundary recovers
    everything written before the tear.

    Damage anywhere but the tail is not something that writer could produce; such a file is
    reported and its session imported without a transcript, rather than taking the whole import
    down with it. -/
private def transcriptText? (path : System.FilePath) : IO (Option String) := do
  let bytes ←
    try
      IO.FS.readBinFile path
    catch e =>
      IO.eprintln s!"[orchestra] the legacy transcript at {path} could not be read ({e}); the \
session is imported without it."
      return none
  if let some s := String.fromUTF8? bytes then return some s
  for back in [1, 2, 3] do
    if bytes.size ≥ back then
      if let some s := String.fromUTF8? (bytes.extract 0 (bytes.size - back)) then
        return some s
  IO.eprintln s!"[orchestra] the legacy transcript at {path} is not valid UTF-8, and not merely \
torn at the end; the session is imported without it."
  return none

/-- Import the interactive sessions and their transcripts.

    One legacy session was a directory of two files, so the unit here is the directory: anything
    under `<data>/interactive` holding a `session.json` is a session, and the `events.jsonl`
    beside it is its transcript. A line that does not parse, or that carries no `seq`, is skipped
    exactly as `readEvents` skipped it — it can only be a torn write, and a transcript's last
    line is the one most likely to be one.

    Events are written with `save` rather than `insert`: a legacy transcript is a file that was
    appended to across restarts, and two lines claiming the same seq are something it could hold.
    The later one wins, and the import does not fail on the whole store because of one of them.

    The count is of sessions, which is the store's record; the transcript is part of a session
    rather than a record of its own. -/
private def importInteractive : IO Unit := do
  let dir ← Interactive.legacySessionsDir
  importStore "interactive sessions" dir do
    let mut candidates := 0
    let mut sessions := 0
    for entry in (← dir.readDir) do
      let recordPath := entry.path / "session.json"
      unless ← recordPath.pathExists do continue
      candidates := candidates + 1
      let some contents ← readFile? "session" recordPath | continue
      match Json.parse contents >>= FromJson.fromJson? (α := Interactive.SessionRecord) with
      | .error e =>
        IO.eprintln s!"[orchestra] legacy session '{entry.fileName}' did not parse: {e}"
      | .ok record =>
        unless ← insertNew "session" record.id record.toRow do continue
        sessions := sessions + 1
        let transcript := entry.path / "events.jsonl"
        if ← transcript.pathExists then
          if let some text ← transcriptText? transcript then
            for line in text.splitOn "\n" do
              let line := line.trimAscii.toString
              if line.isEmpty then continue
              let some j := (Json.parse line).toOption | continue
              let some seq := j.getObjValAs? Nat "seq" |>.toOption | continue
              HasModel.save
                ({ session_id  := record.id
                   seq         := Int.ofNat seq
                   occurred_at := j.getObjValAs? String "occurredAt" |>.toOption
                                    |>.getD record.createdAt
                   doc         := j.compress } : Orchestra.Store.InteractiveEventRow)
    return (candidates, sessions)

/-- Insert the windows of one legacy `<stem>.history.json`, in the order the file lists them.

    Order is the whole of what a window's `AutoKey` means — `loadHistory` reads them back by it —
    so they go in one at a time rather than in whatever order a fold might produce. A file that
    does not parse leaves the source with no history, which is what `loadHistory` made of an
    unreadable one before. -/
private def importUsageHistory (path : System.FilePath) (backend label : String) :
    PostgreSQL.M Unit := do
  let some contents ← readFile? "usage history" path | return
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
    let mut candidates := 0
    let mut sources := 0
    for backendEntry in (← dir.readDir) do
      unless ← backendEntry.path.isDir do continue
      let files ← backendEntry.path.readDir
      -- The stems a state file claims, so that the histories left over can be spotted below.
      let mut claimed : Std.HashSet String := {}
      for entry in files do
        unless entry.fileName.endsWith ".json" do continue
        if entry.fileName.endsWith ".history.json" then continue
        candidates := candidates + 1
        let stem := (entry.fileName.dropEnd ".json".length).toString
        claimed := claimed.insert stem
        let some contents ← readFile? "usage state" entry.path | continue
        match Json.parse contents >>= FromJson.fromJson? (α := Usage.SourceState) with
        | .error e =>
          IO.eprintln s!"[orchestra] legacy usage state '{entry.fileName}' did not parse: {e}"
        | .ok state =>
          unless ← insertNew "usage state" s!"{state.backend}/{state.label}" state.toRow do
            continue
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
        candidates := candidates + 1
        IO.eprintln s!"[orchestra] legacy usage history '{backendEntry.fileName}/{entry.fileName}' \
          has no state file beside it; importing it under '{stem}', which is the file name rather \
          than necessarily the label."
        importUsageHistory entry.path backendEntry.fileName stem
    return (candidates, sources)

/-- Import the listener state.

    Keyed by the file's stem, which is the name a listener has: the config file names the
    listener, and `migrateListenerStateNames` — which carried state written under a config's old
    in-file `name` over to its file name — ran at every daemon start-up until this import
    replaced it. A stem that could not be a listener name is one no config file could match, so
    it is skipped rather than stored under a key nothing will ever look up. -/
private def importListenerState : IO Unit := do
  let dir ← Listener.legacyListenerStateDir
  importStore "listener states" dir do
    let (candidates, states) ← legacyFiles Listener.ListenerState "listener state" dir
    let mut n := 0
    for (name, state) in states do
      match Utils.checkConfigName "listener" name with
      | .error e =>
        IO.eprintln s!"[orchestra] legacy listener state '{name}.json' is not a listener name: {e}"
      | .ok _ =>
        if ← insertNew "listener state" name (state.toRow name) then n := n + 1
    return (candidates, n)

/-- Run one store's import, reporting a failure of it rather than passing it on.

    The stores are independent of each other, and the import happens before the daemon or the
    dashboard has started: a directory that cannot be listed, a disk that fills up half way
    through one store, is a reason to carry the others over and say what went wrong, not a reason
    for orchestra not to start. Nothing is half imported by this — each store's work is one
    transaction, so the one that failed rolled back, marker and all, and will be tried again on
    the next start. -/
private def guarded (store : String) (act : IO Unit) : IO Unit := do
  try
    act
  catch e =>
    IO.eprintln s!"[orchestra] the legacy {store} could not be imported: {e}. \
      The other stores are unaffected, and this one is tried again on the next start."

/-- Carry every legacy directory that is still there into the database.

    Called once at startup by both binaries, before anything else reads a record. Silent when
    there is nothing to do, which on a fresh installation and on every start after the first is
    what happens. -/
def run : IO Unit := do
  guarded "tasks" importTasks
  guarded "series" importSeries
  guarded "queue entries" importQueueEntries
  guarded "concert runs" importConcertRuns
  guarded "interactive sessions" importInteractive
  guarded "usage sources" importUsage
  guarded "listener states" importListenerState

end Orchestra.Store.Import
