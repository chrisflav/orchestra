import OrchestraTest.TestM
import Orchestra

open Orchestra
open Lean (Json ToJson FromJson)

/-!
# The records survive the round trip through the database

A record store is only as good as what comes back out of it, and the way this one can go wrong is
narrow and specific: a field that has no column, a column read into the wrong field, an enumeration
whose name does not round-trip, a list stored as NULL and read back as absent. So the two big
records are written out with *every* optional field set and every default overridden, saved, loaded
and compared whole — a field that got lost on the way has nowhere to hide in that comparison.

Beside that: the write is an upsert and not a second row; the listings come back in the order the
callers rely on, ties broken the way the old in-memory sort broke them; a row this build cannot
read costs that row and no more; the declared schema and the migrations agree; and the legacy
import carries a directory over exactly once.
-/

namespace OrchestraTest.StoreTest

private def withTempData (act : IO α) : IO α := Orchestra.withTempData "store" act

/-! ## Records with nothing left at its default -/

/-- A task record with every optional field set, every default overridden, and a budget that
    only survives as a float — `0.1` is not representable, so a column that rounded or reprinted
    it would come back different. -/
private def fullTask : TaskStore.TaskRecord := {
  id            := "t-full"
  createdAt     := "2026-09-13T10:04:12Z"
  repo          := some { upstream := { owner := "acme", name := "widgets" }
                          fork     := { owner := "bot",  name := "widgets" } }
  mode          := .pr
  prompt        := "rewrite the parser\nwith a second line"
  goal          := some "the suite is green"
  sessionId     := some "sess-1"
  status        := .unfinished
  continuesFrom := some "t-earlier"
  series        := some "parser"
  backend       := some "claude"
  model         := some "opus"
  agent         := some "reviewer"
  systemPrompt  := some "system.md"
  prependPrompt := some "prepend.md"
  budget        := some 0.1
  priority      := 7
  projectId     := some ⟨12⟩
  issueId       := some ⟨34⟩
  role          := some "maintainer"
  identity      := some "ada"
}

/-- A queue entry with the same treatment: every option set, every list non-empty, both JSON
    payloads present, and a spawn policy that is not the empty one. -/
private def fullEntry : Queue.QueueEntry := {
  id                 := "q-full"
  createdAt          := "2026-09-13T10:04:12Z"
  status             := .running
  repo               := some { upstream := { owner := "acme", name := "widgets" }
                               fork     := { owner := "bot",  name := "widgets" } }
  mode               := .pr
  prompt             := "rewrite the parser"
  goal               := some "the suite is green"
  agent              := some "reviewer"
  systemPrompt       := some "system.md"
  prependPrompt      := some "prepend.md"
  backend            := some "claude"
  model              := some "opus"
  continuesFrom      := some "t-earlier"
  series             := some "parser"
  taskId             := some "t-full"
  slot               := some 3
  configPath         := some "/etc/orchestra.json"
  budget             := some 0.1
  memory             := .project
  identity           := some "ada"
  authSource         := some "primary"
  authSources        := ["primary", "spare"]
  authMode           := some .distribute
  tools              := some ["create_pr", "comment"]
  readOnly           := true
  priority           := 7
  concertStepKey     := some "step-3"
  concertId          := some "c-1"
  inputType          := .string
  outputType         := .list .string
  inputJson          := some (Json.str "go")
  outputJson         := some (Json.mkObj [("done", Json.bool true)])
  issueNumber        := some 42
  projectId          := some ⟨12⟩
  issueId            := some ⟨34⟩
  role               := some "maintainer"
  prLabels           := ["automated"]
  triageAddLabels    := ["needs-triage"]
  triageRemoveLabels := ["stale"]
  listenerName       := some "labels"
  spawnPolicy        := some { backends := ["claude"], models := ["opus"], tools := ["comment"]
                               repos := [{ owner := "acme", name := "widgets" }]
                               maxTasks := 3, allowPreClaim := true, maxBudget := some 2.5
                               priority := 4, readOnly := some false }
  spawnedBy          := some "t-parent"
  scopeRoot          := some ⟨56⟩
}

private def sameJson [ToJson α] (a b : α) : Bool :=
  (ToJson.toJson a).compress == (ToJson.toJson b).compress

@[test]
def aFullTaskRecordSurvivesTheRoundTrip : Test := do
  let loaded ← withTempData do
    TaskStore.saveTask fullTask
    TaskStore.loadTask "t-full"
  match loaded with
  | none   => TestM.fail "the record did not come back"
  | some r =>
    TestM.assert (sameJson r fullTask)
      s!"every field survives: got {(ToJson.toJson r).compress}"
    -- Spelled out as well as compared whole, so that a failure says which half went wrong.
    TestM.assert (r.budget == some 0.1) "the budget is the float that was written"
    TestM.assertEqual r.priority 7 (msg := "priority")
    TestM.assert (r.repo.isSome) "the repository pair came back"

@[test]
def aFullQueueEntrySurvivesTheRoundTrip : Test := do
  let loaded ← withTempData do
    Queue.saveEntry fullEntry
    Queue.loadEntry "q-full"
  match loaded with
  | none   => TestM.fail "the entry did not come back"
  | some e =>
    TestM.assert (sameJson e fullEntry)
      s!"every field survives: got {(ToJson.toJson e).compress}"
    TestM.assert (e.authSources == ["primary", "spare"]) "the candidate sources came back"
    TestM.assert (e.tools == some ["create_pr", "comment"]) "the tool list came back"
    TestM.assert (e.spawnPolicy.isSome) "the spawn policy came back"
    TestM.assert (e.inputJson == some (Json.str "go")) "the task input came back"

@[test]
def aConcertRunSurvivesTheRoundTrip : Test := do
  let run : Queue.ConcertRun := {
    id := "c-1", startedAt := "2026-09-13T10:04:12Z", status := .cancelled
    name := some "nightly", workflowFile := some "nightly.yaml"
    finishedAt := some "2026-09-13T11:00:00Z" }
  let loaded ← withTempData do
    Queue.saveConcertRun run
    Queue.loadConcertRun "c-1"
  match loaded with
  | none   => TestM.fail "the run did not come back"
  | some r => TestM.assert (sameJson r run) "every field survives"

/-! ## A save replaces, and does not accumulate -/

@[test]
def savingTwiceLeavesOneRecord : Test := do
  let (all, status) ← withTempData do
    TaskStore.saveTask fullTask
    TaskStore.saveTask { fullTask with status := .completed }
    let all ← TaskStore.loadAllTasks
    pure (all, (← TaskStore.loadTask "t-full").map (·.status))
  TestM.assertEqual all.size 1 (msg := "one row, not two")
  TestM.assert (status == some .completed) "and it is the second write that stands"

@[test]
def savingAnEntryTwiceLeavesOneEntry : Test := do
  let (all, status) ← withTempData do
    Queue.saveEntry fullEntry
    Queue.saveEntry { fullEntry with status := .done }
    let all ← Queue.loadAllEntries
    pure (all, (← Queue.loadEntry "q-full").map (·.status))
  TestM.assertEqual all.size 1 (msg := "one row, not two")
  TestM.assert (status == some .done) "and it is the second write that stands"

/-! ## The order the listings come back in

Newest by `created_at`, with the id breaking ties — which is what `Time.sortNewestFirst` computed
in memory and what the `(created_at, id)` indexes are declared for. The tie matters: ids come from
a clock that restarts at boot, so ordering by id alone puts every pre-reboot record on top. -/

private def stampedTask (id stamp : String) : TaskStore.TaskRecord :=
  { id, createdAt := stamp, repo := none, prompt := "p" }

private def stampedEntry (id stamp : String) : Queue.QueueEntry :=
  { id, createdAt := stamp, repo := none, prompt := "p" }

@[test]
def tasksComeBackNewestFirst : Test := do
  let ids ← withTempData do
    TaskStore.saveTask (stampedTask "t-a" "2026-09-13T10:00:00Z")
    TaskStore.saveTask (stampedTask "t-c" "2026-09-13T09:00:00Z")
    TaskStore.saveTask (stampedTask "t-b" "2026-09-13T10:00:00Z")
    return (← TaskStore.loadAllTasks).map (·.id)
  TestM.assertEqual ids.toList ["t-b", "t-a", "t-c"]
    (msg := "newest first, id breaking the tie")

@[test]
def entriesComeBackNewestFirst : Test := do
  let ids ← withTempData do
    Queue.saveEntry (stampedEntry "q-a" "2026-09-13T10:00:00Z")
    Queue.saveEntry (stampedEntry "q-c" "2026-09-13T09:00:00Z")
    Queue.saveEntry (stampedEntry "q-b" "2026-09-13T10:00:00Z")
    return (← Queue.loadAllEntries).map (·.id)
  TestM.assertEqual ids.toList ["q-b", "q-a", "q-c"]
    (msg := "newest first, id breaking the tie")

/-! ## A row this build cannot read -/

/-- A status name orchestra does not know — what a newer version writing a new state would leave
    behind. The listing loses that row and nothing else; refusing the whole listing would mean one
    such row hiding the entire history. -/
@[test]
def anUnreadableRowIsSkippedNotFatal : Test := do
  let (all, direct) ← withTempData do
    TaskStore.saveTask (stampedTask "t-good" "2026-09-13T10:00:00Z")
    Orchestra.Store.run <| HasModel.insert ({
      id := "t-alien", created_at := "2026-09-13T11:00:00Z"
      upstream := none, fork := none, mode := "fork", prompt := "p"
      goal := none, session_id := none, status := "levitating"
      continues_from := none, series := none, backend := none, model := none
      agent := none, system_prompt := none, prepend_prompt := none
      budget := none, priority := 10, project_id := none, issue_id := none
      role := none, identity := none } : Orchestra.Store.TaskRow)
    pure (← TaskStore.loadAllTasks, ← TaskStore.loadTask "t-alien")
  TestM.assertEqual (all.map (·.id)).toList ["t-good"] (msg := "the readable row still comes back")
  TestM.assert direct.isNone "and the unreadable one reads as absent rather than throwing"

/-! ## The schema and the migrations say the same thing -/

/-- A model change without a migration fails here rather than on a deployment. -/
@[test]
def theMigrationsBuildTheDeclaredSchema : Test := do
  match Db.Migration.planSteps Orchestra.Store.migrations Orchestra.Store.target with
  | .error e    => TestM.fail s!"the schema could not be diffed: {e}"
  | .ok []      => TestM.assert true
  | .ok steps@(_ :: _) =>
    TestM.fail s!"the declared schema is {steps.length} step(s) ahead of the migrations; \
      run `makemigrations` and add the result to `Orchestra.Store.migrations`"

/-! ## The legacy import -/

private def writeJson (path : System.FilePath) (j : Json) : IO Unit := do
  if let some parent := path.parent then IO.FS.createDirAll parent
  IO.FS.writeFile path j.compress

/-- Lay out the four legacy directories as the file stores left them. -/
private def writeLegacyFiles : IO Unit := do
  let base ← Dirs.dataBase
  writeJson (base / "tasks" / "t-old.json") (ToJson.toJson (stampedTask "t-old" "2026-01-01T00:00:00Z"))
  writeJson (base / "tasks" / "t-broken.json") (Json.str "this is not a task record")
  writeJson (base / "series" / "parser.json") (Json.mkObj [("latest_task_id", Json.str "t-old")])
  writeJson (base / "queue" / "q-old.json") (ToJson.toJson (stampedEntry "q-old" "2026-01-01T00:00:00Z"))
  writeJson (base / "concerts" / "c-old.json")
    (ToJson.toJson ({ id := "c-old", startedAt := "2026-01-01T00:00:00Z" } : Queue.ConcertRun))
  -- Not a record: the daemon's own furniture lives in the queue directory too, and the import
  -- has to step over it rather than report it as a file that did not parse.
  IO.FS.writeFile (base / "queue" / "daemon.pid") "4242"

private def markers : IO (Array (String × Int)) := do
  let rows ← Orchestra.Store.run <|
    HasModel.fetch (QuerySet.all (α := Orchestra.Store.LegacyImportRow))
  return rows.map fun r => (r.store, r.records)

@[test]
def theImportCarriesTheFilesOver : Test := do
  let (task, series, entry, run, marks) ← withTempData do
    writeLegacyFiles
    Orchestra.Store.Import.run
    pure (← TaskStore.loadTask "t-old", ← TaskStore.latestInSeries "parser",
          ← Queue.loadEntry "q-old", ← Queue.loadConcertRun "c-old", ← markers)
  TestM.assert task.isSome "the task record is in the database"
  TestM.assert (series == some "t-old") "the series pointer is in the database"
  TestM.assert entry.isSome "the queue entry is in the database"
  TestM.assert run.isSome "the concert run is in the database"
  let named := (marks.map (·.1)).toList
  TestM.assert (named.contains "tasks" && named.contains "series"
      && named.contains "queue entries" && named.contains "concert runs")
    s!"every store is marked as imported: {named}"
  TestM.assert (marks.contains ("tasks", 1))
    s!"the file that did not parse is skipped, not counted: {marks.toList}"

/-- The second run is a no-op, and specifically not one that overwrites what has happened since:
    the marker is what says the directory has been carried over, so a task saved after the import
    must still be there — and must not be replaced by the older file it was written from. -/
@[test]
def theImportRunsOnlyOnce : Test := do
  let (status, marks) ← withTempData do
    writeLegacyFiles
    Orchestra.Store.Import.run
    TaskStore.saveTask { stampedTask "t-old" "2026-01-01T00:00:00Z" with status := .completed }
    Orchestra.Store.Import.run
    pure ((← TaskStore.loadTask "t-old").map (·.status), ← markers)
  TestM.assert (status == some .completed) "the record saved in between is not clobbered"
  TestM.assertEqual marks.size 4 (msg := "one marker per store, still")

/-- Nothing to import is not an event: a fresh installation has no legacy directories, so the
    import writes no markers and says nothing. -/
@[test]
def aFreshInstallationImportsNothing : Test := do
  let marks ← withTempData do
    Orchestra.Store.Import.run
    markers
  TestM.assertEqual marks.size 0 (msg := "no markers at all")

end OrchestraTest.StoreTest
