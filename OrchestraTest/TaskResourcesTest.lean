import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra

/-!
# A task's own pod resources

A task file may name the container `resources` of its pod (`IOTask.resources`), in place of the
configured `execution.options.resources`, for the build that needs more memory than the default
gives. The field's shape is checked where the task file is read; the queue entry carries it through
its JSON, its row, a retry, a restart resume and a continuation built from a task record; and the
kubernetes backend puts it into the pod manifest whole.
-/

namespace OrchestraTest.TaskResources

private def mentions (msg needle : String) : Bool := (msg.splitOn needle).length > 1

private def big : Json :=
  Json.mkObj [("requests", Json.mkObj [("cpu", "2"), ("memory", "8Gi")]),
              ("limits",   Json.mkObj [("cpu", "12"), ("memory", "28Gi")])]

private def taskOf (body : String) : Except String Task :=
  Json.parse body >>= FromJson.fromJson? (α := Task)

/-! ## Where it is read -/

@[test]
def aTaskFileReadsResourcesAsAnObject : Test := do
  let body := s!"\{\"tasks\": [\{\"prompt\": \"build\", \"resources\": {big.compress}}]}"
  match Json.parse body >>= FromJson.fromJson? (α := TaskFile) with
  | .error e => TestM.fail s!"a task file with resources should parse: {e}"
  | .ok tf => TestM.assert (tf.tasks[0]?.bind (·.ioTask.resources) == some big) "kept verbatim"
  match taskOf "{\"prompt\": \"build\"}" with
  | .error e => TestM.fail s!"a task without resources should parse: {e}"
  | .ok t => TestM.assert t.ioTask.resources.isNone "absent, the configured ones apply"
  match taskOf "{\"prompt\": \"build\", \"resources\": null}" with
  | .error e => TestM.fail s!"null resources should read as absent: {e}"
  | .ok t => TestM.assert t.ioTask.resources.isNone "null is absent"

@[test]
def resourcesThatAreNotAnObjectAreRefused : Test := do
  for bad in ["\"8Gi\"", "[]", "4", "true"] do
    match taskOf s!"\{\"prompt\": \"build\", \"resources\": {bad}}" with
    | .ok _ => TestM.fail s!"resources {bad} should be refused"
    | .error e => TestM.assert (mentions e "resources") s!"{bad}: {e}"

/-! ## The entry carries it -/

private def sized : Queue.QueueEntry :=
  { id := "q-1", createdAt := "2026-11-01T08:00:00Z", status := .unfinished
  , repo := none, prompt := "build", taskId := some "t-1", resources := some big }

@[test]
def anEntryKeepsItsResourcesThroughJsonRowsRetryAndResume : Test := do
  match (FromJson.fromJson? (ToJson.toJson sized) : Except String Queue.QueueEntry) with
  | .error e => TestM.fail s!"the entry should read back: {e}"
  | .ok back => TestM.assert (back.resources == some big) "through its JSON"
  match Queue.QueueEntry.ofRow? sized.toRow with
  | .error e => TestM.fail s!"the row should read back: {e}"
  | .ok back => TestM.assert (back.resources == some big) "through its row"
  let retried := Queue.retryEntryFor sized (some "t-1") "q-2" "2026-11-01T09:00:00Z"
  TestM.assert (retried.resources == some big) "through a retry"
  let record : TaskStore.TaskRecord :=
    { id := "t-1", createdAt := "2026-11-01T08:00:00Z", status := .unfinished
    , sessionId := some "s", repo := none, prompt := "build" }
  match Queue.resumeEntryFor sized record { hasSession := true } "q-3" "2026-11-01T09:00:00Z" with
  | .error e => TestM.fail s!"the run should be resumed: {e}"
  | .ok e => TestM.assert (e.resources == some big) "through a restart resume"
  -- And an entry without any says nothing about it.
  TestM.assert ((ToJson.toJson { sized with resources := none }).getObjVal? "resources" |>.isOk
    |> not) "no resources key on an entry without them"

@[test]
def anUnreadableEntryOverrideCostsOnlyTheOverride : Test := do
  -- Orchestra wrote the entry, so a bad value can only be damage; it falls back to the default
  -- rather than losing the entry.
  let j := (ToJson.toJson sized).setObjVal! "resources" (.str "8Gi")
  match (FromJson.fromJson? j : Except String Queue.QueueEntry) with
  | .error e => TestM.fail s!"the entry should still read: {e}"
  | .ok back => TestM.assert back.resources.isNone "the override is dropped"
  match Queue.QueueEntry.ofRow? { sized.toRow with resources := some "{not json" } with
  | .error e => TestM.fail s!"the row should still read: {e}"
  | .ok back => TestM.assert back.resources.isNone "the override is dropped"

@[test]
def theStoreKeepsResourcesAndAContinuationTakesThem : Test := do
  let (stored, carried) ← Orchestra.withTempData "resources" do
    Queue.saveEntry { sized with status := .done }
    let stored := (← Queue.loadEntry "q-1").bind (·.resources)
    let carried := (← Queue.carriedForContinuation "t-1").2.2
    return (stored, carried)
  TestM.assert (stored == some big) "the column round-trips"
  TestM.assert (carried == some big) "a continuation built from the record takes them"

@[test]
def theResourcesMigrationIsTheThirteenth : Test := do
  let names := Store.migrations.map (·.name)
  TestM.assertEqual ((names.drop 11).take 2) ["0012_checkout", "0013_task_resources"]

/-! ## The pod -/

open Orchestra.Exec Orchestra.Exec.Kubernetes in
private def containerResources (configured : Option Json) (asked : Option Json) : Option Json :=
  let opts := Json.mkObj ([("image", .str "ghcr.io/example/agent:1"),
                           ("mcp_host", .str "orchestra.orchestra.svc.cluster.local")]
                          ++ (configured.map fun r => [("resources", r)]).getD [])
  match Config.fromJson opts with
  | .error _ => some (.str "<unparsed>")
  | .ok cfg =>
    let spec : SessionSpec :=
      { workdir := System.FilePath.mk "/var/lib/orchestra/work/acme-widgets-slot0"
        label := "t-1", grants := #[], resources := asked }
    let m := podManifest cfg spec "orchestra-t-1" cfg.image (stagedPaths cfg "/home/daemon" spec)
    match m.getObjVal? "spec" |>.toOption |>.bind (·.getObjVal? "containers" |>.toOption) with
    | some (.arr cs) => cs[0]?.bind (·.getObjVal? "resources" |>.toOption)
    | _ => some (.str "<no container>")

@[test]
def aTasksResourcesReplaceTheConfiguredOnesWhole : Test := do
  let small := Json.mkObj [("requests", Json.mkObj [("cpu", "500m"), ("memory", "2Gi")]),
                           ("limits",   Json.mkObj [("memory", "4Gi")])]
  TestM.assert (containerResources (some small) (some big) == some big) "the task's win"
  let onlyRequests := Json.mkObj [("requests", Json.mkObj [("memory", "16Gi")])]
  TestM.assert (containerResources (some small) (some onlyRequests) == some onlyRequests)
    "not merged: the configured limits do not come along"
  TestM.assert (containerResources (some small) none == some small)
    "without its own, the configured ones"
  TestM.assert (containerResources none (some big) == some big) "with none configured too"
  TestM.assert (containerResources none none == none) "and neither: no resources at all"
  TestM.assert (containerResources (some small) (some (.str "8Gi")) == some small)
    "something that is not an object never reaches the manifest"

end OrchestraTest.TaskResources
