import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra

namespace OrchestraTest.ProjectFields

/-! Verify that the `project_id` / `issue_id` fields:
    1. round-trip through `QueueEntry` / `TaskRecord` JSON as bare numbers (taxis's own id
       convention, since a migration to a taxis-backed `Taxis.IssueId`/`Taxis.IssueId` — see the "Migrate
       Project/Issue data layer" tracking issue),
    2. are *omitted* when `none`, so existing on-disk files keep working,
    3. decode fine when entirely absent (backward compat). -/

private def baseEntry : Queue.QueueEntry :=
  { id := "qe-1"
  , createdAt := "2026-05-05T00:00:00Z"
  , upstream := { owner := "org", name := "repo" }
  , fork     := { owner := "me",  name := "repo" }
  , mode     := .pr
  , prompt   := "do x" }

private def baseRecord : TaskStore.TaskRecord :=
  { id := "task-1"
  , createdAt := "2026-05-05T00:00:00Z"
  , upstream := { owner := "org", name := "repo" }
  , fork     := { owner := "me",  name := "repo" }
  , mode     := .pr
  , prompt   := "do x" }

@[test]
def queueEntryOmitsProjectFieldsWhenNone : Test := do
  let s := (ToJson.toJson baseEntry).compress
  TestM.assert (!s.containsSubstr "\"project_id\"")
    "project_id must be omitted when projectId = none"
  TestM.assert (!s.containsSubstr "\"issue_id\"")
    "issue_id must be omitted when issueId = none"

@[test]
def queueEntryRoundTripWithProjectFields : Test := do
  let entry := { baseEntry with
                 projectId := some (⟨42⟩ : Taxis.IssueId)
               , issueId   := some (⟨99⟩ : Taxis.IssueId) }
  let encoded := ToJson.toJson entry
  match FromJson.fromJson? encoded (α := Queue.QueueEntry) with
  | .error e => TestM.fail s!"QueueEntry round-trip: {e}"
  | .ok got =>
    TestM.assertEqual (got.projectId.map (·.val)) (some (42 : Int64))
      (msg := "QueueEntry.projectId round-trip")
    TestM.assertEqual (got.issueId.map (·.val)) (some (99 : Int64))
      (msg := "QueueEntry.issueId round-trip")

@[test]
def queueEntryDecodesLegacyJson : Test := do
  -- A QueueEntry written before this feature: no project_id / issue_id keys.
  let raw := r#"{
    "id":"qe-old","created_at":"2026-01-01T00:00:00Z","status":"pending",
    "upstream":"org/repo","fork":"me/repo","mode":"pr","prompt":"hi"
  }"#
  match Json.parse raw >>= FromJson.fromJson? (α := Queue.QueueEntry) with
  | .error e => TestM.fail s!"legacy QueueEntry decode failed: {e}"
  | .ok got =>
    TestM.assertEqual got.projectId none (msg := "legacy projectId default")
    TestM.assertEqual got.issueId   none (msg := "legacy issueId default")

@[test]
def taskRecordRoundTripWithProjectFields : Test := do
  let r := { baseRecord with
               projectId := some (⟨42⟩ : Taxis.IssueId)
             , issueId   := some (⟨99⟩ : Taxis.IssueId) }
  let encoded := ToJson.toJson r
  match FromJson.fromJson? encoded (α := TaskStore.TaskRecord) with
  | .error e => TestM.fail s!"TaskRecord round-trip: {e}"
  | .ok got =>
    TestM.assertEqual (got.projectId.map (·.val)) (some (42 : Int64))
      (msg := "TaskRecord.projectId round-trip")
    TestM.assertEqual (got.issueId.map (·.val)) (some (99 : Int64))
      (msg := "TaskRecord.issueId round-trip")

@[test]
def taskRecordDecodesLegacyJson : Test := do
  let raw := r#"{
    "id":"task-old","created_at":"2026-01-01T00:00:00Z",
    "upstream":"org/repo","fork":"me/repo","mode":"pr",
    "prompt":"hi","status":"completed"
  }"#
  match Json.parse raw >>= FromJson.fromJson? (α := TaskStore.TaskRecord) with
  | .error e => TestM.fail s!"legacy TaskRecord decode failed: {e}"
  | .ok got =>
    TestM.assertEqual got.projectId none (msg := "legacy TaskRecord projectId")
    TestM.assertEqual got.issueId   none (msg := "legacy TaskRecord issueId")

@[test]
def ioTaskDecodesProjectFields : Test := do
  let raw := r#"{
    "upstream":"org/repo","fork":"me/repo","mode":"pr","prompt":"hi",
    "project_id":42,"issue_id":99
  }"#
  match Json.parse raw >>= FromJson.fromJson? (α := Task) with
  | .error e => TestM.fail s!"IOTask decode failed: {e}"
  | .ok got =>
    TestM.assertEqual (got.ioTask.projectId.map (·.val)) (some (42 : Int64))
      (msg := "IOTask.projectId decode")
    TestM.assertEqual (got.ioTask.issueId.map (·.val)) (some (99 : Int64))
      (msg := "IOTask.issueId decode")

end OrchestraTest.ProjectFields
