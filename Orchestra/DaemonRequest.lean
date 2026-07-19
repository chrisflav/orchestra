import Lean.Data.Json
import Orchestra.Queue
import Taxis.Domain

open Lean (Json FromJson ToJson)

namespace Orchestra.DaemonRequest

/-- All valid messages a client can send to the queue daemon socket. -/
inductive DaemonRequest where
  /-- Persist a pre-built queue entry. -/
  | addTask    (entry : Queue.QueueEntry)
  /-- Start a concert from a workflow YAML file. -/
  | addConcert (workflowFile : String)
               (vars         : Option Json   := none)
               (configPath   : Option String := none)
  /-- Cancel all currently running tasks. -/
  | cancel
  /-- Shut down the daemon, optionally cancelling running tasks first. -/
  | shutdown   (force : Bool := false)
  /-- Acquire the orchestra-project claim on `issueId` for `taskId`.
      Routed to `ClaimManager.tryClaim` inside the daemon so the in-process
      mutex serialises CLI claims against agent claims. -/
  | claimIssue (projectId : Taxis.IssueId) (issueId : Taxis.IssueId)
               (taskId : String) (agent : String) (series : Option String := none)

instance : FromJson DaemonRequest where
  fromJson? j := do
    let ty ← j.getObjValAs? String "type"
    match ty with
    | "add_task" =>
      let entryJson ← j.getObjVal? "entry"
      let entry ← (FromJson.fromJson? entryJson : Except String Queue.QueueEntry)
      return .addTask entry
    | "add_concert" =>
      let wf    ← j.getObjValAs? String "workflow_file"
      let vars    := j.getObjVal?   "vars"        |>.toOption
      let cfgPath := j.getObjValAs? String "config_path" |>.toOption
      return .addConcert wf vars cfgPath
    | "cancel"   => return .cancel
    | "shutdown" =>
      let force := j.getObjValAs? Bool "force" |>.toOption |>.getD false
      return .shutdown force
    | "claim_issue" =>
      let pid    ← j.getObjValAs? Taxis.IssueId "project_id"
      let iid    ← j.getObjValAs? Taxis.IssueId "issue_id"
      let taskId ← j.getObjValAs? String "task_id"
      let agent  ← j.getObjValAs? String "agent"
      let series := j.getObjValAs? String "series" |>.toOption
      return .claimIssue pid iid taskId agent series
    | t => throw s!"unknown request type: {t}"

/-- All valid responses the daemon can send back over the socket. -/
inductive DaemonResponse where
  /-- Successful operation with no notable return value. -/
  | ok
  /-- Successful operation that produced an ID. -/
  | withId  (id      : String)
  /-- The request failed. -/
  | error   (message : String)

instance : ToJson DaemonResponse where
  toJson
    | .ok         => Json.mkObj [("ok",    Json.bool true)]
    | .withId id  => Json.mkObj [("id",    Json.str  id)]
    | .error msg  => Json.mkObj [("error", Json.str  msg)]

end Orchestra.DaemonRequest
