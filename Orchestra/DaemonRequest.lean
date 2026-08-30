import Lean.Data.Json
import Orchestra.Queue
import Orchestra.Interactive.Store
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
  /-- Cancel a running task by its queue-entry id, or every running task when no id is given.

      One inductive rather than two because the daemon does the same thing in both cases — it
      cancels tokens it holds — and because an absent `id` is what the wire format already meant
      before there was one: `orchestra queue cancel` sends `{"type": "cancel"}` and keeps
      working unchanged. -/
  | cancel     (id : Option String := none)
  /-- Shut down the daemon, optionally cancelling running tasks first. -/
  | shutdown   (force : Bool := false)
  /-- Start an interactive session and answer with its id.

      Here rather than over HTTP for the same reason `cancel` is: a session is a live process
      with a clone slot and an MCP server behind it, and only the daemon can hold one. The API
      route forwards to this. -/
  | interactiveStart (spec : Interactive.SessionSpec)
  /-- Post a turn to a session. -/
  | interactiveMessage (id : String) (text : String)
  /-- Abandon the turn a session is working on, keeping the session. -/
  | interactiveInterrupt (id : String)
  /-- End a session and release everything it holds. -/
  | interactiveEnd (id : String)
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
    | "cancel"   =>
      let id := j.getObjValAs? String "id" |>.toOption
      return .cancel id
    | "shutdown" =>
      let force := j.getObjValAs? Bool "force" |>.toOption |>.getD false
      return .shutdown force
    | "interactive_start" =>
      let specJson ← j.getObjVal? "spec"
      let upstream ← specJson.getObjValAs? Repository "upstream"
      let fork     ← specJson.getObjValAs? Repository "fork"
      return .interactiveStart {
        upstream, fork
        backend      := specJson.getObjValAs? String "backend"       |>.toOption
        model        := specJson.getObjValAs? String "model"         |>.toOption
        budget       := specJson.getObjValAs? Float  "budget"        |>.toOption
        tools        := (specJson.getObjValAs? (List String) "tools" |>.toOption)
        systemPrompt := specJson.getObjValAs? String "systemPrompt"  |>.toOption
        resumeFrom   := specJson.getObjValAs? String "resumeFrom"    |>.toOption
      }
    | "interactive_message" =>
      let id   ← j.getObjValAs? String "id"
      let text ← j.getObjValAs? String "text"
      return .interactiveMessage id text
    | "interactive_interrupt" =>
      return .interactiveInterrupt (← j.getObjValAs? String "id")
    | "interactive_end" =>
      return .interactiveEnd (← j.getObjValAs? String "id")
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
