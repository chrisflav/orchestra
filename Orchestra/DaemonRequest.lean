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
  /-- Withdraw a *pending* entry: mark it `cancelled` before any worker claims it.

      The daemon's because the claim is: done under the lock a claim takes, so an entry is either
      withdrawn or claimed, never claimed and then stamped `cancelled` under its running worker.
      `POST /api/v1/queue/{id}/cancel` sends it for an API client's pending entry. -/
  | withdraw   (id : String)
  /-- Shut down the daemon, optionally cancelling running tasks first (`force`), or handing them
      over to the next daemon rather than waiting for them (`handOff`, what a termination signal
      does; see `Exec.tryHandOff`). Neither: drain, as it always did. -/
  | shutdown   (force : Bool := false) (handOff : Bool := false)
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
  /-- A person's message to a resident: into its inbox, and handed over at once if it can be. -/
  | residentMessage (name : String) (text : String)
  /-- `pause`, `resume` or `reset` a resident. -/
  | residentControl (name : String) (action : String)
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
    | "withdraw" =>
      return .withdraw (← j.getObjValAs? String "id")
    | "shutdown" =>
      let force := j.getObjValAs? Bool "force" |>.toOption |>.getD false
      let handOff := j.getObjValAs? Bool "hand_off" |>.toOption |>.getD false
      return .shutdown force handOff
    | "interactive_start" =>
      let specJson ← j.getObjVal? "spec"
      -- Both or neither: a session without a repository is one with no upstream and no fork.
      -- Present and unreadable is refused rather than read as absent, which would start a
      -- repository-less session somebody meant to have a repository.
      let repo (k : String) : Except String (Option Repository) :=
        match specJson.getObjVal? k with
        | .error _ | .ok Json.null => pure none
        | .ok v => some <$> FromJson.fromJson? v
      let upstream ← repo "upstream"
      let fork     ← repo "fork"
      return .interactiveStart {
        upstream, fork
        backend      := specJson.getObjValAs? String "backend"       |>.toOption
        model        := specJson.getObjValAs? String "model"         |>.toOption
        budget       := specJson.getObjValAs? Float  "budget"        |>.toOption
        tools        := (specJson.getObjValAs? (List String) "tools" |>.toOption)
        systemPrompt := specJson.getObjValAs? String "systemPrompt"  |>.toOption
        resumeFrom   := specJson.getObjValAs? String "resumeFrom"    |>.toOption
        identity     := specJson.getObjValAs? String "identity"      |>.toOption
      }
    | "interactive_message" =>
      let id   ← j.getObjValAs? String "id"
      let text ← j.getObjValAs? String "text"
      return .interactiveMessage id text
    | "interactive_interrupt" =>
      return .interactiveInterrupt (← j.getObjValAs? String "id")
    | "interactive_end" =>
      return .interactiveEnd (← j.getObjValAs? String "id")
    | "resident_message" =>
      return .residentMessage (← j.getObjValAs? String "name") (← j.getObjValAs? String "text")
    | "resident_control" =>
      let action ← j.getObjValAs? String "action"
      unless ["pause", "resume", "reset"].contains action do
        throw s!"unknown resident action '{action}' (pause, resume or reset)"
      return .residentControl (← j.getObjValAs? String "name") action
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
  /-- A message went into a resident's inbox as `item` and could not be handed over yet, and why. -/
  | queued  (item : Nat) (why : String)

instance : ToJson DaemonResponse where
  toJson
    | .ok         => Json.mkObj [("ok",    Json.bool true)]
    | .withId id  => Json.mkObj [("id",    Json.str  id)]
    | .error msg  => Json.mkObj [("error", Json.str  msg)]
    | .queued i w => Json.mkObj [("queued", Lean.toJson i), ("why", Json.str w)]

end Orchestra.DaemonRequest
