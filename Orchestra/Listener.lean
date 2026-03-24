import Lean.Data.Json
import Orchestra.Config
import Orchestra.TaskStore
import Orchestra.Queue

open Lean (Json FromJson ToJson)

namespace Orchestra.Listener

-- Source configuration

inductive SourceConfig where
  | githubIssues    (repo : String) (labels : List String)
  | githubPrReviews (repo : String) (labels : List String)
  /-- Reacts to new issue/PR comments containing `trigger` with a rocket emoji and enqueues a task. -/
  | githubComments  (repo : String) (labels : List String) (trigger : String)
  | shell           (command : String) (args : List String)

instance : ToJson SourceConfig where
  toJson
    | .githubIssues repo labels =>
        Json.mkObj [("type", "github-issues"), ("repo", repo),
                    ("labels", ToJson.toJson labels)]
    | .githubPrReviews repo labels =>
        Json.mkObj [("type", "github-pr-reviews"), ("repo", repo),
                    ("labels", ToJson.toJson labels)]
    | .githubComments repo labels trigger =>
        Json.mkObj [("type", "github-comments"), ("repo", repo),
                    ("labels", ToJson.toJson labels), ("trigger", trigger)]
    | .shell cmd args =>
        Json.mkObj [("type", "shell"), ("command", cmd),
                    ("args", ToJson.toJson args)]

instance : FromJson SourceConfig where
  fromJson? j := do
    let ty ← j.getObjValAs? String "type"
    match ty with
    | "github-issues" =>
        let repo   ← j.getObjValAs? String "repo"
        let labels  := j.getObjValAs? (List String) "labels" |>.toOption |>.getD []
        return .githubIssues repo labels
    | "github-pr-reviews" =>
        let repo   ← j.getObjValAs? String "repo"
        let labels  := j.getObjValAs? (List String) "labels" |>.toOption |>.getD []
        return .githubPrReviews repo labels
    | "github-comments" =>
        let repo    ← j.getObjValAs? String "repo"
        let labels   := j.getObjValAs? (List String) "labels" |>.toOption |>.getD []
        let trigger ← j.getObjValAs? String "trigger"
        return .githubComments repo labels trigger
    | "shell" =>
        let cmd  ← j.getObjValAs? String "command"
        let args  := j.getObjValAs? (List String) "args" |>.toOption |>.getD []
        return .shell cmd args
    | _ => .error s!"unknown source type: {ty}"

-- Action template

structure ActionConfig where
  upstream       : String
  fork           : String
  mode           : TaskMode
  promptTemplate : String
  series         : Option String := none
  backend        : Option String := none
  model          : Option String := none
  agent          : Option String := none
  systemPrompt   : Option String := none
  /-- Maximum spend in USD. Defaults to 4.0 if not set. -/
  budget         : Option Float  := none
  /-- Which memory directories to make available to the agent. Defaults to `both`. -/
  memory         : MemoryMode    := .both

instance : ToJson ActionConfig where
  toJson a :=
    let base : List (String × Json) := [
      ("upstream",        a.upstream),
      ("fork",            a.fork),
      ("mode",            ToJson.toJson a.mode),
      ("prompt_template", a.promptTemplate)
    ]
    let fields := base
    let fields := if let some s := a.series       then fields ++ [("series",        Json.str s)]      else fields
    let fields := if let some s := a.backend      then fields ++ [("backend",       Json.str s)]      else fields
    let fields := if let some s := a.model        then fields ++ [("model",         Json.str s)]      else fields
    let fields := if let some s := a.agent        then fields ++ [("agent",         Json.str s)]      else fields
    let fields := if let some s := a.systemPrompt then fields ++ [("system_prompt", Json.str s)]      else fields
    let fields := if let some b := a.budget       then fields ++ [("budget",        ToJson.toJson b)] else fields
    let fields := fields ++ [("memory", ToJson.toJson a.memory)]
    Json.mkObj fields

instance : FromJson ActionConfig where
  fromJson? j := do
    let upstream       ← j.getObjValAs? String "upstream"
    let fork           ← j.getObjValAs? String "fork"
    let mode           ← j.getObjValAs? TaskMode "mode"
    let promptTemplate ← j.getObjValAs? String "prompt_template"
    let series       := j.getObjValAs? String "series"        |>.toOption
    let backend      := j.getObjValAs? String "backend"       |>.toOption
    let model        := j.getObjValAs? String "model"         |>.toOption
    let agent        := j.getObjValAs? String "agent"         |>.toOption
    let systemPrompt := j.getObjValAs? String "system_prompt" |>.toOption
    -- Accept budget as either a JSON number (2.0) or a JSON string ("2.0")
    let budget : Option Float :=
      match j.getObjVal? "budget" |>.toOption with
      | none => none
      | some (.num n) => some n.toFloat
      | some (.str s) => match Lean.Json.parse s with
          | .ok (.num n) => some n.toFloat
          | _ => none
      | _ => none
    let memory := j.getObjValAs? MemoryMode "memory" |>.toOption |>.getD .both
    return { upstream, fork, mode, promptTemplate, series, backend, model, agent, systemPrompt,
             budget, memory }

-- Listener config

structure ListenerConfig where
  name            : String
  source          : SourceConfig
  action          : ActionConfig
  intervalSeconds : Nat := 60

instance : ToJson ListenerConfig where
  toJson l := Json.mkObj [
    ("name",             l.name),
    ("source",           ToJson.toJson l.source),
    ("action",           ToJson.toJson l.action),
    ("interval_seconds", l.intervalSeconds)
  ]

instance : FromJson ListenerConfig where
  fromJson? j := do
    let name            ← j.getObjValAs? String "name"
    let source          ← j.getObjValAs? SourceConfig "source"
    let action          ← j.getObjValAs? ActionConfig "action"
    let intervalSeconds  := j.getObjValAs? Nat "interval_seconds" |>.toOption |>.getD 60
    return { name, source, action, intervalSeconds }

-- Listener state

structure ListenerState where
  lastChecked  : String       -- ISO 8601 UTC, empty = never
  processedIds : Array String -- source-specific event IDs already queued

instance : ToJson ListenerState where
  toJson s := Json.mkObj [
    ("last_checked",   s.lastChecked),
    ("processed_ids",  ToJson.toJson s.processedIds)
  ]

instance : FromJson ListenerState where
  fromJson? j := do
    let lastChecked  ← j.getObjValAs? String "last_checked"
    let processedIds  := j.getObjValAs? (Array String) "processed_ids" |>.toOption |>.getD #[]
    return { lastChecked, processedIds }

-- Directories

def listenersDir : IO System.FilePath := do
  match ← IO.getEnv "HOME" with
  | some h => return System.FilePath.mk h / ".agent" / "listeners"
  | none   => throw (.userError "HOME not set")

def listenerStateDir : IO System.FilePath := do
  return (← listenersDir) / "state"

-- Config I/O

def loadListenerConfig (name : String) : IO (Option ListenerConfig) := do
  let path := (← listenersDir) / s!"{name}.json"
  if !(← path.pathExists) then return none
  let raw ← IO.FS.readFile path
  match Json.parse raw with
  | .error _ => return none
  | .ok j    =>
    match FromJson.fromJson? j with
    | .error _ => return none
    | .ok cfg  => return some cfg

def loadAllListenerConfigs (dir : System.FilePath) : IO (Array ListenerConfig) := do
  if !(← dir.pathExists) then return #[]
  let entries ← System.FilePath.readDir dir
  let mut configs : Array ListenerConfig := #[]
  for entry in entries do
    let name := entry.fileName
    if !name.endsWith ".json" then continue
    -- skip the state subdirectory entry (it has no .json extension anyway)
    let raw ← IO.FS.readFile entry.path
    match Json.parse raw with
    | .error e => IO.eprintln s!"Warning: failed to parse listener config {name}: {e}"
    | .ok j    =>
      match FromJson.fromJson? j (α := ListenerConfig) with
      | .error e => IO.eprintln s!"Warning: failed to load listener config {name}: {e}"
      | .ok cfg  => configs := configs.push cfg
  return configs

-- State I/O

def loadListenerState (name : String) : IO ListenerState := do
  let path := (← listenerStateDir) / s!"{name}.json"
  if !(← path.pathExists) then return { lastChecked := "", processedIds := #[] }
  let raw ← IO.FS.readFile path
  match Json.parse raw with
  | .error _ => return { lastChecked := "", processedIds := #[] }
  | .ok j    =>
    match FromJson.fromJson? j with
    | .error _ => return { lastChecked := "", processedIds := #[] }
    | .ok s    => return s

def saveListenerState (name : String) (state : ListenerState) : IO Unit := do
  let dir ← listenerStateDir
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / s!"{name}.json") (Lean.Json.compress (ToJson.toJson state))

-- Template rendering

/-- Replace every occurrence of `{{key}}` in `template` with the corresponding value. -/
def renderTemplate (template : String) (vars : List (String × String)) : String :=
  vars.foldl (fun acc (k, v) => acc.replace ("{{" ++ k ++ "}}") v) template

-- Queue entry builder

def buildQueueEntry (action : ActionConfig) (vars : List (String × String)) : IO Queue.QueueEntry := do
  let id        ← TaskStore.generateId
  let createdAt ← TaskStore.currentIso8601
  let prompt    := renderTemplate action.promptTemplate vars
  let series    := action.series.map (renderTemplate · vars)
  let mode      := action.mode
  IO.eprintln s!"[listener] buildQueueEntry: model={repr action.model} budget={repr action.budget} agent={repr action.agent}"
  return {
    id, createdAt, status := .pending,
    upstream     := action.upstream
    fork         := action.fork
    mode
    prompt
    agent        := action.agent
    systemPrompt := action.systemPrompt
    backend      := action.backend
    model        := action.model
    series
    budget       := action.budget
    memory       := action.memory
  }

-- GitHub helpers

private def runGhApi (endpoint : String) (ghToken : String) : IO (Option Json) := do
  let env : Array (String × Option String) :=
    if ghToken.isEmpty then #[] else #[("GH_TOKEN", some ghToken)]
  let child ← IO.Process.spawn {
    cmd  := "gh"
    args := #["api", endpoint, "--paginate"]
    env
    stdin  := .null
    stdout := .piped
    stderr := .null
  }
  let out ← child.stdout.readToEnd
  let _   ← child.wait
  return (Json.parse out.trimAscii.toString).toOption

private def reactToComment (repo : String) (commentId : Nat) (ghToken : String) : IO Unit := do
  let env : Array (String × Option String) :=
    if ghToken.isEmpty then #[] else #[("GH_TOKEN", some ghToken)]
  let child ← IO.Process.spawn {
    cmd  := "gh"
    args := #["api", "--method", "POST",
              s!"/repos/{repo}/issues/comments/{commentId}/reactions",
              "-f", "content=rocket"]
    env
    stdin  := .null
    stdout := .null
    stderr := .null
  }
  let _ ← child.wait

-- Source polling

/--
Poll a source for new events not yet in `state.processedIds`.
Returns an array of `(eventId, templateVars)` pairs.
`eventId` is `""` for shell sources (no deduplication by ID).
-/
def pollSource (source : SourceConfig) (state : ListenerState) (ghToken : String)
    : IO (Array (String × List (String × String))) := do
  match source with

  | .githubIssues repo labels => do
    let labelParam := if labels.isEmpty then "" else "&labels=" ++ ",".intercalate labels
    let endpoint := s!"/repos/{repo}/issues?state=open&per_page=100{labelParam}"
    let jsonOpt ← runGhApi endpoint ghToken
    match jsonOpt with
    | none => return #[]
    | some json =>
      let .ok items := json.getArr? | return #[]
      let mut events : Array (String × List (String × String)) := #[]
      for item in items do
        -- skip pull requests (issues endpoint returns PRs too)
        if (item.getObjVal? "pull_request").isOk then continue
        let .ok numJson := item.getObjVal? "number" | continue
        let numStr := toString numJson
        if state.processedIds.contains numStr then continue
        let title  := item.getObjValAs? String "title"    |>.toOption |>.getD ""
        let body   := item.getObjValAs? String "body"     |>.toOption |>.getD ""
        let url    := item.getObjValAs? String "html_url" |>.toOption |>.getD ""
        let vars   := [("issue_number", numStr), ("title", title), ("body", body), ("url", url)]
        events := events.push (numStr, vars)
      return events

  | .githubPrReviews repo labels => do
    -- Fetch open PRs
    let prJsonOpt ← runGhApi s!"/repos/{repo}/pulls?state=open&per_page=100" ghToken
    let prArr ← match prJsonOpt with
      | none    => return #[]
      | some j  => match j.getArr? with
        | .ok a  => pure a
        | .error _ => return #[]
    let mut events : Array (String × List (String × String)) := #[]
    for pr in prArr do
      let .ok prNum := pr.getObjValAs? Nat "number" | continue
      let prTitle := pr.getObjValAs? String "title" |>.toOption |>.getD ""
      -- Filter by label if any are configured
      if !labels.isEmpty then
        let prLabels : List String :=
          (pr.getObjValAs? (Array Json) "labels" |>.toOption |>.getD #[]).toList.filterMap
            (fun l => l.getObjValAs? String "name" |>.toOption)
        if !labels.any (fun l => prLabels.contains l) then continue
      -- Fetch reviews for this PR
      let reviewJsonOpt ← runGhApi s!"/repos/{repo}/pulls/{prNum}/reviews?per_page=100" ghToken
      let reviews ← match reviewJsonOpt with
        | none   => pure (#[] : Array Json)
        | some j => match j.getArr? with
          | .ok a  => pure a
          | .error _ => pure #[]
      for review in reviews do
        let .ok ridJson := review.getObjVal? "id" | continue
        let ridStr := toString ridJson
        if state.processedIds.contains ridStr then continue
        -- Only include submitted reviews with a non-empty body
        let reviewState := review.getObjValAs? String "state" |>.toOption |>.getD ""
        if reviewState != "COMMENTED" && reviewState != "CHANGES_REQUESTED" &&
           reviewState != "APPROVED" then continue
        let body     := review.getObjValAs? String "body" |>.toOption |>.getD ""
        let reviewer := match review.getObjVal? "user" |>.toOption with
          | none   => ""
          | some u => u.getObjValAs? String "login" |>.toOption |>.getD ""
        let url      := review.getObjValAs? String "html_url" |>.toOption |>.getD ""
        let vars := [
          ("pr_number",  toString prNum),
          ("pr_title",   prTitle),
          ("reviewer",   reviewer),
          ("body",       body),
          ("url",        url)
        ]
        events := events.push (ridStr, vars)
    return events

  | .githubComments repo labels trigger => do
    -- On the first run lastChecked is empty: initialise state and return nothing,
    -- so we don't flood the queue with all historical comments.
    if state.lastChecked.isEmpty then return #[]
    -- Helper: extract an event from one comment JSON object, react with 🚀, return vars.
    let processComment : Json → IO (Option (String × List (String × String))) :=
      fun comment => do
        let .ok idNum := comment.getObjValAs? Nat "id" | return none
        let idStr := toString idNum
        if state.processedIds.contains idStr then return none
        let body := comment.getObjValAs? String "body" |>.toOption |>.getD ""
        -- Only process comments that contain the trigger string
        if !(body.splitOn trigger).length > 1 then return none
        -- React with a rocket emoji (best-effort; ignore failures)
        try reactToComment repo idNum ghToken catch _ => pure ()
        let author   := match comment.getObjVal? "user" |>.toOption with
          | none   => ""
          | some u => u.getObjValAs? String "login" |>.toOption |>.getD ""
        let url      := comment.getObjValAs? String "html_url" |>.toOption |>.getD ""
        -- Extract issue/PR number from the issue_url field
        let issueUrl := comment.getObjValAs? String "issue_url" |>.toOption |>.getD ""
        let issueNum := issueUrl.splitOn "/" |>.getLast? |>.getD ""
        let vars := [("comment_id", idStr), ("body", body), ("author", author),
                     ("url", url), ("issue_number", issueNum)]
        return some (idStr, vars)
    let mut events : Array (String × List (String × String)) := #[]
    if labels.isEmpty then
      -- Use the global issue comments endpoint with a `since` filter
      let endpoint :=
        s!"/repos/{repo}/issues/comments?since={state.lastChecked}&per_page=100&direction=asc"
      let jsonOpt ← runGhApi endpoint ghToken
      let comments ← match jsonOpt with
        | none   => pure (#[] : Array Json)
        | some j => match j.getArr? with
          | .ok a    => pure a
          | .error _ => pure #[]
      for comment in comments do
        if let some ev ← processComment comment then
          events := events.push ev
    else
      -- Fetch only issues/PRs that carry one of the requested labels
      let labelParam := ",".intercalate labels
      let issuesOpt ← runGhApi
        s!"/repos/{repo}/issues?state=open&labels={labelParam}&per_page=100" ghToken
      let issues ← match issuesOpt with
        | none   => pure (#[] : Array Json)
        | some j => match j.getArr? with
          | .ok a    => pure a
          | .error _ => pure #[]
      for issue in issues do
        let .ok issNum := issue.getObjValAs? Nat "number" | continue
        let commentsOpt ← runGhApi
          s!"/repos/{repo}/issues/{issNum}/comments?since={state.lastChecked}&per_page=100"
          ghToken
        let comments ← match commentsOpt with
          | none   => pure (#[] : Array Json)
          | some j => match j.getArr? with
            | .ok a    => pure a
            | .error _ => pure #[]
        for comment in comments do
          if let some ev ← processComment comment then
            events := events.push ev
    return events

  | .shell cmd args => do
    let child ← IO.Process.spawn {
      cmd
      args := args.toArray
      stdin  := .null
      stdout := .piped
      stderr := .null
    }
    let out ← child.stdout.readToEnd
    let _   ← child.wait
    let trimmed := out.trimAscii.toString
    if trimmed.isEmpty then return #[]
    return #[("", [("output", trimmed)])]

end Orchestra.Listener
