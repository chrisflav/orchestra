import Lean.Data.Json
import Orchestra.Config
import Orchestra.TaskStore
import Orchestra.Queue
import Orchestra.Project
import Orchestra.GitHub

open Lean (Json FromJson ToJson)

namespace Orchestra.Listener

-- Repo entry: upstream + fork as Repository values

/-- A source/fork repo pair for a listener.
    `upstream` is the repository to watch (e.g. `"my-account/orchestra"`).
    `fork` is the fork repository to use for the action (e.g. `"my-fork/orchestra"`). -/
structure RepoEntry where
  upstream : Repository
  fork     : Repository
deriving BEq, Repr

instance : ToJson RepoEntry where
  toJson e := Json.mkObj [("upstream", ToJson.toJson e.upstream), ("fork", ToJson.toJson e.fork)]

instance : FromJson RepoEntry where
  fromJson? j := do
    let upstream ← j.getObjValAs? Repository "upstream"
    let fork     ← j.getObjValAs? Repository "fork"
    return { upstream, fork }

-- Source configuration

inductive SourceConfig where
  /-- Poll open issues. Optionally filtered by `labels`.
      If `trigger` is non-empty, only issues whose body contains `trigger` will fire.
      Only users in `authorizedUsers` may trigger (empty = allow all). -/
  | githubIssues    (repos : List RepoEntry) (labels : List String) (trigger : String)
                    (authorizedUsers : List String)
  /-- Poll PR reviews. Optionally filtered by `labels`.
      If `trigger` is non-empty, only reviews whose body contains `trigger` will fire.
      Only users in `authorizedUsers` may trigger (empty = allow all). -/
  | githubPrReviews (repos : List RepoEntry) (labels : List String) (trigger : String)
                    (authorizedUsers : List String)
  /-- Reacts to new issue/PR comments containing `trigger` with a rocket emoji and enqueues a task.
      Only users in `authorizedUsers` may trigger (empty = allow all). -/
  | githubComments  (repos : List RepoEntry) (labels : List String) (trigger : String)
                    (authorizedUsers : List String)
  | shell             (command : String) (args : List String)
  /-- Auto-dispatch project work using role templates.
      `caps` maps role name → maximum concurrent active tasks of that role.
      Each tick the dispatcher counts active per-role queue entries and emits
      a synthetic event per role that is below its cap and whose trigger holds. -/
  | projectDispatcher (projectId : Taxis.IssueId) (caps : List (String × Nat))
  /-- Auto-dispatch across *every* project: work on any taxis issue in scope for `label`,
      wherever it lives in the tracker. Same role templates and `caps` semantics as
      `projectDispatcher`; the difference is only where the issue set and the target come from.

      In scope means the issue *or any ancestor* carries the label, so labelling a project opts
      its whole subtree in; and only leaves are dispatched, since an issue with children has been
      decomposed and those children are the work (`Project.dispatchCandidates`).

      With no project behind these issues there is no `defaultTarget` to inherit, so each issue's
      repository and branch are read off `repository` / `github-branch` artifacts on it or an
      ancestor (`Project.artifactTarget`). An issue missing either is reported and skipped —
      dispatching an agent at a guessed repository is worse than not dispatching. -/
  | labelDispatcher   (label : String) (caps : List (String × Nat))
  /-- Auto-dispatch tracker-wide supervisory roles — a manager that surveys every project
      rather than working inside one.

      Unlike the project- and label-dispatchers there is no issue set and no target: roles are
      spawned unbound, with no project, no issue and no repository. Only the `always` trigger
      is meaningful, since the others all key off an issue set this source does not have;
      roles carrying any other trigger are reported and skipped.

      Cadence is the listener's own `interval_seconds` combined with a cap of 1: the manager
      runs, finishes, and is respawned on the next tick that finds it under its cap. -/
  | managerDispatcher (caps : List (String × Nat))
  /-- Fires whenever the number of open issues or pull requests with the given
      labels on a repository is strictly below `max`.  Emits at most one task per
      tick; the tick is skipped while a task from this listener is already pending
      or running.
      `kind` controls what is counted: `"issues"` (default), `"pulls"`, or `"all"`.
      Template variables: `count`, `max`, `needed` (= max − count), `upstream`, `fork`. -/
  | githubLabelCount  (repos : List RepoEntry) (labels : List String) (max : Nat) (kind : String)
  /-- Fires once for each open issue or pull request that carries at least one of
      the configured `labels` (empty = any label).  Unlike `githubIssues`, this
      source covers **both** issues and pull requests.
      `kind` controls what is matched: `"issues"`, `"pulls"`, or `"all"` (default).
      Only users in `authorizedUsers` may trigger (empty = allow all).
      Template variables: `issue_number`, `title`, `body`, `url`, `author`,
      `labels` (all labels on the item, comma-separated),
      `matched_labels` (subset that matched the configured list),
      `is_pr` (`"true"` / `"false"`), `upstream`, `fork`. -/
  | githubLabels (repos : List RepoEntry) (labels : List String) (kind : String)
                 (authorizedUsers : List String)

instance : ToJson SourceConfig where
  toJson
    | .githubIssues repos labels trigger authorizedUsers =>
        Json.mkObj [("type", "github-issues"),
                    ("repos", ToJson.toJson repos),
                    ("labels", ToJson.toJson labels),
                    ("trigger", trigger),
                    ("authorized_users", ToJson.toJson authorizedUsers)]
    | .githubPrReviews repos labels trigger authorizedUsers =>
        Json.mkObj [("type", "github-pr-reviews"),
                    ("repos", ToJson.toJson repos),
                    ("labels", ToJson.toJson labels),
                    ("trigger", trigger),
                    ("authorized_users", ToJson.toJson authorizedUsers)]
    | .githubComments repos labels trigger authorizedUsers =>
        Json.mkObj [("type", "github-comments"),
                    ("repos", ToJson.toJson repos),
                    ("labels", ToJson.toJson labels),
                    ("trigger", trigger),
                    ("authorized_users", ToJson.toJson authorizedUsers)]
    | .shell cmd args =>
        Json.mkObj [("type", "shell"), ("command", cmd),
                    ("args", ToJson.toJson args)]
    | .projectDispatcher pid caps =>
        Json.mkObj [("type", "project-dispatcher"),
                    ("project_id", ToJson.toJson pid),
                    ("caps", Json.mkObj
                       (caps.map (fun (n, c) => (n, Json.num c))))]
    | .labelDispatcher label caps =>
        Json.mkObj [("type", "label-dispatcher"),
                    ("label", Json.str label),
                    ("caps", Json.mkObj
                       (caps.map (fun (n, c) => (n, Json.num c))))]
    | .managerDispatcher caps =>
        Json.mkObj [("type", "manager-dispatcher"),
                    ("caps", Json.mkObj
                       (caps.map (fun (n, c) => (n, Json.num c))))]
    | .githubLabelCount repos labels max kind =>
        Json.mkObj [("type", "github-label-count"),
                    ("repos", ToJson.toJson repos),
                    ("labels", ToJson.toJson labels),
                    ("max", Json.num max),
                    ("kind", kind)]
    | .githubLabels repos labels kind authorizedUsers =>
        Json.mkObj [("type", "github-labels"),
                    ("repos", ToJson.toJson repos),
                    ("labels", ToJson.toJson labels),
                    ("kind", kind),
                    ("authorized_users", ToJson.toJson authorizedUsers)]

/-- Parse a `repos` list from JSON.  If `"repos"` is absent, fall back to the singular
    `"fork"` string (treated as both `upstream` and `fork`). -/
private def parseRepos (j : Json) : Except String (List RepoEntry) :=
  match j.getObjValAs? (List RepoEntry) "repos" |>.toOption with
  | some rs => .ok rs
  | none    =>
    match j.getObjValAs? Repository "fork" with
    | .ok r  => .ok [{ upstream := r, fork := r }]
    | .error e => .error e

instance : FromJson SourceConfig where
  fromJson? j := do
    let ty ← j.getObjValAs? String "type"
    match ty with
    | "github-issues" =>
        let repos          ← parseRepos j
        let labels          := j.getObjValAs? (List String) "labels" |>.toOption |>.getD []
        let trigger         := j.getObjValAs? String "trigger" |>.toOption |>.getD ""
        let authorizedUsers := j.getObjValAs? (List String) "authorized_users" |>.toOption |>.getD []
        return .githubIssues repos labels trigger authorizedUsers
    | "github-pr-reviews" =>
        let repos          ← parseRepos j
        let labels          := j.getObjValAs? (List String) "labels" |>.toOption |>.getD []
        let trigger         := j.getObjValAs? String "trigger" |>.toOption |>.getD ""
        let authorizedUsers := j.getObjValAs? (List String) "authorized_users" |>.toOption |>.getD []
        return .githubPrReviews repos labels trigger authorizedUsers
    | "github-comments" =>
        let repos          ← parseRepos j
        let labels          := j.getObjValAs? (List String) "labels" |>.toOption |>.getD []
        let trigger        ← j.getObjValAs? String "trigger"
        let authorizedUsers := j.getObjValAs? (List String) "authorized_users" |>.toOption |>.getD []
        return .githubComments repos labels trigger authorizedUsers
    | "shell" =>
        let cmd  ← j.getObjValAs? String "command"
        let args  := j.getObjValAs? (List String) "args" |>.toOption |>.getD []
        return .shell cmd args
    | "project-dispatcher" =>
        let pid  ← j.getObjValAs? Taxis.IssueId "project_id"
        let capsObj := j.getObjVal? "caps" |>.toOption |>.getD (Json.mkObj [])
        let pairs := capsObj.getObj? |>.toOption |>.map (·.toList) |>.getD []
        let caps : List (String × Nat) := pairs.filterMap fun (k, v) =>
          v.getNat?.toOption.map (k, ·)
        return .projectDispatcher pid caps
    | "label-dispatcher" =>
        let label ← j.getObjValAs? String "label"
        let capsObj := j.getObjVal? "caps" |>.toOption |>.getD (Json.mkObj [])
        let pairs := capsObj.getObj? |>.toOption |>.map (·.toList) |>.getD []
        let caps : List (String × Nat) := pairs.filterMap fun (k, v) =>
          v.getNat?.toOption.map (k, ·)
        return .labelDispatcher label caps
    | "manager-dispatcher" =>
        let capsObj := j.getObjVal? "caps" |>.toOption |>.getD (Json.mkObj [])
        let pairs := capsObj.getObj? |>.toOption |>.map (·.toList) |>.getD []
        let caps : List (String × Nat) := pairs.filterMap fun (k, v) =>
          v.getNat?.toOption.map (k, ·)
        return .managerDispatcher caps
    | "github-label-count" =>
        let repos  ← parseRepos j
        let labels  := j.getObjValAs? (List String) "labels" |>.toOption |>.getD []
        let max    ← j.getObjValAs? Nat "max"
        let kind    := j.getObjValAs? String "kind" |>.toOption |>.getD "issues"
        return .githubLabelCount repos labels max kind
    | "github-labels" =>
        let repos          ← parseRepos j
        let labels          := j.getObjValAs? (List String) "labels" |>.toOption |>.getD []
        let kind            := j.getObjValAs? String "kind" |>.toOption |>.getD "all"
        let authorizedUsers := j.getObjValAs? (List String) "authorized_users" |>.toOption |>.getD []
        return .githubLabels repos labels kind authorizedUsers
    | _ => .error s!"unknown source type: {ty}"

-- Action template

structure ActionConfig where
  /-- Upstream org/name. May be a template string (e.g. `"{{upstream}}"`).
      Defaults to `""`, in which case the `upstream` template variable is used. -/
  upstream       : String := ""
  /-- Fork org/name. May be a template string (e.g. `"{{fork}}"`).
      Defaults to `""`, in which case the `fork` template variable is used. -/
  fork           : String := ""
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
  /-- Label of the authentication source to use. Must match a label in the backend's `auth_sources`. -/
  authSource     : Option String := none
  /-- Optional tools to enable beyond the always-available ones.
      When absent, allowed tools are derived from `mode` for backwards compatibility. -/
  tools          : Option (List String) := none
  /-- If true, the project folder is mounted read-only in the sandbox. -/
  readOnly       : Bool := false
  /-- Priority of the queue entry. Defaults to 10. -/
  priority       : Nat  := 10
  /-- Path to a workflow YAML file. When set, a concert is started instead of a
      single task. Template variables are applied to the workflow's upstream/fork
      before conversion. -/
  workflowPath   : Option String := none
  /-- Issue/PR number to associate with the task. May be a template string
      (e.g. `"{{pr_number}}"`) or a literal (e.g. `"69"`). When absent the
      `issue_number` template variable provided by the event source is used. -/
  issueNumber    : Option String := none
  /-- Labels to apply automatically to every PR created via `create_pr` during this task. -/
  prLabels       : List String   := []

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
    let fields := if let some s := a.authSource   then fields ++ [("auth_source",   Json.str s)]      else fields
    let fields := if let some t := a.tools        then fields ++ [("tools",         ToJson.toJson t)] else fields
    let fields := if a.readOnly                   then fields ++ [("read_only",      Json.bool true)]  else fields
    let fields := if a.priority != 10             then fields ++ [("priority",        Json.num a.priority)] else fields
    let fields := if let some p := a.workflowPath then fields ++ [("workflow_path",  Json.str p)]          else fields
    let fields := if let some n := a.issueNumber  then fields ++ [("issue_number",   Json.str n)]          else fields
    let fields := if !a.prLabels.isEmpty          then fields ++ [("pr_labels",      ToJson.toJson a.prLabels)] else fields
    Json.mkObj fields

instance : FromJson ActionConfig where
  fromJson? j := do
    let upstream       := j.getObjValAs? String "upstream" |>.toOption |>.getD ""
    let fork           := j.getObjValAs? String "fork"     |>.toOption |>.getD ""
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
    let authSource := j.getObjValAs? String "auth_source" |>.toOption
    let tools := j.getObjValAs? (List String) "tools" |>.toOption
    let readOnly := j.getObjValAs? Bool "read_only" |>.toOption |>.getD false
    let priority     := j.getObjValAs? Nat    "priority"      |>.toOption |>.getD 10
    let workflowPath := j.getObjValAs? String "workflow_path" |>.toOption
    let issueNumber  := j.getObjValAs? String "issue_number"  |>.toOption
    let prLabels     := j.getObjValAs? (List String) "pr_labels" |>.toOption |>.getD []
    return { upstream, fork, mode, promptTemplate, series, backend, model, agent, systemPrompt,
             budget, memory, authSource, tools, readOnly, priority, workflowPath, issueNumber,
             prLabels }

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
  /-- When false the daemon skips this listener each tick. Toggled via
      `orchestra listener enable/disable` without editing config files. -/
  enabled      : Bool := true

instance : ToJson ListenerState where
  toJson s := Json.mkObj [
    ("last_checked",   s.lastChecked),
    ("processed_ids",  ToJson.toJson s.processedIds),
    ("enabled",        Json.bool s.enabled)
  ]

instance : FromJson ListenerState where
  fromJson? j := do
    let lastChecked  ← j.getObjValAs? String "last_checked"
    let processedIds  := j.getObjValAs? (Array String) "processed_ids" |>.toOption |>.getD #[]
    let enabled       := j.getObjValAs? Bool "enabled" |>.toOption |>.getD true
    return { lastChecked, processedIds, enabled }

-- Directories

def listenersConfigDir : IO System.FilePath :=
  return (← Dirs.configBase) / "listeners"

def listenerStateDir : IO System.FilePath :=
  return (← Dirs.dataBase) / "listeners" / "state"

-- Config I/O

def loadListenerConfig (name : String) : IO (Option ListenerConfig) := do
  let path := (← listenersConfigDir) / s!"{name}.json"
  if !(← path.pathExists) then return none
  let secrets ← loadSecrets
  let raw := applySecrets secrets (← IO.FS.readFile path)
  match Json.parse raw with
  | .error _ => return none
  | .ok j    =>
    match FromJson.fromJson? j with
    | .error _ => return none
    | .ok cfg  => return some cfg


def loadAllListenerConfigs : IO (Array ListenerConfig) := do
  let dir ← listenersConfigDir
  if !(← dir.pathExists) then return #[]
  let secrets ← loadSecrets
  let entries ← System.FilePath.readDir dir
  let mut configs : Array ListenerConfig := #[]
  for entry in entries do
    let name := entry.fileName
    if !name.endsWith ".json" then continue
    -- skip the state subdirectory entry (it has no .json extension anyway)
    let raw := applySecrets secrets (← IO.FS.readFile entry.path)
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

def buildQueueEntry (action : ActionConfig) (vars : List (String × String))
    (listenerName : Option String := none) : IO Queue.QueueEntry := do
  let id        ← TaskStore.generateId
  let createdAt ← TaskStore.currentIso8601
  let prompt    := renderTemplate action.promptTemplate vars
  let series    := action.series.map (renderTemplate · vars)
  let mode      := action.mode
  -- Render upstream/fork through templates; fall back to {{upstream}}/{{fork}} vars if empty.
  let lookupVar (key : String) : String :=
    vars.find? (fun p => p.1 == key) |>.map (·.2) |>.getD ""
  let upstreamStr :=
    let rendered := renderTemplate action.upstream vars
    if rendered.isEmpty then lookupVar "upstream" else rendered
  let forkStr :=
    let rendered := renderTemplate action.fork vars
    if rendered.isEmpty then lookupVar "fork" else rendered
  let upstream ← IO.ofExcept (Repository.parse upstreamStr)
  let fork     ← IO.ofExcept (Repository.parse forkStr)
  -- Resolve issue_number: prefer the explicit template field from the action config;
  -- fall back to the `issue_number` variable supplied by the event source.
  let issueNumber : Option Nat :=
    match action.issueNumber with
    | some tmpl => (renderTemplate tmpl vars).toNat?
    | none      => vars.find? (fun p => p.1 == "issue_number") |>.map (·.2) |>.bind (·.toNat?)
  IO.eprintln s!"[listener] buildQueueEntry: model={repr action.model} budget={repr action.budget} agent={repr action.agent} priority={action.priority}"
  return {
    id, createdAt, status := .pending,
    upstream
    fork
    mode
    prompt
    agent        := action.agent
    systemPrompt := action.systemPrompt
    backend      := action.backend
    model        := action.model
    series
    budget       := action.budget
    memory       := action.memory
    authSource   := action.authSource
    tools        := action.tools
    readOnly     := action.readOnly
    priority     := action.priority
    issueNumber
    prLabels     := action.prLabels
    listenerName
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

private def reactToComment (repo : String) (commentId : Nat) (ghToken : String)
    (inline : Bool := false) : IO Unit := do
  let env : Array (String × Option String) :=
    if ghToken.isEmpty then #[] else #[("GH_TOKEN", some ghToken)]
  let resource := if inline then "pulls" else "issues"
  let child ← IO.Process.spawn {
    cmd  := "gh"
    args := #["api", "--method", "POST",
              s!"/repos/{repo}/{resource}/comments/{commentId}/reactions",
              "-f", "content=rocket"]
    env
    stdin  := .null
    stdout := .null
    stderr := .null
  }
  let _ ← child.wait

-- Authorization helper

/-- Return the effective allowed-user list: source list if non-empty, else global list. -/
private def effectiveAllowed (sourceUsers globalUsers : List String) : List String :=
  if !sourceUsers.isEmpty then sourceUsers else globalUsers

/-- Return `true` if `author` is allowed to trigger given the effective allowed list.
    An empty list means "allow everyone". -/
private def isAuthorized (allowed : List String) (author : String) : Bool :=
  allowed.isEmpty || allowed.contains author

-- Dispatcher decision (pure on its inputs, so it's easy to test).

/-- Inputs to one tick of the project-dispatcher: the active per-role tally
    that the daemon already keeps, the project's current issues, and the
    user-configured caps. Outputs the role spawns (≤1 per role per tick) the
    dispatcher wants to enqueue. -/
structure DispatcherInput where
  /-- Currently active queue entries (status pending|running) for this project,
      grouped by role name. Only roles that appear in `caps` need to be counted. -/
  activeByRole : Std.HashMap String Nat := {}
  /-- Issues a worker may be dispatched onto: already narrowed by `Project.workableIssues` /
      `Project.dispatchCandidates` to those that are open with no open children and no open
      dependencies. The narrowing happens in the caller because it needs the state of issues that
      are *not* workable — children and dependencies that have closed — which this set by
      definition does not contain. -/
  issues       : Array Project.Issue
  /-- Issues awaiting review: open, with an unmerged pull request attached. Kept separate from
      `issues` because the two sets differ — a container with children is not work, but it can
      still have a pull request of its own that needs reviewing, and merge state is resolved by
      the caller since it costs a GitHub call. -/
  reviewable   : Array Project.Issue := #[]
  /-- Caps from the listener config: role name → maximum concurrent. -/
  caps         : List (String × Nat)
  /-- Roles available for this project (project files override globals).
      Roles without a `dispatch` policy are skipped. -/
  roles        : Array Project.Role

/-- A single role to spawn this tick. `issueId` is set when the role's
    trigger is `hasOpenIssues` and we picked a specific issue to bind to. -/
structure RoleSpawn where
  roleName : String
  issueId  : Option Taxis.IssueId := none
deriving Repr, Inhabited

/-- Why a role was or was not dispatched this tick. Carried out of the decision logic rather than
    logged inside it, so the reasoning is available to the caller without making the decision
    impure — and so the log lines and the behaviour cannot drift apart. -/
inductive DispatchOutcome where
  /-- Dispatched, bound to this issue when the trigger picks one. -/
  | spawn (issueId : Option Taxis.IssueId)
  /-- No cap configured for the role, or it is zero, so auto-dispatch is off. -/
  | notEnabled
  /-- At or over the configured cap. -/
  | atCap (active cap : Nat)
  /-- The listener names a role that no role file defines. -/
  | roleMissing
  /-- The role file has no `dispatch` block, so it is manual-spawn only. -/
  | noDispatchPolicy
  /-- Trigger is `has_open_issues`, but nothing is workable — or everything workable was already
      taken by another role this tick. -/
  | noWorkableIssue (workable alreadyTaken : Nat)
  /-- Trigger is `has_in_review_issues`, but nothing is awaiting review. -/
  | nothingToReview
  /-- Trigger is `idle`, but there is still work or review outstanding. -/
  | notIdle (workable reviewable : Nat)
deriving Repr, Inhabited

structure RoleDecision where
  roleName : String
  trigger  : Option Project.RoleTrigger
  outcome  : DispatchOutcome
deriving Repr, Inhabited

private def triggerName : Project.RoleTrigger → String
  | .hasOpenIssues     => "has_open_issues"
  | .hasInReviewIssues => "has_in_review_issues"
  | .idle              => "idle"
  | .always            => "always"

/-- One log line explaining what was checked for a role and what was decided. -/
def renderDecision (d : RoleDecision) : String :=
  let trig := d.trigger.map (fun t => s!" ({triggerName t})") |>.getD ""
  let verdict := match d.outcome with
    | .spawn (some iid) => s!"DISPATCH, bound to issue {iid.toString}"
    | .spawn none       => "DISPATCH (no issue bound)"
    | .notEnabled       => "skip: no cap configured for this role (auto-dispatch is opt-in)"
    | .atCap active cap => s!"skip: {active} active, cap {cap}"
    | .roleMissing      => "skip: named in caps but no role file defines it"
    | .noDispatchPolicy => "skip: role has no dispatch policy, so it is manual-spawn only"
    | .noWorkableIssue workable taken =>
        s!"skip: no workable issue ({workable} workable, {taken} already taken this tick)"
    | .nothingToReview  => "skip: nothing awaiting review"
    | .notIdle workable reviewable =>
        s!"skip: not idle ({workable} workable, {reviewable} awaiting review)"
  s!"  role '{d.roleName}'{trig}: {verdict}"

/-- Pure decision logic, reporting a verdict for every role the listener names. Spawns at most one
    of each role per tick to avoid bursts; if you want N at once across consecutive ticks, set the
    cap and the dispatcher will fill up gradually. -/
def dispatcherDecisions (input : DispatcherInput) : Array RoleDecision := Id.run do
  let mut decisions : Array RoleDecision := #[]
  let mut taken : Array String := #[]
  for (roleName, cap) in input.caps do
    let role? := input.roles.find? (·.name == roleName)
    let trigger := role?.bind (·.dispatch.map (·.trigger))
    let record (o : DispatchOutcome) : RoleDecision := { roleName, trigger, outcome := o }
    if cap == 0 then
      decisions := decisions.push (record .notEnabled); continue
    let some role := role? | decisions := decisions.push (record .roleMissing); continue
    let some dispatch := role.dispatch
      | decisions := decisions.push (record .noDispatchPolicy); continue
    let active := input.activeByRole.getD roleName 0
    if active >= cap then
      decisions := decisions.push (record (.atCap active cap)); continue
    match dispatch.trigger with
    | .hasOpenIssues =>
      -- Everything in `issues` is already workable, so the only thing left to avoid is spawning
      -- two workers onto the same issue in one tick.
      match input.issues.find? (fun i => !taken.contains i.id.toString) with
      | none =>
        decisions := decisions.push
          (record (.noWorkableIssue input.issues.size taken.size))
      | some issue =>
        taken := taken.push issue.id.toString
        decisions := decisions.push (record (.spawn (some issue.id)))
    | .hasInReviewIssues =>
      -- Bound to the issue being reviewed, like `hasOpenIssues` binds the one being worked. Safe
      -- for the claim protocol because reviewer roles set `pre_claim: false`; nothing claims it.
      -- Drawn from `reviewable` rather than `issues`, so a container with a pull request of its
      -- own still gets reviewed even though no worker would be dispatched onto it.
      match input.reviewable.find? (fun i => !taken.contains i.id.toString) with
      | none => decisions := decisions.push (record .nothingToReview)
      | some issue =>
        taken := taken.push issue.id.toString
        decisions := decisions.push (record (.spawn (some issue.id)))
    | .idle =>
      if input.issues.isEmpty && input.reviewable.isEmpty then
        decisions := decisions.push (record (.spawn none))
      else
        decisions := decisions.push
          (record (.notIdle input.issues.size input.reviewable.size))
    | .always =>
      -- Unbound on purpose, so nothing is added to `taken`: this role picks its own work and
      -- claims it through the daemon's claim manager, which is what keeps two of them off the
      -- same issue. The dispatcher has no issue to reserve on its behalf and no reason to keep
      -- one out of another role's set — see `splitForDispatch`, which this trigger bypasses.
      decisions := decisions.push (record (.spawn none))
  return decisions

/-- The spawns among `dispatcherDecisions`. -/
def dispatcherTick (input : DispatcherInput) : Array RoleSpawn :=
  (dispatcherDecisions input).filterMap fun d =>
    match d.outcome with
    | .spawn issueId => some { roleName := d.roleName, issueId }
    | _ => none

/-! ### Bound and unbound roles in one `caps` block

The label-dispatcher places issue-bound roles and `always` roles by different rules — the former
per issue, the latter per labelled root — so a single `caps` block has to be split before either
can be counted. Both halves are pure so the cap arithmetic, which is what stops a runaway, can be
tested without a tracker. -/

/-- Partition `caps` into (issue-bound roles, unbound `always` roles). A capped name that no role
    defines stays on the bound side, where `dispatcherDecisions` reports it as `roleMissing`
    rather than dropping it silently. -/
def splitCapsByBinding (roles : Array Project.Role) (caps : List (String × Nat)) :
    List (String × Nat) × List (String × Nat) :=
  let isUnbound (roleName : String) : Bool :=
    match (roles.find? (·.name == roleName)).bind (·.dispatch) with
    | some d => d.trigger == .always
    | none   => false
  (caps.filter (fun (n, _) => !isUnbound n), caps.filter (fun (n, _) => isUnbound n))

/-- Per-role tally of the active unbound entries scoped to `root`.

    Counts entries with **no** `issueId` and `projectId == root`, which is exactly how
    `buildRoleEntry` stamps an unbound spawn. The issue-bound tally cannot be reused: it keys on
    `issueId` being in the labelled set, so every unbound entry falls through it, leaving the
    role's cap permanently unreached and spawning a fresh one on every single tick. -/
def unboundActiveByRole (entries : Array Queue.QueueEntry) (root : Taxis.IssueId) :
    Std.HashMap String Nat := Id.run do
  let mut active : Std.HashMap String Nat := {}
  for e in entries do
    if !(e.status == .pending || e.status == .running) then continue
    if e.issueId.isSome then continue
    if e.projectId != some root then continue
    if let some r := e.role then
      active := active.insert r ((active.getD r 0) + 1)
  return active

/-! ## Review routing

An open issue carrying a pull request belongs to exactly one role, and which one is derived
rather than stored. Before the taxis migration an `.inReview` status kept the reviewer's set and
the implementor's set apart by construction; the migration made "awaiting review" derived
(`classifyReview` below) but left `Project.workableIssues` filtering on bare `.open`, so the two
sets silently started to overlap. Whichever role a dispatcher happens to evaluate first — which
is alphabetical, since `caps` is parsed from a sorted `Json.obj` — then takes the issue and the
other never sees it. `splitForDispatch` is what re-establishes the split. -/

/-- What has to happen next on an open issue that carries pull requests, and therefore which
    role should pick it up. -/
inductive ReviewDisposition where
  /-- An unmerged pull request with no outstanding change request: a reviewer decides. -/
  | awaitingReview
  /-- The latest review verdict asked for changes, so the ball is back with an implementor.
      Deliberately *not* reviewable: sending it to a reviewer again would loop it between
      reviewers forever while the requested changes were never made. -/
  | changesRequested
  /-- Every attached pull request has landed. Merging is not completing — that is a separate
      `decide_issue complete` call — so this is still a reviewer's move, not an implementor's.
      Leaving it out of both sets is what let a merged issue fall back to an implementor and be
      worked a second time. -/
  | merged
deriving Repr, BEq, DecidableEq, Inhabited

/-- The disposition implied by the two facts that cost a network call to establish. Pure, so the
    routing table is testable without GitHub or taxis. -/
def dispositionOf (anyUnmerged requestsChanges : Bool) : ReviewDisposition :=
  if !anyUnmerged then .merged
  else if requestsChanges then .changesRequested
  else .awaitingReview

/-- Whether the most recent review verdict on `iid` asked for changes.

    taxis returns comments ordered by id, so the last one carrying a verdict is the current one;
    a plain comment posted afterwards does not clear a rejection, only a later verdict does.
    Answers `false` when the thread cannot be read, which routes the issue to a reviewer — the
    same direction `isPrMerged` fails in, and the one that cannot silently drop work. -/
def latestReviewRequestsChanges (iid : Taxis.IssueId) : IO Bool := do
  try
    let comments ← Project.loadComments iid
    return (comments.filterMap (·.review)).back? == some .requestChanges
  catch _ => return false

/-- Classify those of `issues` (whose `attachedPRs` must already be populated) that carry a pull
    request. Issues without one are absent from the result: nothing about review applies to them,
    and they are an implementor's on the structural test alone.

    Costs a GitHub call per pull request and a taxis call per issue per tick, which is why this
    lives here rather than in the pure selection. `isPrMerged` answers `false` when it cannot
    tell, so an unreachable GitHub queues a reviewer that finds nothing to do rather than
    silently dropping review of real work. -/
def classifyReview (ghToken : String) (issues : Array Project.Issue) :
    IO (Array (Project.Issue × ReviewDisposition)) := do
  let mut out : Array (Project.Issue × ReviewDisposition) := #[]
  for i in issues do
    if i.status != .open || i.attachedPRs.isEmpty then continue
    let mut anyUnmerged := false
    for pr in i.attachedPRs do
      unless anyUnmerged do
        unless ← GitHub.isPrMerged ghToken pr.repo pr.number do anyUnmerged := true
    let requestsChanges ← if anyUnmerged then latestReviewRequestsChanges i.id else pure false
    out := out.push (i, dispositionOf anyUnmerged requestsChanges)
  return out

/-- Split the dispatcher's two candidate sets so that no issue is offered to both roles.

    `structuralWorkable` is what `Project.workableIssues` (or the label dispatcher's `work` set)
    produced knowing only the issue tree — open, no open children, nothing blocking. `classified`
    adds what only a network call can say. An issue a reviewer owns is removed from the
    implementor's set; one sent back for changes stays there, which is the whole point of
    distinguishing it. -/
def splitForDispatch (structuralWorkable : Array Project.Issue)
    (classified : Array (Project.Issue × ReviewDisposition)) :
    Array Project.Issue × Array Project.Issue :=
  let reviewable := classified.filterMap fun (i, d) =>
    match d with
    | .awaitingReview | .merged => some i
    | .changesRequested         => none
  let reviewableIds := reviewable.map (·.id.val)
  let workable := structuralWorkable.filter (fun i => !reviewableIds.contains i.id.val)
  (workable, reviewable)

/-- Re-fetch `issues` with their attached pull requests. `Project.loadIssues` leaves `attachedPRs`
    empty for speed, so the review path has to ask for detail; only open issues are worth it. -/
def withAttachedPRs (pid : Taxis.IssueId) (issues : Array Project.Issue) :
    IO (Array Project.Issue) := do
  let mut out : Array Project.Issue := #[]
  for i in issues do
    if i.status != .open then continue
    if let some full ← Project.loadIssue pid i.id then out := out.push full
  return out

/-- Build a queue entry for a role spawn. Returns `none` if the role refers
    to a missing target (multi-org project where neither the role's bound
    issue nor the project default sets one).

    `targetOverride` is used by the project-independent dispatcher, whose target comes from taxis
    artifacts rather than from the project or the issue's own override — see
    `Project.artifactTarget`. It wins over both; `none` keeps the ordinary resolution. -/
def buildRoleEntry (project : Project.Project) (role : Project.Role)
    (issue? : Option Project.Issue) (instructions : String := "")
    (targetOverride : Option Project.RepoTarget := none) :
    IO (Option Queue.QueueEntry) := do
  -- A repository-independent role has no target to resolve and must not be rejected for
  -- lacking one; every other role still is, since a missing target means a misconfigured
  -- project rather than a deliberate choice.
  let target? :=
    if role.repoless then none
    else targetOverride
      <|> issue?.bind (Project.effectiveTarget project ·)
      <|> project.defaultTarget
  if target?.isNone && !role.repoless then return none
  Project.warnRepolessPerms role
  let id ← TaskStore.generateId
  let createdAt ← TaskStore.currentIso8601
  -- One extra fetch per dispatch (not per tick) so the thread lands in the prompt: a worker
  -- picking up a rejected issue would otherwise have to know to ask for it.
  let comments ← match issue? with
    | some i => Project.renderCommentThread i.id
    | none   => pure none
  let vars   := Project.renderVarsFor project issue? instructions targetOverride comments
  let prompt := Project.render role.promptTemplate vars
  return some
    { id, createdAt
    , upstream      := target?.map (·.repo)
    , fork          := target?.map (·.repo)
    , mode          := target?.map (fun _ => .pr)
    , prompt
    , backend       := role.backend
    , model         := role.model
    , systemPrompt  := role.systemPrompt
    , prependPrompt := role.prependPrompt
    , budget        := role.budget
    , priority      := role.priority
    , readOnly      := role.readOnly
    , tools         := some role.permissions
    , projectId     := some project.id
    , issueId       := issue?.map (·.id)
    , role          := some role.name }

/-- Build a queue entry for a tracker-wide supervisory role: bound to no project, no issue and
    no repository.

    Distinct from `buildRoleEntry` because there is no `Project` to render against — the whole
    point of the role is that it spans every project — so the prompt template gets empty
    project variables and the entry carries no `projectId`. That absence is also what the
    manager dispatcher counts to enforce its cap. -/
def buildManagerEntry (role : Project.Role) (instructions : String := "") :
    IO Queue.QueueEntry := do
  Project.warnRepolessPerms role
  let id ← TaskStore.generateId
  let createdAt ← TaskStore.currentIso8601
  let vars : Project.RenderVars :=
    { projectId := "", projectName := "", instructions }
  let entry : Queue.QueueEntry :=
    { id, createdAt
    , prompt        := Project.render role.promptTemplate vars
    , backend       := role.backend
    , model         := role.model
    , systemPrompt  := role.systemPrompt
    , prependPrompt := role.prependPrompt
    , budget        := role.budget
    , priority      := role.priority
    , readOnly      := role.readOnly
    , tools         := some role.permissions
    , role          := some role.name }
  return entry

-- Source polling

/--
Poll a source for new events not yet in `state.processedIds`.
Returns a pair of:
- an array of `(eventId, templateVars)` pairs, and
- an optional replacement for `processedIds` (used by sources that need to prune stale IDs,
  e.g. `githubLabels` removes IDs for items whose label was since stripped so the listener
  re-fires when the label is re-applied).  `none` means "append new IDs as usual".
`eventId` is `""` for shell sources (no deduplication by ID).
Event IDs for GitHub sources are prefixed with the upstream slug
(e.g. `"my-account/orchestra:12345"`) so a single state file
correctly deduplicates events across multiple repos.
-/
def pollSource (source : SourceConfig) (state : ListenerState) (ghToken : String)
    (globalAuthorizedUsers : List String := [])
    : IO (Array (String × List (String × String)) × Option (Array String)) := do
  match source with

  | .githubIssues repos labels trigger sourceAuthorizedUsers => do
    let allowed := effectiveAllowed sourceAuthorizedUsers globalAuthorizedUsers
    let labelParam := if labels.isEmpty then "" else "&labels=" ++ ",".intercalate labels
    let mut allEvents : Array (String × List (String × String)) := #[]
    for entry in repos do
      let endpoint := s!"/repos/{entry.upstream}/issues?state=open&per_page=100{labelParam}"
      let jsonOpt ← runGhApi endpoint ghToken
      match jsonOpt with
      | none => pure ()
      | some json =>
        let .ok items := json.getArr? | pure ()
        for item in items do
          -- skip pull requests (issues endpoint returns PRs too)
          if (item.getObjVal? "pull_request").isOk then continue
          let .ok numJson := item.getObjVal? "number" | continue
          let numStr  := toString numJson
          let eventId := s!"{entry.upstream}:{numStr}"
          if state.processedIds.contains eventId then continue
          let author :=
            match item.getObjVal? "user" |>.toOption with
            | none   => ""
            | some u => u.getObjValAs? String "login" |>.toOption |>.getD ""
          if !isAuthorized allowed author then continue
          let title  := item.getObjValAs? String "title"    |>.toOption |>.getD ""
          let body   := item.getObjValAs? String "body"     |>.toOption |>.getD ""
          -- If a trigger is set, skip issues whose body does not contain it
          if !trigger.isEmpty && !(body.splitOn trigger).length > 1 then continue
          let url    := item.getObjValAs? String "html_url" |>.toOption |>.getD ""
          let vars   := [("issue_number", numStr), ("title", title), ("body", body),
                         ("url", url), ("author", author),
                         ("upstream", entry.upstream.toString), ("fork", entry.fork.toString),
                         ("upstream_escaped", entry.upstream.toString.replace "/" "_"),
                         ("fork_escaped", entry.fork.toString.replace "/" "_")]
          allEvents := allEvents.push (eventId, vars)
    return (allEvents, none)

  | .githubPrReviews repos labels trigger sourceAuthorizedUsers => do
    let allowed := effectiveAllowed sourceAuthorizedUsers globalAuthorizedUsers
    let mut allEvents : Array (String × List (String × String)) := #[]
    for entry in repos do
      -- Fetch open PRs
      let prJsonOpt ← runGhApi s!"/repos/{entry.upstream}/pulls?state=open&per_page=100" ghToken
      let prArr ← match prJsonOpt with
        | none    => pure (#[] : Array Json)
        | some j  => match j.getArr? with
          | .ok a  => pure a
          | .error _ => pure #[]
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
        let reviewJsonOpt ← runGhApi
          s!"/repos/{entry.upstream}/pulls/{prNum}/reviews?per_page=100" ghToken
        let reviews ← match reviewJsonOpt with
          | none   => pure (#[] : Array Json)
          | some j => match j.getArr? with
            | .ok a  => pure a
            | .error _ => pure #[]
        for review in reviews do
          let .ok ridJson := review.getObjVal? "id" | continue
          let ridStr  := toString ridJson
          let eventId := s!"{entry.upstream}:{ridStr}"
          if state.processedIds.contains eventId then continue
          -- Only include submitted reviews with a non-empty body
          let reviewState := review.getObjValAs? String "state" |>.toOption |>.getD ""
          if reviewState != "COMMENTED" && reviewState != "CHANGES_REQUESTED" &&
             reviewState != "APPROVED" then continue
          let reviewer := match review.getObjVal? "user" |>.toOption with
            | none   => ""
            | some u => u.getObjValAs? String "login" |>.toOption |>.getD ""
          if !isAuthorized allowed reviewer then continue
          let body := review.getObjValAs? String "body" |>.toOption |>.getD ""
          -- If a trigger is set, skip reviews whose body does not contain it
          if !trigger.isEmpty && !(body.splitOn trigger).length > 1 then continue
          let url  := review.getObjValAs? String "html_url" |>.toOption |>.getD ""
          let vars := [
            ("pr_number",  toString prNum),
            ("pr_title",   prTitle),
            ("reviewer",   reviewer),
            ("body",       body),
            ("url",        url),
            ("upstream",   entry.upstream.toString),
            ("fork",       entry.fork.toString),
            ("upstream_escaped", entry.upstream.toString.replace "/" "_"),
            ("fork_escaped",     entry.fork.toString.replace "/" "_")
          ]
          allEvents := allEvents.push (eventId, vars)
    return (allEvents, none)

  | .githubComments repos labels trigger sourceAuthorizedUsers => do
    -- On the first run lastChecked is empty: initialise state and return nothing,
    -- so we don't flood the queue with all historical comments.
    if state.lastChecked.isEmpty then return (#[], none)
    let allowed := effectiveAllowed sourceAuthorizedUsers globalAuthorizedUsers
    let mut allEvents : Array (String × List (String × String)) := #[]
    for entry in repos do
      -- Helper: extract an event from a comment JSON object, react with 🚀, return vars.
      -- `inline = true` handles inline PR review comments (different ID prefix, URL field, and
      -- reaction endpoint) vs regular issue/PR comments.
      let processCommentJson (inline : Bool) : Json → IO (Option (String × List (String × String))) :=
        fun comment => do
          let .ok idNum := comment.getObjValAs? Nat "id" | return none
          let idStr   := if inline then s!"inline:{toString idNum}" else toString idNum
          let eventId := s!"{entry.upstream}:{idStr}"
          if state.processedIds.contains eventId then return none
          let body := comment.getObjValAs? String "body" |>.toOption |>.getD ""
          -- Only process comments that contain the trigger string
          if !(body.splitOn trigger).length > 1 then return none
          let author := match comment.getObjVal? "user" |>.toOption with
            | none   => ""
            | some u => u.getObjValAs? String "login" |>.toOption |>.getD ""
          -- Authorization check: skip unauthorized users (no reaction either)
          if !isAuthorized allowed author then return none
          -- React with a rocket emoji (best-effort; ignore failures)
          try reactToComment entry.upstream.toString idNum ghToken inline catch _ => pure ()
          let url           := comment.getObjValAs? String "html_url" |>.toOption |>.getD ""
          -- Extract issue/PR number from the relevant parent URL field
          let parentUrlField := if inline then "pull_request_url" else "issue_url"
          let parentUrl      := comment.getObjValAs? String parentUrlField |>.toOption |>.getD ""
          let issueNum       := parentUrl.splitOn "/" |>.getLast? |>.getD ""
          -- For inline comments, also expose the numeric ID as `inline_comment_id` so prompt
          -- templates can pass it directly to the `comment` tool's `reply_to_comment_id` argument.
          let inlineCommentId := if inline then toString idNum else ""
          let vars := [("comment_id", idStr), ("inline_comment_id", inlineCommentId),
                       ("body", body), ("author", author),
                       ("url", url), ("issue_number", issueNum),
                       ("upstream", entry.upstream.toString), ("fork", entry.fork.toString),
                       ("upstream_escaped", entry.upstream.toString.replace "/" "_"),
                       ("fork_escaped", entry.fork.toString.replace "/" "_")]
          return some (eventId, vars)
      if labels.isEmpty then
        -- Use the global issue comments endpoint with a `since` filter
        let endpoint :=
          s!"/repos/{entry.upstream}/issues/comments?since={state.lastChecked}&per_page=100&direction=asc"
        let jsonOpt ← runGhApi endpoint ghToken
        let comments ← match jsonOpt with
          | none   => pure (#[] : Array Json)
          | some j => match j.getArr? with
            | .ok a    => pure a
            | .error _ => pure #[]
        for comment in comments do
          if let some ev ← processCommentJson false comment then
            allEvents := allEvents.push ev
        -- Also fetch inline PR review comments
        let inlineEndpoint :=
          s!"/repos/{entry.upstream}/pulls/comments?since={state.lastChecked}&per_page=100&direction=asc"
        let inlineJsonOpt ← runGhApi inlineEndpoint ghToken
        let inlineComments ← match inlineJsonOpt with
          | none   => pure (#[] : Array Json)
          | some j => match j.getArr? with
            | .ok a    => pure a
            | .error _ => pure #[]
        for comment in inlineComments do
          if let some ev ← processCommentJson true comment then
            allEvents := allEvents.push ev
      else
        -- Fetch only issues/PRs that carry one of the requested labels
        let labelParam := ",".intercalate labels
        let issuesOpt ← runGhApi
          s!"/repos/{entry.upstream}/issues?state=open&labels={labelParam}&per_page=100" ghToken
        let issues ← match issuesOpt with
          | none   => pure (#[] : Array Json)
          | some j => match j.getArr? with
            | .ok a    => pure a
            | .error _ => pure #[]
        for issue in issues do
          let .ok issNum := issue.getObjValAs? Nat "number" | continue
          let commentsOpt ← runGhApi
            s!"/repos/{entry.upstream}/issues/{issNum}/comments?since={state.lastChecked}&per_page=100"
            ghToken
          let comments ← match commentsOpt with
            | none   => pure (#[] : Array Json)
            | some j => match j.getArr? with
              | .ok a    => pure a
              | .error _ => pure #[]
          for comment in comments do
            if let some ev ← processCommentJson false comment then
              allEvents := allEvents.push ev
          -- Also fetch inline PR review comments for this PR
          let inlineCommentsOpt ← runGhApi
            s!"/repos/{entry.upstream}/pulls/{issNum}/comments?since={state.lastChecked}&per_page=100"
            ghToken
          let inlineComments ← match inlineCommentsOpt with
            | none   => pure (#[] : Array Json)
            | some j => match j.getArr? with
              | .ok a    => pure a
              | .error _ => pure #[]
          for comment in inlineComments do
            if let some ev ← processCommentJson true comment then
              allEvents := allEvents.push ev
    return (allEvents, none)

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
    if trimmed.isEmpty then return (#[], none)
    return (#[("", [("output", trimmed)])], none)

  | .projectDispatcher pid caps => do
    let some _project ← Project.loadProject pid
      | IO.eprintln s!"[dispatcher] project {pid.toString} not found; skipping"; return (#[], none)
    let issues ← Project.loadIssues pid
    let roles  ← Project.loadAllRoles pid
    -- Count active per-role queue entries scoped to this project.
    let allEntries ← Queue.loadAllEntries
    let mut active : Std.HashMap String Nat := {}
    for e in allEntries do
      let isActive := e.status == .pending || e.status == .running
      if !isActive then continue
      if e.projectId != some pid then continue
      if let some r := e.role then
        active := active.insert r ((active.getD r 0) + 1)
    let classified ← classifyReview ghToken (← withAttachedPRs pid issues)
    -- Narrowed here, where every issue in the project is in hand: whether a child or dependency
    -- has closed cannot be told from the workable set itself. The review split then takes the
    -- reviewer's issues back out of it, so no issue is offered to two roles in one tick.
    let (workable, reviewable) := splitForDispatch (Project.workableIssues issues) classified
    let reworking := classified.filter (·.2 == .changesRequested) |>.size
    IO.println s!"[dispatcher] project {pid.toString}: {issues.size} issues, \
      {workable.size} workable (of which {reworking} sent back for changes), \
      {reviewable.size} awaiting review; roles available: \
      {String.intercalate ", " (roles.map (·.name)).toList}"
    let input : DispatcherInput :=
      { activeByRole := active, issues := workable, reviewable, caps, roles }
    let decisions := dispatcherDecisions input
    if decisions.isEmpty then
      IO.println "[dispatcher] no roles named in caps, so nothing to check"
    for d in decisions do
      IO.println s!"[dispatcher] {renderDecision d}"
    let spawns := dispatcherTick input
    -- Emit synthetic events. eventId is empty so the listener-state dedup
    -- doesn't accumulate (each tick is fresh; the cap is the dedup mechanism).
    return (spawns.map fun s =>
      let baseVars : List (String × String) := [("role_name", s.roleName)]
      let vars := match s.issueId with
        | some iid => baseVars ++ [("issue_id", iid.toString)]
        | none     => baseVars
      ("", vars), none)

  | .labelDispatcher label caps => do
    let some sets ← Project.issuesWithLabel label
      | IO.eprintln s!"[dispatcher] label '{label}' does not exist on the taxis instance; \
          skipping"; return (#[], none)
    let gaps := sets.gaps
    -- Report unroutable issues every tick rather than once: the fix is to attach an artifact in
    -- taxis, and a line that scrolled past on daemon start is not going to prompt that.
    for (iid, gap) in gaps do
      match gap with
      | .noRepository =>
        IO.eprintln s!"[dispatcher] issue {iid.toString} is in scope for '{label}' but has no \
          repository artifact on it or any ancestor; skipping"
      | .noBranch repo =>
        IO.eprintln s!"[dispatcher] issue {iid.toString} is in scope for '{label}' and \
          resolves to {repo} but has no github-branch artifact on it or any ancestor; skipping"
    let issues := sets.work.map (·.1)
    -- What happens next to an issue carrying a PR needs a GitHub call per PR and a taxis call
    -- per issue, so it is settled here rather than in the selection. `issuesWithLabel` builds
    -- `work` and `reviewable` from two independent tests, so an issue that is both a dispatch
    -- candidate and carries a PR appears in both; the split below is what keeps one role from
    -- taking it out from under the other.
    let classified ← classifyReview ghToken (sets.reviewable.map (·.1))
    let (issues, reviewable) := splitForDispatch issues classified
    let reworking := classified.filter (·.2 == .changesRequested) |>.size
    let roles ← Project.loadGlobalRoles
    IO.println s!"[dispatcher] label '{label}': {issues.size} workable \
      (of which {reworking} sent back for changes), {sets.reviewable.size} with a PR attached \
      of which {reviewable.size} await a reviewer, \
      {gaps.size} skipped for want of a target; roles available: \
      {String.intercalate ", " (roles.map (·.name)).toList}"
    -- Roles that bind an issue and roles that don't are dispatched by different rules here, so
    -- they are counted and decided separately. An `always` role has no issue to bind, so it is
    -- scoped to a labelled root instead (one spawn set per root) — see below.
    let (boundCaps, unboundCaps) := splitCapsByBinding roles caps
    -- Cap counting is scoped to the labelled set, so the caps bound concurrent work *on labelled
    -- issues* rather than colliding with per-project dispatchers running the same role names.
    let labelled : Array Taxis.IssueId := issues.map (·.id) ++ reviewable.map (·.id)
    let allEntries ← Queue.loadAllEntries
    let activeEntries := allEntries.filter fun e => e.status == .pending || e.status == .running
    let mut active : Std.HashMap String Nat := {}
    for e in activeEntries do
      let some eIid := e.issueId | continue
      if !labelled.contains eIid then continue
      if let some r := e.role then
        active := active.insert r ((active.getD r 0) + 1)
    let input : DispatcherInput :=
      { activeByRole := active, issues, reviewable, caps := boundCaps, roles }
    let decisions := dispatcherDecisions input
    if decisions.isEmpty then
      IO.println "[dispatcher] no roles named in caps, so nothing to check"
    for d in decisions do
      IO.println s!"[dispatcher] {renderDecision d}"
    let spawns := dispatcherTick input
    -- Report rather than silently drop: an entry's repository and branch come from the issue's
    -- own artifacts, so a role that binds no issue has no target here — unless it is `always`,
    -- which is scoped to a labelled root below and never reaches this loop. `idle` roles
    -- (planners) still cannot run here: they bind nothing *and* have no root to stand in for it.
    for s in spawns do
      if s.issueId.isNone then
        IO.eprintln s!"[dispatcher] role '{s.roleName}' was due but binds to no issue, and this \
          dispatcher takes its target from the issue; not dispatched. Use a project-dispatcher \
          for roles with the 'idle' trigger, or the 'always' trigger to run one per labelled root."
    let boundEvents := spawns.filterMap fun s =>
      match s.issueId with
      | none => none
      | some iid =>
        match (sets.work ++ sets.reviewable).find? (fun (i, _) => i.id == iid) with
        | none => none
        | some (issue, target) =>
          some ("", [ ("role_name",     s.roleName)
                    , ("issue_id",      iid.toString)
                    , ("project_id",    issue.projectId.toString)
                    , ("target_repo",   target.repo.toString)
                    , ("target_branch", target.branch) ])
    -- Unbound roles, one decision set per labelled root. The root is the scope: it supplies the
    -- target and stands in as the project, and its own id is what the cap is counted against —
    -- the tally above keys on `issueId`, which an unbound entry does not have, so counting these
    -- there would leave their cap permanently unreached and spawn one every tick.
    let mut unboundEvents : Array (String × List (String × String)) := #[]
    if !unboundCaps.isEmpty then
      if sets.roots.isEmpty then
        IO.eprintln s!"[dispatcher] label '{label}' has unbound roles configured \
          ({String.intercalate ", " (unboundCaps.map (·.1))}) but no open issue carries the label \
          directly, so there is no root to scope them to; not dispatched"
      for (root, target) in sets.roots do
        let rootInput : DispatcherInput :=
          { activeByRole := unboundActiveByRole allEntries root.id
          , issues := #[], reviewable := #[], caps := unboundCaps, roles }
        for d in dispatcherDecisions rootInput do
          IO.println s!"[dispatcher] root {root.id.toString} \"{root.title}\": {renderDecision d}"
        for s in dispatcherTick rootInput do
          unboundEvents := unboundEvents.push
            ("", [ ("role_name",     s.roleName)
                 , ("project_id",    root.id.toString)
                 , ("target_repo",   target.repo.toString)
                 , ("target_branch", target.branch) ])
    return (boundEvents ++ unboundEvents, none)

  | .managerDispatcher caps => do
    -- Global roles only. There is no project whose `roles/` directory could take precedence,
    -- for the same reason as the label-dispatcher: these roles span the whole tracker.
    let roles ← Project.loadGlobalRoles
    -- Only `always` is meaningful without an issue set. The others would either never fire or
    -- fire for the wrong reason — `idle` in particular reads "no open and no in-review issues",
    -- which is trivially true of the empty sets below and would spawn a planner every tick.
    let (boundCaps, unboundCaps) := splitCapsByBinding roles caps
    for (n, _) in boundCaps do
      match (roles.find? (·.name == n)).bind (·.dispatch) with
      | none   => IO.eprintln s!"[dispatcher] manager role '{n}' has no dispatch policy or does \
          not exist among the global roles; not dispatched"
      | some d => IO.eprintln s!"[dispatcher] manager role '{n}' has the \
          '{triggerName d.trigger}' trigger, which needs an issue set this dispatcher does not \
          have; use the 'always' trigger for a tracker-wide role. Not dispatched."
    -- A manager binds no project, so it is the *absence* of one that identifies its entries.
    -- Counting them any other way would leave the cap permanently unreached.
    let allEntries ← Queue.loadAllEntries
    let mut active : Std.HashMap String Nat := {}
    for e in allEntries do
      let isActive := e.status == .pending || e.status == .running
      if !isActive then continue
      if e.projectId != none || e.issueId != none then continue
      if let some r := e.role then
        active := active.insert r ((active.getD r 0) + 1)
    let input : DispatcherInput :=
      { activeByRole := active, issues := #[], reviewable := #[]
      , caps := unboundCaps, roles }
    let decisions := dispatcherDecisions input
    if decisions.isEmpty && boundCaps.isEmpty then
      IO.println "[dispatcher] no manager roles named in caps, so nothing to check"
    for d in decisions do
      IO.println s!"[dispatcher] {renderDecision d}"
    return (dispatcherTick input |>.map fun s => ("", [("role_name", s.roleName)]), none)

  | .githubLabelCount repos labels max kind => do
    let mut allEvents : Array (String × List (String × String)) := #[]
    for entry in repos do
      let labelParam := if labels.isEmpty then "" else "&labels=" ++ ",".intercalate labels
      let endpoint := s!"/repos/{entry.upstream}/issues?state=open&per_page=100{labelParam}"
      let jsonOpt ← runGhApi endpoint ghToken
      let items ← match jsonOpt with
        | none   => pure (#[] : Array Json)
        | some j => match j.getArr? with
          | .ok a  => pure a
          | .error _ => pure #[]
      -- Count items matching the configured kind.
      let shouldCount (item : Json) : Bool :=
        let isPr := (item.getObjVal? "pull_request").isOk
        match kind with
        | "issues" => !isPr
        | "pulls"  => isPr
        | _        => true  -- "all" or unrecognised
      let count := (items.filter shouldCount).size
      if count < max then
        let needed := max - count
        let vars := [("count", toString count), ("max", toString max),
                     ("needed", toString needed),
                     ("upstream", entry.upstream.toString), ("fork", entry.fork.toString),
                     ("upstream_escaped", entry.upstream.toString.replace "/" "_"),
                     ("fork_escaped",     entry.fork.toString.replace "/" "_")]
        allEvents := allEvents.push ("", vars)
    return (allEvents, none)

  | .githubLabels repos labels kind sourceAuthorizedUsers => do
    let allowed := effectiveAllowed sourceAuthorizedUsers globalAuthorizedUsers
    let mut allEvents : Array (String × List (String × String)) := #[]
    -- currentIds: all kind-matching labeled items visible this tick (used to prune
    -- processedIds so that a label removal followed by re-application re-triggers).
    let mut currentIds : Array String := #[]
    for entry in repos do
      -- Collect candidate items. One query per label (OR logic); one unlabelled query if empty.
      let mut items : Array Json := #[]
      if labels.isEmpty then
        let jsonOpt ← runGhApi
          s!"/repos/{entry.upstream}/issues?state=open&per_page=100" ghToken
        items := match jsonOpt with
          | none   => #[]
          | some j => j.getArr?.toOption |>.getD #[]
      else
        for lbl in labels do
          let jsonOpt ← runGhApi
            s!"/repos/{entry.upstream}/issues?state=open&per_page=100&labels={lbl}" ghToken
          let batch : Array Json := match jsonOpt with
            | none   => #[]
            | some j => j.getArr?.toOption |>.getD #[]
          items := items ++ batch
      for item in items do
        let isPr := (item.getObjVal? "pull_request").isOk
        let kindMatch := match kind with
          | "issues" => !isPr
          | "pulls"  => isPr
          | _        => true
        if !kindMatch then continue
        let .ok numJson := item.getObjVal? "number" | continue
        let numStr  := toString numJson
        let eventId := s!"{entry.upstream}:{numStr}"
        -- Deduplicate within this tick and record as currently visible.
        if currentIds.contains eventId then continue
        currentIds := currentIds.push eventId
        if state.processedIds.contains eventId then continue
        let itemLabelNames : List String :=
          (item.getObjValAs? (Array Json) "labels" |>.toOption |>.getD #[]).toList.filterMap
            (fun l => l.getObjValAs? String "name" |>.toOption)
        let matchedLabels :=
          if labels.isEmpty then itemLabelNames
          else labels.filter (fun l => itemLabelNames.contains l)
        if !labels.isEmpty && matchedLabels.isEmpty then continue
        let author := match item.getObjVal? "user" |>.toOption with
          | none   => ""
          | some u => u.getObjValAs? String "login" |>.toOption |>.getD ""
        if !isAuthorized allowed author then continue
        let title := item.getObjValAs? String "title"    |>.toOption |>.getD ""
        let body  := item.getObjValAs? String "body"     |>.toOption |>.getD ""
        let url   := item.getObjValAs? String "html_url" |>.toOption |>.getD ""
        let vars := [
          ("issue_number",   numStr),
          ("title",          title),
          ("body",           body),
          ("url",            url),
          ("author",         author),
          ("labels",         ",".intercalate itemLabelNames),
          ("matched_labels", ",".intercalate matchedLabels),
          ("is_pr",          if isPr then "true" else "false"),
          ("upstream",       entry.upstream.toString),
          ("fork",           entry.fork.toString),
          ("upstream_escaped", entry.upstream.toString.replace "/" "_"),
          ("fork_escaped",     entry.fork.toString.replace "/" "_")
        ]
        allEvents := allEvents.push (eventId, vars)
    -- Prune processedIds to only currently-visible items so that a label removal
    -- followed by re-application causes the listener to fire again.
    let prunedProcessed := state.processedIds.filter currentIds.contains
    return (allEvents, some (prunedProcessed ++ allEvents.map (·.1)))

end Orchestra.Listener
