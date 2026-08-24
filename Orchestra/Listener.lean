import Lean.Data.Json
import Orchestra.Config
import Orchestra.TaskStore
import Orchestra.Queue
import Orchestra.Project
import Orchestra.GitHub
import Orchestra.Utils.Files
import Orchestra.Usage

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
      dispatching an agent at a guessed repository is worse than not dispatching.

      `limitUnclaimed` bounds the caps of roles that do **not** pre-claim by the work in scope,
      and takes the issues an agent is already on out of the tick's selection, so a cap of three
      against one open issue dispatches one agent rather than three onto the same piece of work —
      see `limitBoundCaps` / `limitUnboundCaps` for the cap rule and why pre-claiming roles keep
      theirs, and `unattendedIssues` for the selection half. Off by default: it lowers configured
      caps, which is not something to start doing to an existing listener without being asked.

      `excludeRoots` says the labelled issues are epics rather than units of work: they leave the
      workable set and the per-root counts, and only what inherited the label is dispatched onto
      (`Project.dispatchCandidates`, `Project.openIssuesByRoot`). They stay roots — an unbound
      role is still placed on them — and a root carrying a pull request is still reviewed. -/
  | labelDispatcher   (label : String) (caps : List (String × Nat)) (limitUnclaimed : Bool)
                      (excludeRoots : Bool)
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
    | .labelDispatcher label caps limitUnclaimed excludeRoots =>
        Json.mkObj [("type", "label-dispatcher"),
                    ("label", Json.str label),
                    ("caps", Json.mkObj
                       (caps.map (fun (n, c) => (n, Json.num c)))),
                    ("limit_unclaimed_to_open_issues", Json.bool limitUnclaimed),
                    ("exclude_root_issues", Json.bool excludeRoots)]
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
        let limitUnclaimed :=
          j.getObjValAs? Bool "limit_unclaimed_to_open_issues" |>.toOption |>.getD false
        let excludeRoots :=
          j.getObjValAs? Bool "exclude_root_issues" |>.toOption |>.getD false
        return .labelDispatcher label caps limitUnclaimed excludeRoots
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
      Defaults to `""`, in which case the `fork` template variable is used.

      Both empty, with no variable to fall back on either, queues a repository-independent task —
      one that runs in a scratch workspace with nothing checked out. Every GitHub event source
      supplies both variables, so that is reachable only from a listener that genuinely has no
      repository to hand, such as one on a `shell` source. -/
  fork           : String := ""
  mode           : TaskMode := .fork
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
  /-- Candidate authentication sources for tasks this listener queues, tried per `authMode`.

      A listener firing repeatedly is exactly the case multiple sources exist for: it can keep
      producing work after one account's weekly window closes. Which source each task lands on is
      decided when the daemon claims it, not here. -/
  authSources    : List String := []
  /-- How to choose among the candidates: `"ordered"` or `"distribute"`.
      Absent leaves it to the backend's `default_auth_mode`, which is what a pooled
      `default_auth_source` is walked with. -/
  authMode       : Option AuthMode := none
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
    let fields := if !a.authSources.isEmpty       then fields ++ [("auth_sources",  ToJson.toJson a.authSources)] else fields
    let fields := if let some m := a.authMode     then fields ++ [("auth_mode",     ToJson.toJson m)]             else fields
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
    -- Absent is fine — a listener that queues repository-independent tasks has no answer for a
    -- field that is the deprecated spelling of a task's tools. Unreadable is not; see
    -- `parseTaskMode?`.
    let mode           ← parseTaskMode? j
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
    let authSources := j.getObjValAs? (List String) "auth_sources" |>.toOption |>.getD []
    let authMode := j.getObjValAs? AuthMode "auth_mode" |>.toOption
    let tools := j.getObjValAs? (List String) "tools" |>.toOption
    let readOnly := j.getObjValAs? Bool "read_only" |>.toOption |>.getD false
    let priority     := j.getObjValAs? Nat    "priority"      |>.toOption |>.getD 10
    let workflowPath := j.getObjValAs? String "workflow_path" |>.toOption
    let issueNumber  := j.getObjValAs? String "issue_number"  |>.toOption
    let prLabels     := j.getObjValAs? (List String) "pr_labels" |>.toOption |>.getD []
    return { upstream, fork, mode, promptTemplate, series, backend, model, agent, systemPrompt,
             budget, memory, authSource, authSources, authMode, tools, readOnly, priority,
             workflowPath, issueNumber, prLabels }

-- Dispatch rate limits

/-- A ceiling on how often a listener may dispatch: at most `max` in any window of
    `windowSeconds` ending now.

    This is not `interval_seconds`, which says how often a listener *looks*. A source can hand a
    listener twenty events in a single tick, and a listener watching a busy repository can have
    something to do on every tick of the day; neither is a reason to spend twenty tasks' worth of
    an account's budget in a minute. Several limits compose — "five an hour and thirty a day" is
    two of them — and a dispatch has to fit under all of them.

    What is over the ceiling is *held*, not dropped: an event held back is not marked processed,
    so the listener offers it again on a later tick once the window has moved. -/
structure RateLimit where
  max           : Nat
  windowSeconds : Nat
deriving BEq, DecidableEq, Repr, Inhabited

/-- Seconds in a unit name, in the spellings a person actually writes. -/
private def unitSeconds? (u : String) : Option Nat :=
  match u.toLower with
  | "s" | "sec" | "secs" | "second" | "seconds" => some 1
  | "m" | "min" | "mins" | "minute" | "minutes" => some 60
  | "h" | "hr" | "hrs" | "hour" | "hours"       => some 3600
  | "d" | "day" | "days"                        => some 86400
  | "w" | "week" | "weeks"                      => some 604800
  | _                                           => none

/-- Read a window: a bare unit (`"hour"`), or a count and a unit (`"6h"`, `"90 minutes"`).
    `none` on anything else, so that a misspelled window is reported rather than quietly
    becoming no limit at all — which is the one way this feature could fail dangerously. -/
def parseWindow? (s : String) : Option Nat :=
  let cs     := s.trimAscii.toString.toList
  let digits := cs.takeWhile Char.isDigit
  let rest   := String.ofList (cs.drop digits.length) |>.trimAscii.toString
  let count  := if digits.isEmpty then some 1 else (String.ofList digits).toNat?
  match count, unitSeconds? rest with
  | some c, some u => if c == 0 then none else some (c * u)
  | _,      _      => none

/-- A limit as a person would say it: `"5 per hour"`, `"20 per 6 hours"`, `"3 per 90s"`. -/
def RateLimit.describe (l : RateLimit) : String :=
  let w   := l.windowSeconds
  let per :=
    if w == 1 then "second" else if w == 60 then "minute" else if w == 3600 then "hour"
    else if w == 86400 then "day" else if w == 604800 then "week"
    else if w % 604800 == 0 then s!"{w / 604800} weeks"
    else if w % 86400 == 0  then s!"{w / 86400} days"
    else if w % 3600 == 0   then s!"{w / 3600} hours"
    else if w % 60 == 0     then s!"{w / 60} minutes"
    else s!"{w}s"
  s!"{l.max} per {per}"

instance : ToJson RateLimit where
  toJson l := Json.mkObj [
    ("max",         ToJson.toJson l.max),
    ("per_seconds", ToJson.toJson l.windowSeconds)
  ]

instance : FromJson RateLimit where
  fromJson? j := do
    let max ← j.getObjValAs? Nat "max"
    match j.getObjValAs? Nat "per_seconds" |>.toOption with
    | some 0 => .error "'per_seconds' must be at least 1"
    | some w => return { max, windowSeconds := w }
    | none   =>
      match j.getObjValAs? String "per" |>.toOption with
      | none   => .error "a rate limit needs a window: 'per' (\"hour\", \"6h\", \"90 minutes\") \
or 'per_seconds'"
      | some p =>
        match parseWindow? p with
        | some w => return { max, windowSeconds := w }
        | none   => .error s!"'per' is not a window this understands: '{p}'. Write a unit \
(\"minute\", \"hour\", \"day\", \"week\"), a count and a unit (\"6h\", \"90 minutes\"), or give \
'per_seconds'"

/-- How many of `dispatches` fall in the `windowSeconds` ending at `now`, both epoch seconds.

    A stamp we cannot read does not count: a state file edited by hand must not be able to hold
    a listener shut forever. A stamp in the *future* does — a clock that jumped is a reason to
    dispatch less, not more. -/
def countWithin (dispatches : Array String) (now : Int) (windowSeconds : Nat) : Nat :=
  dispatches.foldl (init := 0) fun n s =>
    match Usage.parseIso8601 s with
    | some t => if t > now - (windowSeconds : Int) then n + 1 else n
    | none   => n

/-- The first of `limits` that `dispatches` has already filled at `now`. `none` means there is
    room under every one of them, and the listener may dispatch. -/
def rateLimitHit? (limits : List RateLimit) (dispatches : Array String) (now : Int) :
    Option RateLimit :=
  limits.find? fun l => countWithin dispatches now l.windowSeconds ≥ l.max

/-- Drop the stamps no configured limit can still count, so that a listener running for months
    does not accumulate a state file of them. No limits means nothing worth remembering. -/
def pruneDispatches (limits : List RateLimit) (dispatches : Array String) (now : Int) :
    Array String :=
  if limits.isEmpty then #[] else
    let longest := limits.foldl (fun m l => max m l.windowSeconds) 0
    dispatches.filter fun s =>
      match Usage.parseIso8601 s with
      | some t => t > now - (longest : Int)
      | none   => false

/-- Whether a source pages by time — using `lastChecked` as the cursor for what it has not
    looked at yet — rather than re-deriving its candidates from the world each tick.

    `github-comments` asks GitHub for the comments updated `since` the last check, so the slice
    of time it has not read *is* its candidate set. Every other source asks for a state of the
    world — open issues, labelled items, a project's issues, a command's output — and filters
    what comes back against `processedIds`.

    That difference is what a *held* event costs. A source that re-derives offers a held event
    again on the next tick for nothing. A source that pages by time will not: advance its cursor
    past an event it held and the event is not late, it is gone. So a tick that held anything
    has to leave the cursor where it found it. -/
def pagesByTime : SourceConfig → Bool
  | .githubComments .. => true
  | _                  => false

/-- The processed-event set a tick leaves behind.

    `newIds` are the event ids the tick handled and `held` the ones a rate limit turned away.
    `replacement` is what a source that rewrites the whole set hands back — `github-labels`
    prunes the ids whose label was stripped, so that re-applying it fires the listener again —
    and it names every id that source saw this tick, the ones never reached included. That is
    why a replacement has `held` taken back out of it, rather than the held ids merely being
    left out of `newIds`: an event marked processed is not paced, it is lost, and a ceiling is
    supposed to slow work down rather than throw it away.

    Only ever the *new* ids are filtered. `previous` passes through untouched, so an id that was
    already processed cannot be un-processed by sharing a name with something held this tick. -/
def nextProcessedIds (previous newIds held : Array String)
    (replacement : Option (Array String)) : Array String :=
  match replacement with
  | some r => r.filter       fun id => !held.contains id
  | none   => previous ++ newIds.filter fun id => !held.contains id

/-- Where one limit stands right now. Reported by the API and by `orchestra listener show`, so
    that "why has this listener gone quiet" has an answer that does not need the log. -/
structure RateLimitStatus where
  limit         : RateLimit
  /-- Dispatches inside this limit's window. -/
  used          : Nat
  /-- When the window next has room, as epoch seconds. `none` when it has room already — or, for
      the degenerate `max: 0` that `validateListenerConfig` refuses to store, when it never
      will. Read `used < limit.max` for "may dispatch now"; this only answers "when". -/
  nextAllowedAt : Option Int
deriving Repr

/-- `limits` measured against the dispatches a listener has on record. -/
def rateLimitStatuses (limits : List RateLimit) (dispatches : Array String) (now : Int) :
    List RateLimitStatus :=
  limits.map fun l =>
    let inWindow := (dispatches.filterMap Usage.parseIso8601).filter
      (fun t => t > now - (l.windowSeconds : Int))
    let used := inWindow.size
    -- With the window full, room appears when the oldest dispatch still being counted ages out
    -- of it — the `(used - max + 1)`-th oldest, so that a window over its cap (a limit lowered
    -- since) reports when it will be back under rather than merely one short.
    let nextAllowedAt :=
      if used < l.max then none
      else (inWindow.qsort (· < ·))[used - l.max]?.map (· + (l.windowSeconds : Int))
    { limit := l, used, nextAllowedAt }

-- Listener config

/-- What a listener does; *not* what it is called.

    A listener is named by its file: `<config>/listeners/nightly.json` is the listener `nightly`,
    and that spelling is what keys its state file, its API routes and every line the daemon logs
    about it. The document used to carry a `name` of its own as well, and the two could disagree
    — a file placed by hand, or an example copied without renaming it. Everything that *listed*
    listeners then reported the in-file name while everything that *loaded* one built a path from
    it, so a mismatched listener was visible everywhere and reachable nowhere: the daemon's
    supervisor spawned a fiber for it, the fiber re-read its own config by name, found nothing,
    logged "config is gone" and retired, and fifteen seconds later the scan called it new again.
    One name, held by the filesystem, is the only version of this that cannot drift. -/
structure ListenerConfig where
  source          : SourceConfig
  action          : ActionConfig
  intervalSeconds : Nat := 60
  /-- Ceilings on how often this listener may dispatch, all of which a dispatch has to fit
      under. Empty — the default, and what every listener written before this field existed
      gets — means the source's own pace is the only limit. -/
  rateLimits      : List RateLimit := []

instance : ToJson ListenerConfig where
  toJson l :=
    let fields : List (String × Json) := [
      ("source",           ToJson.toJson l.source),
      ("action",           ToJson.toJson l.action),
      ("interval_seconds", ToJson.toJson l.intervalSeconds)
    ]
    -- Written out only when there are any, so that a config round-tripped through this instance
    -- does not grow a field its author never asked for.
    Json.mkObj (if l.rateLimits.isEmpty then fields
                else fields ++ [("rate_limits", ToJson.toJson l.rateLimits)])

instance : FromJson ListenerConfig where
  fromJson? j := do
    let source          ← j.getObjValAs? SourceConfig "source"
    let action          ← j.getObjValAs? ActionConfig "action"
    let intervalSeconds  := j.getObjValAs? Nat "interval_seconds" |>.toOption |>.getD 60
    -- Absent is fine; present and unreadable is not. A limit whose window is a typo would
    -- otherwise parse as no limit at all, which is the failure this field exists to prevent.
    let rateLimits ← match j.getObjVal? "rate_limits" with
      | .error _ => pure []
      | .ok v    => (FromJson.fromJson? v : Except String (List RateLimit))
    return { source, action, intervalSeconds, rateLimits }

-- Listener state

structure ListenerState where
  lastChecked  : String       -- ISO 8601 UTC, empty = never
  processedIds : Array String -- source-specific event IDs already queued
  /-- When false the daemon skips this listener each tick. Toggled via
      `orchestra listener enable/disable` without editing config files. -/
  enabled      : Bool := true
  /-- ISO 8601 UTC stamp of each dispatch this listener has made and a configured rate limit can
      still count, oldest first. Pruned to the longest window on every write, so a listener with
      no `rate_limits` carries none of these at all. -/
  dispatches   : Array String := #[]

instance : ToJson ListenerState where
  toJson s := Json.mkObj [
    ("last_checked",   s.lastChecked),
    ("processed_ids",  ToJson.toJson s.processedIds),
    ("enabled",        Json.bool s.enabled),
    ("dispatches",     ToJson.toJson s.dispatches)
  ]

instance : FromJson ListenerState where
  fromJson? j := do
    let lastChecked  ← j.getObjValAs? String "last_checked"
    let processedIds  := j.getObjValAs? (Array String) "processed_ids" |>.toOption |>.getD #[]
    let enabled       := j.getObjValAs? Bool "enabled" |>.toOption |>.getD true
    let dispatches    := j.getObjValAs? (Array String) "dispatches" |>.toOption |>.getD #[]
    return { lastChecked, processedIds, enabled, dispatches }

-- Directories

/-- Optional override for the listener config directory (tests redirect this, as
    `Project.globalRolesDirOverride` does for roles). -/
initialize listenersConfigDirOverride : IO.Ref (Option System.FilePath) ← IO.mkRef none

def setListenersConfigDirOverride (p : Option System.FilePath) : IO Unit :=
  listenersConfigDirOverride.set p

def listenersConfigDir : IO System.FilePath := do
  match ← listenersConfigDirOverride.get with
  | some p => return p
  | none   => return (← Dirs.configBase) / "listeners"

/-- Optional override for the listener state directory. Paired with the one above so a test that
    redirects configs does not leave state behind in the real data dir. -/
initialize listenerStateDirOverride : IO.Ref (Option System.FilePath) ← IO.mkRef none

def setListenerStateDirOverride (p : Option System.FilePath) : IO Unit :=
  listenerStateDirOverride.set p

def listenerStateDir : IO System.FilePath := do
  match ← listenerStateDirOverride.get with
  | some p => return p
  | none   => return (← Dirs.dataBase) / "listeners" / "state"

-- Config I/O

def listenerConfigFile (name : String) : IO System.FilePath := do
  return (← listenersConfigDir) / s!"{name}.json"

def loadListenerConfig (name : String) : IO (Option ListenerConfig) := do
  Utils.ensureConfigName "listener" name
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


/-- The name a listener config file gives its listener: its basename without the extension.

    Total on purpose — the caller has already established the `.json` suffix, and a stem that
    could not be a config name (a dotfile, say) is filtered by `checkConfigName` rather than by
    failing to be computed. -/
def listenerNameOfFile (fileName : String) : String :=
  (System.FilePath.mk fileName).fileStem.getD fileName

/-- Every listener, paired with the name its file gives it.

    Pairs rather than a `name` field on the config: the name belongs to the store, not to the
    document, and returning it beside the document is what stops a caller from having to decide
    which of two spellings to trust. -/
def loadAllListenerConfigs : IO (Array (String × ListenerConfig)) := do
  let dir ← listenersConfigDir
  if !(← dir.pathExists) then return #[]
  let secrets ← loadSecrets
  let entries ← System.FilePath.readDir dir
  let mut configs : Array (String × ListenerConfig) := #[]
  for entry in entries do
    let file := entry.fileName
    -- skips the state subdirectory entry (it has no .json extension anyway)
    if !file.endsWith ".json" then continue
    let name := listenerNameOfFile file
    -- The name keys the state file and the dashboard's detail route, so a file whose stem cannot
    -- be one — a dotfile, say — is skipped rather than returned. Nothing this API wrote can be
    -- in that state; a file placed by hand can.
    match Utils.checkConfigName "listener" name with
    | .error e => IO.eprintln s!"Warning: ignoring listener config {file}: {e}"; continue
    | .ok _    => pure ()
    let raw := applySecrets secrets (← IO.FS.readFile entry.path)
    match Json.parse raw with
    | .error e => IO.eprintln s!"Warning: failed to parse listener config {file}: {e}"
    | .ok j    =>
      match FromJson.fromJson? j (α := ListenerConfig) with
      | .error e => IO.eprintln s!"Warning: failed to load listener config {file}: {e}"
      | .ok cfg  => configs := configs.push (name, cfg)
  return configs

/-! ### Editing a listener config

`loadListenerConfig` hands back a *parsed* config with `{{secret}}` placeholders already expanded
(`applySecrets`), which is what the daemon needs and exactly what an editor must not have: a
client that fetched one, changed a field and sent it back would persist the expanded secret into
a file whose whole point is that it does not contain one.

So the edit path never round-trips through `ListenerConfig`. `loadListenerConfigRaw` returns the
file as written, placeholders intact, and `saveListenerConfigRaw` stores the text the client
sent. `validateListenerConfig` type-checks that text — after substitution, so a config that
legitimately references a secret is judged against the value the daemon will see — without the
result ever reaching the disk. -/

/-- The listener config file exactly as stored, `{{secret}}` placeholders unexpanded. -/
def loadListenerConfigRaw (name : String) : IO (Option String) := do
  Utils.ensureConfigName "listener" name
  let path ← listenerConfigFile name
  if !(← path.pathExists) then return none
  return some (← IO.FS.readFile path)

/-- Whether `raw` is a listener config that can be stored under `name`.

    Returns the config as the daemon would read it, so a caller can report what it accepted.
    Everything that can be wrong is reported as a sentence naming the field, because the only
    consumer of these is a person who has just typed something wrong. -/
def validateListenerConfig (name : String) (raw : String) :
    IO (Except String ListenerConfig) := do
  match Utils.checkConfigName "listener" name with
  | .error e => return .error e
  | .ok _    => pure ()
  let secrets ← loadSecrets
  let j ← match Json.parse (applySecrets secrets raw) with
    | .error e => return .error s!"the body is not valid JSON: {e}"
    | .ok j    => pure j
  let cfg ← match (FromJson.fromJson? j : Except String ListenerConfig) with
    | .error e => return .error s!"the body is not a listener config: {e}"
    | .ok c    => pure c
  -- A listener is named by its file, so `name` is not a field any more and a body that still
  -- carries one from before is simply ignored — except when the two disagree, which is the one
  -- case where ignoring it would store the document under a name its author did not intend.
  if let .ok stale := j.getObjValAs? String "name" then
    if stale != name then
      return .error s!"the body carries a legacy 'name' field of '{stale}', but it is being \
stored as '{name}'. A listener is named by its file; drop the field, or store this under \
'{stale}'"
  if cfg.intervalSeconds == 0 then
    return .error "'interval_seconds' must be at least 1; a listener polling zero seconds apart \
would spin against its source as fast as the network allows"
  -- A ceiling of nothing is not a ceiling; it is an off switch, and there is already one of
  -- those that does not lose the listener's configured pace along the way.
  if cfg.rateLimits.any (·.max == 0) then
    return .error "a rate limit of 0 dispatches would stop this listener from ever firing; \
to switch one off, use 'orchestra listener disable' instead"
  return .ok cfg

/-- Store a listener config verbatim. Validation is the caller's business — see
    `validateListenerConfig`, which the API and the CLI both run first. -/
def saveListenerConfigRaw (name : String) (raw : String) : IO Unit := do
  Utils.ensureConfigName "listener" name
  Utils.writeFileAtomically (← listenerConfigFile name) raw

/-- Remove a listener config and the state that belonged to it. `false` when there was none.

    The state goes with the config on purpose: it is a list of event ids already handled, and
    keeping it would mean a listener re-created under the same name silently ignored every event
    its predecessor had seen. -/
def deleteListenerConfig (name : String) : IO Bool := do
  Utils.ensureConfigName "listener" name
  let path ← listenerConfigFile name
  if !(← path.pathExists) then return false
  IO.FS.removeFile path
  let statePath := (← listenerStateDir) / s!"{name}.json"
  if ← statePath.pathExists then IO.FS.removeFile statePath
  return true

-- State I/O

def loadListenerState (name : String) : IO ListenerState := do
  Utils.ensureConfigName "listener" name
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
  Utils.ensureConfigName "listener" name
  let dir ← listenerStateDir
  IO.FS.createDirAll dir
  -- Atomic because the daemon reads this file every tick and the API writes it whenever someone
  -- toggles a listener: a truncate-then-write would let a tick land on an empty file and read it
  -- as "never checked, nothing processed", which re-queues every event the listener has seen.
  Utils.writeFileAtomically (dir / s!"{name}.json") (Lean.Json.compress (ToJson.toJson state))

/-- Carry state left under a config's old in-file `name` over to the name its file gives it.

    Listeners used to be named by a `name` field inside the document, and their state — the list
    of event ids already handled — was keyed on that. Now the file names them, so a config whose
    two spellings disagreed would come back under a name with no state behind it and re-fire
    every event it had already handled. This carries the state across instead, once: afterwards
    nothing is left under the old name, so a later run finds nothing to do and says nothing.

    The daemon runs it at start-up, before any listener polls. It is not the only writer of
    listener state, which is why the destination is not assumed to be free: `PUT
    /api/v1/listeners/{name}/enabled` writes one too, and since the CLI/backend split the API can
    be a process of its own that never runs this. A `disable` issued under the new name before
    the daemon came up leaves a state file with no history in it, and skipping on its account
    would abandon the listener's history for good — so that one is filled in, keeping the
    `enabled` just set. A destination that *has* history belongs to a listener already polling
    under this name, and is left alone. -/
def migrateListenerStateNames : IO Unit := do
  let dir ← listenersConfigDir
  if !(← dir.pathExists) then return
  let stateDir ← listenerStateDir
  for entry in ← System.FilePath.readDir dir do
    let file := entry.fileName
    if !file.endsWith ".json" then continue
    let name := listenerNameOfFile file
    if (Utils.checkConfigName "listener" name).toOption.isNone then continue
    -- Read raw and unsubstituted: a legacy name is a plain string, and a config that no longer
    -- parses still has state worth keeping under the right name.
    let raw ← IO.FS.readFile entry.path
    let some legacy := (Json.parse raw).toOption.bind (·.getObjValAs? String "name" |>.toOption)
      | continue
    if legacy == name then continue
    if (Utils.checkConfigName "listener" legacy).toOption.isNone then continue
    -- The legacy name may be a listener in its own right — `a.json` claiming to be `b` while
    -- `b.json` sits beside it. That state belongs to `b`, and moving it would hand one
    -- listener's processed events to another.
    if ← (dir / s!"{legacy}.json").pathExists then continue
    let source := stateDir / s!"{legacy}.json"
    let dest   := stateDir / s!"{name}.json"
    if !(← source.pathExists) then continue
    if ← dest.pathExists then
      let standing ← loadListenerState name
      if !standing.lastChecked.isEmpty || !standing.processedIds.isEmpty then continue
      let carried ← loadListenerState legacy
      saveListenerState name { carried with enabled := standing.enabled }
      IO.FS.removeFile source
    else
      IO.FS.rename source dest
    IO.println s!"Listener '{name}': carried state over from its old name '{legacy}'. \
A listener is named by its file now; the config's own 'name' field is ignored."

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
  -- Neither named, by the action or by the event source, means a repository-independent task:
  -- the listener watches something that is not one repository's business — a shell command, a
  -- tracker — and the task it queues runs in a scratch workspace. Every GitHub event source
  -- supplies both variables, so this is reachable only from a listener that genuinely has no
  -- repository to hand, never from one whose event lost it on the way.
  let repo ← if upstreamStr.isEmpty && forkStr.isEmpty then pure none else do
    let upstream ← IO.ofExcept (Repository.parse upstreamStr)
    let fork     ← IO.ofExcept (Repository.parse forkStr)
    pure (some { upstream, fork : RepoPair })
  -- Resolve issue_number: prefer the explicit template field from the action config;
  -- fall back to the `issue_number` variable supplied by the event source.
  let issueNumber : Option Nat :=
    match action.issueNumber with
    | some tmpl => (renderTemplate tmpl vars).toNat?
    | none      => vars.find? (fun p => p.1 == "issue_number") |>.map (·.2) |>.bind (·.toNat?)
  IO.eprintln s!"[listener] buildQueueEntry: model={repr action.model} budget={repr action.budget} agent={repr action.agent} priority={action.priority}"
  return {
    id, createdAt, status := .pending,
    repo
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
    authSources  := action.authSources
    authMode     := action.authMode
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

/-! ### Bounding the roles that do not pre-claim

A role that pre-claims cannot be dispatched twice onto one issue: the second dispatch takes the
daemon's claim mutex, finds the issue already claimed and is dropped, so its cap is really a cap
on *concurrent issues* and the tracker enforces it. Nothing arbitrates for a role that does not
pre-claim. A reviewer is handed whatever is awaiting review — including, on the next tick, the
issue a reviewer is already on — and an `always` role is handed no issue at all and picks its own.
Against one open issue, a cap of three is then three agents doing the same piece of work.

`limit_unclaimed_to_open_issues` bounds those caps by the work actually in scope. The two halves
below are the arithmetic, kept pure and separate because the bound differs with the binding: an
issue-bound role draws from the label-wide candidate set, an unbound one from the issues its own
root owns. -/

/-- A cap lowered to the work available to it.

    Never lowered below 1: at zero the trigger's own verdict (`noWorkableIssue`,
    `nothingToReview`) already says why nothing was dispatched, and that is a better log line than
    a cap of zero, which `dispatcherDecisions` reports as "auto-dispatch is off". A configured cap
    of zero stays zero — that one really is off. -/
def capToAvailable (cap available : Nat) : Nat := min cap (max 1 available)

/-- `caps` for issue-bound roles, each lowered to the issues its trigger may draw from.

    Roles that pre-claim keep their cap: the claim is what stops two of them meeting on one issue,
    and it does so per issue rather than per tick. `idle` roles keep theirs too — they are bound
    to nothing and are not dispatched by this dispatcher anyway. -/
def limitBoundCaps (roles : Array Project.Role) (caps : List (String × Nat))
    (workable reviewable : Nat) : List (String × Nat) :=
  caps.map fun (name, cap) =>
    match (roles.find? (·.name == name)).bind (·.dispatch) with
    | none   => (name, cap)
    | some d =>
      if d.preClaim then (name, cap)
      else match d.trigger with
        | .hasOpenIssues     => (name, capToAvailable cap workable)
        | .hasInReviewIssues => (name, capToAvailable cap reviewable)
        | .idle | .always    => (name, cap)

/-- `caps` for unbound roles at one root, each lowered to the open issues that root owns
    (`Project.LabelDispatchSets.openUnder`). Unbound roles never pre-claim — there is no issue at
    spawn time to claim — so the limit applies to all of them.

    Counted over the open issues in scope rather than the dispatch candidates, because an unbound
    role picks its own work and is not held to the leaf rule: a decomposed container can still
    need planning or carry a pull request of its own, and a dependency-blocked issue can still be
    the thing to look at. The cost is that a container counts as one issue like anything else, so
    a deep tree bounds higher than the work at its leaves. -/
def limitUnboundCaps (caps : List (String × Nat)) (openUnderRoot : Nat) : List (String × Nat) :=
  caps.map fun (name, cap) => (name, capToAvailable cap openUnderRoot)

/-- Those of `issues` that no active queue entry is already bound to.

    The cap says how many agents may run; this says which issues are still worth offering them.
    `dispatcherDecisions` keeps two roles off one issue within a tick (`taken`), but that array is
    fresh every tick, and for a role that does not pre-claim nothing outlives the tick — so the
    issue an agent is already on is offered again on the next one. Applied to the candidate sets
    and not to the tally the caps are counted against: an agent at work is still one agent. -/
def unattendedIssues (issues : Array Project.Issue) (activeEntries : Array Queue.QueueEntry) :
    Array Project.Issue :=
  let busy := activeEntries.filterMap (·.issueId.map (·.val))
  issues.filter fun i => !busy.contains i.id.val

/-- The caps a limit actually lowered, as (role, configured, effective). Reported by the caller
    rather than applied silently: a cap that does not do what the config file says is exactly the
    thing to say out loud. -/
def loweredCaps (before after : List (String × Nat)) : List (String × Nat × Nat) :=
  before.filterMap fun (name, cap) =>
    match after.lookup name with
    | some cap' => if cap' < cap then some (name, cap, cap') else none
    | none      => none

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
def buildRoleEntry (appConfig : AppConfig) (project : Project.Project) (role : Project.Role)
    (issue? : Option Project.Issue) (instructions : String := "")
    (targetOverride : Option Project.RepoTarget := none) :
    IO (Option Queue.QueueEntry) := do
  let target := targetOverride
            <|> issue?.bind (Project.effectiveTarget project ·)
            <|> project.defaultTarget
  let some target' := target | return none
  -- The agent pushes to `fork`; PRs land in `upstream` (= the target repo). When the App can push
  -- to the target directly the fork is the target itself, otherwise it is a fork in the configured
  -- default organisation. `none` means the task cannot be dispatched (nothing writable to push to).
  let some fork ← GitHub.resolveFork appConfig target'.repo | return none
  let id ← TaskStore.generateId
  let createdAt ← TaskStore.currentIso8601
  -- Two extra fetches per dispatch (not per tick) so the thread and the context notes land in
  -- the prompt: a worker picking up a rejected issue, or one an earlier worker already learned
  -- something about, would otherwise have to know to ask for either.
  let comments ← match issue? with
    | some i => Project.renderCommentThread i.id
    | none   => pure none
  let context ← match issue? with
    | some i => Project.renderContextNotesForPrompt i.id
    | none   => pure none
  let vars   := Project.renderVarsFor project issue? instructions targetOverride comments context
  let prompt := Project.render role.promptTemplate vars
  return some
    { id, createdAt
    , repo          := some { upstream := target'.repo, fork }
    , mode          := .pr
    , prompt
    , goal          := Project.goalFor issue?
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

  | .labelDispatcher label caps limitUnclaimed excludeRoots => do
    let some sets ← Project.issuesWithLabel label excludeRoots
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
    -- Named in the tick's own line rather than once at startup: "nothing workable" on a tracker
    -- whose only labelled issue is the root is otherwise indistinguishable from a broken config.
    -- Says what the option did, not what it prevented: a root with children was never workable
    -- anyway, so claiming each one as work held back would overstate it.
    let rootNote :=
      if excludeRoots then
        s!", {sets.roots.size} labelled root(s) treated as epics by exclude_root_issues \
          (neither dispatched onto nor counted)"
      else ""
    IO.println s!"[dispatcher] label '{label}': {issues.size} workable \
      (of which {reworking} sent back for changes), {sets.reviewable.size} with a PR attached \
      of which {reviewable.size} await a reviewer, \
      {gaps.size} skipped for want of a target{rootNote}; roles available: \
      {String.intercalate ", " (roles.map (·.name)).toList}"
    -- Roles that bind an issue and roles that don't are dispatched by different rules here, so
    -- they are counted and decided separately. An `always` role has no issue to bind, so it is
    -- scoped to a labelled root instead (one spawn set per root) — see below.
    let (configuredBound, unboundCaps) := splitCapsByBinding roles caps
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
    -- Bound roles that do not pre-claim draw from the label-wide candidate sets, so that is what
    -- their cap is lowered to — the sets *before* the busy issues come out of them, since the
    -- agents on those are exactly the ones the cap is counting. Unbound roles are lowered per
    -- root, below, where the root is known.
    let boundCaps :=
      if limitUnclaimed then limitBoundCaps roles configuredBound issues.size reviewable.size
      else configuredBound
    for (n, cap, cap') in loweredCaps configuredBound boundCaps do
      IO.println s!"[dispatcher] cap for '{n}' lowered from {cap} to {cap'} by \
        limit_unclaimed_to_open_issues: it does not pre-claim, so it is bounded by the issues in \
        scope for its trigger (counted above)"
    -- An issue an agent is already on is out of this tick's selection, so the cap is spent on
    -- work nobody is doing (`unattendedIssues`). Under the limit only: it lowers what would
    -- otherwise dispatch, which is the option's whole business.
    let free (is : Array Project.Issue) : Array Project.Issue :=
      if limitUnclaimed then unattendedIssues is activeEntries else is
    let (freeIssues, freeReviewable) := (free issues, free reviewable)
    let held := (issues.size - freeIssues.size) + (reviewable.size - freeReviewable.size)
    if held > 0 then
      IO.println s!"[dispatcher] {held} issue(s) left out of this tick's selection by \
        limit_unclaimed_to_open_issues: an agent is on them already"
    let input : DispatcherInput :=
      { activeByRole := active, issues := freeIssues, reviewable := freeReviewable
      , caps := boundCaps, roles }
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
        -- Per root, because the cap is: filling one root's cap leaves the others alone, and the
        -- work that bounds it is the work that root owns. A root counts itself, so this is never
        -- zero — a root with an empty subtree still gets the one agent that would plan it.
        let openHere := sets.openUnder root.id
        let rootCaps := if limitUnclaimed then limitUnboundCaps unboundCaps openHere
                        else unboundCaps
        for (n, cap, cap') in loweredCaps unboundCaps rootCaps do
          IO.println s!"[dispatcher] root {root.id.toString}: cap for '{n}' lowered from {cap} \
            to {cap'} by limit_unclaimed_to_open_issues: it claims nothing at spawn, and this \
            root has {openHere} open issue(s) in scope"
        let rootInput : DispatcherInput :=
          { activeByRole := unboundActiveByRole allEntries root.id
          , issues := #[], reviewable := #[], caps := rootCaps, roles }
        for d in dispatcherDecisions rootInput do
          IO.println s!"[dispatcher] root {root.id.toString} \"{root.title}\": {renderDecision d}"
        for s in dispatcherTick rootInput do
          unboundEvents := unboundEvents.push
            ("", [ ("role_name",     s.roleName)
                 , ("project_id",    root.id.toString)
                 , ("target_repo",   target.repo.toString)
                 , ("target_branch", target.branch) ])
    return (boundEvents ++ unboundEvents, none)

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
