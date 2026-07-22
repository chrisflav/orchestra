import Lean.Data.Json
import Std.Internal.UV.TCP
import Std.Net
import Orchestra.Config
import Orchestra.Dirs
import Orchestra.Queue
import Orchestra.TaskStore
import Orchestra.Listener
import Orchestra.Project.Basic
import Orchestra.Project.Claim
import Orchestra.Usage
import Orchestra.Dashboard.Site

open Lean (Json ToJson)
open Std.Net
open Std.Internal.UV.TCP
open Orchestra.Project (Project Issue IssueStatus Claim
  loadAllProjects loadProject loadIssues loadClaims)

/-!
# Orchestra Web Dashboard

The dashboard is split into two independently-run pieces:

  * **`generate`** (in `Orchestra/Dashboard/Site.lean`) writes a fully static
    website (HTML/CSS/JS) into a target directory. The pages are authored as
    Verso `#doc (Page)` documents and emitted with Verso's Blog pipeline
    (`blogMain`), like `leanprover/verso-website`. The generated files can be
    served by any plain HTTP server — orchestra ships no front-end server.

  * **`serve`** (this module) runs a minimal HTTP/1.1 server (bound to
    `127.0.0.1`) exposing only the JSON API under `/api/...` and Server-Sent
    Event streams under `/sse/...`, with permissive CORS so the separately-served
    static site can fetch cross-origin. It serves no HTML.

Adding a page = a verso `#doc (Page)` shell (in `Dashboard/Pages/`) wired into
`versoSite` + a Lean `IO Json` builder (here) + a JS renderer keyed on
`data-page` (in `Dashboard/dashboard.js`).
-/

namespace Orchestra.Dashboard

-- API authentication
--
-- The server binds to `127.0.0.1` but replies with a wildcard `Access-Control-
-- Allow-Origin`, so any web page the user visits could otherwise `fetch` the
-- API cross-origin. A shared bearer token gates every `/api/...` and `/sse/...`
-- request. `generate` bakes the same token into the static site (see
-- `Dashboard/Site.lean`) and `dashboard.js` presents it on each request.

/-- The env var that overrides the persisted dashboard API token. -/
def tokenEnvVar : String := "ORCHESTRA_DASHBOARD_TOKEN"

/-- Where an auto-generated token is persisted so that `generate` and `serve`
    (typically separate invocations) agree on the same secret. -/
private def tokenFile : IO System.FilePath := do
  return (← Dirs.dataBase) / "dashboard.token"

private def hexDigits : Array Char := "0123456789abcdef".toList.toArray

private def toHex (b : UInt8) : String :=
  let n := b.toNat
  String.ofList [hexDigits[n >>> 4]!, hexDigits[n &&& 0xf]!]

/-- Generate a fresh random token (60 hex chars / 30 bytes of entropy) sourced
    from `/dev/urandom`. -/
private def genToken : IO String := do
  let bytes ← IO.FS.withFile "/dev/urandom" .read fun h => h.read 30
  return String.join (bytes.toList.map toHex)

/-- Resolve the dashboard API token, in priority order:
    1. an explicit `flagToken` (e.g. `--token`),
    2. the `ORCHESTRA_DASHBOARD_TOKEN` env var,
    3. a token previously persisted under the data dir,
    4. otherwise a freshly generated token (persisted for reuse).

    Shared by `generate` (which bakes the token into the site) and `serve`
    (which enforces it), so both agree without the user wiring anything up. -/
def resolveToken (flagToken : Option String := none) : IO String := do
  if let some t := flagToken then
    if !t.trimAscii.isEmpty then return t.trimAscii.toString
  if let some t ← IO.getEnv tokenEnvVar then
    if !t.trimAscii.isEmpty then return t.trimAscii.toString
  let path ← tokenFile
  if ← path.pathExists then
    let t := (← IO.FS.readFile path).trimAscii.toString
    if !t.isEmpty then return t
  let t ← genToken
  IO.FS.createDirAll (← Dirs.dataBase)
  IO.FS.writeFile path (t ++ "\n")
  -- Best-effort: restrict the token file to the owner.
  try
    let _ ← IO.Process.run { cmd := "chmod", args := #["600", path.toString] }
  catch _ => pure ()
  return t

-- TCP helper (mirrors the one in Orchestra.Server)

private def awaitTcp (p : IO.Promise (Except IO.Error α)) : IO α := do
  let result ← IO.wait p.result!
  match result with
  | .error e => throw e
  | .ok v    => return v

-- HTTP types

private structure HttpRequest where
  method  : String
  path    : String
  headers : Array (String × String)
  body    : String

private structure HttpResponse where
  status  : Nat
  headers : Array (String × String)
  body    : String

private def statusText : Nat → String
  | 200 => "OK"           | 204 => "No Content"
  | 400 => "Bad Request"  | 404 => "Not Found"
  | 401 => "Unauthorized" | 405 => "Method Not Allowed"
  | n   => s!"Status {n}"

/-- Permissive CORS headers so a separately-served static front-end can call the
    API/SSE endpoints from another origin. -/
private def corsHeaders : Array (String × String) :=
  #[("Access-Control-Allow-Origin", "*"),
    ("Access-Control-Allow-Methods", "GET, OPTIONS"),
    ("Access-Control-Allow-Headers", "Content-Type, Authorization")]

private def httpResponse (resp : HttpResponse) : String :=
  let statusLine := s!"HTTP/1.1 {resp.status} {statusText resp.status}\r\n"
  let contentLen := s!"Content-Length: {resp.body.utf8ByteSize}\r\n"
  let hdrs := resp.headers.map (fun (k, v) => s!"{k}: {v}\r\n") |>.toList
  statusLine ++ String.join hdrs ++ contentLen ++ "Connection: close\r\n\r\n" ++ resp.body

private def jsonResp (j : Json) (status : Nat := 200) : HttpResponse :=
  { status
    headers := #[("Content-Type", "application/json; charset=utf-8")] ++ corsHeaders
    body    := Json.compress j }

-- The static-site generator (`generate`, the theme, the page shells) lives in
-- `Orchestra/Dashboard/Site.lean`; this module contributes only the JSON API and
-- SSE server below.

-- View-model builders: each `/api/...` endpoint produces a `Json` value
-- tailored for the corresponding page renderer in `dashboard.js`.

private def qStText : Queue.QueueStatus → String
  | .pending    => "pending"   | .running    => "running"
  | .done       => "done"      | .failed     => "failed"
  | .unfinished => "unfinished"| .cancelled  => "cancelled"

private def tStText : TaskStore.TaskStatus → String
  | .running    => "running"   | .completed  => "completed"
  | .failed     => "failed"    | .unfinished => "unfinished"
  | .cancelled  => "cancelled"

private def cStText : Queue.ConcertStatus → String
  | .running   => "running"  | .done      => "done"
  | .failed    => "failed"   | .cancelled => "cancelled"

private def queueEntryJson (e : Queue.QueueEntry) : Json :=
  Json.mkObj [
    ("id",            e.id),
    ("status",        qStText e.status),
    ("fork",          e.fork.toString),
    ("createdAt",     e.createdAt),
    ("priority",      ToJson.toJson e.priority),
    ("series",        e.series.getD ""),
    ("concertId",     e.concertId.getD ""),
    ("concertStepKey", e.concertStepKey.getD ""),
    ("prompt",        e.prompt)
  ]

private def taskRecJson (r : TaskStore.TaskRecord) : Json :=
  Json.mkObj [
    ("id",        r.id),
    ("status",    tStText r.status),
    ("fork",      r.fork.toString),
    ("createdAt", r.createdAt),
    ("series",    r.series.getD ""),
    ("prompt",    r.prompt)
  ]

private def concertRunJson (r : Queue.ConcertRun) : Json :=
  Json.mkObj [
    ("id",           r.id),
    ("status",       cStText r.status),
    ("name",         r.name.getD ""),
    ("workflowFile", r.workflowFile.getD ""),
    ("startedAt",    r.startedAt),
    ("finishedAt",   r.finishedAt.getD "")
  ]

-- Authentication sources
--
-- The dashboard is a *reader* of `Orchestra.Usage`'s on-disk state, never a poller: the
-- queue daemon refreshes it on its own cadence, and the usage endpoint meters requests, so
-- a page that re-polled on every SSE tick (every two seconds) would rate-limit the very
-- monitor it is displaying. Every row therefore also carries when it was last polled, so a
-- stale view reads as stale rather than as current.

/-- How long ago a past epoch-second timestamp was, in words. `none` reads as "never". -/
private def agoText (epoch : Option Int) (now : Int) : String :=
  match epoch with
  | none   => "never"
  | some e =>
    if now ≤ e then "just now" else
    let secs := (now - e).toNat
    if secs < 60 then s!"{secs}s ago"
    else if secs < 3600 then s!"{secs / 60}m ago"
    else if secs < 86400 then s!"{secs / 3600}h {(secs % 3600) / 60}m ago"
    else s!"{secs / 86400}d ago"

private def limitJson (l : Usage.Limit) (now : Int) : Json :=
  Json.mkObj [
    ("kind",     l.kind.toString),
    ("scope",    l.scopeModel.getD ""),
    ("percent",  ToJson.toJson l.percent),
    ("severity", l.severity),
    ("active",   Json.bool l.isActive),
    ("resets",   match l.resetsAt.bind Usage.parseIso8601 with
      | some r => Json.str (Usage.relativeToNow r now)
      | none   => Json.str "")
  ]

/-- One configured authentication source, joined with whatever the usage store knows about it.

    Availability is judged with no model, which is what a reader with no task in hand can ask:
    a `weekly_scoped` limit only closes one model family, so a source carrying nothing but an
    exhausted scoped limit is genuinely still usable — and the limit rows show it regardless. -/
private def authSourceJson (backend : String) (src : AuthSource) (isDefault : Bool) (now : Int)
    : IO Json := do
  let st ← Usage.loadState backend src.label
  let (kind, baseUrl) := match src.kind with
    | .oauthToken _    => ("oauth", "")
    | .apiKey _ base   => ("api-key", base.getD "")
  let (state, reason, resets) := match Usage.availabilityOf st none now with
    | .available   => ("available", "", "")
    | .blocked u r => ("blocked", r, match u with
        | some u => Usage.relativeToNow u now
        | none   => "")
  let backoff := match st.pollAfter with
    | some p => if p > now then Usage.relativeToNow p now else ""
    | none   => ""
  return Json.mkObj [
    ("label",     src.label),
    ("kind",      kind),
    ("baseUrl",   baseUrl),
    ("isDefault", Json.bool isDefault),
    -- Only OAuth sources have a subscription to report on; an API-key source bills per token
    -- and has no window to poll, so an empty limit list on one is expected, not a gap.
    ("pollable",  Json.bool (kind == "oauth")),
    ("state",     state),
    ("reason",    reason),
    ("resets",    resets),
    ("pressure",  ToJson.toJson (Usage.pressureOf st none)),
    ("polled",    agoText st.fetchedEpoch now),
    -- `lastUsedTick` is nanoseconds (it orders dispatches); everything else here is seconds.
    ("lastUsed",  agoText (st.lastUsedTick.map (· / 1000000000)) now),
    ("lastError", st.lastError.getD ""),
    ("backoff",   backoff),
    ("limits",    Json.arr (st.limits.map (limitJson · now)))
  ]

/-- Every configured backend and source, or the reason the config could not be read.

    The config is re-read per request rather than captured at start-up, so a source added to
    `config.json` shows up without restarting the server. -/
private def authApi (configPath : Option System.FilePath) : IO Json := do
  let now ← Usage.nowEpoch
  let cfg ← try
      pure (Except.ok (← loadAppConfig configPath))
    catch e => pure (Except.error (toString e))
  match cfg with
  | .error e => return Json.mkObj [("configError", e), ("backends", Json.arr #[])]
  | .ok cfg =>
    let backends ← cfg.agentAuthConfigs.mapM fun a => do
      let sources ← a.authSources.mapM fun src => do
        -- `Usage.candidatesFor`: an explicit default, or the sole source when there is
        -- exactly one. Marking it matters because it is what a task that names no source gets.
        let isDefault := match a.defaultAuthSource with
          | some d => d == src.label
          | none   => a.authSources.size == 1
        authSourceJson a.name src isDefault now
      return Json.mkObj [
        ("name",          a.name),
        ("defaultSource", a.defaultAuthSource.getD ""),
        ("sources",       Json.arr sources)
      ]
    return Json.mkObj [("configError", ""), ("backends", Json.arr backends)]

/-- Available/total across every configured source, for the overview's stat box. Silent about
    failure: the overview must still render when the config is unreadable. -/
private def authCounts (configPath : Option System.FilePath) : IO (Nat × Nat) := do
  try
    let cfg ← loadAppConfig configPath
    let now ← Usage.nowEpoch
    let mut total := 0
    let mut free := 0
    for a in cfg.agentAuthConfigs do
      for src in a.authSources do
        total := total + 1
        let st ← Usage.loadState a.name src.label
        if (Usage.availabilityOf st none now).isAvailable then free := free + 1
    return (free, total)
  catch _ => return (0, 0)

private def overviewApi (configPath : Option System.FilePath) : IO Json := do
  let entries  ← Queue.loadAllEntries
  let tasks    ← TaskStore.loadAllTasks
  let lsCfgs   ← Listener.loadAllListenerConfigs
  let concerts ← Queue.loadAllConcertRuns
  let (authFree, authTotal) ← authCounts configPath
  let running := entries.filter (·.status == .running)
  let pending := entries.filter (·.status == .pending)
  let failed  := entries.filter (·.status == .failed)
  let rConcerts := concerts.filter (·.status == .running)
  let active := running ++ pending
  let recent := tasks.toList.take 10
  return Json.mkObj [
    ("counts", Json.mkObj [
      ("running",    ToJson.toJson running.size),
      ("pending",    ToJson.toJson pending.size),
      ("failed",     ToJson.toJson failed.size),
      ("concerts",   ToJson.toJson rConcerts.size),
      ("listeners",  ToJson.toJson lsCfgs.size),
      ("totalTasks", ToJson.toJson tasks.size),
      ("authFree",   ToJson.toJson authFree),
      ("authTotal",  ToJson.toJson authTotal)
    ]),
    ("activeQueue", Json.arr (active.map queueEntryJson)),
    ("recentTasks", Json.arr (recent.map taskRecJson).toArray)
  ]

private def queueApi : IO Json := do
  let entries  ← Queue.loadAllEntries
  let concerts ← Queue.loadAllConcertRuns
  return Json.mkObj [
    ("concerts", Json.arr (concerts.map concertRunJson)),
    ("entries",  Json.arr (entries.map queueEntryJson))
  ]

private def concertsApi : IO Json := do
  let concerts ← Queue.loadAllConcertRuns
  return Json.mkObj [
    ("concerts", Json.arr (concerts.map concertRunJson))
  ]

private def concertDetailApi (id : String) : IO (Option Json) := do
  match ← Queue.loadConcertRun id with
  | none => return none
  | some r =>
    let entries ← Queue.loadAllEntries
    let steps := entries.filter (fun e => e.concertId == some id)
    return some (Json.mkObj [
      ("concert", concertRunJson r),
      ("steps",   Json.arr (steps.map queueEntryJson))
    ])

private def sourceSummary : Listener.SourceConfig → (String × String)
  | .githubIssues repos labels _ _    =>
      let r := String.intercalate ", " (repos.map (·.upstream.toString))
      ("github-issues",
        r ++ (if labels.isEmpty then "" else s!" [labels: {String.intercalate "," labels}]"))
  | .githubPrReviews repos labels _ _ =>
      let r := String.intercalate ", " (repos.map (·.upstream.toString))
      ("github-pr-reviews",
        r ++ (if labels.isEmpty then "" else s!" [labels: {String.intercalate "," labels}]"))
  | .githubComments repos labels _ _  =>
      let r := String.intercalate ", " (repos.map (·.upstream.toString))
      ("github-comments",
        r ++ (if labels.isEmpty then "" else s!" [labels: {String.intercalate "," labels}]"))
  | .shell cmd args                   => ("shell", s!"{cmd} {String.intercalate " " args}")
  | .projectDispatcher pid caps       =>
      let cs := String.intercalate ", " (caps.map (fun (n,c) => s!"{n}={c}"))
      ("project-dispatcher", s!"project={pid.toString} caps=[{cs}]")
  | .labelDispatcher label caps       =>
      let cs := String.intercalate ", " (caps.map (fun (n,c) => s!"{n}={c}"))
      ("label-dispatcher", s!"label={label} caps=[{cs}]")
  | .githubLabelCount repos labels max kind =>
      let r := String.intercalate ", " (repos.map (·.upstream.toString))
      ("github-label-count", s!"{r} kind={kind} max={max} labels=[{String.intercalate "," labels}]")
  | .githubLabels repos labels kind _ =>
      let r := String.intercalate ", " (repos.map (·.upstream.toString))
      ("github-labels", s!"{r} kind={kind} labels=[{String.intercalate "," labels}]")

private def sourceExtras : Listener.SourceConfig → List (String × String)
  | .githubIssues _ _ trig authU
  | .githubPrReviews _ _ trig authU
  | .githubComments _ _ trig authU =>
      [("trigger", trig), ("authorized users", String.intercalate ", " authU)]
  | .githubLabels _ _ _ authU =>
      [("authorized users", String.intercalate ", " authU)]
  | _ => []

private def listenerSummaryJson (c : Listener.ListenerConfig) (st : Listener.ListenerState) : Json :=
  let (srcType, _) := sourceSummary c.source
  Json.mkObj [
    ("name",            c.name),
    ("enabled",         Json.bool st.enabled),
    ("sourceType",      srcType),
    ("intervalSeconds", ToJson.toJson c.intervalSeconds),
    ("lastChecked",     st.lastChecked),
    ("eventCount",      ToJson.toJson st.processedIds.size)
  ]

private def listenersApi : IO Json := do
  let cfgs ← Listener.loadAllListenerConfigs
  let rows : Array Json ← cfgs.mapM fun c => do
    let st ← Listener.loadListenerState c.name
    return listenerSummaryJson c st
  return Json.mkObj [("listeners", Json.arr rows)]

private def actionJson (a : Listener.ActionConfig) : Json :=
  Json.mkObj [
    ("mode",           ToJson.toJson a.mode),
    ("upstream",       a.upstream),
    ("fork",           a.fork),
    ("series",         a.series.getD ""),
    ("backend",        a.backend.getD ""),
    ("model",          a.model.getD ""),
    ("workflowPath",   a.workflowPath.getD ""),
    ("priority",       ToJson.toJson a.priority),
    ("promptTemplate", a.promptTemplate)
  ]

private def listenerDetailApi (name : String) : IO (Option Json) := do
  match ← Listener.loadListenerConfig name with
  | none => return none
  | some c =>
    let st ← Listener.loadListenerState c.name
    let (srcType, srcDetail) := sourceSummary c.source
    let extras := sourceExtras c.source
    let extrasJson : Array Json := (extras.filter (fun (_, v) => ! v.isEmpty)).map
      (fun (k, v) => (Json.arr #[Json.str k, Json.str v])) |>.toArray
    let recent := st.processedIds.toList.reverse.take 50
    return some (Json.mkObj [
      ("name",            c.name),
      ("enabled",         Json.bool st.enabled),
      ("intervalSeconds", ToJson.toJson c.intervalSeconds),
      ("lastChecked",     st.lastChecked),
      ("eventCount",      ToJson.toJson st.processedIds.size),
      ("sourceType",      srcType),
      ("sourceDetail",    srcDetail),
      ("sourceExtras",    Json.arr extrasJson),
      ("action",          actionJson c.action),
      ("recentEvents",    Json.arr (recent.map Json.str).toArray)
    ])

private def tasksApi : IO Json := do
  let tasks ← TaskStore.loadAllTasks
  return Json.mkObj [
    ("tasks", Json.arr ((tasks.toList.take 50).map taskRecJson).toArray)
  ]

-- Projects & issues

private def issueStText : IssueStatus → String
  | .open      => "open"      | .claimed   => "claimed"
  | .completed => "completed" | .abandoned => "abandoned"

/-- Per-status issue counts, for a project's issue set. The four statuses here are the
    whole set taxis backs (`Orchestra/Project/Basic.lean`): "in review", "blocked" and
    "rejected" are read from the tree and from GitHub rather than stored, so they are not
    counted here. -/
private def issueCountsJson (issues : Array Issue) : Json :=
  let n := fun (s : IssueStatus) => (issues.filter (·.status == s)).size
  Json.mkObj [
    ("open",      ToJson.toJson (n .open)),
    ("claimed",   ToJson.toJson (n .claimed)),
    ("completed", ToJson.toJson (n .completed)),
    ("abandoned", ToJson.toJson (n .abandoned))
  ]

private def projectSummaryJson (p : Project) (issues : Array Issue) : Json :=
  Json.mkObj [
    -- Ids are taxis integers; emitted as strings so the front-end can compare them and put
    -- them in URLs without worrying about numeric coercion.
    ("id",            Json.str p.id.toString),
    ("name",          p.name),
    ("description",   p.description.getD ""),
    ("createdAt",     p.createdAt),
    ("defaultTarget", match p.defaultTarget with
      | some t => Json.str s!"{t.repo}@{t.branch}" | none => Json.str ""),
    ("issueCount",    ToJson.toJson issues.size),
    ("counts",        issueCountsJson issues)
  ]

/-- A single issue as a dependency-graph node: identity, status, its parent and
    dependency edges (both by issue id), and who (if anyone) currently holds it. -/
private def issueNodeJson (i : Issue) (claim : Option Claim) : Json :=
  Json.mkObj [
    ("id",           Json.str i.id.toString),
    ("title",        i.title),
    ("status",       issueStText i.status),
    ("parentId",     match i.parentId with | some p => Json.str p.toString | none => Json.str ""),
    ("dependencies", Json.arr (i.dependencies.map (Json.str ·.toString))),
    ("prCount",      ToJson.toJson i.attachedPRs.size),
    ("claimedBy",    match claim with | some c => Json.str c.agent | none => Json.str ""),
    ("updatedAt",    i.updatedAt)
  ]

private def projectsApi : IO Json := do
  let projects ← loadAllProjects
  let rows ← projects.mapM fun p => do
    return projectSummaryJson p (← loadIssues p.id)
  return Json.mkObj [("projects", Json.arr rows)]

private def projectDetailApi (id : String) : IO (Option Json) := do
  -- A taxis id is an integer; anything else is a 404 rather than a lookup, so a stray
  -- `?id=` in the URL bar can't reach the tracker at all.
  let some pid := Taxis.IssueId.parse? id | return none
  match ← loadProject pid with
  | none => return none
  | some p =>
    let issues ← loadIssues pid
    let claims ← loadClaims pid
    let nodes := issues.map fun i =>
      let claim := (claims.find? (fun (iid, _) => iid == i.id)).map (·.2)
      issueNodeJson i claim
    return some (Json.mkObj [
      ("project", projectSummaryJson p issues),
      ("issues",  Json.arr nodes)
    ])

/-- Parse the per-task structured JSONL log into an array of `Json` events.
    Returns an empty array if the file is missing. -/
private def loadTaskLog (fork : Repository) (id : String) : IO (Array Json) := do
  let path := (← Dirs.dataBase) / "logs" / fork.toString / s!"{id}.log"
  if !(← path.pathExists) then return #[]
  let raw ← IO.FS.readFile path
  let mut out : Array Json := #[]
  for line in raw.splitOn "\n" do
    if line.trimAscii.isEmpty then continue
    match Json.parse line with
    | .ok j    => out := out.push j
    | .error _ => out := out.push (Json.mkObj [("type", "unknown"), ("event_type", "parse_error")])
  return out

private def taskDetailApi (id : String) : IO (Option Json) := do
  let record  ← TaskStore.loadTask id
  let entries ← Queue.loadAllEntries
  let qEntry  := entries.find? (·.id == id)
  let infoOpt : Option (Repository × String × String × String) :=
    match record with
    | some r => some (r.fork, tStText r.status, r.createdAt, r.prompt)
    | none =>
      match qEntry with
      | some q => some (q.fork, qStText q.status, q.createdAt, q.prompt)
      | none   => none
  match infoOpt with
  | none => return none
  | some (fork, st, createdAt, prompt) =>
    let log ← loadTaskLog fork id
    return some (Json.mkObj [
      ("id",        id),
      ("status",    st),
      ("fork",      fork.toString),
      ("createdAt", createdAt),
      ("prompt",    prompt),
      ("log",       Json.arr log)
    ])

/-- Dispatch a `/api/...` or `/sse/...` kind string to the matching builder. -/
private def renderApi (configPath : Option System.FilePath) (kind : String) : IO (Option Json) := do
  if kind == "overview"  then return some (← overviewApi configPath)
  if kind == "queue"     then return some (← queueApi)
  if kind == "concerts"  then return some (← concertsApi)
  if kind == "listeners" then return some (← listenersApi)
  if kind == "tasks"     then return some (← tasksApi)
  if kind == "projects"  then return some (← projectsApi)
  if kind == "auth"      then return some (← authApi configPath)
  if kind.startsWith "projects/" then
    return ← projectDetailApi (kind.drop "projects/".length).toString
  if kind.startsWith "concerts/" then
    return ← concertDetailApi (kind.drop "concerts/".length).toString
  if kind.startsWith "listeners/" then
    return ← listenerDetailApi (kind.drop "listeners/".length).toString
  if kind.startsWith "tasks/" then
    return ← taskDetailApi (kind.drop "tasks/".length).toString
  return none

/-- Encode a payload as a single SSE `message` event. -/
private def sseFrame (payload : String) : String :=
  let lines := payload.splitOn "\n"
  (lines.map (fun l => "data: " ++ l) |> String.intercalate "\n") ++ "\n\n"

-- HTTP request parsing

private def parseRequest (raw : String) : Option HttpRequest :=
  match raw.splitOn "\r\n" with
  | [] => none
  | reqLine :: rest =>
    match reqLine.splitOn " " with
    | method :: path :: _ =>
      let rec parseHdrs (acc : Array (String × String)) : List String → Array (String × String)
        | [] | "" :: _ => acc
        | h :: t =>
          match h.splitOn ": " with
          | k :: vs => parseHdrs (acc.push (k, String.intercalate ": " vs)) t
          | _       => parseHdrs acc t
      let headers := parseHdrs #[] rest
      let body := match rest.dropWhile (· != "") with | _ :: b => String.intercalate "\r\n" b | _ => ""
      some { method, path, headers, body }
    | _ => none

private def pathOnly (path : String) : String :=
  match path.splitOn "?" with
  | p :: _ => p
  | _ => path

/-- Extract the API/SSE kind from a `/api/<kind>` (or `.json`-suffixed) path. -/
private def apiKind (path : String) : Option String :=
  if path.startsWith "/api/" then
    let raw := (path.drop "/api/".length).toString
    some (if raw.endsWith ".json" then (raw.take (raw.length - 5)).toString else raw)
  else none

-- Authentication: every request must present the shared token, either as an
-- `Authorization: Bearer <token>` header (used by `fetch`) or a `?token=<token>`
-- query parameter (needed for SSE, since `EventSource` cannot set headers).

/-- Look up a request header case-insensitively. -/
private def headerValue (req : HttpRequest) (name : String) : Option String :=
  (req.headers.find? (fun (k, _) => k.toLower == name.toLower)).map (·.2)

/-- The bearer token from an `Authorization: Bearer <token>` header, if present. -/
private def bearerToken (req : HttpRequest) : Option String :=
  match headerValue req "authorization" with
  | some v =>
    if v.startsWith "Bearer " then some (v.drop "Bearer ".length).trimAscii.toString else none
  | none   => none

/-- The value of a `?key=…` (or `&key=…`) query parameter in a raw path. Tokens
    are hex, so no percent-decoding is required. -/
private def queryParam (path : String) (key : String) : Option String :=
  match path.splitOn "?" with
  | _ :: q :: _ =>
    (q.splitOn "&").findSome? fun kv =>
      match kv.splitOn "=" with
      | k :: v :: _ => if k == key then some v else none
      | _           => none
  | _ => none

/-- Whether the request carries the expected token (header or query param). -/
private def authorized (token : String) (req : HttpRequest) : Bool :=
  let supplied := (bearerToken req).orElse (fun _ => queryParam req.path "token")
  supplied == some token

private def unauthorizedResp : HttpResponse :=
  jsonResp (Json.mkObj [("error", "unauthorized")]) 401

/-- How a `serve` invocation is configured. -/
structure ServeConfig where
  /-- Shared bearer token required on every `/api/…` and `/sse/…` request. -/
  token : String
  /-- Port to bind; `0` auto-assigns. -/
  port : UInt16 := 8080
  /-- Address to bind. Loopback by default — the API is unencrypted, so anything wider is a
      deliberate choice (a container publishing a port, a reverse proxy terminating TLS). -/
  host : String := "127.0.0.1"
  /-- A generated static site to serve alongside the API, so one origin answers both. When
      absent no HTML is served at all and the site has to be hosted separately. -/
  siteDir : Option System.FilePath := none
  /-- `config.json` to read authentication sources from; `none` uses the XDG default. -/
  configPath : Option System.FilePath := none

-- Static site serving
--
-- Optional (`--site`), and unauthenticated by design: the generated pages carry no data at
-- all — every one of them is an empty shell that fetches its content from `/api/…`, which
-- *is* gated. Serving them from the same origin as the API is what makes a single published
-- container port enough, and removes the cross-origin hop the split deployment needs.

private def contentTypeOf (name : String) : String :=
  if      name.endsWith ".html" then "text/html; charset=utf-8"
  else if name.endsWith ".css"  then "text/css; charset=utf-8"
  else if name.endsWith ".js"   then "text/javascript; charset=utf-8"
  else if name.endsWith ".json" then "application/json; charset=utf-8"
  else if name.endsWith ".svg"  then "image/svg+xml"
  else if name.endsWith ".png"  then "image/png"
  else if name.endsWith ".ico"  then "image/x-icon"
  else if name.endsWith ".woff2" then "font/woff2"
  else "application/octet-stream"

/-- Resolve a request path to a file under `root`, or `none` if it names nothing.

    Path segments are rebuilt onto `root` one at a time and `.`/`..` are rejected outright
    rather than normalised, so no request can address a file outside the site directory.
    A directory resolves to its `index.html`, which is how the generated site is laid out
    (`queue/index.html` and friends). -/
private def resolveStatic (root : System.FilePath) (path : String) : IO (Option System.FilePath) := do
  let segs := (path.splitOn "/").filter (!·.isEmpty)
  if segs.any (fun s => s == "." || s == ".." || s.contains '\\') then return none
  let file := segs.foldl (fun (p : System.FilePath) (s : String) => p / s) root
  if ← file.isDir then
    let index := file / "index.html"
    return if ← index.pathExists then some index else none
  return if ← file.pathExists then some file else none

/-- Response head for a static file. Sent uncached: the site is regenerated in place by
    `dashboard generate`, and a stale `dashboard.js` against a newer API is a confusing
    failure to debug. -/
private def staticHead (contentType : String) (length : Nat) : String :=
  "HTTP/1.1 200 OK\r\n" ++
  s!"Content-Type: {contentType}\r\n" ++
  s!"Content-Length: {length}\r\n" ++
  "Cache-Control: no-cache\r\n" ++
  "Connection: close\r\n\r\n"

private def notFoundResp : HttpResponse :=
  { status := 404, headers := #[("Content-Type", "text/plain; charset=utf-8")], body := "not found\n" }

/-- Send a file from the site directory, or a 404 if the path resolves to nothing.

    Sent as raw bytes rather than through `HttpResponse`, whose body is a `String`: the
    generated site is text today, but a font or an image dropped into it must not become a
    decoding error.  -/
private def serveStatic (root : System.FilePath) (path : String) (client : Socket) : IO Unit := do
  match ← resolveStatic root path with
  | none => let _ ← awaitTcp (← client.send #[(httpResponse notFoundResp).toUTF8])
  | some file =>
    let bytes ← IO.FS.withFile file .read fun h => h.readBinToEnd
    let head := staticHead (contentTypeOf file.toString) bytes.size
    let _ ← awaitTcp (← client.send #[head.toUTF8, bytes])

-- Route dispatch. Only `/api/...` (JSON) is handled here; `/sse/...` and the optional static
-- site are handled by `serveClient` before reaching this function.

private def dispatch (cfg : ServeConfig) (req : HttpRequest) : IO HttpResponse := do
  if req.method == "OPTIONS" then
    -- CORS preflight carries no credentials; never require auth for it.
    return { status := 204, headers := corsHeaders, body := "" }
  if req.method != "GET" then
    return jsonResp (Json.mkObj [("error", "method not allowed")]) 405
  unless authorized cfg.token req do
    return unauthorizedResp
  match apiKind (pathOnly req.path) with
  | some kind =>
    match ← renderApi cfg.configPath kind with
    | some j => return jsonResp j
    | none   => return jsonResp (Json.mkObj [("error", "not found")]) 404
  | none => return jsonResp (Json.mkObj [("error", "not found")]) 404

-- SSE streaming handler

private def sseHeader : String :=
  "HTTP/1.1 200 OK\r\n" ++
  "Content-Type: text/event-stream\r\n" ++
  "Cache-Control: no-cache\r\n" ++
  "Connection: keep-alive\r\n" ++
  "Access-Control-Allow-Origin: *\r\n" ++
  "X-Accel-Buffering: no\r\n\r\n"

/-- Refresh interval for SSE pushes, in milliseconds. -/
private def sseIntervalMs : UInt32 := 2000

private partial def sseLoop (cfg : ServeConfig) (client : Socket) (kind : String) : IO Unit := do
  let jsonOpt ← try renderApi cfg.configPath kind catch _ => pure none
  match jsonOpt with
  | none => return -- unknown kind or vanished resource; close stream
  | some j =>
    let ok ← try
      let _ ← awaitTcp (← client.send #[(sseFrame (Json.compress j)).toUTF8])
      pure true
    catch _ => pure false
    if !ok then return
    IO.sleep sseIntervalMs
    sseLoop cfg client kind

private def serveSse (cfg : ServeConfig) (client : Socket) (kind : String) : IO Unit := do
  try
    let _ ← awaitTcp (← client.send #[sseHeader.toUTF8])
    sseLoop cfg client kind
  catch _ => pure ()

-- TCP client handler

private def serveClient (cfg : ServeConfig) (client : Socket) : IO Unit := do
  let buf ← IO.mkRef ""
  let mut i := 0
  while i < 16 do
    i := i + 1
    let chunk? ← awaitTcp (← client.recv? 16384)
    match chunk? with
    | none => i := 16
    | some bytes =>
      buf.modify (· ++ String.fromUTF8! bytes)
      let cur ← buf.get
      if (cur.splitOn "\r\n\r\n").length > 1 then i := 16
  let raw ← buf.get
  match parseRequest raw with
  | none =>
    let _ ← awaitTcp (← client.send #[httpResponse (jsonResp (Json.mkObj [("error", "bad request")]) 400) |>.toUTF8])
  | some req =>
    let path := pathOnly req.path
    if path.startsWith "/sse/" && req.method == "GET" then
      if authorized cfg.token req then
        serveSse cfg client (path.drop "/sse/".length).toString
      else
        let _ ← awaitTcp (← client.send #[httpResponse unauthorizedResp |>.toUTF8])
    else if req.method == "GET" && !(path.startsWith "/api/") && cfg.siteDir.isSome then
      serveStatic cfg.siteDir.get! path client
    else
      let resp ← dispatch cfg req
      let _ ← awaitTcp (← client.send #[httpResponse resp |>.toUTF8])

/-- Start the dashboard JSON API + SSE backend (and, with `cfg.siteDir`, the static site),
    bound to `cfg.host` on `cfg.port` (`0` = auto-assign). Every `/api/…` and `/sse/…`
    request must present `cfg.token` (see `resolveToken`).

    Returns `(boundPort, shutdown)`. Throws if `cfg.host` is not an IPv4 address. -/
def serve (cfg : ServeConfig) : IO (UInt16 × IO Unit) := do
  let some hostAddr := IPv4Addr.ofString cfg.host
    | throw (.userError s!"dashboard: '{cfg.host}' is not an IPv4 address")
  let server ← Socket.new
  server.bind (SocketAddress.v4 { addr := hostAddr, port := cfg.port })
  server.listen 32
  let boundPort := match ← server.getSockName with
    | .v4 a => a.port | .v6 a => a.port
  let running ← IO.mkRef true
  let _acceptTask ← IO.asTask (prio := .dedicated) do
    while ← running.get do
      match ← IO.wait (← server.accept).result! with
      | .error _ => break
      | .ok client =>
        if !(← running.get) then break
        let _ ← IO.asTask (prio := .dedicated) do
          try serveClient cfg client
          catch _ => pure ()
  let shutdown : IO Unit := do
    running.set false
    -- Unblock the accept loop with a throwaway connection. Always over loopback: a server
    -- bound to 0.0.0.0 answers there too, and it is the one address reachable regardless of
    -- what `host` was.
    try
      let dummy ← Socket.new
      let dAddr := SocketAddress.v4 { addr := IPv4Addr.ofParts 127 0 0 1, port := boundPort }
      let _ ← dummy.connect dAddr
    catch _ => pure ()
  return (boundPort, shutdown)

end Orchestra.Dashboard
