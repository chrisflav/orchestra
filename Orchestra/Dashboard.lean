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

open Lean (Json ToJson)
open Std.Net
open Std.Internal.UV.TCP
open Orchestra.Project (Project Issue IssueStatus Claim
  loadAllProjects loadProject loadIssues loadClaims)

/-!
# Orchestra Web Dashboard

`orchestra dashboard serve` runs a minimal HTTP/1.1 server that answers three things on one
port:

  * `POST /api/login`, `POST /api/logout`, `GET /api/session` — the authentication surface.
  * `GET /api/<kind>` — JSON view models, one per page.
  * `GET /sse/<kind>` — the same JSON pushed every two seconds as Server-Sent Events.
  * everything else — the compiled front-end from `--site`, with a single-page-app fallback.

The front-end is a React/TypeScript app under `web/`, built by Vite into `web/dist` and
handed to `serve` with `--site`. It is not compiled into the binary: the bundle is a build
artifact, so the Docker image builds it in a Node stage and copies it to a fixed path (see
`docker/Dockerfile`). Nothing here renders HTML.

Adding a page = a Lean `IO Json` builder plus a route arm in `renderApi` (here) + a typed
client call and a React page under `web/src/pages/`.

## Authentication

One shared secret, presented two ways:

  * **Browsers** `POST /api/login` with it and get an opaque session id back in an `HttpOnly`,
    `SameSite=Strict` cookie. The secret itself never reaches JavaScript, so an XSS bug in the
    front-end cannot exfiltrate it, and the cookie rides SSE requests automatically — which is
    what lets `EventSource` authenticate without a token in the URL (and therefore out of
    access logs, `Referer` headers and browser history).
  * **Scripts** send `Authorization: Bearer <secret>` directly.

Both comparisons are constant-time. There is no CORS header anywhere: the site and the API
are the same origin, and a wildcard `Access-Control-Allow-Origin` is incompatible with cookie
credentials in the first place. Development against Vite's dev server goes through its proxy
(`web/vite.config.ts`), which keeps that property.
-/

namespace Orchestra.Dashboard

/-! ## Secret resolution -/

/-- The env var that supplies the dashboard password. -/
def passwordEnvVar : String := "ORCHESTRA_DASHBOARD_PASSWORD"

/-- Where a generated password is persisted, so restarts don't invalidate what the user
    already wrote down. -/
private def secretFile : IO System.FilePath := do
  return (← Dirs.dataBase) / "dashboard.secret"

private def hexDigits : Array Char := "0123456789abcdef".toList.toArray

private def toHex (b : UInt8) : String :=
  let n := b.toNat
  String.ofList [hexDigits[n >>> 4]!, hexDigits[n &&& 0xf]!]

/-- `n` bytes of `/dev/urandom`, hex-encoded. -/
private def randomHex (n : Nat) : IO String := do
  let bytes ← IO.FS.withFile "/dev/urandom" .read fun h => h.read n.toUSize
  return String.join (bytes.toList.map toHex)

/-- Resolve the dashboard password, in priority order:
    1. an explicit `flagPassword` (`--password`),
    2. `$ORCHESTRA_DASHBOARD_PASSWORD`,
    3. one previously persisted under the data dir,
    4. otherwise a freshly generated one (persisted for reuse).

    Returns the password and whether it had to be generated, so the caller can print a
    generated one prominently and stay quiet about a configured one. -/
def resolvePassword (flagPassword : Option String := none) : IO (String × Bool) := do
  if let some p := flagPassword then
    if !p.trimAscii.isEmpty then return (p.trimAscii.toString, false)
  if let some p ← IO.getEnv passwordEnvVar then
    if !p.trimAscii.isEmpty then return (p.trimAscii.toString, false)
  let path ← secretFile
  if ← path.pathExists then
    let p := (← IO.FS.readFile path).trimAscii.toString
    if !p.isEmpty then return (p, false)
  let p ← randomHex 24
  IO.FS.createDirAll (← Dirs.dataBase)
  -- Created empty with owner-only permissions *before* the secret goes in, so the value is
  -- never briefly world-readable under a permissive umask.
  IO.FS.writeFile path ""
  try
    let _ ← IO.Process.run { cmd := "chmod", args := #["600", path.toString] }
  catch _ => pure ()
  IO.FS.writeFile path (p ++ "\n")
  return (p, true)

/-- Length-independent byte comparison, so a wrong secret takes the same time to reject
    whatever its content. Length itself is not hidden — it leaks through the early return —
    which is the standard trade-off and harmless for a high-entropy secret. -/
def constantTimeEq (a b : String) : Bool := Id.run do
  let ab := a.toUTF8
  let bb := b.toUTF8
  if ab.size != bb.size then return false
  let mut diff : UInt8 := 0
  for i in [0:ab.size] do
    diff := diff ||| (ab[i]! ^^^ bb[i]!)
  return diff == 0

/-! ## HTTP types -/

structure HttpRequest where
  method  : String
  path    : String
  headers : Array (String × String)
  body    : String
  deriving Inhabited

structure HttpResponse where
  status  : Nat
  headers : Array (String × String)
  body    : String

private def statusText : Nat → String
  | 200 => "OK"           | 204 => "No Content"
  | 400 => "Bad Request"  | 404 => "Not Found"
  | 401 => "Unauthorized" | 405 => "Method Not Allowed"
  | 413 => "Payload Too Large"
  | 500 => "Internal Server Error"
  | n   => s!"Status {n}"

/-- Sent on every response. `nosniff` and `DENY` matter because this server hands out both
    JSON and the app bundle: they stop a JSON body from being coerced into a script context
    and stop the whole dashboard from being framed by another page. -/
private def securityHeaders : Array (String × String) :=
  #[("X-Content-Type-Options", "nosniff"),
    ("X-Frame-Options", "DENY"),
    ("Referrer-Policy", "no-referrer")]

private def httpResponse (resp : HttpResponse) : String :=
  let statusLine := s!"HTTP/1.1 {resp.status} {statusText resp.status}\r\n"
  let contentLen := s!"Content-Length: {resp.body.utf8ByteSize}\r\n"
  let hdrs := (resp.headers ++ securityHeaders).map (fun (k, v) => s!"{k}: {v}\r\n") |>.toList
  statusLine ++ String.join hdrs ++ contentLen ++ "Connection: close\r\n\r\n" ++ resp.body

private def jsonResp (j : Json) (status : Nat := 200)
    (extraHeaders : Array (String × String) := #[]) : HttpResponse :=
  { status
    headers := #[("Content-Type", "application/json; charset=utf-8"),
                 ("Cache-Control", "no-store")] ++ extraHeaders
    body    := Json.compress j }

private def errorResp (msg : String) (status : Nat) : HttpResponse :=
  jsonResp (Json.mkObj [("error", msg)]) status

private def unauthorizedResp : HttpResponse := errorResp "unauthorized" 401
private def notFoundJsonResp : HttpResponse := errorResp "not found" 404

/-! ## Request parsing -/

private def hexVal? (c : Char) : Option UInt8 :=
  if '0' ≤ c && c ≤ '9' then some (UInt8.ofNat (c.toNat - '0'.toNat))
  else if 'a' ≤ c && c ≤ 'f' then some (UInt8.ofNat (c.toNat - 'a'.toNat + 10))
  else if 'A' ≤ c && c ≤ 'F' then some (UInt8.ofNat (c.toNat - 'A'.toNat + 10))
  else none

private def percentDecodeAux : List Char → ByteArray → Option ByteArray
  | [],               acc => some acc
  | '%' :: h :: l :: t, acc =>
    match hexVal? h, hexVal? l with
    | some hv, some lv => percentDecodeAux t (acc.push (hv * 16 + lv))
    | _,       _       => none
  | '%' :: _,         _   => none
  | c :: t,           acc => percentDecodeAux t (acc ++ c.toString.toUTF8)

/-- Percent-decode a URL component. `none` when the escapes are malformed or the result is
    not valid UTF-8 — a rejected request beats a silently mangled one. -/
def percentDecode (s : String) : Option String := do
  let bytes ← percentDecodeAux s.toList ByteArray.empty
  String.fromUTF8? bytes

/-- Decode one path component and accept it only if it can name nothing but itself.

    Every detail endpoint (`/api/tasks/<id>`, `/api/listeners/<name>`, …) feeds its component
    to a loader that builds a filename from it, so this is the same boundary `staticCandidate`
    guards for the site directory, and it is enforced *after* decoding — otherwise `%2e%2e`
    would be a second spelling of `..` that the raw check never sees. -/
def safeSegment (raw : String) : Option String := do
  let s ← percentDecode raw
  if s.isEmpty || s == "." || s == ".." then none
  else if s.any (fun c => c == '/' || c == '\\' || c.toNat < 0x20 || c.toNat == 0x7f) then none
  else some s

def parseRequest (raw : String) : Option HttpRequest :=
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
      let body := match rest.dropWhile (· != "") with
        | _ :: b => String.intercalate "\r\n" b
        | _      => ""
      some { method, path, headers, body }
    | _ => none

private def pathOnly (path : String) : String :=
  match path.splitOn "?" with
  | p :: _ => p
  | _      => path

/-- Look up a request header case-insensitively. -/
def headerValue (req : HttpRequest) (name : String) : Option String :=
  (req.headers.find? (fun (k, _) => k.toLower == name.toLower)).map (·.2)

/-- The value of one cookie in a `Cookie:` header value. -/
def cookieValue (raw : String) (name : String) : Option String :=
  (raw.splitOn ";").findSome? fun kv =>
    match kv.trimAscii.toString.splitOn "=" with
    | k :: vs => if k == name then some (String.intercalate "=" vs) else none
    | _       => none

/-- Extract the API/SSE kind from a `/api/<kind>` or `/sse/<kind>` path. The prefix is
    stripped verbatim; individual components are validated later by `renderApi`. -/
def apiKind (prefix_ : String) (path : String) : Option String :=
  if path.startsWith prefix_ then some (path.drop prefix_.length).toString else none

/-! ## View-model builders

Each `/api/...` endpoint produces a `Json` value tailored for the matching React page. The
shapes here are mirrored by the TypeScript declarations in `web/src/api.ts`; changing one
means changing the other. -/

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

/-- How many trailing log events a task detail response carries.

    Bounded because this payload is re-sent on every SSE tick: an agent that has been running
    for hours produces a log far larger than anything a reader scrolls through, and shipping
    all of it every two seconds costs the server a full re-read and the browser a full
    re-parse. The tail is the end people actually read; `logTotal` reports what was left out. -/
private def logTailLimit : Nat := 500

/-- Parse the per-task structured JSONL log, keeping only the last `logTailLimit` events.
    Returns the kept events and the total number present. -/
private def loadTaskLog (fork : Repository) (id : String) : IO (Array Json × Nat) := do
  let path := (← Dirs.dataBase) / "logs" / fork.toString / s!"{id}.log"
  if !(← path.pathExists) then return (#[], 0)
  let raw ← IO.FS.readFile path
  let lines := (raw.splitOn "\n").filter (!·.trimAscii.isEmpty)
  let total := lines.length
  let kept := if total ≤ logTailLimit then lines else lines.drop (total - logTailLimit)
  let mut out : Array Json := #[]
  for line in kept do
    match Json.parse line with
    | .ok j    => out := out.push j
    | .error _ => out := out.push (Json.mkObj [("type", "unknown"), ("event_type", "parse_error")])
  return (out, total)

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
    let (log, total) ← loadTaskLog fork id
    return some (Json.mkObj [
      ("id",        id),
      ("status",    st),
      ("fork",      fork.toString),
      ("createdAt", createdAt),
      ("prompt",    prompt),
      ("log",       Json.arr log),
      ("logTotal",  ToJson.toJson total),
      ("logTruncated", Json.bool (total > log.size))
    ])

/-- Dispatch an `/api/…` or `/sse/…` kind to the matching builder.

    Detail kinds run their component through `safeSegment` first: every one of them ends up
    in a filename, and this is the only place that check can be made once for all of them. -/
private def renderApi (configPath : Option System.FilePath) (kind : String)
    : IO (Option Json) := do
  if kind == "overview"  then return some (← overviewApi configPath)
  if kind == "queue"     then return some (← queueApi)
  if kind == "concerts"  then return some (← concertsApi)
  if kind == "listeners" then return some (← listenersApi)
  if kind == "tasks"     then return some (← tasksApi)
  if kind == "projects"  then return some (← projectsApi)
  if kind == "auth"      then return some (← authApi configPath)
  let detail (prefix_ : String) (f : String → IO (Option Json)) : IO (Option (Option Json)) := do
    if kind.startsWith prefix_ then
      match safeSegment (kind.drop prefix_.length).toString with
      | some seg => return some (← f seg)
      | none     => return some none
    else return none
  if let some r ← detail "projects/"  projectDetailApi  then return r
  if let some r ← detail "concerts/"  concertDetailApi  then return r
  if let some r ← detail "listeners/" listenerDetailApi then return r
  if let some r ← detail "tasks/"     taskDetailApi     then return r
  return none

/-! ## Server configuration and session state -/

/-- How a `serve` invocation is configured. -/
structure ServeConfig where
  /-- The shared secret: the password browsers log in with, and the bearer token scripts
      send. -/
  password : String
  /-- Port to bind; `0` auto-assigns. -/
  port : UInt16 := 8080
  /-- Address to bind. Loopback by default — the API is unencrypted, so anything wider is a
      deliberate choice (a container publishing a port, a reverse proxy terminating TLS). -/
  host : String := "127.0.0.1"
  /-- The built front-end (`web/dist`) to serve alongside the API, so one origin answers
      both. When absent no HTML is served and only the JSON API answers. -/
  siteDir : Option System.FilePath := none
  /-- `config.json` to read authentication sources from; `none` uses the XDG default. -/
  configPath : Option System.FilePath := none
  /-- How long a session cookie stays valid, in seconds. -/
  sessionTtlSeconds : Nat := 43200
  /-- Add `Secure` to the session cookie. Off by default because the default deployment is
      plain HTTP on loopback, where `Secure` would stop the cookie being stored at all; turn
      it on whenever a TLS-terminating proxy sits in front. -/
  secureCookie : Bool := false

/-- Live server state: the config plus the set of issued sessions.

    An in-memory store, so a restart logs everyone out. That is the honest behaviour for a
    process whose password can be regenerated on the same restart, and it means a stolen
    cookie cannot outlive the process it was issued by. -/
private structure ServeState where
  cfg : ServeConfig
  /-- Session id paired with its expiry, as an epoch second. A handful of browser tabs at
      most, so a linear scan is cheaper than a hash map and prunes in the same pass. -/
  sessions : IO.Ref (Array (String × Int))

def sessionCookieName : String := "orchestra_session"

private def issueSession (st : ServeState) : IO String := do
  let id ← randomHex 32
  let now ← Usage.nowEpoch
  st.sessions.modify fun ss =>
    (ss.filter (fun (_, exp) => exp > now)).push (id, now + st.cfg.sessionTtlSeconds)
  return id

private def sessionValid (st : ServeState) (id : String) : IO Bool := do
  let now ← Usage.nowEpoch
  let ss ← st.sessions.get
  return ss.any fun (sid, exp) => exp > now && constantTimeEq sid id

private def revokeSession (st : ServeState) (id : String) : IO Unit := do
  st.sessions.modify (·.filter fun (sid, _) => !constantTimeEq sid id)

/-- The session id the request carries, if any. -/
private def requestSession (req : HttpRequest) : Option String :=
  (headerValue req "cookie").bind (cookieValue · sessionCookieName)

/-- Whether a request may reach the API: a valid session cookie, or the shared secret as a
    bearer token. Both comparisons are constant-time. -/
private def authenticated (st : ServeState) (req : HttpRequest) : IO Bool := do
  if let some v := headerValue req "authorization" then
    if v.startsWith "Bearer " then
      if constantTimeEq (v.drop "Bearer ".length).trimAscii.toString st.cfg.password then
        return true
  if let some sid := requestSession req then
    return ← sessionValid st sid
  return false

private def cookieHeader (st : ServeState) (value : String) (maxAge : Nat) : String :=
  let attrs := s!"{sessionCookieName}={value}; HttpOnly; SameSite=Strict; Path=/; \
Max-Age={maxAge}"
  if st.cfg.secureCookie then attrs ++ "; Secure" else attrs

/-! ## Authentication endpoints -/

private def loginHandler (st : ServeState) (req : HttpRequest) : IO HttpResponse := do
  let supplied := match Json.parse req.body with
    | .ok j    => (j.getObjValAs? String "password").toOption
    | .error _ => none
  match supplied with
  | none => return errorResp "expected a JSON body of the form {\"password\": \"…\"}" 400
  | some p =>
    unless constantTimeEq p st.cfg.password do
      return errorResp "invalid password" 401
    let sid ← issueSession st
    return jsonResp (Json.mkObj [("authenticated", Json.bool true)]) 200
      #[("Set-Cookie", cookieHeader st sid st.cfg.sessionTtlSeconds)]

private def logoutHandler (st : ServeState) (req : HttpRequest) : IO HttpResponse := do
  if let some sid := requestSession req then revokeSession st sid
  return jsonResp (Json.mkObj [("authenticated", Json.bool false)]) 200
    #[("Set-Cookie", cookieHeader st "" 0)]

/-- Whether the caller is already authenticated. Deliberately reachable without credentials:
    it is what the front-end asks on load to decide between the login screen and the app, and
    it discloses nothing but a boolean the caller could learn by trying any other endpoint. -/
private def sessionHandler (st : ServeState) (req : HttpRequest) : IO HttpResponse := do
  return jsonResp (Json.mkObj [("authenticated", Json.bool (← authenticated st req))])

/-! ## Static single-page-app serving -/

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

/-- The file a request path names under `root`, or `none` if it escapes.

    Segments are rebuilt onto `root` one at a time and `.`/`..` are rejected outright rather
    than normalised, so no request can address a file outside the site directory. Nothing is
    percent-decoded, which is what keeps `%2e%2e` an ordinary (and absent) filename rather
    than a second spelling of `..`.

    Not `private`: `OrchestraTest.Dashboard` covers the traversal cases directly, which is the
    part of static serving worth testing and the part that must not silently regress. -/
def staticCandidate (root : System.FilePath) (path : String) : Option System.FilePath :=
  let segs := (path.splitOn "/").filter (!·.isEmpty)
  if segs.any (fun s => s == "." || s == ".." || s.contains '\\') then none
  else some (segs.foldl (fun (p : System.FilePath) (s : String) => p / s) root)

/-- Whether a path should fall back to `index.html` when it names no file.

    Client-side routing means `/tasks/abc123` is a valid app URL with no file behind it, so a
    reload of that URL has to be answered with the app shell. Paths whose last segment looks
    like a filename — anything with an extension — are excluded, so a missing
    `/assets/main.js` still 404s instead of returning HTML that the browser would then fail
    to parse as a script. -/
def wantsAppShell (path : String) : Bool :=
  match (path.splitOn "/").filter (!·.isEmpty) |>.getLast? with
  | none      => true
  | some last => !last.contains '.'

/-- Resolve a request path to a file under `root`: the file itself, that directory's
    `index.html`, or the app shell for a client-side route. -/
private def resolveStatic (root : System.FilePath) (path : String)
    : IO (Option System.FilePath) := do
  let shell := root / "index.html"
  let fallback : IO (Option System.FilePath) := do
    if wantsAppShell path && (← shell.pathExists) then return some shell else return none
  let some file := staticCandidate root path | return none
  if ← file.isDir then
    let index := file / "index.html"
    if ← index.pathExists then return some index else return ← fallback
  if ← file.pathExists then return some file else return ← fallback

/-- Response head for a static file.

    Vite fingerprints everything under `assets/`, so those are immutable and cached for a
    year; `index.html` names them and must never be cached, or a reload after a rebuild
    pairs a new shell with stale chunks. -/
private def staticHead (path : String) (contentType : String) (length : Nat) : String :=
  let cache :=
    if path.startsWith "/assets/" then "public, max-age=31536000, immutable" else "no-cache"
  "HTTP/1.1 200 OK\r\n" ++
  s!"Content-Type: {contentType}\r\n" ++
  s!"Content-Length: {length}\r\n" ++
  s!"Cache-Control: {cache}\r\n" ++
  "X-Content-Type-Options: nosniff\r\n" ++
  "X-Frame-Options: DENY\r\n" ++
  "Connection: close\r\n\r\n"

private def notFoundResp : HttpResponse :=
  { status := 404, headers := #[("Content-Type", "text/plain; charset=utf-8")],
    body := "not found\n" }

/-! ## Socket plumbing -/

private def awaitTcp (p : IO.Promise (Except IO.Error α)) : IO α := do
  let result ← IO.wait p.result!
  match result with
  | .error e => throw e
  | .ok v    => return v

/-- Send a response and close the write side.

    Every response advertises `Connection: close`; without the `shutdown` the peer waits for
    a FIN that only arrives when the socket is finalised, which for a long-running server is
    an unbounded wait and a leaked descriptor per request. -/
private def sendAndClose (client : Socket) (chunks : Array ByteArray) : IO Unit := do
  try
    let _ ← awaitTcp (← client.send chunks)
  catch _ => pure ()
  try
    let _ ← awaitTcp (← client.shutdown)
  catch _ => pure ()

private def serveStatic (root : System.FilePath) (path : String) (client : Socket) : IO Unit := do
  match ← resolveStatic root path with
  | none => sendAndClose client #[(httpResponse notFoundResp).toUTF8]
  | some file =>
    -- Sent as raw bytes rather than through `HttpResponse`, whose body is a `String`: the
    -- bundle is text today, but a font or an image emitted by Vite must not become a
    -- decoding error.
    let bytes ← IO.FS.withFile file .read fun h => h.readBinToEnd
    let head := staticHead path (contentTypeOf file.toString) bytes.size
    sendAndClose client #[head.toUTF8, bytes]

/-! ## Route dispatch -/

private def dispatch (st : ServeState) (req : HttpRequest) : IO HttpResponse := do
  let path := pathOnly req.path
  -- The authentication surface, before the gate it configures.
  if path == "/api/login" then
    return ← if req.method == "POST" then loginHandler st req
             else pure (errorResp "method not allowed" 405)
  if path == "/api/logout" then
    return ← if req.method == "POST" then logoutHandler st req
             else pure (errorResp "method not allowed" 405)
  if path == "/api/session" then
    return ← if req.method == "GET" then sessionHandler st req
             else pure (errorResp "method not allowed" 405)
  unless ← authenticated st req do
    return unauthorizedResp
  if req.method != "GET" then
    return errorResp "method not allowed" 405
  match apiKind "/api/" path with
  | some kind =>
    -- A builder that throws must still produce a response. `projects` reaches out to taxis and
    -- `auth` reads `config.json`, so an unreachable tracker or an unparseable config is an
    -- ordinary runtime condition here — and without this the exception would escape
    -- `serveClient`, leaving the caller with a closed connection and no status at all.
    match ← (try
        (·.map Except.ok) <$> renderApi st.cfg.configPath kind
      catch e => pure (some (Except.error (toString e)))) with
    | some (.ok j)  => return jsonResp j
    | some (.error e) => return errorResp e 500
    | none          => return notFoundJsonResp
  | none => return notFoundJsonResp

/-! ## SSE streaming -/

private def sseHead : String :=
  "HTTP/1.1 200 OK\r\n" ++
  "Content-Type: text/event-stream\r\n" ++
  "Cache-Control: no-store\r\n" ++
  "Connection: keep-alive\r\n" ++
  "X-Content-Type-Options: nosniff\r\n" ++
  "X-Accel-Buffering: no\r\n\r\n"

/-- Encode a payload as a single SSE `message` event. -/
private def sseFrame (payload : String) : String :=
  let lines := payload.splitOn "\n"
  (lines.map (fun l => "data: " ++ l) |> String.intercalate "\n") ++ "\n\n"

/-- Refresh interval for SSE pushes, in milliseconds. -/
private def sseIntervalMs : UInt32 := 2000

/-- Ticks between keep-alive comments when nothing has changed. At the 2s interval above this
    is one line every 30s, which is under the idle timeout of every proxy worth naming. -/
private def sseKeepAliveTicks : Nat := 15

/-- Push `kind` until the client goes away.

    Only *changed* payloads are sent. An idle orchestra re-renders to a byte-identical
    document, and a dashboard left open on a second monitor should cost the network nothing
    while that is true — the client would discard the frame anyway. -/
private partial def sseLoop (st : ServeState) (client : Socket) (kind : String)
    (lastSent : String) (idleTicks : Nat) : IO Unit := do
  -- A builder that throws (taxis unreachable, config momentarily unreadable) skips this tick
  -- rather than closing the stream: the client keeps the last good frame on screen and picks
  -- the next one up when the condition clears. Closing would instead blank the page and set
  -- `EventSource` reconnecting every few seconds for as long as the outage lasts.
  let outcome ← try
      (·.map Except.ok) <$> renderApi st.cfg.configPath kind
    catch e => pure (some (Except.error (toString e)))
  match outcome with
  | none => return -- unknown kind or vanished resource; close the stream
  | some (.error _) =>
    IO.sleep sseIntervalMs
    sseLoop st client kind lastSent idleTicks
  | some (.ok j) =>
    let payload := Json.compress j
    let (frame, idleTicks) :=
      if payload != lastSent then (some (sseFrame payload), 0)
      else if idleTicks + 1 ≥ sseKeepAliveTicks then (some ": keep-alive\n\n", 0)
      else (none, idleTicks + 1)
    let ok ← match frame with
      | none => pure true
      | some f =>
        try
          let _ ← awaitTcp (← client.send #[f.toUTF8])
          pure true
        catch _ => pure false
    if !ok then return
    IO.sleep sseIntervalMs
    sseLoop st client kind payload idleTicks

private def serveSse (st : ServeState) (client : Socket) (kind : String) : IO Unit := do
  try
    let _ ← awaitTcp (← client.send #[sseHead.toUTF8])
    sseLoop st client kind "" 0
  catch _ => pure ()

/-! ## Connection handling -/

/-- Cap on a single request, headers and body together. Every route here is a `GET` or a
    one-field login, so anything larger is a mistake or an attempt to sit on memory. -/
private def maxRequestBytes : Nat := 65536

private def findHeaderEnd (raw : String) : Bool :=
  (raw.splitOn "\r\n\r\n").length > 1

/-- The declared body length, or 0 when absent or unparseable. -/
private def contentLength (req : HttpRequest) : Nat :=
  match (headerValue req "content-length").bind (·.trimAscii.toString.toNat?) with
  | some n => n
  | none   => 0

/-- Read one request off the socket.

    Bytes are accumulated as a `ByteArray` and decoded only once the whole thing is in hand:
    a chunk boundary can land in the middle of a multi-byte character, and decoding each
    chunk in isolation would corrupt it. -/
private partial def readRequest (client : Socket) (acc : ByteArray)
    : IO (Option HttpRequest) := do
  if acc.size > maxRequestBytes then return none
  match String.fromUTF8? acc with
  | none => -- a partial multi-byte character; read more
    match ← awaitTcp (← client.recv? 16384) with
    | none       => return none
    | some bytes => readRequest client (acc ++ bytes)
  | some raw =>
    if findHeaderEnd raw then
      match parseRequest raw with
      | none     => return none
      | some req =>
        -- Headers are complete; keep reading until the declared body has arrived too.
        if req.body.utf8ByteSize ≥ contentLength req then return some req
        match ← awaitTcp (← client.recv? 16384) with
        | none       => return some req
        | some bytes => readRequest client (acc ++ bytes)
    else
      match ← awaitTcp (← client.recv? 16384) with
      | none       => return none
      | some bytes => readRequest client (acc ++ bytes)

private def serveClient (st : ServeState) (client : Socket) : IO Unit := do
  match ← readRequest client ByteArray.empty with
  | none =>
    sendAndClose client #[(httpResponse (errorResp "bad request" 400)).toUTF8]
  | some req =>
    let path := pathOnly req.path
    if path.startsWith "/sse/" && req.method == "GET" then
      if ← authenticated st req then
        match apiKind "/sse/" path with
        | some kind => serveSse st client kind
        | none      => sendAndClose client #[(httpResponse notFoundJsonResp).toUTF8]
      else
        sendAndClose client #[(httpResponse unauthorizedResp).toUTF8]
    else if req.method == "GET" && !(path.startsWith "/api/") && st.cfg.siteDir.isSome then
      serveStatic st.cfg.siteDir.get! path client
    else
      let resp ← dispatch st req
      sendAndClose client #[(httpResponse resp).toUTF8]

/-- Start the dashboard server: the JSON API, the SSE streams, and — with `cfg.siteDir` — the
    built front-end, all on `cfg.host`:`cfg.port` (`0` = auto-assign).

    Returns `(boundPort, shutdown)`. Throws if `cfg.host` is not an IPv4 address. -/
def serve (cfg : ServeConfig) : IO (UInt16 × IO Unit) := do
  let some hostAddr := IPv4Addr.ofString cfg.host
    | throw (.userError s!"dashboard: '{cfg.host}' is not an IPv4 address")
  let sessions ← IO.mkRef (#[] : Array (String × Int))
  let st : ServeState := { cfg, sessions }
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
          try serveClient st client
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
      let _ ← awaitTcp (← dummy.shutdown)
    catch _ => pure ()
  return (boundPort, shutdown)

end Orchestra.Dashboard
