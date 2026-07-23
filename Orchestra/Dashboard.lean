import Lean.Data.Json
import Std.Http
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
open Std.Async
open Std.Http (Request Response Body Chunk Header Status Method)
open Std.Http.Server (Handler)
open Orchestra.Project (Project Issue IssueStatus Claim
  loadAllProjects loadProject loadIssues loadClaims)

/-!
# Orchestra Web Dashboard

`orchestra dashboard` answers five things on one port:

  * `GET /api/openapi.json` — the API's own description, uncredentialed.
  * `POST /api/login`, `POST /api/logout`, `GET /api/session` — the authentication surface.
  * `GET /api/v1/<kind>` — the read API: resources and collections, described by that spec.
  * `GET /sse/v1/<kind>` — the same payloads pushed as Server-Sent Events when they change.
  * everything else — the compiled front-end from `--site`, with a single-page-app fallback.

The API is a general one that this repository's front-end happens to be a client of, not a
set of view models shaped for its pages. That distinction is what the wire conventions and
the collection envelope below are for: the payloads carry instants rather than rendered
phrases, `null` rather than `""`, and every list is pageable, so a script or another UI can
consume them without knowing which page asked first. It is read-only — the only routes that
are not `GET` are the two that open and close a session.

The transport is `Std.Http`, Lean's HTTP/1.1 server. This module supplies a `Handler` and
nothing below it: request parsing, header validation, keep-alive, chunked encoding, the
timeouts that bound a slow client, and the accept loop are all the library's. What is left
here is routing, authentication, and turning orchestra's state into JSON.

`Std.Http` leaves a request-target alone — percent-escapes undecoded, dot-segments
unnormalised — which is exactly what the path guards below rely on: `%2e%2e` has to stay an
ordinary filename that names nothing, rather than arrive as a second spelling of `..`.

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

/-! ## Responses -/

/-- What every response carries. `nosniff` and `DENY` matter because this server hands out
    both JSON and the app bundle: they stop a JSON body from being coerced into a script
    context and stop the whole dashboard from being framed by another page. -/
private def secured (b : Response.Builder) : Response.Builder :=
  b |>.header! "X-Content-Type-Options" "nosniff"
    |>.header! "X-Frame-Options" "DENY"
    |>.header! "Referrer-Policy" "no-referrer"

/-- A JSON response, optionally setting the session cookie. -/
private def jsonResp (j : Json) (status : Status := .ok) (setCookie : Option String := none)
    : Async (Response Body.Any) := do
  let b := (secured (Response.withStatus status)).header! "Cache-Control" "no-store"
  let b := match setCookie with
    | some cookie => b.header! "Set-Cookie" cookie
    | none        => b
  return ← b.json (Json.compress j)

private def errorResp (msg : String) (status : Status) : Async (Response Body.Any) :=
  jsonResp (Json.mkObj [("error", msg)]) status

private def unauthorizedResp : Async (Response Body.Any) := errorResp "unauthorized" .unauthorized
private def notFoundJsonResp : Async (Response Body.Any) := errorResp "not found" .notFound
private def methodNotAllowedResp : Async (Response Body.Any) :=
  errorResp "method not allowed" .methodNotAllowed

/-! ## Request accessors

The request-target is used verbatim throughout: `Std.Http` neither decodes percent-escapes
nor normalises dot-segments, so what these return is what the client sent. -/

/-- The request path, without the query string and with its segments still percent-encoded.

    Not `private`: this is the input every path guard below is handed, and its verbatim-ness
    is a property of `Std.Http` rather than of this code, so `OrchestraTest.Dashboard` pins it
    directly. -/
def pathOf (uri : Std.Http.RequestTarget) : String :=
  toString uri.path

/-- Look up a request header. `Header.Name` is case-insensitive, so `Cookie` and `cookie` are
    the same key. -/
private def reqHeader (req : Request Body.Stream) (name : String) : Option String :=
  (req.line.headers.get? (Header.Name.ofString! name)).map toString

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

/-! ## Paging

Every collection answers the same three parameters, and every collection answers in the same
envelope. A bad value is a `400`, and so is a parameter the collection cannot honour: a caller
that sends `?limit=abc`, or asks a list of listeners for everything `?since` yesterday, has a
bug, and answering with a plausible page hides it. Parameters this version does not know are
ignored, so a later one can add some without breaking a caller that sends them. -/

/-- What a read produced. `badRequest` exists so that a rejected parameter is distinguishable
    from a resource that does not exist — the two are different bugs on the caller's side. -/
private inductive ApiResult
  | ok (payload : Json)
  | notFound
  | badRequest (why : String)

/-- Page size when `limit` is absent. -/
private def defaultLimit : Nat := 50

/-- The most any one response will carry, whatever `limit` asks for. Present so that a client
    cannot turn a single request into an unbounded read of the task store. -/
private def maxLimit : Nat := 500

/-- How much of a task's log a response carries when `logLimit` is absent.

    Bounded by default because this payload is re-sent on every SSE tick: an agent that has
    been running for hours produces a log far larger than anything a reader scrolls through,
    and shipping all of it every two seconds costs the server a full re-read and the client a
    full re-parse. The tail is the end people actually read, and `logTotal` reports what was
    left out — a caller that wants more asks for it. -/
private def defaultLogLimit : Nat := 500

private def maxLogLimit : Nat := 10000

private structure Page where
  limit  : Nat
  offset : Nat
  /-- Keep only items created at or after this instant, as epoch seconds. -/
  since  : Option Int

private abbrev Query := Std.Http.URI.Query

private def natParam (q : Query) (name : String) (dflt cap : Nat) : Except String Nat :=
  match q.get name with
  | none     => .ok dflt
  | some raw =>
    match raw.toNat? with
    | some n => .ok (min n cap)
    | none   => .error s!"'{name}' must be a non-negative integer, got '{raw}'"

/-- Reject a paging parameter this collection has no meaning for. -/
private def refuse (q : Query) (name : String) (why : String) : Except String Unit :=
  if (q.get name).isSome then .error s!"'{name}' {why}" else .ok ()

/-- Parse the page for a collection ordered by time. -/
private def parsePage (q : Query) : Except String Page := do
  let limit  ← natParam q "limit" defaultLimit maxLimit
  let offset ← natParam q "offset" 0 maxLimit
  let since ← match q.get "since" with
    | none     => pure none
    | some raw =>
      match Usage.parseIso8601 raw with
      | some e => pure (some e)
      | none   => .error s!"'since' must be an RFC 3339 timestamp, got '{raw}'"
  return { limit, offset, since }

/-- Parse the page for a collection that has no time order — a listener or a project is
    configuration, not an event, so `since` would have nothing to compare against. -/
private def parseUnorderedPage (q : Query) : Except String Page := do
  refuse q "since" "is not supported by this collection, which is not ordered by time"
  let limit  ← natParam q "limit" defaultLimit maxLimit
  let offset ← natParam q "offset" 0 maxLimit
  return { limit, offset, since := none }

/-- The one collection envelope.

    `total` counts what matched *before* the window is applied, which is what lets a caller
    tell "50 of 812" from "the last 50 that exist", and is what makes `offset` usable without
    guessing. -/
private def collection (p : Page) (total : Nat) (items : Array Json) : Json :=
  Json.mkObj [
    ("items",  Json.arr items),
    ("total",  ToJson.toJson total),
    ("limit",  ToJson.toJson p.limit),
    ("offset", ToJson.toJson p.offset)
  ]

/-- Filter by `since`, take the window, and render. `createdAt` names the field the collection
    is ordered by. -/
private def pageOver (p : Page) (createdAt : α → String) (render : α → Json)
    (items : Array α) : Json :=
  let kept := match p.since with
    | none   => items
    | some s => items.filter fun i =>
        match Usage.parseIso8601 (createdAt i) with
        | some e => e ≥ s
        | none   => false
  collection p kept.size ((kept.toList.drop p.offset |>.take p.limit).toArray.map render)

/-- The same, for a collection with no time order. -/
private def pageOverUnordered (p : Page) (items : Array α) (render : α → IO Json) : IO Json := do
  let window := (items.toList.drop p.offset |>.take p.limit).toArray
  return collection p items.size (← window.mapM render)

/-! ## Wire conventions

Four rules, applied by every builder below and asserted by `docs/openapi.json`. They exist so
that a consumer that is not this repository's front-end can read a payload without knowing
which page it was shaped for.

  * **Instants** are RFC 3339 in UTC (`2026-07-23T10:04:11Z`), in a field named `…At`. Never a
    rendered phrase: "3m ago" cannot be compared, thresholded, or read outside English.
  * **Durations** are integer seconds, in a field named `…Seconds`.
  * **Absent** is `null`. An empty string means the value is present and empty, which for a
    name or an id is a different fact.
  * **Enumerations** are the lower-case names the daemon itself uses. -/

private def optStr : Option String → Json
  | some s => Json.str s
  | none   => Json.null

private def optNum [ToJson α] : Option α → Json
  | some n => ToJson.toJson n
  | none   => Json.null

/-- Epoch seconds as an RFC 3339 UTC instant. -/
def isoOfEpoch (epoch : Int) : String :=
  let ts := Std.Time.Timestamp.ofSecondsSinceUnixEpoch (Std.Time.Second.Offset.ofInt epoch)
  (Std.Time.DateTime.ofTimestamp ts Std.Time.TimeZone.UTC).format "uuuu-MM-dd'T'HH:mm:ss'Z'"

private def optEpochIso : Option Int → Json
  | some e => Json.str (isoOfEpoch e)
  | none   => Json.null

/-- A stored timestamp, restated in the one format the API promises.

    The stores write ISO 8601 already, but not all of it in UTC with a `Z`, and a contract that
    says "RFC 3339 UTC" has to be true of every value or it is not a contract. Anything
    unparseable passes through untouched rather than becoming a wrong instant. -/
private def normIso (s : String) : Json :=
  match Usage.parseIso8601 s with
  | some e => Json.str (isoOfEpoch e)
  | none   => Json.str s

private def optNormIso : Option String → Json
  | some s => normIso s
  | none   => Json.null

/-! ## Resources

Two resources describe work, because the daemon has two of them: a **queue entry** is work
waiting or running, with a priority and a place in a concert; a **task** is a run that
happened, with a log. They share an id and a core of fields, and an entry that has been
claimed carries the `taskId` of the run it became. -/

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
    ("id",             e.id),
    ("status",         qStText e.status),
    ("createdAt",      normIso e.createdAt),
    ("priority",       ToJson.toJson e.priority),
    ("upstream",       e.upstream.toString),
    ("fork",           e.fork.toString),
    ("prompt",         e.prompt),
    ("series",         optStr e.series),
    ("backend",        optStr e.backend),
    ("model",          optStr e.model),
    ("taskId",         optStr e.taskId),
    ("concertId",      optStr e.concertId),
    ("concertStepKey", optStr e.concertStepKey)
  ]

private def taskRecJson (r : TaskStore.TaskRecord) : Json :=
  Json.mkObj [
    ("id",            r.id),
    ("status",        tStText r.status),
    ("createdAt",     normIso r.createdAt),
    ("upstream",      r.upstream.toString),
    ("fork",          r.fork.toString),
    ("prompt",        r.prompt),
    ("series",        optStr r.series),
    ("backend",       optStr r.backend),
    ("model",         optStr r.model),
    ("sessionId",     optStr r.sessionId),
    ("continuesFrom", optStr r.continuesFrom),
    ("budgetUsd",     optNum r.budget)
  ]

private def concertRunJson (r : Queue.ConcertRun) : Json :=
  Json.mkObj [
    ("id",           r.id),
    ("status",       cStText r.status),
    ("name",         optStr r.name),
    ("workflowFile", optStr r.workflowFile),
    ("startedAt",    normIso r.startedAt),
    ("finishedAt",   optNormIso r.finishedAt)
  ]

-- Authentication sources
--
-- The dashboard is a *reader* of `Orchestra.Usage`'s on-disk state, never a poller: the
-- queue daemon refreshes it on its own cadence, and the usage endpoint meters requests, so
-- a page that re-polled on every SSE tick (every two seconds) would rate-limit the very
-- monitor it is displaying. Every row therefore also carries when it was last polled, so a
-- stale view reads as stale rather than as current.

private def limitJson (l : Usage.Limit) : Json :=
  Json.mkObj [
    ("kind",     l.kind.toString),
    ("scope",    optStr l.scopeModel),
    ("percent",  ToJson.toJson l.percent),
    ("severity", l.severity),
    ("active",   Json.bool l.isActive),
    ("resetsAt", optNormIso l.resetsAt)
  ]

/-- One configured authentication source, joined with whatever the usage store knows about it.

    Availability is judged with no model, which is what a reader with no task in hand can ask:
    a `weekly_scoped` limit only closes one model family, so a source carrying nothing but an
    exhausted scoped limit is genuinely still usable — and the limit rows show it regardless. -/
private def authSourceJson (backend : String) (src : AuthSource) (isDefault : Bool) (now : Int)
    : IO Json := do
  let st ← Usage.loadState backend src.label
  let (kind, baseUrl) := match src.kind with
    | .oauthToken _    => ("oauth", none)
    | .apiKey _ base   => ("api-key", base)
  let (state, reason, availableAt) := match Usage.availabilityOf st none now with
    | .available   => ("available", none, none)
    | .blocked u r => ("blocked", some r, u)
  return Json.mkObj [
    ("label",        src.label),
    ("backend",      backend),
    ("kind",         kind),
    ("baseUrl",      optStr baseUrl),
    ("isDefault",    Json.bool isDefault),
    -- Only OAuth sources have a subscription to report on; an API-key source bills per token
    -- and has no window to poll, so an empty limit list on one is expected, not a gap.
    ("pollable",     Json.bool (kind == "oauth")),
    ("state",        state),
    ("reason",       optStr reason),
    -- When the block lifts. `null` on an available source, and `null` on a blocked one whose
    -- limit reported no reset time — which is why `state` is what you branch on, not this.
    ("availableAt",  optEpochIso availableAt),
    ("pressure",     ToJson.toJson (Usage.pressureOf st none)),
    ("polledAt",     optEpochIso st.fetchedEpoch),
    -- `lastUsedTick` is nanoseconds (it orders dispatches); everything else here is seconds.
    ("lastUsedAt",   optEpochIso (st.lastUsedTick.map (· / 1000000000))),
    ("lastError",    optStr st.lastError),
    -- Set only while a backoff is still in the future; a lapsed one is not a fact about now.
    ("backoffUntil", optEpochIso (st.pollAfter.filter (· > now))),
    ("limits",       Json.arr (st.limits.map limitJson))
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
  | .error e => return Json.mkObj [("configError", Json.str e), ("backends", Json.arr #[])]
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
        ("defaultSource", optStr a.defaultAuthSource),
        ("sources",       Json.arr sources)
      ]
    return Json.mkObj [("configError", Json.null), ("backends", Json.arr backends)]

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

/-- The configured taxis instance, if there is one.

    Projects live in taxis and are its business to display, so the dashboard links out rather
    than re-rendering them. That link needs somewhere to point, and a client needs to know
    whether to offer it at all — `null` means no tracker is configured and there is nothing to
    link to. Unreadable config reads the same way: nothing to offer. -/
private def taxisUrl (configPath : Option System.FilePath) : IO (Option String) := do
  try
    return (← loadAppConfig configPath).taxis.map (·.url)
  catch _ => return none

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
    ("recentTasks", Json.arr (recent.map taskRecJson).toArray),
    ("taxisUrl",    optStr (← taxisUrl configPath))
  ]

private def queueApi (p : Page) : IO Json := do
  return pageOver p (·.createdAt) queueEntryJson (← Queue.loadAllEntries)

private def concertsApi (p : Page) : IO Json := do
  return pageOver p (·.startedAt) concertRunJson (← Queue.loadAllConcertRuns)

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
    ("lastCheckedAt",   optNormIso (if st.lastChecked.isEmpty then none else some st.lastChecked)),
    ("eventCount",      ToJson.toJson st.processedIds.size)
  ]

private def listenersApi (p : Page) : IO Json := do
  pageOverUnordered p (← Listener.loadAllListenerConfigs) fun c => do
    return listenerSummaryJson c (← Listener.loadListenerState c.name)

private def actionJson (a : Listener.ActionConfig) : Json :=
  Json.mkObj [
    ("mode",           ToJson.toJson a.mode),
    ("upstream",       a.upstream),
    ("fork",           a.fork),
    ("series",         optStr a.series),
    ("backend",        optStr a.backend),
    ("model",          optStr a.model),
    ("workflowPath",   optStr a.workflowPath),
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
      ("lastCheckedAt",   optNormIso (if st.lastChecked.isEmpty then none else some st.lastChecked)),
      ("eventCount",      ToJson.toJson st.processedIds.size),
      ("sourceType",      srcType),
      ("sourceDetail",    srcDetail),
      ("sourceExtras",    Json.arr extrasJson),
      ("action",          actionJson c.action),
      ("recentEvents",    Json.arr (recent.map Json.str).toArray)
    ])

private def tasksApi (p : Page) : IO Json := do
  return pageOver p (·.createdAt) taskRecJson (← TaskStore.loadAllTasks)

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
    ("description",   optStr p.description),
    ("createdAt",     normIso p.createdAt),
    ("defaultTarget", optStr (p.defaultTarget.map fun t => s!"{t.repo}@{t.branch}")),
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
    ("parentId",     optStr (i.parentId.map (·.toString))),
    ("dependencies", Json.arr (i.dependencies.map (Json.str ·.toString))),
    ("prCount",      ToJson.toJson i.attachedPRs.size),
    ("claimedBy",    optStr (claim.map (·.agent))),
    ("updatedAt",    normIso i.updatedAt)
  ]

private def projectsApi (p : Page) : IO Json := do
  pageOverUnordered p (← loadAllProjects) fun proj => do
    return projectSummaryJson proj (← loadIssues proj.id)

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

/-- Parse the per-task structured JSONL log, keeping only the last `limit` events. Returns the
    kept events and the total number present, so a caller can tell a tail from the whole. -/
private def loadTaskLog (fork : Repository) (id : String) (limit : Nat)
    : IO (Array Json × Nat) := do
  let path := (← Dirs.dataBase) / "logs" / fork.toString / s!"{id}.log"
  if !(← path.pathExists) then return (#[], 0)
  let raw ← IO.FS.readFile path
  let lines := (raw.splitOn "\n").filter (!·.trimAscii.isEmpty)
  let total := lines.length
  let kept := if total ≤ limit then lines else lines.drop (total - limit)
  let mut out : Array Json := #[]
  for line in kept do
    match Json.parse line with
    | .ok j    => out := out.push j
    | .error _ => out := out.push (Json.mkObj [("type", "unknown"), ("event_type", "parse_error")])
  return (out, total)

private def taskDetailApi (id : String) (logLimit : Nat) : IO (Option Json) := do
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
    let (log, total) ← loadTaskLog fork id logLimit
    return some (Json.mkObj [
      ("id",           id),
      ("status",       st),
      ("fork",         fork.toString),
      ("createdAt",    normIso createdAt),
      ("prompt",       prompt),
      ("log",          Json.arr log),
      ("logTotal",     ToJson.toJson total),
      ("logLimit",     ToJson.toJson logLimit),
      ("logTruncated", Json.bool (total > log.size))
    ])

/-- Dispatch an `/api/…` or `/sse/…` kind to the matching builder.

    Detail kinds run their component through `safeSegment` first: every one of them ends up
    in a filename, and this is the only place that check can be made once for all of them. -/
private def renderApi (configPath : Option System.FilePath) (kind : String) (q : Query)
    : IO ApiResult := do
  -- Paging is parsed before the read so a malformed parameter costs nothing, and so the same
  -- rejection reaches `/api` and `/sse` alike.
  let paged (f : Page → IO Json) : IO ApiResult := do
    match parsePage q with
    | .error e => return .badRequest e
    | .ok p    => return .ok (← f p)
  let unpaged (f : Page → IO Json) : IO ApiResult := do
    match parseUnorderedPage q with
    | .error e => return .badRequest e
    | .ok p    => return .ok (← f p)
  let plain (f : IO Json) : IO ApiResult := do
    match refuse q "limit" "is not supported by this endpoint, which is not a collection" *>
          refuse q "offset" "is not supported by this endpoint, which is not a collection" *>
          refuse q "since" "is not supported by this endpoint, which is not a collection" with
    | .error e => return .badRequest e
    | .ok _    => return .ok (← f)

  if kind == "overview"  then return ← plain (overviewApi configPath)
  if kind == "auth"      then return ← plain (authApi configPath)
  if kind == "queue"     then return ← paged queueApi
  if kind == "concerts"  then return ← paged concertsApi
  if kind == "tasks"     then return ← paged tasksApi
  if kind == "listeners" then return ← unpaged listenersApi
  if kind == "projects"  then return ← unpaged projectsApi

  let detail (prefix_ : String) (f : String → IO (Option Json)) : IO (Option ApiResult) := do
    unless kind.startsWith prefix_ do return none
    -- An id that could name a directory or an ancestor never reaches a loader; it simply
    -- names nothing, which is a 404 rather than an error.
    let some seg := safeSegment (kind.drop prefix_.length).toString | return some .notFound
    match ← f seg with
    | some j => return some (.ok j)
    | none   => return some .notFound
  if let some r ← detail "projects/"  projectDetailApi  then return r
  if let some r ← detail "concerts/"  concertDetailApi  then return r
  if let some r ← detail "listeners/" listenerDetailApi then return r
  if kind.startsWith "tasks/" then
    match natParam q "logLimit" defaultLogLimit maxLogLimit with
    | .error e => return .badRequest e
    | .ok limit =>
      let some seg := safeSegment (kind.drop "tasks/".length).toString | return .notFound
      match ← taskDetailApi seg limit with
      | some j => return .ok j
      | none   => return .notFound
  return .notFound

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
private def requestSession (req : Request Body.Stream) : Option String :=
  (reqHeader req "cookie").bind (cookieValue · sessionCookieName)

/-- Whether a request may reach the API: a valid session cookie, or the shared secret as a
    bearer token. Both comparisons are constant-time. -/
private def authenticated (st : ServeState) (req : Request Body.Stream) : IO Bool := do
  if let some v := reqHeader req "authorization" then
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

/-- Cap on a login body. One JSON object with one field, so anything larger is a mistake or
    an attempt to sit on memory; `Std.Http.Config` bounds everything else about a request. -/
private def maxLoginBytes : UInt64 := 8192

private def loginHandler (st : ServeState) (body : String) : Async (Response Body.Any) := do
  let supplied := match Json.parse body with
    | .ok j    => (j.getObjValAs? String "password").toOption
    | .error _ => none
  match supplied with
  | none => errorResp "expected a JSON body of the form {\"password\": \"…\"}" .badRequest
  | some p =>
    unless constantTimeEq p st.cfg.password do
      return ← errorResp "invalid password" .unauthorized
    let sid ← issueSession st
    jsonResp (Json.mkObj [("authenticated", Json.bool true)])
      (setCookie := cookieHeader st sid st.cfg.sessionTtlSeconds)

private def logoutHandler (st : ServeState) (req : Request Body.Stream)
    : Async (Response Body.Any) := do
  if let some sid := requestSession req then revokeSession st sid
  jsonResp (Json.mkObj [("authenticated", Json.bool false)])
    (setCookie := cookieHeader st "" 0)

/-- Whether the caller is already authenticated. Deliberately reachable without credentials:
    it is what the front-end asks on load to decide between the login screen and the app, and
    it discloses nothing but a boolean the caller could learn by trying any other endpoint. -/
private def sessionHandler (st : ServeState) (req : Request Body.Stream)
    : Async (Response Body.Any) := do
  jsonResp (Json.mkObj [("authenticated", Json.bool (← authenticated st req))])

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

/-- Serve one file from the site directory.

    Sent as raw bytes with an explicit `Content-Type` rather than through one of the text
    helpers: the bundle is text today, but a font or an image emitted by Vite must not become
    a decoding error.

    Vite fingerprints everything under `assets/`, so those are immutable and cached for a
    year; `index.html` names them and must never be cached, or a reload after a rebuild pairs
    a new shell with stale chunks. -/
private def serveStatic (root : System.FilePath) (path : String)
    : Async (Response Body.Any) := do
  let some file ← resolveStatic root path
    | return ← (secured Response.notFound).text "not found\n"
  let bytes ← IO.FS.withFile file .read fun h => h.readBinToEnd
  let cache :=
    if path.startsWith "/assets/" then "public, max-age=31536000, immutable" else "no-cache"
  return ← (secured Response.ok)
    |>.header! "Content-Type" (contentTypeOf file.toString)
    |>.header! "Cache-Control" cache
    |>.fromBytes bytes

/-! ## Route dispatch -/

/-- Answer a `/api/<kind>` read.

    A builder that throws must still produce a response. `projects` reaches out to taxis and
    `auth` reads `config.json`, so an unreachable tracker or an unparseable config is an
    ordinary runtime condition here, and a 500 carrying the reason beats a dropped
    connection. -/
private def apiResponse (st : ServeState) (kind : String) (q : Query)
    : Async (Response Body.Any) := do
  match ← (try
      Except.ok <$> renderApi st.cfg.configPath kind q
    catch e => pure (Except.error (toString e))) with
  | .ok (.ok j)          => jsonResp j
  | .ok .notFound        => notFoundJsonResp
  | .ok (.badRequest e)  => errorResp e .badRequest
  | .error e             => errorResp e .internalServerError

/-! ## SSE streaming -/

/-- Encode a payload as a single SSE `message` event. -/
private def sseFrame (payload : String) : String :=
  let lines := payload.splitOn "\n"
  (lines.map (fun l => "data: " ++ l) |> String.intercalate "\n") ++ "\n\n"

/-- Refresh interval for SSE pushes, in milliseconds. -/
private def sseIntervalMs : Std.Time.Millisecond.Offset := 2000

/-- Ticks between keep-alive comments when nothing has changed. At the 2s interval above this
    is one line every 30s, which is under the idle timeout of every proxy worth naming. -/
private def sseKeepAliveTicks : Nat := 15

/-- Push `kind` into `out` until the client goes away.

    Only *changed* payloads are sent. An idle orchestra re-renders to a byte-identical
    document, and a dashboard left open on a second monitor should cost the network nothing
    while that is true — the client would discard the frame anyway.

    A disconnected client surfaces as a throw from `send` once the connection is torn down,
    which ends the generator and closes the stream. -/
private partial def sseLoop (st : ServeState) (out : Body.Stream) (kind : String) (q : Query)
    (lastSent : String) (idleTicks : Nat) : Async Unit := do
  -- A builder that throws (taxis unreachable, config momentarily unreadable) skips this tick
  -- rather than closing the stream: the client keeps the last good frame on screen and picks
  -- the next one up when the condition clears. Closing would instead blank the page and set
  -- `EventSource` reconnecting every few seconds for as long as the outage lasts.
  let outcome ← try
      Except.ok <$> renderApi st.cfg.configPath kind q
    catch e => pure (Except.error (toString e))
  match outcome with
  -- The resource is gone or the parameters are wrong; neither improves by waiting, so the
  -- stream ends. The status was already sent, so the client learns by the stream closing.
  | .ok .notFound | .ok (.badRequest _) => return
  | .error _ =>
    Std.Async.sleep sseIntervalMs
    sseLoop st out kind q lastSent idleTicks
  | .ok (.ok j) =>
    let payload := Json.compress j
    let (frame, idleTicks) :=
      if payload != lastSent then (some (sseFrame payload), 0)
      else if idleTicks + 1 ≥ sseKeepAliveTicks then (some ": keep-alive\n\n", 0)
      else (none, idleTicks + 1)
    if let some f := frame then
      out.send (Chunk.ofByteArray f.toUTF8)
    Std.Async.sleep sseIntervalMs
    sseLoop st out kind q payload idleTicks

/-- The SSE response for `kind`. `X-Accel-Buffering` tells an nginx in front not to hold
    frames back; the rest is what `EventSource` requires. -/
private def sseResponse (st : ServeState) (kind : String) (q : Query)
    : Async (Response Body.Any) := do
  -- The first payload is produced *before* the status is committed, so a rejected parameter
  -- or a resource that does not exist is a status the client can read. A stream that opens
  -- `200` and closes immediately tells `EventSource` only to reconnect, which turns a typo in
  -- a query string into a silent retry loop.
  match ← (try
      Except.ok <$> renderApi st.cfg.configPath kind q
    catch e => pure (Except.error (toString e))) with
  | .ok .notFound       => notFoundJsonResp
  | .ok (.badRequest e) => errorResp e .badRequest
  | .error e            => errorResp e .internalServerError
  | .ok (.ok j) =>
    let first := Json.compress j
    return ← (secured Response.ok)
      |>.header! "Content-Type" "text/event-stream"
      |>.header! "Cache-Control" "no-store"
      |>.header! "X-Accel-Buffering" "no"
      |>.stream fun out => do
        out.send (Chunk.ofByteArray (sseFrame first).toUTF8)
        sseLoop st out kind q first 0

/-! ## Route dispatch -/

/-! ## The published contract

`docs/openapi.json` describes every route below. It is embedded at build time and served, so
the description a client reads always came from the same binary that answers it — a spec that
lives only in a repository is a spec that has already drifted. `OrchestraTest.Dashboard`
checks the two against each other, so a route added without a spec entry fails the suite. -/

/-- The OpenAPI description of this API, embedded from `docs/openapi.json`. -/
def openApiSpec : String := include_str "../docs/openapi.json"

/-- Where the spec is served, and the prefix everything it describes lives under. -/
def apiVersion : String := "v1"

/-- Every read the API serves, as the `<kind>` that `renderApi` dispatches on.

    Not `private`: the spec cross-check in `OrchestraTest.Dashboard` walks this, which is what
    keeps `docs/openapi.json` honest. Detail routes appear with their OpenAPI template
    parameter, since that is how the spec names them. -/
def apiKinds : Array String :=
  #["overview", "queue", "tasks", "tasks/{id}", "concerts", "concerts/{id}",
    "listeners", "listeners/{name}", "projects", "projects/{id}", "auth"]

/-- Everything the dashboard answers, in one place.

    The authentication surface comes first, since `/api/login` has to be reachable by
    definition. The site is next and is *not* gated: the browser has to be able to load the
    app shell and its bundle in order to render the login screen that acquires a session at
    all. Everything the site then asks for — the API and the streams — is gated. -/
private def route (st : ServeState) (req : Request Body.Stream) : Async (Response Body.Any) := do
  let path := pathOf req.line.uri
  let query := req.line.uri.query
  let method := req.line.method

  if path == "/api/login" then
    unless method == .post do return ← methodNotAllowedResp
    let body : String ← req.body.readAll (maximumSize := some maxLoginBytes)
    return ← loginHandler st body
  if path == "/api/logout" then
    unless method == .post do return ← methodNotAllowedResp
    return ← logoutHandler st req
  if path == "/api/session" then
    unless method == .get do return ← methodNotAllowedResp
    return ← sessionHandler st req

  -- Served without a credential and outside the version prefix: a client has to be able to
  -- discover what it is talking to, and how to authenticate, before it can authenticate.
  if path == "/api/openapi.json" then
    unless method == .get do return ← methodNotAllowedResp
    return ← (secured Response.ok)
      |>.header! "Cache-Control" "no-cache"
      |>.json openApiSpec

  if let some kind := apiKind s!"/sse/{apiVersion}/" path then
    unless method == .get do return ← methodNotAllowedResp
    unless ← authenticated st req do return ← unauthorizedResp
    return ← sseResponse st kind query

  if method == .get && !(path.startsWith "/api/") && !(path.startsWith "/sse/") then
    if let some root := st.cfg.siteDir then
      return ← serveStatic root path

  unless ← authenticated st req do return ← unauthorizedResp
  unless method == .get do return ← methodNotAllowedResp
  match apiKind s!"/api/{apiVersion}/" path with
  | some kind => apiResponse st kind query
  | none      => notFoundJsonResp

/-- The `Std.Http` handler: state plus the routing above. -/
private structure Dashboard where
  st : ServeState

private instance : Handler Dashboard where
  onRequest d req := route d.st req
  -- A dropped connection is the normal way a dashboard tab goes away, and a socket error
  -- carries nothing an operator can act on. Silence beats a log line per closed tab.
  onFailure _ _ := pure ()

/-! ## Server -/

/-- Bounds on a single request. Every route is a `GET` or a one-field login, so the defaults
    for bodies and header counts are far larger than anything legitimate; SSE is what the
    rest is tuned for.

    `maxRequests` has to be unlimited: it caps requests per connection, and a browser holding
    an `EventSource` open reuses one connection for as long as the page is open. -/
private def httpConfig : Std.Http.Config where
  maxRequests := 0
  maxBodySize := 65536
  maxUriLength := 4096

/-- Start the dashboard server: the JSON API, the SSE streams, and — with `cfg.siteDir` — the
    built front-end, all on `cfg.host`:`cfg.port` (`0` = auto-assign).

    Returns `(boundPort, shutdown)`. Throws if `cfg.host` is not an IPv4 address. -/
def serve (cfg : ServeConfig) : IO (UInt16 × IO Unit) := do
  let some hostAddr := IPv4Addr.ofString cfg.host
    | throw (.userError s!"dashboard: '{cfg.host}' is not an IPv4 address")
  let sessions ← IO.mkRef (#[] : Array (String × Int))
  let st : ServeState := { cfg, sessions }
  let server ← Async.block do
    Std.Http.Server.serve (SocketAddress.v4 { addr := hostAddr, port := cfg.port })
      (Dashboard.mk st) httpConfig
  let boundPort := match server.localAddr with
    | some (.v4 a) => a.port
    | some (.v6 a) => a.port
    | none         => cfg.port
  return (boundPort, Async.block server.shutdownAndWait)

end Orchestra.Dashboard
