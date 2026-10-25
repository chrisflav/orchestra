import Lean.Data.Json
import Orchestra.Secret
import Orchestra.Utils.Http
import Orchestra.Exec.Backend

open Lean (Json)

/-!
# The API client

`orchestra` is a client of `orchestrad`. This is the half of that sentence that lives in the
library: resolving where the server is and which secret to present, making the request, and
turning what comes back into either a `Json` or a sentence a person can act on.

The transport is curl, via `Orchestra.Utils.Http`, for the same reason the GitHub and usage code
uses it: it is already a dependency, it handles TLS and redirects and proxies, and a hand-rolled
HTTP client would be a second one of those to get wrong.

Authentication is the bearer half of the dashboard's scheme (see "Authentication" in
`Orchestra.Dashboard`): `Authorization: Bearer <secret>` on every request, no cookie, no
session. A CLI has no ambient credential to be confused about and nothing to protect a session
from, so the browser half —
`POST /api/login`, `SameSite=Strict`, CSRF — does not apply to it at all.

## Where the server is

`--api-url`, then `$ORCHESTRA_API_URL`, then `http://127.0.0.1:8080` — the address `orchestrad
dashboard` binds by default. The secret is resolved by `Orchestra.Secret`, which reads the same
three places the server does, so a CLI on the same host as the server finds it in
`<data>/dashboard.secret` without being told anything.

That last point is what keeps the split from costing the single-host user anything: `orchestrad
serve` and `orchestra listener list` share a filesystem and therefore share a secret. Pointing
the CLI at a *remote* orchestra is the case that needs `$ORCHESTRA_API_URL` and
`$ORCHESTRA_DASHBOARD_PASSWORD` set, and it is a case that did not exist before.

## Who it is talking to

The default URL is a port on loopback, and whatever listens there is sent the password. While the
dashboard is down that could be anything running as this user. So a CLI on the daemon's machine
first has the server prove it can read the daemon's token (`verifyServer`, `Secret.daemonProof`)
and refuses to go on when it cannot.
-/

namespace Orchestra.Client

/-- The env var naming the server to talk to. -/
def urlEnvVar : String := "ORCHESTRA_API_URL"

/-- Where `orchestrad dashboard` listens unless told otherwise. -/
def defaultBaseUrl : String := "http://127.0.0.1:8080"

/-- A resolved server address and credential. -/
structure Config where
  /-- Base URL with no trailing slash. -/
  baseUrl : String
  /-- The shared secret, sent as a bearer token. -/
  token   : String

/-- What to say when there is no secret to be found. Long because every one of the three ways out
    is a thing the reader may not know exists yet. -/
private def noSecretMessage : String :=
  s!"No orchestra API secret configured.\n\
\n\
The CLI talks to the backend over its HTTP API and has to authenticate. Give it the secret in \
any of the ways the server accepts:\n\
  * export ${Secret.passwordEnvVar}=<secret>\n\
  * pass --api-token <secret>\n\
  * or run the CLI on the host where the server persisted it (<data>/dashboard.secret)\n\
\n\
`orchestrad serve` prints a generated secret once, on first start."

/-- Resolve where to talk and what to present. -/
def resolve (flagUrl : Option String := none) (flagToken : Option String := none) :
    IO (Except String Config) := do
  let baseUrl ← match flagUrl with
    | some u => pure u
    | none   => pure ((← IO.getEnv urlEnvVar).getD defaultBaseUrl)
  -- A trailing slash would produce `//api/v1/...`, which some proxies normalise and others
  -- 404; dropping it once here is cheaper than being careful at every call site.
  let baseUrl := if baseUrl.endsWith "/" then baseUrl.dropEnd 1 |>.toString else baseUrl
  match ← Secret.lookupPassword flagToken with
  | none       => return .error noSecretMessage
  | some token => return .ok { baseUrl, token }

private def hexDigit (n : Nat) : Char :=
  if n < 10 then Char.ofNat (n + '0'.toNat) else Char.ofNat (n - 10 + 'A'.toNat)

/-- Percent-encode one path component.

    Configuration names are checked against `Utils.validConfigName` before they get here, so a
    separator can never appear in one — but a space or a `#` can, and either would silently
    truncate or re-route the request. Everything outside the unreserved set is escaped, which is
    more than strictly necessary and exactly what the server's `safeSegment` decodes. -/
def encodeSegment (s : String) : String :=
  s.toUTF8.foldl (init := "") fun acc b =>
    let c := Char.ofNat b.toNat
    if ('a' ≤ c && c ≤ 'z') || ('A' ≤ c && c ≤ 'Z') || ('0' ≤ c && c ≤ '9')
        || c == '-' || c == '_' || c == '.' || c == '~' then
      acc.push c
    else
      acc ++ String.ofList ['%', hexDigit (b.toNat >>> 4), hexDigit (b.toNat &&& 0xf)]

/-- The `error` field of an API error body, or the body itself when it is not one of ours.

    Every route in `Orchestra.Dashboard` answers a failure as `{"error": "…"}` carrying a
    sentence; passing that sentence through unchanged is the whole point of the server having
    written one. -/
private def errorText (status : Nat) (body : String) : String :=
  match Json.parse body with
  | .ok j =>
    match j.getObjValAs? String "error" with
    | .ok e    => e
    | .error _ => s!"HTTP {status}: {body.trimAscii}"
  | .error _ =>
    if body.trimAscii.isEmpty then s!"HTTP {status}" else s!"HTTP {status}: {body.trimAscii}"

/-- The base URLs this process has already seen prove themselves (`verifyServer`), so the proof
    costs one round trip per process rather than one per request. -/
initialize verifiedServersRef : IO.Ref (Array String) ← IO.mkRef #[]

/-- Whether the server at `cfg.baseUrl` may be sent the password: `.ok` when it proved it can read
    the daemon's token (`Secret.daemonProof`), or when there is nothing to check it against.

    Checked when the URL is plain HTTP on this machine's loopback — the default, and the one place
    something other than orchestrad could be listening as this user while the dashboard is down —
    and the CLI can read `daemon.token`, which is to say it runs on the daemon's machine as the
    daemon's user. A CLI elsewhere has no token to check with and talks to whatever it was pointed
    at, as before; a server elsewhere is behind TLS and a name the operator chose. -/
def verifyServer (cfg : Config) : IO (Except String Unit) := do
  if (← verifiedServersRef.get).contains cfg.baseUrl then return .ok ()
  if (Exec.loopbackPort? cfg.baseUrl).isNone then return .ok ()
  let some token ← Secret.lookupDaemonToken | return .ok ()
  let nonce ← Exec.randomSecret 16
  let refuse (why : String) : IO (Except String Unit) :=
    return .error s!"Not sending the orchestra password to {cfg.baseUrl}: {why}\n\
Whatever answers there could not prove it is orchestrad (it cannot read daemon.token in the \
configuration directory). If the dashboard is not running, start it with 'orchestrad serve'; if \
something else holds the port, stop it."
  let (status, body) ← try
      Utils.Http.curlWithStatus #[s!"{cfg.baseUrl}/api/proof?nonce={nonce}"] (maxTime := 10)
    catch e =>
      -- Nothing listening: the request proper would fail the same way, and says so better.
      return .error s!"Could not reach the orchestra API at {cfg.baseUrl}: {e}\n\
Is the backend running? Start it with 'orchestrad serve', or point the CLI elsewhere with \
--api-url / ${urlEnvVar}."
  unless status == 200 do return ← refuse s!"it answered the proof request with HTTP {status}."
  let proof := (Json.parse body).toOption.bind (·.getObjValAs? String "proof" |>.toOption)
  match proof with
  | some p =>
    if Secret.constantTimeEq p (Secret.daemonProof token nonce) then
      verifiedServersRef.modify (·.push cfg.baseUrl)
      return .ok ()
    else refuse "its proof is wrong."
  | none => refuse "its answer to the proof request carries no proof."

/-- One request. `body` is sent as `application/json`, which is what the server requires of a
    write (see the CSRF discussion in `Orchestra.Dashboard`).

    A `204` and an empty body both come back as `Json.null` rather than as a parse error: they
    are the successful answer to a `DELETE`, not a malformed one to anything. -/
def request (cfg : Config) (method : String) (path : String) (body : Option String := none)
    (maxTime : Nat := 30) : IO (Except String Json) := do
  if let .error e ← verifyServer cfg then return .error e
  let url := cfg.baseUrl ++ path
  let mut args := #["-X", method]
  if let some b := body then
    args := args ++ #["-H", "Content-Type: application/json", "--data-binary", b]
  let (status, respBody) ← try
      -- The secret goes to curl over stdin, not in `argv`; see `Utils.Http.bearerConfig`.
      Utils.Http.curlWithStatus (args.push url) (maxTime := maxTime) (bearer := some cfg.token)
    catch e =>
      -- A transport failure is the commonest way this goes wrong on a first run, and "connection
      -- refused" on its own does not say which of the two halves is missing.
      return .error s!"Could not reach the orchestra API at {cfg.baseUrl}: {e}\n\
Is the backend running? Start it with 'orchestrad serve', or point the CLI elsewhere with \
--api-url / ${urlEnvVar}."
  if status == 401 then
    return .error s!"The orchestra API at {cfg.baseUrl} rejected the secret.\n\
It resolves the same secret the server does; if they are not on the same host, set \
${Secret.passwordEnvVar} to the server's."
  if status < 200 || status ≥ 300 then
    return .error (errorText status respBody)
  if respBody.trimAscii.isEmpty then return .ok Json.null
  match Json.parse respBody with
  | .ok j    => return .ok j
  | .error e => return .error s!"The orchestra API answered {status} with a body that is not \
JSON: {e}"

def get (cfg : Config) (path : String) (maxTime : Nat := 30) : IO (Except String Json) :=
  request cfg "GET" path (maxTime := maxTime)

/-- `maxTime` is worth passing for a route that does real work before it answers. Starting an
    interactive session clones a repository and launches an agent inside the sandbox, and the
    30 s that is right for a route reading a file off disk cuts that off at the point where it
    has already begun — leaving a session running on the daemon that the caller was told had
    failed. -/
def post (cfg : Config) (path : String) (body : String) (maxTime : Nat := 30)
    : IO (Except String Json) :=
  request cfg "POST" path (some body) (maxTime := maxTime)

def put (cfg : Config) (path : String) (body : String) : IO (Except String Json) :=
  request cfg "PUT" path (some body)

def delete (cfg : Config) (path : String) : IO (Except String Json) :=
  request cfg "DELETE" path

/-! ## Collections

Every list the API serves answers in one envelope (`items`, `total`, `limit`, `offset`). The CLI
prints tables, so it wants the items and the count of what it did not ask for. -/

/-- The `items` of a collection envelope, and its `total`. -/
def items (j : Json) : Array Json × Nat :=
  let arr := (j.getObjVal? "items" |>.toOption).bind (·.getArr?.toOption) |>.getD #[]
  let total := (j.getObjValAs? Nat "total").toOption |>.getD arr.size
  (arr, total)

/-- A string field of a payload, or `dflt` when it is absent or `null`. -/
def str (j : Json) (field : String) (dflt : String := "") : String :=
  (j.getObjValAs? String field).toOption |>.getD dflt

/-- A `Nat` field of a payload, or `dflt`. -/
def nat (j : Json) (field : String) (dflt : Nat := 0) : Nat :=
  (j.getObjValAs? Nat field).toOption |>.getD dflt

/-- A `Bool` field of a payload, or `dflt`. -/
def bool (j : Json) (field : String) (dflt : Bool := false) : Bool :=
  (j.getObjValAs? Bool field).toOption |>.getD dflt

/-! ## Transcript streams

The one thing this client does that is not a request and a reply. -/

/-- Attach to a session's transcript from `after`.

    The caller reads frames off the returned stream and closes it when it is done. Detaching is
    not ending: closing this leaves the session up on the daemon, which is what lets a person
    close a laptop and pick the conversation up from somewhere else. -/
def openTranscript (cfg : Config) (id : String) (after : Nat) : IO Utils.Http.Stream := do
  if let .error e ← verifyServer cfg then throw (.userError e)
  Utils.Http.openStream
    s!"{cfg.baseUrl}/sse/v1/interactive/{encodeSegment id}/events?after={after}" cfg.token

/-- The payload of one SSE `data:` line, or `none` for anything else on the wire — a comment,
    the `id:` line, or the blank line between frames.

    The `id:` line is not needed here: the seqs are in the payload, and a client that trusts the
    payload cannot disagree with itself about where it has got to. -/
def sseData (line : String) : Option String :=
  let line := line.trimAsciiEnd.toString
  if line.startsWith "data: " then some (line.drop "data: ".length).toString else none

end Orchestra.Client
