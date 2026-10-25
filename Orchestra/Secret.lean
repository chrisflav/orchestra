import Orchestra.Dirs
import Taxis.Crypto

/-!
# The shared secret

One secret authenticates everything that is not a public read: the browser exchanges it for a
session cookie, a script sends it as a bearer token, and `orchestra` — which is now an HTTP
client of the server rather than a second writer of the same files — sends it the same way.

It lives here rather than in `Orchestra.Dashboard` because both binaries need it and only one of
them is the server. `orchestrad` resolves it with `resolvePassword`, which generates and persists
one when there is nothing to find; `orchestra` resolves it with `lookupPassword`, which does not.
That asymmetry is deliberate: a client that generated a secret would write a value the server has
never seen, and the failure would surface as a confusing `401` instead of "no secret configured".
-/

namespace Orchestra.Secret

/-- The env var that supplies the shared secret, to server and client alike. -/
def passwordEnvVar : String := "ORCHESTRA_DASHBOARD_PASSWORD"

/-- Where a generated secret is persisted, so restarts don't invalidate what the user already
    wrote down — and so the CLI on the same host can find it without being told. -/
def secretFile : IO System.FilePath := do
  return (← Dirs.dataBase) / "dashboard.secret"

private def hexDigits : Array Char := "0123456789abcdef".toList.toArray

private def toHex (b : UInt8) : String :=
  let n := b.toNat
  String.ofList [hexDigits[n >>> 4]!, hexDigits[n &&& 0xf]!]

/-- `n` bytes of `/dev/urandom`, hex-encoded. -/
def randomHex (n : Nat) : IO String := do
  let bytes ← IO.FS.withFile "/dev/urandom" .read fun h => h.read n.toUSize
  return String.join (bytes.toList.map toHex)

/-- The secret as configured, without ever inventing one, in priority order:
    1. an explicit `flagPassword` (`--password`),
    2. `$ORCHESTRA_DASHBOARD_PASSWORD`,
    3. one previously persisted under the data dir.

    `none` means nothing is configured — which for a client is a diagnosable condition and for
    the server is the cue to generate one. -/
def lookupPassword (flagPassword : Option String := none) : IO (Option String) := do
  if let some p := flagPassword then
    if !p.trimAscii.isEmpty then return some p.trimAscii.toString
  if let some p ← IO.getEnv passwordEnvVar then
    if !p.trimAscii.isEmpty then return some p.trimAscii.toString
  let path ← secretFile
  if ← path.pathExists then
    let p := (← IO.FS.readFile path).trimAscii.toString
    if !p.isEmpty then return some p
  return none

/-- Resolve the shared secret for the server: `lookupPassword`, or a freshly generated one
    (persisted for reuse).

    Returns the secret and whether it had to be generated, so the caller can print a generated
    one prominently and stay quiet about a configured one. -/
def resolvePassword (flagPassword : Option String := none) : IO (String × Bool) := do
  if let some p ← lookupPassword flagPassword then return (p, false)
  let p ← randomHex 24
  let path ← secretFile
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

/-! ## The daemon's control-socket token

The queue daemon takes commands over a Unix socket (`Queue.socketFile`): queue this task, start that
concert, shut down, start an interactive session, claim an issue. Filesystem permissions were its
only protection, and they protect nothing from a process of the same user — which is what every
agent is. Landlock does not mediate `connect(2)` on a pathname Unix socket (not as of ABI 6), so an
agent that knows the path can connect without any grant on the directory it is in.

So every request carries a token, and the daemon refuses one that does not (`DaemonRequest.gate`).
The token is a random value in `<config>/daemon.token`, mode 0600, created by the daemon on its
first start and kept across restarts so a dashboard and a CLI that read it keep working. The
configuration directory is one no agent grant may reach (`Landrun.withoutGuarded`), and the token
is never put in any process's environment, so an agent has no way to learn it. The dashboard
(forwarding API calls), the CLI and `orchestra project claim` read the file. -/

/-- Where the daemon's control-socket token is kept: in the configuration directory, next to
    `secrets.json`, where no agent grant reaches. -/
def daemonTokenFile : IO System.FilePath := do
  return (← Dirs.configBase) / "daemon.token"

/-- Write `content` to `path` so that it is never readable by anyone but its owner, not even for a
    moment: a fresh file in the same directory, restricted to 0600 while still empty, filled, and
    renamed over `path`. -/
def writePrivateFile (path : System.FilePath) (content : String) : IO Unit := do
  if let some dir := path.parent then IO.FS.createDirAll dir
  let tmp := System.FilePath.mk s!"{path}.{← randomHex 6}.tmp"
  IO.FS.writeFile tmp ""
  IO.setAccessRights tmp { user := { read := true, write := true } }
  IO.FS.writeFile tmp content
  IO.FS.rename tmp path

/-- The daemon's token as the file holds it, or `none` when there is no file or it is empty. -/
def lookupDaemonToken : IO (Option String) := do
  let path ← daemonTokenFile
  let t ← try (·.trimAscii.toString) <$> IO.FS.readFile path catch _ => pure ""
  return if t.isEmpty then none else some t

/-- The daemon's token, generated and persisted on first use. Only the daemon calls this: a client
    that minted one would hold a value the daemon has never seen, and its first request would be
    refused for a reason nobody could see. A file that exists is also made owner-only again, in
    case something loosened it. -/
def ensureDaemonToken : IO String := do
  let path ← daemonTokenFile
  if let some t ← lookupDaemonToken then
    try IO.setAccessRights path { user := { read := true, write := true } } catch _ => pure ()
    return t
  let t ← randomHex 32
  writePrivateFile path (t ++ "\n")
  return t

/-! ## Proving the server is orchestrad

The CLI sends the bearer password to `http://127.0.0.1:8080` unless told otherwise. Whatever
listens there gets it — and while the dashboard is down, that could be anything running as this
user, a sandboxed agent included (no agent is granted that port any more, see
`Exec.orchestraPorts`, but the CLI should not have to trust that). So before the first request, a
CLI that can read `daemon.token` — one on the daemon's own machine — asks the server to prove it
can read it too: it sends a fresh nonce to `/api/proof` and checks the answer is
`HMAC-SHA256(token, "orchestrad-proof:" ++ nonce)` (`daemonProof`). Only a process that can read the
configuration directory can answer, and no agent can. The token itself never crosses the wire. -/

/-- The answer to the proof challenge for `nonce`, under the daemon's `token`. -/
def daemonProof (token nonce : String) : String :=
  Taxis.Crypto.hmacHex token.toUTF8 ("orchestrad-proof:" ++ nonce)

end Orchestra.Secret
