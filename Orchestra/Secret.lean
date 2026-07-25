import Orchestra.Dirs

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

end Orchestra.Secret
