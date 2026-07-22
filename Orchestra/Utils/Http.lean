/-
A minimal HTTP client over `curl`, for the handful of REST calls orchestra makes outside of
`gh`.

`Orchestra.GitHub` grew the original version of this against api.github.com; it is lifted here
so the usage-limit poller can reuse it instead of keeping a second copy of the same careful
error handling.
-/

namespace Orchestra.Utils.Http

/-- Separates the response body from the status code `-w` appends. Long and unlikely enough that
    a body containing it would have to be trying. -/
def statusMarker : String := "\n<<<orchestra-http-status:"

/-- Split curl's `-w`-augmented output into `(status, body)`, or `none` when the status line is
    absent — which means curl died before writing it and the whole output is a diagnostic.

    Splits on the *last* marker, so a response body that happens to contain the marker is
    returned intact rather than truncated at it. -/
def splitStatus (out : String) : Option (Nat × String) :=
  let parts := out.splitOn statusMarker
  if parts.length < 2 then none
  else
    let status := (parts.getLastD "").trimAscii.toString
    match status.toNat? with
    | none   => none
    | some s => some (s, statusMarker.intercalate parts.dropLast)

/-- Failure text for a curl invocation that exited non-zero. Arguments are deliberately never
    included: they carry bearer tokens. -/
private def curlFailure (code : UInt32) (stdout stderr : String) : String :=
  let err := stderr.trimAscii.toString
  let out := stdout.trimAscii.toString
  let detail :=
    if !err.isEmpty then (if out.isEmpty then err else s!"{err}\n{out}")
    else if !out.isEmpty then out
    else "(curl produced no output on stdout or stderr)"
  s!"curl failed (exit {code}): {detail}"

/-- Run curl and return `(status, body)`.

    Deliberately **not** `curl -f`: with `-f` curl exits 22 on any 4xx/5xx *and discards the
    response body*, so an authentication failure arrives as a bare exit code when the server had
    in fact replied with a sentence saying what was wrong. The status is requested explicitly via
    `-w` and the body kept, so the caller can report both. `-S` restores curl's own message for
    transport failures (DNS, refused connection) that produce no body at all. -/
def curlWithStatus (args : Array String) (connectTimeout : Nat := 10) (maxTime : Nat := 30)
    : IO (Nat × String) := do
  let full := #["-sS", "--connect-timeout", toString connectTimeout,
                "--max-time", toString maxTime,
                "-w", statusMarker ++ "%{http_code}"] ++ args
  let child ← IO.Process.spawn {
    cmd := "curl", args := full, stdin := .null, stdout := .piped, stderr := .piped
  }
  let stdout ← child.stdout.readToEnd
  let stderr ← child.stderr.readToEnd
  let code   ← child.wait
  if code != 0 then
    throw (.userError (curlFailure code stdout stderr))
  match splitStatus stdout with
  | some r => return r
  | none   => throw (.userError s!"curl wrote no status line; its output was:\n{stdout}")

/-- A GET with bearer authentication. The token never appears in an error message. -/
def getBearer (url token : String) (extraHeaders : Array String := #[])
    (maxTime : Nat := 15) : IO (Nat × String) := do
  let mut args := #["-H", s!"Authorization: Bearer {token}"]
  for h in extraHeaders do
    args := args.push "-H" |>.push h
  curlWithStatus (args.push url) (maxTime := maxTime)

end Orchestra.Utils.Http
