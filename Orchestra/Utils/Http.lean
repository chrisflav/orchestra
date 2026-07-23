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

/-- The `name: value` lines of one header block, with the status line dropped and names
    lowercased so a lookup does not depend on how the server capitalised them. -/
private def parseHeaderBlock (block : String) : Array (String × String) :=
  block.splitOn "\n" |>.foldl (init := #[]) fun acc line =>
    let line := line.trimAscii.toString
    if line.startsWith "HTTP/" then acc
    else match line.splitOn ":" with
      | name :: v :: rest =>
        acc.push (name.trimAscii.toString.toLower, (":".intercalate (v :: rest)).trimAscii.toString)
      | _ => acc

/-- The status code of a header block's first line, for the two spellings curl writes:
    `HTTP/1.1 200 OK` and HTTP/2's `HTTP/2 200`. -/
private def blockStatus (block : String) : Option Nat :=
  match ((block.splitOn "\n").headD "").trimAscii.toString.splitOn " " with
  | _ :: code :: _ => code.trimAscii.toString.toNat?
  | _              => none

/-- Is this block only a preamble to the real response?

    curl emits more than one header block in exactly two cases: an informational `1xx`, and a
    proxy's answer to the `CONNECT` that precedes a tunnelled request. Everything else is the
    response itself, and what follows it is body — even in the unlikely event that the body
    starts with something that reads like a status line. -/
private def blockIsPreamble (block : String) : Bool :=
  match blockStatus block with
  | some c => c < 200 || (c == 200 && (block.splitOn "Connection established").length > 1)
  | none   => false

/-- Split a `-D -` header dump from the body that follows it.

    curl writes a status line, the headers, a blank line, and then the body. Preamble blocks are
    consumed until the real response is reached, so the headers returned are the ones the server
    that answered actually sent. Splitting only where a block begins with `HTTP/` is what keeps a
    body containing a blank line intact. -/
partial def splitHeaders (out : String) : Array (String × String) × String :=
  if !out.startsWith "HTTP/" then (#[], out)
  else
    -- `\r\n\r\n` is what the wire uses; the bare `\n\n` fallback covers a curl built to
    -- translate line endings, which some platforms ship.
    let cut : Option (String × String) :=
      match out.splitOn "\r\n\r\n" with
      | head :: _ :: _ => some (head, (out.drop (head.length + 4)).toString)
      | _ => match out.splitOn "\n\n" with
        | head :: _ :: _ => some (head, (out.drop (head.length + 2)).toString)
        | _ => none
    match cut with
    -- No blank line at all: headers with an empty body, which is what a `HEAD` or a bodiless
    -- error response looks like.
    | none => (parseHeaderBlock out, "")
    | some (block, rest) =>
      if blockIsPreamble block && rest.startsWith "HTTP/" then splitHeaders rest
      else (parseHeaderBlock block, rest)

/-- Look up a header by name, case-insensitively. -/
def header? (headers : Array (String × String)) (name : String) : Option String :=
  headers.find? (·.1 == name.toLower) |>.map (·.2)

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

/-- Run curl and return `(status, headers, body)`.

    Deliberately **not** `curl -f`: with `-f` curl exits 22 on any 4xx/5xx *and discards the
    response body*, so an authentication failure arrives as a bare exit code when the server had
    in fact replied with a sentence saying what was wrong. The status is requested explicitly via
    `-w` and the body kept, so the caller can report both. `-S` restores curl's own message for
    transport failures (DNS, refused connection) that produce no body at all.

    The headers come from `-D -`, which prepends them to the same stream; a server that answers
    a request with `retry-after` is telling us something we would otherwise have to guess. -/
def curlFull (args : Array String) (connectTimeout : Nat := 10) (maxTime : Nat := 30)
    : IO (Nat × Array (String × String) × String) := do
  let full := #["-sS", "--connect-timeout", toString connectTimeout,
                "--max-time", toString maxTime, "-D", "-",
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
  | some (status, rest) =>
    let (headers, body) := splitHeaders rest
    return (status, headers, body)
  | none => throw (.userError s!"curl wrote no status line; its output was:\n{stdout}")

/-- `curlFull` for the callers that only need the status and the body. -/
def curlWithStatus (args : Array String) (connectTimeout : Nat := 10) (maxTime : Nat := 30)
    : IO (Nat × String) := do
  let (status, _, body) ← curlFull args connectTimeout maxTime
  return (status, body)

/-- A GET with bearer authentication, keeping the response headers. The token never appears in
    an error message. -/
def getBearerFull (url token : String) (extraHeaders : Array String := #[])
    (maxTime : Nat := 15) : IO (Nat × Array (String × String) × String) := do
  let mut args := #["-H", s!"Authorization: Bearer {token}"]
  for h in extraHeaders do
    args := args.push "-H" |>.push h
  curlFull (args.push url) (maxTime := maxTime)

/-- A GET with bearer authentication. The token never appears in an error message. -/
def getBearer (url token : String) (extraHeaders : Array String := #[])
    (maxTime : Nat := 15) : IO (Nat × String) := do
  let (status, _, body) ← getBearerFull url token extraHeaders maxTime
  return (status, body)

end Orchestra.Utils.Http
