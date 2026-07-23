import OrchestraTest.TestM
import Orchestra

open Orchestra
open Orchestra.Dashboard

namespace OrchestraTest.Dashboard

/-!
# Dashboard request handling

Everything covered here is a pure decision the server makes about a string that arrived over
a socket: where a path is allowed to land, whether a credential matches, and which endpoint a
URL names. They are the parts that must not quietly regress, and the only parts that can be
tested without a running server.
-/

/-! ## Static file serving -/

private def root : System.FilePath := "/srv/site"

@[test]
def staticCandidate_resolvesTheBuiltLayout : Test := do
  TestM.assertEqual (staticCandidate root "/") (some root)
    (msg := "the root path is the directory itself (the caller then looks for index.html)")
  TestM.assertEqual (staticCandidate root "/assets/index-abc123.js")
    (some (root / "assets" / "index-abc123.js"))
  TestM.assertEqual (staticCandidate root "/index.html") (some (root / "index.html"))

@[test]
def staticCandidate_rejectsTraversal : Test := do
  -- Rejected, not normalised: `..` never gets the chance to cancel a segment, so no
  -- arrangement of them lands outside the site directory.
  TestM.assertEqual (staticCandidate root "/../etc/passwd") none (msg := "leading ..")
  TestM.assertEqual (staticCandidate root "/assets/../../etc/passwd") none
    (msg := ".. after a legitimate segment")
  TestM.assertEqual (staticCandidate root "/./index.html") none (msg := "single dot")
  TestM.assertEqual (staticCandidate root "/a\\..\\b") none
    (msg := "a backslash, in case the separator is ever read as one")

@[test]
def staticCandidate_doesNotDecodeEscapes : Test := do
  -- Percent-decoding is what would turn `%2e%2e` into a second spelling of `..`; not decoding
  -- leaves it an ordinary filename, which simply does not exist in the built site.
  TestM.assertEqual (staticCandidate root "/%2e%2e/etc/passwd")
    (some (root / "%2e%2e" / "etc" / "passwd"))
    (msg := "an undecoded escape stays a (nonexistent) name, never a traversal")

@[test]
def wantsAppShell_distinguishesRoutesFromAssets : Test := do
  -- Client-side routes have no file behind them, so reloading one must return the app shell.
  TestM.assert (wantsAppShell "/") (msg := "the root is the app shell")
  TestM.assert (wantsAppShell "/tasks") (msg := "a list route")
  TestM.assert (wantsAppShell "/tasks/abc123") (msg := "a detail route")
  TestM.assert (wantsAppShell "/projects/42") (msg := "a numeric detail route")
  -- A missing asset must 404 rather than return HTML the browser then fails to parse.
  TestM.assert (!wantsAppShell "/assets/index-abc123.js") (msg := "a missing script")
  TestM.assert (!wantsAppShell "/assets/index-abc123.css") (msg := "a missing stylesheet")
  TestM.assert (!wantsAppShell "/favicon.ico") (msg := "a missing icon")

/-! ## Credential comparison -/

@[test]
def constantTimeEq_matchesOrdinaryEquality : Test := do
  TestM.assert (constantTimeEq "s3cret" "s3cret") (msg := "identical strings match")
  TestM.assert (!constantTimeEq "s3cret" "s3creT") (msg := "a differing last byte fails")
  TestM.assert (!constantTimeEq "s3cret" "S3cret") (msg := "a differing first byte fails")
  TestM.assert (!constantTimeEq "s3cret" "s3cret ") (msg := "a longer candidate fails")
  TestM.assert (!constantTimeEq "s3cret" "s3cre") (msg := "a shorter candidate fails")
  TestM.assert (!constantTimeEq "s3cret" "") (msg := "the empty string never matches")
  TestM.assert (constantTimeEq "" "") (msg := "empty matches empty")
  -- Multi-byte content is compared per byte, not per character; both must still agree.
  TestM.assert (constantTimeEq "pässwörd" "pässwörd") (msg := "identical non-ASCII matches")
  TestM.assert (!constantTimeEq "pässwörd" "pässword") (msg := "differing non-ASCII fails")

/-! ## URL decoding and path components -/

@[test]
def percentDecode_handlesEscapesAndRejectsGarbage : Test := do
  TestM.assertEqual (percentDecode "plain") (some "plain")
  TestM.assertEqual (percentDecode "with%20space") (some "with space")
  TestM.assertEqual (percentDecode "%2e%2e") (some "..")
  TestM.assertEqual (percentDecode "%2F") (some "/") (msg := "uppercase hex decodes too")
  -- Multi-byte characters arrive as a run of escapes and must reassemble.
  TestM.assertEqual (percentDecode "caf%C3%A9") (some "café")
  TestM.assertEqual (percentDecode "%zz") none (msg := "non-hex digits are a rejection")
  TestM.assertEqual (percentDecode "%2") none (msg := "a truncated escape is a rejection")
  TestM.assertEqual (percentDecode "%FF") none
    (msg := "an escape that is not valid UTF-8 is a rejection, not a mangled string")

@[test]
def safeSegment_acceptsIdsAndRejectsPaths : Test := do
  TestM.assertEqual (safeSegment "abc123") (some "abc123")
  TestM.assertEqual (safeSegment "my-listener") (some "my-listener")
  TestM.assertEqual (safeSegment "with%20space") (some "with space")
    (msg := "the client percent-encodes, so a name with a space must survive the round trip")
  -- Every detail endpoint turns its component into a filename, so a component that can name
  -- a directory or an ancestor must never get that far.
  TestM.assertEqual (safeSegment "..") none (msg := "the parent directory")
  TestM.assertEqual (safeSegment ".") none (msg := "the current directory")
  TestM.assertEqual (safeSegment "%2e%2e") none
    (msg := "the check runs after decoding, so an escaped .. is caught too")
  TestM.assertEqual (safeSegment "..%2F..%2Fetc%2Fpasswd") none (msg := "an escaped traversal")
  TestM.assertEqual (safeSegment "a/b") none (msg := "a raw separator")
  TestM.assertEqual (safeSegment "a%2Fb") none (msg := "an escaped separator")
  TestM.assertEqual (safeSegment "a\\b") none (msg := "a backslash separator")
  TestM.assertEqual (safeSegment "") none (msg := "the empty component")
  TestM.assertEqual (safeSegment "a%00b") none (msg := "an embedded NUL")
  TestM.assertEqual (safeSegment "a%0Ab") none (msg := "an embedded newline")

@[test]
def apiKind_stripsOnlyItsOwnPrefix : Test := do
  TestM.assertEqual (apiKind "/api/" "/api/overview") (some "overview")
  TestM.assertEqual (apiKind "/api/" "/api/tasks/abc123") (some "tasks/abc123")
  TestM.assertEqual (apiKind "/sse/" "/sse/queue") (some "queue")
  TestM.assertEqual (apiKind "/api/" "/sse/queue") none
    (msg := "an SSE path is not an API path")
  TestM.assertEqual (apiKind "/api/" "/index.html") none

/-! ## Request parsing -/

private def crlf (lines : List String) : String := String.intercalate "\r\n" lines

@[test]
def parseRequest_readsTheRequestLineAndHeaders : Test := do
  let raw := crlf ["GET /api/overview HTTP/1.1", "Host: localhost:8080",
    "Cookie: orchestra_session=abc", "", ""]
  match parseRequest raw with
  | none => TestM.fail "a well-formed GET should parse"
  | some req =>
    TestM.assertEqual req.method "GET"
    TestM.assertEqual req.path "/api/overview"
    TestM.assertEqual (headerValue req "host") (some "localhost:8080")
    TestM.assertEqual (headerValue req "COOKIE") (some "orchestra_session=abc")
      (msg := "header lookup is case-insensitive")
    TestM.assertEqual (headerValue req "authorization") none

@[test]
def parseRequest_keepsTheBody : Test := do
  let body := "{\"password\":\"hunter2\"}"
  let raw := crlf ["POST /api/login HTTP/1.1", "Content-Type: application/json",
    s!"Content-Length: {body.length}", "", body]
  match parseRequest raw with
  | none => TestM.fail "a well-formed POST should parse"
  | some req =>
    TestM.assertEqual req.method "POST"
    TestM.assertEqual req.body body
      (msg := "the login handler parses this as JSON, so it must survive intact")

@[test]
def parseRequest_rejectsGarbage : Test := do
  TestM.assertEqual ((parseRequest "").map (·.method)) none
    (msg := "an empty request line has no method")
  TestM.assertEqual ((parseRequest "NONSENSE\r\n\r\n").map (·.method)) none
    (msg := "a request line without a path is not a request")

/-! ## Cookies -/

@[test]
def cookieValue_findsTheSessionAmongOthers : Test := do
  TestM.assertEqual (cookieValue "orchestra_session=abc123" sessionCookieName) (some "abc123")
  TestM.assertEqual (cookieValue "theme=dark; orchestra_session=abc123; tz=UTC" sessionCookieName)
    (some "abc123")
    (msg := "the dashboard's cookie must be found next to unrelated ones")
  TestM.assertEqual (cookieValue "theme=dark" sessionCookieName) none
  TestM.assertEqual (cookieValue "" sessionCookieName) none
  TestM.assertEqual (cookieValue "orchestra_session=" sessionCookieName) (some "")
    (msg := "a cleared cookie reads as empty, which no session id ever equals")

end OrchestraTest.Dashboard
