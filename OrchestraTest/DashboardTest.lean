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

/-! ## Request targets

Parsing a request is `Std.Http`'s job, not this module's, so there is nothing here about
malformed request lines or truncated bodies — the library rejects those before a handler runs.

What *is* covered is the one property of that parse the guards above are built on: the
request-target arrives verbatim. `Std.Http` does not decode percent-escapes and does not
normalise dot-segments, which is what keeps `%2e%2e` an ordinary filename rather than a second
spelling of `..`. That is a property of the toolchain rather than of this code, and if a future
version starts normalising, `staticCandidate` would silently be handed traversals it was never
designed to see. Hence: pinned. -/

private def parsedPath (raw : String) : String :=
  pathOf (Std.Http.RequestTarget.parse! raw)

@[test]
def pathOf_leavesTheRequestTargetVerbatim : Test := do
  TestM.assertEqual (parsedPath "/api/overview") "/api/overview"
  TestM.assertEqual (parsedPath "/api/tasks/abc123?x=1") "/api/tasks/abc123"
    (msg := "the query string is not part of the path")
  -- The two that matter. Either one normalised is a traversal `staticCandidate` cannot see.
  TestM.assertEqual (parsedPath "/%2e%2e/etc/passwd") "/%2e%2e/etc/passwd"
    (msg := "percent-escapes are not decoded")
  TestM.assertEqual (parsedPath "/assets/../../etc/passwd") "/assets/../../etc/passwd"
    (msg := "dot-segments are not normalised away")
  TestM.assertEqual (parsedPath "/tasks/with%20space") "/tasks/with%20space"
    (msg := "an escaped space survives for safeSegment to decode")

/-! ## The published contract

`docs/openapi.json` is embedded in the binary and served, so a client's description of the API
and the API itself ship together. That is only worth anything if the two agree, and nothing
about writing a spec by hand makes them agree — so the agreement is checked here rather than
assumed. A route added without a spec entry, or a spec entry for a route that was removed,
fails the suite. -/

private def specJson : Except String Lean.Json := Lean.Json.parse openApiSpec

private def specPaths : Except String (Array String) := do
  let paths ← (← specJson).getObjVal? "paths"
  let obj ← paths.getObj?
  return obj.toArray.map (·.1)

@[test]
def openApiSpec_isValidAndDescribesEveryRoute : Test := do
  match specPaths with
  | .error e => TestM.fail s!"docs/openapi.json is not parseable JSON: {e}"
  | .ok paths =>
    TestM.assert (paths.size > 0) (msg := "the spec documents at least one path")
    -- Every kind the dispatcher serves is documented …
    for kind in apiKinds do
      let expected := s!"/api/{apiVersion}/{kind}"
      TestM.assert (paths.contains expected)
        (msg := s!"{expected} is served but missing from docs/openapi.json")
    -- … and nothing under the version prefix is documented that is not served.
    let prefix_ := s!"/api/{apiVersion}/"
    for path in paths do
      if path.startsWith prefix_ then
        let kind := (path.drop prefix_.length).toString
        TestM.assert (apiKinds.contains kind)
          (msg := s!"docs/openapi.json documents {path}, which no route serves")

@[test]
def openApiSpec_documentsTheAuthenticationSurface : Test := do
  match specPaths with
  | .error _ => TestM.fail "docs/openapi.json is not parseable JSON"
  | .ok paths =>
    -- These three sit outside the version prefix, so the sweep above cannot see them; they
    -- are also the only routes a client can reach without a credential.
    for path in ["/api/login", "/api/logout", "/api/session", "/api/openapi.json"] do
      TestM.assert (paths.contains path) (msg := s!"{path} is served but not documented")

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
