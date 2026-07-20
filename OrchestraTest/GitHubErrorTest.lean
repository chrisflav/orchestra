import OrchestraTest.TestM
import Orchestra.GitHub

open Lean (Json)
open Orchestra

namespace OrchestraTest.GitHubError

/-!
# GitHub App error reporting

Every task begins by minting an installation token, so a failure there is the first thing
anyone reads in the log — and it used to read `curl failed (exit 22):` followed by a blank
line. `curl -f` discards the response body on a 4xx and `-s` suppresses curl's own message, so
GitHub's explanation ("Bad credentials", "Integration not found") was fetched and then thrown
away.

The network halves cannot be tested here. These cover the two pure pieces the reporting is
built from: recovering the status curl appends, and pulling GitHub's sentence out of the body.
-/

-- GitHub.splitHttpStatus

private def marker := "\n<<<orchestra-http-status:"

@[test]
def splitHttpStatus_separatesBodyFromStatus : Test := do
  match GitHub.splitHttpStatus ("{\"message\":\"Bad credentials\"}" ++ marker ++ "401") with
  | some (status, body) =>
    TestM.assertEqual status 401 (msg := "status is read back")
    TestM.assertEqual body "{\"message\":\"Bad credentials\"}" (msg := "body is intact")
  | none => TestM.fail "expected a status line"

@[test]
def splitHttpStatus_handlesSuccessAndEmptyBody : Test := do
  match GitHub.splitHttpStatus (marker ++ "204") with
  | some (status, body) =>
    TestM.assertEqual status 204 (msg := "status with no body")
    TestM.assertEqual body "" (msg := "empty body")
  | none => TestM.fail "expected a status line"

@[test]
def splitHttpStatus_noneWhenCurlWroteNothing : Test := do
  -- curl dying before it can write `-w` output (DNS failure, refused connection) leaves only
  -- its own diagnostic, and the caller has to report that verbatim rather than mis-parse it.
  TestM.assertEqual (GitHub.splitHttpStatus "curl: (6) Could not resolve host: api.github.com")
    none (msg := "no marker, no status")
  TestM.assertEqual (GitHub.splitHttpStatus "") none (msg := "no output at all")

@[test]
def splitHttpStatus_bodyContainingTheMarkerSurvives : Test := do
  -- Splitting on the last marker, not the first, so a body that quotes it is not truncated.
  let body := "prefix" ++ marker ++ "not-a-status"
  match GitHub.splitHttpStatus (body ++ marker ++ "500") with
  | some (status, got) =>
    TestM.assertEqual status 500 (msg := "the trailing marker is the status")
    TestM.assertEqual got body (msg := "the embedded one stays in the body")
  | none => TestM.fail "expected a status line"

@[test]
def splitHttpStatus_noneWhenStatusIsNotANumber : Test := do
  TestM.assertEqual (GitHub.splitHttpStatus ("body" ++ marker ++ "nonsense")) none
    (msg := "a non-numeric status is not a status")

-- GitHub.githubErrorDetail

@[test]
def errorDetail_prefersGitHubsOwnMessage : Test := do
  let body := "{\"message\":\"Bad credentials\",\"documentation_url\":\"https://docs.github.com/rest\"}"
  TestM.assertEqual (GitHub.githubErrorDetail body) "Bad credentials"
    (msg := "the sentence a human needs, not the whole envelope")

@[test]
def errorDetail_fallsBackToTheRawBody : Test := do
  -- A proxy or gateway answers with HTML, which has no `message` field. Passing it through is
  -- the point: the failure mode being fixed was discarding evidence, not formatting it badly.
  let html := "<html><body>502 Bad Gateway</body></html>"
  TestM.assertEqual (GitHub.githubErrorDetail html) html (msg := "non-JSON is kept")
  -- Valid JSON without a `message` field likewise.
  TestM.assertEqual (GitHub.githubErrorDetail "{\"error\":\"nope\"}") "{\"error\":\"nope\"}"
    (msg := "JSON without a message field is kept")

@[test]
def errorDetail_saysSoWhenThereIsNoBody : Test := do
  -- Distinguishable from "a body we could not read": a 401 with no body at all is itself a
  -- clue, and an empty string in the log looks like the old bug.
  TestM.assertEqual (GitHub.githubErrorDetail "") "(empty response body)"
    (msg := "empty is reported as empty")
  TestM.assertEqual (GitHub.githubErrorDetail "   \n ") "(empty response body)"
    (msg := "whitespace counts as empty")

end OrchestraTest.GitHubError
