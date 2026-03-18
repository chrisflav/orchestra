import Lean.Data.Json

open Lean (Json FromJson ToJson)

namespace Orchestra.GitHub

private def runCmd (cmd : String) (args : Array String)
    (input : Option String := none)
    (env : Array (String × Option String) := #[]) : IO String := do
  let child ← IO.Process.spawn {
    cmd, args, env
    stdin := if input.isSome then .piped else .null
    stdout := .piped
    stderr := .piped
  }
  if let some s := input then
    child.stdin.putStr s
    child.stdin.flush
  let stdout ← child.stdout.readToEnd
  let stderr ← child.stderr.readToEnd
  let code ← child.wait
  if code != 0 then
    throw (.userError s!"{cmd} failed (exit {code}):\n{stderr}")
  return stdout.trimAscii.toString

private def runCmd' (cmd : String) (args : Array String)
    (input : Option String := none) : IO Unit := do
  let _ ← runCmd cmd args input

/-- Create a JWT for GitHub App authentication using `openssl`. -/
def createJWT (appId : Nat) (privateKeyPath : String) : IO String := do
  let now ← runCmd "date" #["+%s"]
  let iat := now.toNat!
  let exp := iat + 600
  let header := "{\"alg\":\"RS256\",\"typ\":\"JWT\"}"
  let payload := s!"\{\"iss\":{appId},\"iat\":{iat},\"exp\":{exp}}"
  -- Base64url encode header and payload
  let b64Header ← runCmd "sh" #["-c",
    s!"echo -n '{header}' | openssl base64 -e -A | tr '+/' '-_' | tr -d '='"]
  let b64Payload ← runCmd "sh" #["-c",
    s!"echo -n '{payload}' | openssl base64 -e -A | tr '+/' '-_' | tr -d '='"]
  let unsigned := s!"{b64Header}.{b64Payload}"
  -- Sign with RS256
  let signature ← runCmd "sh" #["-c",
    s!"echo -n '{unsigned}' | openssl dgst -sha256 -sign {privateKeyPath} | openssl base64 -e -A | tr '+/' '-_' | tr -d '='"]
  return s!"{unsigned}.{signature}"

/-- Get the installation ID for a GitHub App on a given owner/org. -/
def getInstallationId (jwt : String) (owner : String) : IO Nat := do
  let result ← runCmd "curl" #[
    "-s", "-f",
    "-H", s!"Authorization: Bearer {jwt}",
    "-H", "Accept: application/vnd.github+json",
    s!"https://api.github.com/app/installations"
  ]
  match Json.parse result with
  | .error e => throw (.userError s!"failed to parse installations response: {e}")
  | .ok j =>
    let .ok installations := j.getArr? | throw (.userError "expected array of installations")
    for inst in installations do
      let .ok account := inst.getObjVal? "account" | continue
      let .ok login := account.getObjValAs? String "login" | continue
      if login == owner then
        let .ok id := inst.getObjValAs? Nat "id" | throw (.userError "installation missing id")
        return id
    throw (.userError s!"no installation found for owner '{owner}'")

/-- Create an installation access token. -/
def createInstallationToken (jwt : String) (installationId : Nat) : IO String := do
  let result ← runCmd "curl" #[
    "-s", "-f", "-X", "POST",
    "-H", s!"Authorization: Bearer {jwt}",
    "-H", "Accept: application/vnd.github+json",
    s!"https://api.github.com/app/installations/{installationId}/access_tokens"
  ]
  match Json.parse result with
  | .error e => throw (.userError s!"failed to parse token response: {e}")
  | .ok j =>
    let .ok token := j.getObjValAs? String "token"
      | throw (.userError "token response missing 'token' field")
    return token

/-- Configure `gh` CLI to use the given token. -/
def setupGhAuth (token : String) : IO Unit := do
  runCmd' "sh" #["-c", s!"echo '{token}' | gh auth login --with-token"]

/-- Create a pull request on the upstream repo using a PAT. -/
def createPullRequest (pat : String) (upstream : String)
    (head base title body : String) : IO String := do
  runCmd "gh" #[
    "pr", "create",
    "--repo", upstream,
    "--head", head,
    "--base", base,
    "--title", title,
    "--body", body
  ] (env := #[("GH_TOKEN", some pat)])

/--
Fetch review threads for a pull request via the GitHub GraphQL API.
Returns the raw parsed JSON response.
If `pat` is non-empty it is used as the GH_TOKEN; otherwise the token already
configured by `setupGhAuth` is used.
-/
def getPrReviewThreads (upstream : String) (prNumber : Nat) (pat : String) : IO Json := do
  let parts := upstream.splitOn "/"
  let owner := parts[0]?.getD ""
  let repo  := parts[1]?.getD ""
  let query :=
    "query($owner:String!,$repo:String!,$number:Int!){" ++
    "repository(owner:$owner,name:$repo){" ++
    "pullRequest(number:$number){" ++
    "reviewThreads(first:100){nodes{isResolved isOutdated " ++
    "comments(first:100){nodes{" ++
    "body path line author{login}}}}}}}}"
  let env := if pat.isEmpty then #[] else #[("GH_TOKEN", some pat)]
  let result ← runCmd "gh" #[
    "api", "graphql",
    "-f", s!"query={query}",
    "-f", s!"owner={owner}",
    "-f", s!"repo={repo}",
    "-F", s!"number={prNumber}"
  ] (env := env)
  match Json.parse result with
  | .error e => throw (.userError s!"failed to parse GraphQL response: {e}")
  | .ok j => return j

end Orchestra.GitHub
