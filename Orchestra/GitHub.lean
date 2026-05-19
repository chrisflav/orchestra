import Lean.Data.Json
import Orchestra.Config

open Lean (Json FromJson ToJson)

namespace Orchestra.GitHub

/-- An inline review comment to include in a PR review. -/
structure InlineComment where
  path : String
  line : Nat
  body : String
  side : String

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
    -- takeStdin extracts stdin so it can be dropped (closed/EOF) while child is still alive
    let (stdinHandle, child') ← child.takeStdin
    stdinHandle.putStr s
    stdinHandle.flush
    -- stdinHandle drops here, signaling EOF to the child process
    let stdout ← child'.stdout.readToEnd
    let stderr ← child'.stderr.readToEnd
    let code ← child'.wait
    if code != 0 then
      throw (.userError s!"{cmd} failed (exit {code}):\n{stderr}")
    return stdout.trimAscii.toString
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

/-- Ensure the given labels exist on a repository.
    Labels that are missing are created with a default colour; existing ones are not modified. -/
def ensureLabels (token : String) (repo : Repository) (labels : List String) : IO Unit := do
  let env := if token.isEmpty then #[] else #[("GH_TOKEN", some token)]
  for label in labels do
    try
      let _ ← runCmd "gh" #["label", "create", label, "--repo", repo.toString, "--color", "0075ca"]
        (env := env)
    catch _ => pure ()

/-- Create a pull request on the upstream repo using a PAT. -/
def createPullRequest (pat : String) (upstream : Repository)
    (head base title body : String) (labels : List String := []) : IO String := do
  unless labels.isEmpty do
    ensureLabels pat upstream labels
  let labelArgs : Array String := (labels.flatMap fun l => ["--label", l]).toArray
  runCmd "gh" (#[
    "pr", "create",
    "--repo", upstream.toString,
    "--head", head,
    "--base", base,
    "--title", title,
    "--body", body
  ] ++ labelArgs) (env := #[("GH_TOKEN", some pat)])

/-- Create a pull request entirely within a single repository (no cross-repo
    `owner:branch` head prefix), authenticated by `token` — typically a fresh
    GitHub App installation token. Used when the agent wants to open a PR on
    its fork rather than the upstream. -/
def createPullRequestOnRepo (token : String) (repo : Repository)
    (head base title body : String) (labels : List String := []) : IO String := do
  unless labels.isEmpty do
    ensureLabels token repo labels
  let labelArgs : Array String := (labels.flatMap fun l => ["--label", l]).toArray
  runCmd "gh" (#[
    "pr", "create",
    "--repo", repo.toString,
    "--head", head,
    "--base", base,
    "--title", title,
    "--body", body
  ] ++ labelArgs) (env := #[("GH_TOKEN", some token)])

/--
Fetch review threads for a pull request via the GitHub GraphQL API.
Returns the raw parsed JSON response.
If `pat` is non-empty it is used as the GH_TOKEN; otherwise the token already
configured by `setupGhAuth` is used.
-/
def getPrReviewThreads (upstream : Repository) (prNumber : Nat) (pat : String) : IO Json := do
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
    "-f", s!"owner={upstream.owner}",
    "-f", s!"repo={upstream.name}",
    "-F", s!"number={prNumber}"
  ] (env := env)
  match Json.parse result with
  | .error e => throw (.userError s!"failed to parse GraphQL response: {e}")
  | .ok j => return j

/-- Post a comment on an issue or pull request. -/
def createIssueComment (pat : String) (upstream : Repository) (issueNumber : Nat) (body : String) : IO String := do
  let env := if pat.isEmpty then #[] else #[("GH_TOKEN", some pat)]
  let payload := Json.mkObj [("body", body)]
  runCmd "gh" #[
    "api", "--method", "POST",
    s!"/repos/{upstream.owner}/{upstream.name}/issues/{issueNumber}/comments",
    "--input", "-"
  ] (input := payload.compress) (env := env)

/-- Reply to an inline PR review comment. -/
def replyToPrReviewComment (pat : String) (upstream : Repository) (prNumber : Nat) (commentId : Nat) (body : String) : IO String := do
  let env := if pat.isEmpty then #[] else #[("GH_TOKEN", some pat)]
  let payload := Json.mkObj [("body", body)]
  runCmd "gh" #[
    "api", "--method", "POST",
    s!"/repos/{upstream.owner}/{upstream.name}/pulls/{prNumber}/comments/{commentId}/replies",
    "--input", "-"
  ] (input := payload.compress) (env := env)

/-- Get the latest commit SHA of a pull request. -/
private def getPrLatestCommit (pat : String) (upstream : Repository) (prNumber : Nat) : IO String := do
  let env := if pat.isEmpty then #[] else #[("GH_TOKEN", some pat)]
  runCmd "gh" #[
    "api",
    s!"/repos/{upstream.owner}/{upstream.name}/pulls/{prNumber}",
    "--jq", ".head.sha"
  ] (env := env)

/-- Create a new inline PR review comment on a specific file and line. -/
def createPrReviewComment (pat : String) (upstream : Repository) (prNumber : Nat)
    (body path : String) (line : Nat) (side : String) : IO String := do
  let env := if pat.isEmpty then #[] else #[("GH_TOKEN", some pat)]
  let commitId ← getPrLatestCommit pat upstream prNumber
  let payload := Json.mkObj [
    ("body", body),
    ("path", path),
    ("side", side),
    ("commit_id", commitId),
    ("line", Json.num ⟨(line : Int), 0⟩)
  ]
  runCmd "gh" #[
    "api", "--method", "POST",
    s!"/repos/{upstream.owner}/{upstream.name}/pulls/{prNumber}/comments",
    "--input", "-"
  ] (input := payload.compress) (env := env)

/-- Post a review (with optional inline comments) on a pull request. -/
def createPrReview (pat : String) (upstream : Repository) (prNumber : Nat)
    (body : String) (comments : Array InlineComment := #[]) : IO String := do
  let env := if pat.isEmpty then #[] else #[("GH_TOKEN", some pat)]
  let commitId ← if comments.isEmpty then pure ""
                 else getPrLatestCommit pat upstream prNumber
  let commentsJson := comments.map fun c =>
    Json.mkObj [
      ("path", c.path),
      ("line", Json.num ⟨(c.line : Int), 0⟩),
      ("body", c.body),
      ("side", c.side)
    ]
  let baseFields : List (String × Json) :=
    [("body", body), ("event", "COMMENT"), ("comments", Json.arr commentsJson)]
  let payload := Json.mkObj (
    baseFields ++ if commitId.isEmpty then [] else [("commit_id", Json.str commitId)])
  runCmd "gh" #[
    "api", "--method", "POST",
    s!"/repos/{upstream.owner}/{upstream.name}/pulls/{prNumber}/reviews",
    "--input", "-"
  ] (input := payload.compress) (env := env)

/-- Format a raw `getPrReviewThreads` GraphQL response as human-readable text.
    Applies `unresolvedOnly` and `excludeOutdated` filters. -/
def formatPrReviewThreads (response : Json) (unresolvedOnly excludeOutdated : Bool) : String :=
  Id.run do
    let threads :=
      response.getObjVal? "data"             |>.toOption
      |>.bind (·.getObjVal? "repository"    |>.toOption)
      |>.bind (·.getObjVal? "pullRequest"   |>.toOption)
      |>.bind (·.getObjVal? "reviewThreads" |>.toOption)
      |>.bind (·.getObjVal? "nodes"         |>.toOption)
      |>.bind (·.getArr?                    |>.toOption)
      |>.getD #[]
    let mut lines : Array String := #[]
    let mut shown := 0
    let mut filtered := 0
    for thread in threads do
      let isResolved := thread.getObjValAs? Bool "isResolved" |>.toOption |>.getD false
      let isOutdated := thread.getObjValAs? Bool "isOutdated" |>.toOption |>.getD false
      if unresolvedOnly && isResolved then filtered := filtered + 1; continue
      if excludeOutdated && isOutdated then filtered := filtered + 1; continue
      shown := shown + 1
      let mut tags : Array String := #[]
      if !isResolved then tags := tags.push "unresolved"
      if isResolved  then tags := tags.push "resolved"
      if isOutdated  then tags := tags.push "outdated"
      let tagStr :=
        if tags.isEmpty then ""
        else s!" ({String.join (tags.toList.intersperse ", ")})"
      lines := lines.push s!"--- Thread {shown}{tagStr}"
      let comments :=
        thread.getObjVal? "comments" |>.toOption
        |>.bind (·.getObjVal? "nodes" |>.toOption)
        |>.bind (·.getArr?            |>.toOption)
        |>.getD #[]
      for comment in comments do
        let path   := comment.getObjValAs? String "path" |>.toOption |>.getD ""
        let body   := comment.getObjValAs? String "body" |>.toOption |>.getD ""
        let author := comment.getObjVal? "author" |>.toOption
          |>.bind (·.getObjValAs? String "login" |>.toOption)
          |>.getD "unknown"
        let lineStr := match comment.getObjValAs? Nat "line" |>.toOption with
          | some l => s!":{l}"
          | none   => ""
        lines := lines.push s!"  @{author} — {path}{lineStr}"
        for bodyLine in body.splitOn "\n" do
          lines := lines.push s!"    {bodyLine}"
      lines := lines.push ""
    let summary :=
      if filtered == 0 then s!"({shown} thread(s))"
      else s!"({shown} thread(s) shown, {filtered} filtered)"
    lines := lines.push summary
    return String.join (lines.toList.intersperse "\n")

/-- Return the pull-request number that a review comment belongs to. -/
def getPrReviewCommentPrNumber (pat : String) (upstream : Repository) (commentId : Nat) : IO Nat := do
  let env := if pat.isEmpty then #[] else #[("GH_TOKEN", some pat)]
  let url ← runCmd "gh" #[
    "api",
    s!"/repos/{upstream.owner}/{upstream.name}/pulls/comments/{commentId}",
    "--jq", ".pull_request_url"
  ] (env := env)
  let trimmed : String := url.trimAscii.toString
  match trimmed.splitOn "/" |>.getLast? with
  | some s => match s.toNat? with
    | some n => return n
    | none => throw (.userError s!"unexpected pull_request_url: {trimmed}")
  | none => throw (.userError s!"unexpected pull_request_url: {trimmed}")

end Orchestra.GitHub
