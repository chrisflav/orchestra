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

/-- Failure text for a command that exited non-zero.

    Falls back to stdout when stderr is empty, and says so explicitly when both are: `curl -s`
    writes nothing to stderr, so a message built from stderr alone came out as a bare exit code
    with a blank line under it and no way to tell a rejected credential from an unreachable
    host. Command arguments are deliberately never included — they carry the JWT and the
    installation token. -/
private def commandFailure (cmd : String) (code : UInt32) (stdout stderr : String) : String :=
  let err := stderr.trimAscii.toString
  let out := stdout.trimAscii.toString
  let detail :=
    if !err.isEmpty then (if out.isEmpty then err else s!"{err}\n{out}")
    else if !out.isEmpty then out
    else "(the command produced no output on stdout or stderr)"
  s!"{cmd} failed (exit {code}):\n{detail}"

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
      throw (.userError (commandFailure cmd code stdout stderr))
    return stdout.trimAscii.toString
  let stdout ← child.stdout.readToEnd
  let stderr ← child.stderr.readToEnd
  let code ← child.wait
  if code != 0 then
    throw (.userError (commandFailure cmd code stdout stderr))
  return stdout.trimAscii.toString

private def runCmd' (cmd : String) (args : Array String)
    (input : Option String := none) : IO Unit := do
  let _ ← runCmd cmd args input

/-! ## GitHub App REST calls

These run against `api.github.com` with a JWT rather than through `gh`, because `gh` cannot
authenticate as an App. They are the first thing every task does, so their failures are the
ones most worth reading. -/

/-- Separates the response body from the status code `-w` appends. Long and unlikely enough that
    a body containing it would have to be trying. -/
private def httpStatusMarker : String := "\n<<<orchestra-http-status:"

/-- Split curl's `-w`-augmented output into `(status, body)`, or `none` when the status line is
    absent — which means curl died before writing it and the whole output is a diagnostic.

    Splits on the *last* marker, so a response body that happens to contain the marker is
    returned intact rather than truncated at it. Not private: this is the one part of the error
    path with logic worth testing, and it needs no network to test. -/
def splitHttpStatus (out : String) : Option (Nat × String) :=
  let parts := out.splitOn httpStatusMarker
  if parts.length < 2 then none
  else
    let status := (parts.getLastD "").trimAscii.toString
    match status.toNat? with
    | none   => none
    | some s => some (s, httpStatusMarker.intercalate parts.dropLast)

/-- Run curl and return `(status, body)`.

    Deliberately **not** `curl -f`. With `-f` curl exits 22 on any 4xx/5xx *and discards the
    response body*, and with `-s` it prints nothing to stderr either — so every authentication
    failure arrived as `curl failed (exit 22)` with nothing after the colon, when GitHub had in
    fact replied with a sentence saying exactly what was wrong. The status is requested
    explicitly via `-w` and the body kept, so the caller can report both. `-S` restores curl's
    own message for transport failures (DNS, refused connection) that produce no body at all.

    Bounded in time on purpose. Without `--max-time` a connection that is accepted and then never
    answered hangs forever, and callers that retry — `probeWriteAccessRetrying`, which runs inside
    the listener's dispatch path — would hang rather than reach the "inconclusive" branch they were
    written around. A timeout makes curl exit non-zero, which surfaces as a thrown error, which is
    exactly the inconclusive signal. -/
private def curlWithStatus (args : Array String) : IO (Nat × String) := do
  let out ← runCmd "curl" (#["-sS", "--connect-timeout", "10", "--max-time", "60",
    "-w", httpStatusMarker ++ "%{http_code}"] ++ args)
  match splitHttpStatus out with
  | some r => return r
  | none   => throw (.userError s!"curl wrote no status line; its output was:\n{out}")

/-- GitHub's own explanation of a failure, for putting in a log line.

    Error responses are `{"message": ..., "documentation_url": ...}`, so the `message` field is
    the useful sentence; anything else (an HTML error page from a proxy, say) is passed through
    truncated rather than dropped, since the point of this is to stop discarding evidence.

    Not private: exercised directly by `OrchestraTest.GitHubError`. -/
def githubErrorDetail (body : String) : String :=
  let trimmed := body.trimAscii.toString
  if trimmed.isEmpty then "(empty response body)"
  else match Json.parse trimmed with
    | .ok j =>
      match j.getObjValAs? String "message" with
      | .ok m => m
      | .error _ => (trimmed.take 2000).toString
    | .error _ => (trimmed.take 2000).toString

/-- What usually causes a given status on the App endpoints, when it is worth saying. -/
private def appAuthHint (status : Nat) : String :=
  if status == 401 then
    "\n  A 401 here means GitHub rejected the signed JWT. Check github_app.app_id and \
github_app.private_key_path in config.json, that the key is the App's current private key, \
and that this machine's clock is right — a JWT whose 'iat' is in the future is rejected."
  else if status == 404 then
    -- Observed: app_id 12345 against a well-formed key answers 404 \"Integration not found\",
    -- so a wrong app_id lands here rather than on the 401 path one might expect.
    "\n  A 404 here means GitHub found no App or installation to match. Check that \
github_app.app_id is the App's own id (not the installation id), and that the App is installed \
on the account owning the repository."
  else ""

/-- Call a GitHub App REST endpoint, returning the response body.

    `what` names the operation for the error message. Request headers are never included in it:
    they carry the JWT. Neither is the body of a *successful* response, since for the token
    endpoint that is the credential itself — only error bodies are quoted, and those carry no
    secret. -/
private def githubAppApi (what : String) (jwt : String) (method : String) (url : String) :
    IO String := do
  let (status, body) ← curlWithStatus #[
    "-X", method,
    "-H", s!"Authorization: Bearer {jwt}",
    "-H", "Accept: application/vnd.github+json",
    url ]
  if status < 200 || status >= 300 then
    throw (.userError
      s!"{what} failed: GitHub answered HTTP {status} to {method} {url}\n  \
{githubErrorDetail body}{appAuthHint status}")
  return body

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

/-- GET a GitHub App REST endpoint under the App JWT, returning `(status, body)` without treating
    a non-2xx as fatal — for the endpoints where a 404 is an answer rather than a failure. -/
private def githubAppGet (jwt : String) (url : String) : IO (Nat × String) :=
  curlWithStatus #[
    "-X", "GET",
    "-H", s!"Authorization: Bearer {jwt}",
    "-H", "Accept: application/vnd.github+json",
    url ]

/-- Read the `id` out of one of the `.../installation` endpoints' responses: `none` on a 404 (the
    App is not installed *there*, a definitive answer), the id on a 2xx, and a thrown error on
    anything else — a status or a body that leaves the question unanswered.

    `what` names the lookup for the error message. The body is safe to quote: these endpoints
    return installation metadata, not a credential. -/
private def installationIdFrom (what : String) (status : Nat) (body : String) :
    IO (Option Nat) := do
  if status == 404 then return none
  if status < 200 || status >= 300 then
    throw (.userError s!"{what} failed: GitHub answered HTTP {status}\n  \
      {githubErrorDetail body}{appAuthHint status}")
  match Json.parse body with
  | .error e =>
    throw (.userError s!"{what}: could not parse the response: {e}\n  response was:\n\
      {body.take 2000}")
  | .ok j =>
    match j.getObjValAs? Nat "id" with
    | .ok id    => return some id
    | .error _  => throw (.userError s!"{what}: the installation object carries no 'id'")

/-- Get the installation ID for a GitHub App on a given owner/org, or `none` when the App has no
    installation on that account.

    The `none` case is a *definitive* answer — the App is not installed there — as opposed to the
    thrown errors, which are transport or parse failures (an unreachable GitHub, a malformed
    response) that leave the question unanswered. Callers deciding whether the App can write to a
    repo rely on that distinction: "not installed" means it cannot, where a transport failure
    means "unknown".

    Asks GitHub for *this account's* installation directly rather than scanning
    `GET /app/installations` for it. That listing is paginated at 30, so an App installed on more
    accounts than that answered "not installed" for everything past the first page, which
    `forkRepo` would report as "the App is not installed on that org" for an org it is installed
    on. An account is either an org or a user and GitHub has a separate endpoint for each, so a 404
    from the org one is not yet an answer; only a 404 from both is. -/
def getInstallationId? (jwt : String) (owner : String) : IO (Option Nat) := do
  let (orgStatus, orgBody) ← githubAppGet jwt s!"https://api.github.com/orgs/{owner}/installation"
  match ← installationIdFrom s!"looking up the App's installation on org '{owner}'"
      orgStatus orgBody with
  | some id => return some id
  | none =>
    let (userStatus, userBody) ←
      githubAppGet jwt s!"https://api.github.com/users/{owner}/installation"
    installationIdFrom s!"looking up the App's installation on user '{owner}'" userStatus userBody

/-- Get the installation ID for a GitHub App on a given owner/org. Throws when the App has no
    installation on that account; use `getInstallationId?` when "not installed" is a case to
    handle rather than an error. -/
def getInstallationId (jwt : String) (owner : String) : IO Nat := do
  match ← getInstallationId? jwt owner with
  | some id => return id
  | none    => throw (.userError s!"no installation found for owner '{owner}'")

/-- Create an installation access token. -/
def createInstallationToken (jwt : String) (installationId : Nat) : IO String := do
  let result ← githubAppApi s!"minting an installation token for installation {installationId}"
    jwt "POST" s!"https://api.github.com/app/installations/{installationId}/access_tokens"
  match Json.parse result with
  -- Deliberately without the response body: a 2xx here *is* the token, so quoting what could
  -- not be parsed would put a live credential in the daemon log.
  | .error e => throw (.userError
      s!"failed to parse token response: {e} (body withheld — it may contain the token)")
  | .ok j =>
    let .ok token := j.getObjValAs? String "token"
      | throw (.userError "token response missing 'token' field")
    return token

/-! ## Write-access probing and forking

Project/role-based tasks name a *target* repository their pull requests must land in. The agent
does its work on a `fork` it can push to; when the App can already push to the target, the fork
*is* the target. These functions decide which case applies and, when a fork is needed, create it.
They authenticate as the App, not with the PAT: the question is specifically what the *App* may
do. -/

/-- Decide repo write access from a `GET /repos/{owner}/{repo}/installation` response, as a
    three-way answer: `some true`/`some false` are definitive (the App can / cannot push), `none`
    means the response did not settle it and the caller should retry or treat it as unknown.

    The signal is the installation's own `permissions.contents`, which is what a git push under an
    installation token is checked against: `write` can push, `read` cannot, and a `permissions`
    object carrying no `contents` key at all is GitHub omitting a permission that was never
    granted — also a no. A 404 means no installation of this App reaches the repository (not
    installed on the owner, or installed with a repository selection that excludes it), a
    definitive "cannot push". Anything else (403, 5xx, an unparseable body, a 2xx with no
    `permissions` object) is inconclusive. Pure, so the mapping is tested without a network.

    Deliberately *not* taken from `GET /repos/{owner}/{repo}`'s `permissions` block, which is what
    this used to read. That block reports a *user's* role on the repository — `admin`/`push`/`pull`
    — and under an installation token GitHub returns every field of it `false`, including for
    repositories the installation holds `contents: write` on. Reading `push` out of it therefore
    made every target look unwritable, so every project/role-based task was forked away from its
    target, or skipped outright where no `default_organization` was configured.

    403 is deliberately *not* a definitive no. A repository the App cannot see answers 404, not
    403; a 403 is overwhelmingly a primary or secondary rate limit or an org blocking the App —
    transient or needing a human, and neither a reason to go create a repository. Treating it as
    "cannot push" would turn a rate limit into a real fork and a pull request opened from the wrong
    head. A suspended installation is inconclusive for the same reason: it still reports its
    permissions, but cannot exercise them until a human unsuspends it, and forking is not the
    answer to that either. -/
def installationWriteDecision (status : Nat) (body : String) : Option Bool :=
  if status == 404 then some false
  else if status < 200 || status >= 300 then none
  else match Json.parse body with
    | .error _ => none
    | .ok j =>
      match j.getObjVal? "suspended_at" with
      | .ok (.str _) => none
      | _ =>
        match j.getObjVal? "permissions" with
        | .error _ => none
        | .ok perms =>
          match perms.getObjValAs? String "contents" with
          | .ok "write" => some true
          | _           => some false

/-- Whether the GitHub App can push to `target`. `some true`/`some false` are definitive; `none`
    means the question could not be answered (an unreachable GitHub, say).

    The installation is looked up from `target` itself rather than using `installationId` from the
    config: that field names one installation, but the target may live under a different account,
    and the point here is whether the installation reaching *this repository* grants write. One
    request under the App JWT settles it — no installation token is minted, because the token's
    permissions are exactly what the installation object already reports. -/
def probeWriteAccess (appConfig : AppConfig) (target : Repository) : IO (Option Bool) := do
  try
    let jwt ← createJWT appConfig.appId appConfig.privateKeyPath
    let (status, body) ← githubAppGet jwt
      s!"https://api.github.com/repos/{target.owner}/{target.name}/installation"
    return installationWriteDecision status body
  catch e =>
    IO.eprintln s!"[fork] could not determine App write access to {target}: {e}"
    return none

/-- `probeWriteAccess`, retried while the answer is inconclusive (`none`), up to `attempts` times.
    A definitive answer returns immediately; a persistently unreachable GitHub returns `none` after
    the last attempt. The pause doubles between tries, so the common case of one blip costs a
    second while a rate limit — which `installationWriteDecision` reports as inconclusive — gets a
    little longer to clear. -/
def probeWriteAccessRetrying (appConfig : AppConfig) (target : Repository)
    (attempts : Nat := 3) : IO (Option Bool) := do
  let mut result : Option Bool := none
  let mut pause : UInt32 := 1000
  for i in [0:attempts] do
    result ← probeWriteAccess appConfig target
    if result.isSome then return result
    if i + 1 < attempts then
      IO.sleep pause
      pause := pause * 2
  return result

/-- Fork `target` into organisation `org`, returning the fork GitHub says it created.

    Authenticates as `org`'s own installation — the one with permission to create repositories
    there — so the App must be installed on `org`. Idempotent: GitHub returns the existing fork if
    one is already present. Forking is asynchronous, so this polls until the fork is queryable
    before returning, so a task cloning it next does not race the copy.

    The fork's name is *read back* from the response rather than assumed to be `org/{target.name}`.
    It normally is, and the request asks for it explicitly, but the returned repository is what a
    task will be dispatched at: were GitHub ever to answer with a different name, guessing would
    point the poll — and then the agent — at whatever unrelated repository happens to sit at the
    guessed path. A response that does not identify the fork is an error, which `resolveFork` turns
    into a skip; that is the safe direction to fail in. -/
def forkRepo (appConfig : AppConfig) (target : Repository) (org : String) : IO Repository := do
  let jwt ← createJWT appConfig.appId appConfig.privateKeyPath
  let some instId ← getInstallationId? jwt org
    | throw (.userError s!"cannot fork {target} into '{org}': the GitHub App is not installed on \
        '{org}', so it cannot create repositories there")
  let token ← createInstallationToken jwt instId
  let reqBody := Json.mkObj
    [("organization", Json.str org), ("name", Json.str target.name)] |>.compress
  let (status, respBody) ← curlWithStatus #[
    "-X", "POST",
    "-H", s!"Authorization: Bearer {token}",
    "-H", "Accept: application/vnd.github+json",
    "-d", reqBody,
    s!"https://api.github.com/repos/{target.owner}/{target.name}/forks" ]
  -- 202 Accepted is the normal answer (the fork is created asynchronously); 200 comes back when
  -- the fork already exists. Both are fine; anything else is a failure worth quoting.
  if status < 200 || status >= 300 then
    throw (.userError s!"forking {target} into '{org}' failed: GitHub answered HTTP {status}\n  \
      {githubErrorDetail respBody}")
  let .ok respJson := Json.parse respBody
    | throw (.userError s!"forking {target} into '{org}': GitHub answered HTTP {status} with a \
        body that is not JSON, so the fork cannot be identified")
  let .ok fullName := respJson.getObjValAs? String "full_name"
    | throw (.userError s!"forking {target} into '{org}': the response carries no 'full_name', so \
        the fork cannot be identified\n  {githubErrorDetail respBody}")
  let .ok fork := Repository.parse fullName
    | throw (.userError s!"forking {target} into '{org}': GitHub named the fork {repr fullName}, \
        which is not an 'owner/repo'")
  for _ in [0:10] do
    let (s, _) ← curlWithStatus #[
      "-X", "GET",
      "-H", s!"Authorization: Bearer {token}",
      "-H", "Accept: application/vnd.github+json",
      s!"https://api.github.com/repos/{fork.owner}/{fork.name}" ]
    if s >= 200 && s < 300 then return fork
    IO.sleep 1000
  -- The POST succeeded, so the fork will appear; return it rather than failing on a slow copy.
  return fork

/-- The decision half of `resolveFork`, over an injected `probe` and `mkFork`. Split out so the
    branch table below can be exercised against stubs, without a network:

    - `probe` says the App can push to `target` → `some target` (work on it directly, no fork).
    - cannot push, `defaultOrganization` set → `mkFork` it into that org and return the fork.
    - cannot push, no `defaultOrganization` → `none` (nothing to fork into).
    - `probe` could not tell → `none`.

    Never throws: a `mkFork` that fails is logged and yields `none` like the other skip cases, so
    the caller has a single "cannot dispatch" branch to handle. Every branch logs its reason, all
    to stderr, so `[fork]` is one stream to go looking in. -/
def resolveForkWith (probe : Repository → IO (Option Bool))
    (mkFork : Repository → String → IO Repository)
    (defaultOrganization : Option String) (target : Repository) : IO (Option Repository) := do
  match ← probe target with
  | some true =>
    return some target
  | some false =>
    match defaultOrganization with
    | some org =>
      try
        IO.eprintln s!"[fork] the GitHub App cannot push to {target}; forking into '{org}'"
        let fork ← mkFork target org
        IO.eprintln s!"[fork] using fork {fork} (upstream {target})"
        return some fork
      catch e =>
        IO.eprintln s!"[fork] could not fork {target} into '{org}': {e}; skipping"
        return none
    | none =>
      IO.eprintln s!"[fork] the GitHub App cannot push to {target} and no default_organization \
        is configured to fork into; skipping"
      return none
  | none =>
    IO.eprintln s!"[fork] could not determine whether the GitHub App can push to {target} after \
      retrying; skipping"
    return none

/-- Fork resolutions already reached in this process, as `(target, fork)` pairs.

    Resolving costs a JWT (four subprocesses) plus up to three HTTPS round trips, and
    `Listener.buildRoleEntry` pays it on *every* dispatch — including the ordinary case where the
    App can push to the target and the answer is "itself". Whether the App can push to a given
    repository does not change over a daemon's lifetime in any way worth chasing, so the answer is
    kept.

    Only successes are cached, deliberately: a target that could not be resolved because GitHub was
    briefly unreachable must be retried on the next dispatch rather than being written off for the
    life of the process. Racing writers can duplicate a probe, which costs a redundant round trip
    and an idempotent fork request — cheaper than holding a lock across the network call. -/
initialize forkResolutions : IO.Ref (Array (Repository × Repository)) ← IO.mkRef #[]

/-- Resolve which repository a project/role-based task should push to, given `target` as its
    upstream / pull-request destination. Returns `none` when the task cannot be dispatched; see
    `resolveForkWith` for the branch table, and `forkResolutions` for what is remembered. -/
def resolveFork (appConfig : AppConfig) (target : Repository) : IO (Option Repository) := do
  if let some (_, fork) := (← forkResolutions.get).find? (·.1 == target) then
    return some fork
  let resolved ← resolveForkWith (probeWriteAccessRetrying appConfig ·) (forkRepo appConfig)
    appConfig.defaultOrganization target
  if let some fork := resolved then
    forkResolutions.modify (·.push (target, fork))
  return resolved

/-- Configure `gh` CLI to use the given token, by writing it to `~/.config/gh/hosts.yml`.

    **Process-global, and shared with every other process run by this user.** Only safe in a
    one-shot CLI command that runs a single task. The queue daemon must not call it: it runs
    tasks concurrently, each with its own installation token, and whichever task authenticated
    last would supply the credentials for all of them. Pass the token explicitly instead —
    `Repo`'s helpers take one, `GitHub`'s take a `pat`/`token` argument, and `Sandbox` injects
    `GH_TOKEN` into the agent's environment.

    Rejects an empty token rather than passing it on: `gh auth login --with-token` given no token
    does not fail, it falls back to an interactive device-code prompt and blocks forever — which
    in the daemon is an unattended hang with nothing in the log to explain it.

    The token goes over stdin rather than being interpolated into `sh -c "echo ... | gh ..."`, so
    there is no shell to quote it for, and a failure is reported as `gh failed` rather than the
    considerably less helpful `sh failed`. -/
def setupGhAuth (token : String) : IO Unit := do
  if token.isEmpty then
    throw (.userError
      "refusing to run `gh auth login` with an empty token (it would block on an interactive \
       prompt). Check github_app.app_id and github_app.private_key_path in config.json.")
  runCmd' "gh" #["auth", "login", "--with-token"] (input := token)

/-- Whether a pull request has been merged.

    Used by the dispatcher to decide whether an issue still needs review: an attached PR that is
    already merged does not. Returns `false` when the state cannot be determined — an unreachable
    GitHub or a deleted PR then queues a reviewer that finds nothing to do, which is recoverable,
    where returning `true` would silently drop review of real work. -/
def isPrMerged (token : String) (repo : Repository) (number : Nat) : IO Bool := do
  let env := if token.isEmpty then #[] else #[("GH_TOKEN", some token)]
  try
    -- `state`, not `merged`: gh has no `merged` field, and asking for one makes the whole call
    -- fail. `state` is OPEN / CLOSED / MERGED, so it also distinguishes a PR closed without
    -- merging — which still counts as unmerged here, since the issue's work did not land.
    let out ← runCmd "gh"
      #["pr", "view", toString number, "--repo", repo.toString, "--json", "state", "-q", ".state"]
      (env := env)
    return out.trimAscii.toString == "MERGED"
  catch _ =>
    return false

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

/-- Add labels to a GitHub issue or pull request. -/
def addIssueLabels (pat : String) (repo : Repository) (issueNumber : Nat) (labels : List String) : IO Unit := do
  unless labels.isEmpty do
    let env := if pat.isEmpty then #[] else #[("GH_TOKEN", some pat)]
    let payload := Json.mkObj [("labels", Json.arr (labels.map Json.str |>.toArray))]
    let _ ← runCmd "gh" #[
      "api", "--method", "POST",
      s!"/repos/{repo.owner}/{repo.name}/issues/{issueNumber}/labels",
      "--input", "-"
    ] (input := payload.compress) (env := env)

/-- Remove a label from a GitHub issue or pull request. -/
def removeIssueLabel (pat : String) (repo : Repository) (issueNumber : Nat) (label : String) : IO Unit := do
  let env := if pat.isEmpty then #[] else #[("GH_TOKEN", some pat)]
  let _ ← runCmd "gh" #[
    "api", "--method", "DELETE",
    s!"/repos/{repo.owner}/{repo.name}/issues/{issueNumber}/labels/{label}"
  ] (env := env)

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
