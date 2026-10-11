import Lean.Data.Json
import Orchestra.Config
import Orchestra.Dirs
import Orchestra.Exec.Spec
import Orchestra.Exec.Backend
import Orchestra.Utils.Http

/-!
# Reaching the world through kleis

Each task resolves a **source** (`choose`): a kleis spec named by the task, its identity, its role
or listener action or `kleis.default_spec`, or a token handed to the task or its identity. From it
orchestra *derives* a task token (`/.kleis/v1/derive`), asks kleis what a sandbox needs to use it
(`/.kleis/v1/setup`: environment, files in the home, the CA, and a **card** saying in prose what the
token allows), starts the sandbox with all of that, puts the card in the system prompt, and revokes
the token when the task ends. Orchestra does not know which services the token reaches; kleis does.

A task with no source, under a `kleis` block without `default_spec`, keeps the older path described
below: a token minted through `/tokens` naming `kleis.grants`, with orchestra's own GitHub prompt.
Both paths are in this module; `docs/kleis.md` describes them.

## The legacy path: GitHub through kleis

With `kleis` configured, no sandbox holds a GitHub credential. kleis — a credential proxy — holds
the GitHub App's key and the PAT, and each task is given a token of its own that says, as facts,
which task it is: its fork, its upstream, the issue it was launched from, the tools it was
granted. The grants configured in kleis are written over those facts, so one set of grants serves
every task and a task's token can do exactly what that task may do and nothing else.

The agent then uses the real `git` and `gh` against the real URLs. Every program in the sandbox
is pointed at the proxy with `HTTPS_PROXY`, and trusts kleis's CA, which is what lets the proxy
read a request before it attaches a credential to it. The token rides in the proxy URL; it is the
agent's to read, and it is worth nothing outside this task — it names this task's repositories,
it expires, and orchestra revokes it the moment the task ends.

What this module does is the orchestra half of that: mint and revoke the token through kleis's
issuer endpoint, assemble the CA bundle, and work out the environment and files a sandbox needs.
The policy itself — what a task may push, comment on, merge — lives in kleis's grants, not here;
`docs/kleis.md` and kleis's `examples/orchestra/` have them.
-/

open Lean (Json)

namespace Orchestra.Kleis

/-- What a task is, as far as its token is concerned. -/
structure TaskFacts where
  /-- The orchestra task id, so kleis's audit log can be read against orchestra's. -/
  taskId : String
  /-- The repositories the task works on, if any. -/
  repo : Option RepoPair := none
  /-- The GitHub issue or pull request the task was launched from. -/
  issueNumber : Option Nat := none
  /-- The tools the task was granted, which the grants read as `task_tool(name)`. -/
  tools : List String := []
  /-- A read-only task may fetch but not push. -/
  readOnly : Bool := false
  /-- Labels the task's pull requests are to carry. -/
  prLabels : List String := []
  /-- The identity the task runs under, for the audit log. -/
  identity : Option String := none
  /-- Where the task may push, when the operator limited it. -/
  pushPrefix : Option String := none
  /-- The organisation the task may create repositories in — `default_organization`, the
      one tasks are forked into. -/
  org : Option String := none

/-- One fact as kleis's issuer endpoint takes it. A list term becomes a set, which is what a grant
    tests membership in. -/
def fact (name : String) (terms : List Json) : Json :=
  Json.mkObj [("name", .str name), ("terms", .arr terms.toArray)]

/-- The facts a task's token carries. Every name begins with `task_`, which is the prefix the
    `orchestra` issuer is confined to in kleis's configuration. -/
def TaskFacts.toFacts (t : TaskFacts) : List Json :=
  [fact "task_id" [.str t.taskId]]
  ++ (match t.repo with
      | some r =>
        [fact "task_fork" [.str r.fork.owner, .str r.fork.name],
         fact "task_upstream" [.str r.upstream.owner, .str r.upstream.name]]
      | none => [])
  ++ (t.issueNumber.map fun n => fact "task_issue" [.num n]).toList
  ++ t.tools.eraseDups.map (fun tool => fact "task_tool" [.str tool])
  ++ (if t.readOnly then [] else [fact "task_writable" [.bool true]])
  ++ [fact "task_pr_labels" [.arr (t.prLabels.map Json.str).toArray]]
  -- Always stated, true or false: the grant requires it before it allows any label at all,
  -- so a token that lacked it could not have its labels left unchecked.
  ++ [fact "task_label_any" [.bool (t.tools.contains "label_issue")]]
  ++ (t.org.map fun o => fact "task_org" [.str o]).toList
  ++ (t.identity.map fun i => fact "task_identity" [.str i]).toList
  ++ (t.pushPrefix.map fun p => fact "task_push_prefix" [.str p]).toList

/-- A token kleis minted for a task. -/
structure Minted where
  /-- The token, which goes into the sandbox's proxy URL. -/
  token : String
  /-- The identifier it is revoked by. -/
  revocationId : String
  /-- When it expires, in seconds since the epoch; `0` when kleis said it some other way. -/
  expires : Nat
  /-- The token's card, when kleis sent one with it (`derive` does, `/tokens` does not). -/
  card : Option String := none

instance : Repr Minted where
  reprPrec m _ := s!"\{ token := <redacted>, revocationId := {repr m.revocationId} }"

/-- `url` without a trailing slash. -/
private def base (cfg : KleisConfig) : String :=
  if cfg.url.endsWith "/" then (cfg.url.dropEnd 1).toString else cfg.url

/-- The issuer credential: from {name}`KleisConfig.issuerTokenFile` when there is one, read each
    time so a credential kleisd renewed there is picked up without a restart. -/
def issuerCredential (cfg : KleisConfig) : IO String := do
  match cfg.issuerTokenFile with
  | some f =>
    let text := (← IO.FS.readFile f).trimAscii.toString
    if text.isEmpty then throw (.userError s!"kleis's issuer credential file {f} is empty")
    return text
  | none => return cfg.issuerToken

/-- POST to one of kleis's own endpoints with the issuer credential. Directly, never through a
    proxy: the daemon's environment may have one set, and this request is to the proxy itself. -/
private def postAdmin (cfg : KleisConfig) (path body : String) : IO (Nat × String) := do
  let (status, _, out) ← Utils.Http.curlFull
    #["--noproxy", "*", "-X", "POST", "-H", "Content-Type: application/json",
      "--data-raw", body, s!"{base cfg}{path}"]
    (bearer := some (← issuerCredential cfg))
  return (status, out)

/-- The error kleis put in its answer, or the answer itself. -/
private def errorOf (body : String) : String :=
  match Json.parse body >>= (·.getObjValAs? String "error") with
  | .ok e => e
  | .error _ => body.trimAscii.toString

/-- Mint a token for a task. -/
def mint (cfg : KleisConfig) (t : TaskFacts) : IO Minted := do
  let body := Json.mkObj [
    ("grants", .arr (cfg.grants.map Json.str).toArray),
    ("bearer", .str s!"orchestra:{t.taskId}"),
    ("ttl", .str cfg.ttl),
    ("facts", .arr t.toFacts.toArray)]
  let (status, out) ← postAdmin cfg "/.kleis/v1/tokens" body.compress
  if status != 200 then
    throw (.userError s!"kleis refused to mint a token for task {t.taskId} ({status}): \
{errorOf out}")
  match Json.parse out with
  | .error e => throw (.userError s!"kleis answered a token request with malformed JSON: {e}")
  | .ok j =>
    let .ok token := j.getObjValAs? String "token"
      | throw (.userError "kleis answered a token request without a token")
    -- Without one the token could not be revoked when the task ends, so it is refused here rather
    -- than handed to a task that would leave it live for its whole lifetime.
    let some revocationId := j.getObjValAs? (List String) "revocation_ids" |>.toOption
        |>.bind (·.head?)
      | throw (.userError "kleis minted a token without a revocation id; refusing to use it")
    let expires := (j.getObjValAs? Nat "expires").toOption.getD 0
    return { token, revocationId, expires }

/-- Revoke a task's token. A token kleis no longer knows is already as good as revoked. -/
def revoke (cfg : KleisConfig) (m : Minted) : IO Unit := do
  if m.revocationId.isEmpty then return
  let body := Json.mkObj [("revocation_id", .str m.revocationId)]
  let (status, out) ← postAdmin cfg "/.kleis/v1/revoke" body.compress
  if status != 200 && status != 404 then
    throw (.userError s!"kleis refused to revoke a task's token ({status}): {errorOf out}")

/-- The `host:port` sandboxes reach the proxy at: `proxy` if configured, else `url`'s. -/
def proxyAuthority (cfg : KleisConfig) : String :=
  match cfg.proxy with
  | some p => p
  | none =>
    let rest := match (base cfg).splitOn "://" with
      | [_, r] => r
      | _ => base cfg
    (rest.splitOn "/").headD rest

/-- The port sandboxes reach the proxy on, for a backend that grants ports one by one: the
    authority's, else the default for `url`'s scheme. -/
def proxyPort (cfg : KleisConfig) : Nat :=
  match (proxyAuthority cfg).splitOn ":" |>.getLast? |>.bind (·.toNat?) with
  | some p => p
  | none => if cfg.proxy.isNone && cfg.url.startsWith "https:" then 443 else 80

/-- Percent-encode everything but the unreserved characters, for the token's place in a URL. -/
def percentEncode (s : String) : String :=
  s.toUTF8.foldl (init := "") fun acc b =>
    let c := Char.ofNat b.toNat
    if c.isAlphanum || c == '-' || c == '_' || c == '.' || c == '~' then acc.push c
    else
      let hex := "0123456789ABCDEF".toList
      acc.push '%' |>.push (hex.getD (b.toNat / 16) '0') |>.push (hex.getD (b.toNat % 16) '0')

/-- Where the system's CA bundle is, on the machines orchestra runs on. -/
private def systemBundle? : IO (Option System.FilePath) := do
  let fromEnv := (← IO.getEnv "NIX_SSL_CERT_FILE").toList ++ (← IO.getEnv "SSL_CERT_FILE").toList
  let candidates := fromEnv ++
    ["/etc/ssl/certs/ca-certificates.crt", "/etc/ssl/certs/ca-bundle.crt",
     "/etc/pki/tls/certs/ca-bundle.crt", "/etc/ssl/cert.pem"]
  for c in candidates do
    if ← System.FilePath.pathExists c then return some c
  return none

/-- The kleis CA, from `ca_file` or from the daemon. -/
def caPem (cfg : KleisConfig) : IO String := do
  match cfg.caFile with
  | some f => IO.FS.readFile f
  | none =>
    let (status, _, out) ← Utils.Http.curlFull #["--noproxy", "*", s!"{base cfg}/.kleis/v1/ca"]
      (bearer := some (← issuerCredential cfg))
    if status != 200 then
      throw (.userError s!"could not fetch kleis's CA ({status}): {errorOf out}")
    return out

/-- A CA bundle for the sandbox: the system's roots and kleis's CA.

    Both, not kleis's alone. `SSL_CERT_FILE` *replaces* a program's trust store rather than adding
    to it, and not everything goes through the proxy: a host in `NO_PROXY`, or one the proxy lets
    through untouched, presents its real certificate, which only the system roots verify. Written
    once per content into orchestra's data directory, under a name derived from it. -/
def bundleOf (kleisCa : String) : IO System.FilePath := do
  let system ← match ← systemBundle? with
    | some p => IO.FS.readFile p
    | none => do
      IO.eprintln "  [kleis] warning: no system CA bundle found; the sandbox will trust only kleis"
      pure ""
  -- Not twice: a daemon whose own trust store already holds kleis's CA keeps it as it is.
  let already := (system.splitOn kleisCa.trimAscii.toString).length > 1
  let contents := if already then system
    else system ++ (if system.endsWith "\n" || system.isEmpty then "" else "\n") ++ kleisCa
  let dir := (← Dirs.dataBase) / "kleis"
  IO.FS.createDirAll dir
  -- Named by its contents and never replaced: a sandbox is granted the file it was started with,
  -- and on landrun that grant is on the file itself, so replacing it under a running task would
  -- take its trust store away mid-run. A changed bundle — kleis's CA rotated, the system's roots
  -- updated — is a new file for the tasks that start after it.
  let digest := String.ofList ((toString (hash contents).toNat).toList.take 20)
  let path := dir / s!"ca-bundle-{digest}.pem"
  if !(← path.pathExists) then
    let tmp := dir / s!"ca-bundle-{digest}.pem.{← IO.monoNanosNow}"
    IO.FS.writeFile tmp contents
    IO.FS.rename tmp path
  return path

/-- `bundleOf` kleis's CA as `ca_file` or the daemon gives it: the bundle for the legacy path, and
    for a session opened before a token exists. -/
def caBundle (cfg : KleisConfig) : IO System.FilePath := do
  bundleOf (← caPem cfg)

/-- What the agent is told about reaching GitHub, for its system prompt.

    The facts its token carries, said to the agent: the proxy decides by them, and an agent that
    does not know which repository is its fork or which issue it may comment on would find out by
    being refused. The pull request labels are here because nothing applies them for it any more —
    `create_pr` used to. -/
def systemPrompt (t : TaskFacts) : String := Id.run do
  let mut lines := #["## GitHub",
    "",
    "You reach GitHub through a proxy that holds the credentials: use `git` and `gh api` " ++
    "directly (see the orchestra-pull-requests skill). It decides each request by what this " ++
    "task may do, and a refusal quotes the check that failed."]
  if let some r := t.repo then
    lines := lines.push s!"- Your fork, which you push to: `{r.fork.owner}/{r.fork.name}`"
    lines := lines.push s!"- The upstream, which pull requests go to: `{r.upstream.owner}/{r.upstream.name}`"
  if let some n := t.issueNumber then
    lines := lines.push s!"- The issue or pull request this task was launched from, the only one you may comment on: #{n}"
  if !t.prLabels.isEmpty then
    lines := lines.push s!"- Labels to add to every pull request you open: {", ".intercalate (t.prLabels.map (s!"`{·}`"))}"
  if t.readOnly then
    lines := lines.push "- This task is read-only: you may fetch, not push."
  if let some p := t.pushPrefix then
    lines := lines.push s!"- Push only refs under `{p}`."
  if let (some o, true) := (t.org, t.tools.contains "create_repository") then
    lines := lines.push s!"- You may create repositories in `{o}`, with `gh api orgs/{o}/repos -f name=…`; \
the same token may then push to them."
  return "\n".intercalate lines.toList

/-- What a sandbox needs to reach GitHub through the proxy. -/
structure Launch where
  /-- Environment for every process in the sandbox. -/
  env : Array (String × String)
  /-- Files the sandbox must be able to read — the CA bundle. -/
  files : Array Exec.PathGrant
  /-- The port the proxy listens on. -/
  port : Nat

/-- The CA bundle as a path the session is opened with. A backend that runs the agent elsewhere
    carries the paths it is opened with there, and one granted only later, once the token is
    minted, has to be placed in a pod that is already running as a user who may not create the
    directory it goes in. -/
def bundleGrant (bundle : System.FilePath) : Exec.PathGrant :=
  { path := bundle.toString, access := .ro, from_ := .orchestra, required := true }

/-- The environment and files for a sandbox whose task holds `m`.

    `HTTPS_PROXY` in both spellings, because curl reads only the lowercase one for `http_proxy` and
    Go programs read either; the CA in each of the variables the common tools read, since there is
    no one variable they all honour. `GH_TOKEN` is a placeholder: `gh` refuses to run without one,
    and kleis strips whatever `Authorization` a client sends before attaching the real credential.
    git is told to answer the proxy's challenge with basic authentication straight away rather
    than probing for a scheme first, through the environment so nothing in the sandbox's git
    configuration has to change. -/
def launch (cfg : KleisConfig) (m : Minted) (bundle : System.FilePath) : Launch :=
  { env := proxyEnv cfg m.token bundle ++ #[
      ("GH_TOKEN", "kleis"),
      ("GIT_CONFIG_COUNT", "1"),
      ("GIT_CONFIG_KEY_0", "http.proxyAuthMethod"), ("GIT_CONFIG_VALUE_0", "basic"),
      ("GIT_TERMINAL_PROMPT", "0")]
    files := #[bundleGrant bundle]
    port := proxyPort cfg }
where
  /-- What every sandbox behind the proxy gets, on either path: the proxy with `token` in it, the
      hosts it does not go through, and the CA bundle under every name the common tools read. -/
  proxyEnv (cfg : KleisConfig) (token : String) (bundle : System.FilePath)
      (extraNoProxy : List String := []) : Array (String × String) :=
    -- With the port always written out: the sandbox is let through to `proxyPort` and no
    -- other, and a URL without one would have clients dial 80 whatever that says.
    let host := match (proxyAuthority cfg).splitOn ":" with
      | h :: _ :: _ => h
      | _ => proxyAuthority cfg
    let proxyUrl := s!"http://orchestra:{percentEncode token}@{host}:{proxyPort cfg}"
    let noProxy := ",".intercalate
      ((["localhost", "127.0.0.1", "::1"] ++ cfg.noProxy ++ extraNoProxy).eraseDups)
    let ca := bundle.toString
    #[("HTTPS_PROXY", proxyUrl), ("https_proxy", proxyUrl),
      ("HTTP_PROXY", proxyUrl), ("http_proxy", proxyUrl),
      ("NO_PROXY", noProxy), ("no_proxy", noProxy),
      ("SSL_CERT_FILE", ca), ("GIT_SSL_CAINFO", ca), ("CURL_CA_BUNDLE", ca),
      ("REQUESTS_CA_BUNDLE", ca), ("NODE_EXTRA_CA_CERTS", ca)]

/-! ## Sources (B.2.2 of kleis's `docs/design/specs.md`) -/

/-- Every place a task's source can be written, in the order `choose` consults them. -/
structure SourceLevels where
  /-- The task's own `kleis_token`: a `{{secret}}` reference, unresolved. -/
  taskToken : Option String := none
  /-- The task's own `kleis_spec`. -/
  taskSpec : Option String := none
  /-- Its identity's `kleis_spec`. -/
  identitySpec : Option String := none
  /-- Whether its identity carries a `kleis_token` (already resolved, so only whether). -/
  identityToken : Bool := false
  /-- Its role's `kleis_spec`, else its listener action's (`IOTask.kleisRoleSpec`). -/
  roleSpec : Option String := none
  /-- `kleis.default_spec`. -/
  defaultSpec : Option String := none
deriving Repr, Inhabited

/-- Which source a task's token is to be derived from. -/
inductive Choice where
  /-- The task's own token, by its `{{secret}}` reference. -/
  | taskToken (ref : String)
  /-- A spec, and the level that named it (`task`, `identity`, `role`, `default_spec`). -/
  | spec (name : String) (level : String)
  /-- The identity's token. -/
  | identityToken
  /-- No source: no kleis, or the legacy mint when `kleis` is configured without a default. -/
  | none
deriving Repr, BEq, DecidableEq, Inhabited

/-- The first source that is set, in the order B.2.2 fixes: the task's token, the task's spec,
    the identity's spec or token, the role's then the listener action's spec, `default_spec`.

    The identity comes before the role so that a task run as an identity keeps that identity's
    access whatever role it fills; a role that wants less narrows instead of replacing. -/
def choose (l : SourceLevels) : Choice :=
  match l.taskToken, l.taskSpec, l.identitySpec, l.identityToken, l.roleSpec, l.defaultSpec with
  | some ref, _, _, _, _, _ => .taskToken ref
  | none, some s, _, _, _, _ => .spec s "task"
  | none, none, some s, _, _, _ => .spec s "identity"
  | none, none, none, true, _, _ => .identityToken
  | none, none, none, false, some s, _ => .spec s "role"
  | none, none, none, false, none, some s => .spec s "default_spec"
  | none, none, none, false, none, none => .none

/-- What a task queued by this one carries as its own source (B.3): the spec's name, or the
    reference of a token the task named — never a resolved token. A source that came off the
    identity's token is not carried: the queued task inherits the identity, which names it again.
    Returned as `(kleisSpec, kleisToken)`. -/
def Choice.inherited : Choice → Option String × Option String
  | .taskToken ref => (Option.none, some ref)
  | .spec s _      => (some s, Option.none)
  | .identityToken => (Option.none, Option.none)
  | .none          => (Option.none, Option.none)

/-- For a log line. Never the token. -/
def Choice.describe : Choice → String
  | .taskToken ref => s!"the token the task names ({ref})"
  | .spec s level  => s!"spec '{s}' (from {level})"
  | .identityToken => "the identity's token"
  | .none          => "no source"

/-- What a task token is derived from, resolved. -/
inductive Source where
  | spec (name : String)
  | token (token : String)

instance : Repr Source where
  reprPrec
    | .spec s, _  => s!"spec {repr s}"
    | .token _, _ => "token <redacted>"

/-- The token a `{{key}}` reference names in `secrets`. -/
def resolveTokenRef (secrets : List (String × String)) (ref : String) : Except String String :=
  match secretRefKey? ref with
  | none => .error (kleisTokenRefError? ref |>.getD s!"'{ref}' is not a secret reference")
  | some key =>
    match secrets.find? (·.1 == key) with
    | none => .error s!"kleis_token names the secret '{key}', which secrets.json does not define"
    | some (_, v) =>
      if v.trimAscii.isEmpty then .error s!"the secret '{key}' named by kleis_token is empty"
      else .ok v.trimAscii.toString

/-- `c` resolved to what derive is called with: a task's token reference looked up in
    `secrets.json` now, at launch, so the queue never held it; the identity's token as its record
    was read. `none` for no source. -/
def resolve (c : Choice) (identityToken : Option String) : IO (Option Source) := do
  match c with
  | .none => return none
  | .spec s _ => return some (.spec s)
  | .taskToken ref => return some (.token (← IO.ofExcept (resolveTokenRef (← loadSecrets) ref)))
  | .identityToken =>
    match identityToken with
    | some t => return some (.token t)
    | none => throw (.userError "the identity's kleis_token is gone")

/-! ## `derive` (A.7) -/

/-- The body of a `/.kleis/v1/derive` request. From a spec: the spec, the task's facts, its bearer;
    from a token: the token alone. Either way the TTL and the narrowings — omitted when there are
    none. -/
def deriveBody (cfg : KleisConfig) (src : Source) (facts : TaskFacts) (narrow : List String)
    : Json :=
  let narrowField : List (String × Json) :=
    if narrow.isEmpty then [] else [("narrow", .arr (narrow.map Json.str).toArray)]
  match src with
  | .spec s => Json.mkObj ([
      ("spec", .str s),
      ("ttl", .str cfg.ttl),
      ("bearer", .str s!"orchestra:{facts.taskId}"),
      ("facts", .arr facts.toFacts.toArray)] ++ narrowField)
  | .token t => Json.mkObj ([("token", .str t), ("ttl", .str cfg.ttl)] ++ narrowField)

/-- The token in a derive answer. `revocation_id` is one id (A.7.2); a list is taken too, as long
    as it holds exactly one, since revoking a list could revoke the source a token came from. -/
def parseDerived (body : String) : Except String Minted := do
  let j ← Json.parse body |>.mapError (s!"kleis answered derive with malformed JSON: {·}")
  let .ok token := j.getObjValAs? String "token"
    | .error "kleis answered derive without a token"
  let revocationId ← match j.getObjValAs? String "revocation_id" with
    | .ok r => pure r
    | .error _ => match j.getObjValAs? (List String) "revocation_ids" with
      | .ok [r] => pure r
      | _ => .error "kleis derived a token without a single revocation id; refusing to use it"
  if revocationId.isEmpty then
    .error "kleis derived a token with an empty revocation id; refusing to use it"
  let expires := (j.getObjValAs? Nat "expires").toOption.getD 0
  let card := (j.getObjValAs? String "card").toOption.filter (!·.trimAscii.isEmpty)
  return { token, revocationId, expires, card }

/-- Derive a task token (`POST /.kleis/v1/derive`, with the issuer credential). -/
def derive (cfg : KleisConfig) (src : Source) (facts : TaskFacts) (narrow : List String)
    : IO Minted := do
  let (status, out) ← postAdmin cfg "/.kleis/v1/derive" (deriveBody cfg src facts narrow).compress
  if status != 200 then
    let what := match src with
      | .spec s => s!"from spec '{s}'"
      | .token _ => "from the token it was given"
    throw (.userError s!"kleis refused to derive a token {what} for task {facts.taskId} \
({status}): {errorOf out}")
  IO.ofExcept (parseDerived out)

/-! ## `setup` and the card (A.8, A.9) -/

/-- One file a sandbox is to have in its home. -/
structure SetupFile where
  /-- Relative to the home; checked by `Exec.homePathError?`. -/
  path : String
  /-- Octal, as `"0600"`. -/
  mode : String := "0600"
  content : String
deriving Repr, Inhabited

/-- What kleis says a sandbox needs to use a token with ordinary tools. -/
structure Setup where
  /-- kleis's CA, which the sandbox's bundle adds to the system's roots. -/
  caPem : Option String := none
  /-- Environment for every process in the sandbox. Holds no secrets: placeholders kleis
      replaces (`GH_TOKEN=kleis`). -/
  env : Array (String × String) := #[]
  files : Array SetupFile := #[]
  /-- Hosts the sandbox reaches directly, on top of orchestra's own. -/
  noProxy : List String := []
  /-- The token's card. -/
  card : Option String := none
deriving Repr, Inhabited

/-- The variables orchestra sets itself and a setup may not (A.8, B.2.3): the proxy, the CA and
    the hosts the proxy is bypassed for. A setup that set one would decide where the sandbox's
    traffic goes, or what it trusts, and that is the consumer's to say. -/
def reservedSetupEnv : List String :=
  ["HTTPS_PROXY", "HTTP_PROXY", "https_proxy", "http_proxy", "NO_PROXY", "no_proxy",
   "SSL_CERT_FILE", "GIT_SSL_CAINFO", "CURL_CA_BUNDLE", "REQUESTS_CA_BUNDLE",
   "NODE_EXTRA_CA_CERTS"]

/-- Whether `n` can be an environment variable's name. -/
def envNameValid (n : String) : Bool :=
  !n.isEmpty && !(n.front.isDigit) && n.all (fun c => c.isAlphanum || c == '_')

/-- A `/setup` answer, checked. Anything this would have to refuse fails the whole setup rather
    than being dropped: a sandbox missing half of what kleis said it needs fails later, and less
    legibly, than one that never started. -/
def parseSetup (j : Json) : Except String Setup := do
  let caPem := (j.getObjVal? "proxy" >>= (·.getObjValAs? String "ca_pem")).toOption
    |>.filter (!·.trimAscii.isEmpty)
  let env ← match j.getObjVal? "env" with
    | .error _ | .ok .null => pure #[]
    | .ok (.obj kvs) => kvs.toArray.mapM fun (k, v) => do
      let .str s := v | .error s!"setup.env.{k} is not a string"
      if !envNameValid k then .error s!"setup.env names '{k}', which is not a variable name"
      if reservedSetupEnv.contains k then
        .error s!"setup.env sets {k}, which is orchestra's to set (the proxy, the CA and \
NO_PROXY are the consumer's); refusing the setup"
      pure (k, s)
    | .ok _ => .error "setup.env is not an object"
  let files ← match j.getObjVal? "files" with
    | .error _ | .ok .null => pure #[]
    | .ok (.arr fs) => fs.mapM fun f => do
      let path ← f.getObjValAs? String "path" |>.mapError (fun _ => "a setup file has no path")
      if let some e := Exec.homePathError? path then .error s!"setup.files: {e}; refusing the setup"
      let mode := (f.getObjValAs? String "mode").toOption.getD "0600"
      if (Exec.fileModeOf? mode).isNone then
        .error s!"setup.files: {path} has mode '{mode}', which is not a file mode"
      let content ← f.getObjValAs? String "content"
        |>.mapError (fun _ => s!"setup.files: {path} has no content")
      pure { path, mode, content }
    | .ok _ => .error "setup.files is not a list"
  let noProxy ← match j.getObjVal? "no_proxy" with
    | .error _ | .ok .null => pure []
    | .ok v => (Lean.FromJson.fromJson? v : Except String (List String))
      |>.mapError (fun _ => "setup.no_proxy is not a list of hosts")
  if noProxy.any (fun h => h.isEmpty || h.any (fun c => c == ',' || c.isWhitespace)) then
    .error "setup.no_proxy holds something that is not a host"
  let card := (j.getObjValAs? String "card").toOption.filter (!·.trimAscii.isEmpty)
  return { caPem, env, files, noProxy, card }

/-- GET one of kleis's own endpoints as the bearer of `token`. Directly, never through a proxy,
    as `postAdmin` does. -/
private def getAsBearer (cfg : KleisConfig) (path token : String) : IO (Nat × String) := do
  let (status, _, out) ← Utils.Http.curlFull #["--noproxy", "*", s!"{base cfg}{path}"]
    (bearer := some token)
  return (status, out)

/-- What a sandbox needs for `token` (`GET /.kleis/v1/setup`). -/
def fetchSetup (cfg : KleisConfig) (token : String) : IO Setup := do
  let (status, out) ← getAsBearer cfg "/.kleis/v1/setup" token
  if status != 200 then
    throw (.userError s!"kleis refused the setup for a task token ({status}): {errorOf out}")
  match Json.parse out with
  | .error e => throw (.userError s!"kleis answered setup with malformed JSON: {e}")
  | .ok j => match parseSetup j with
    | .ok s => return s
    | .error e => throw (.userError s!"kleis's setup for a task token: {e}")

/-- The card for `token` (`GET /.kleis/v1/card`), as JSON `{"card": …}` or as text. For when
    neither derive nor setup carried one. -/
def fetchCard (cfg : KleisConfig) (token : String) : IO String := do
  let (status, out) ← getAsBearer cfg "/.kleis/v1/card" token
  if status != 200 then
    throw (.userError s!"kleis refused the card for a task token ({status}): {errorOf out}")
  match Json.parse out >>= (·.getObjValAs? String "card") with
  | .ok c => return c
  | .error _ => return out

/-- A task's token, derived, with what kleis said its sandbox needs. -/
structure Prepared where
  minted : Minted
  setup : Setup
  /-- What the agent is told its token allows: setup's card, else derive's, else `/card`'s. -/
  card : String
  /-- Where it came from, for logs and for what a queued task inherits. -/
  choice : Choice

/-- Derive a token from `src` and fetch its setup. The token is revoked before this throws, so
    a caller holding nothing has nothing to clean up. -/
def prepare (cfg : KleisConfig) (choice : Choice) (src : Source) (facts : TaskFacts)
    (narrow : List String) : IO Prepared := do
  let minted ← derive cfg src facts narrow
  try
    let setup ← fetchSetup cfg minted.token
    let card ← match setup.card <|> minted.card with
      | some c => pure c
      | none => fetchCard cfg minted.token
    return { minted, setup, card, choice }
  catch e =>
    try revoke cfg minted catch _ => pure ()
    throw e

/-- Everything a launch site needs to know to get a task its token, given the levels of its
    source. `none` when there is no source: no kleis at all, or the legacy mint. Throws when a
    level names a source and no kleis is configured — running such a task without one would hand
    it whatever the sandbox holds without kleis — and when a narrowing would be lost to the legacy
    mint, which cannot narrow. -/
def prepareFor (kleis : Option KleisConfig) (levels : SourceLevels)
    (identityToken : Option String) (facts : TaskFacts) (narrow : List String)
    : IO (Option Prepared) := do
  let choice := choose { levels with defaultSpec := kleis.bind (·.defaultSpec) }
  match kleis, choice with
  | none, .none =>
    unless narrow.isEmpty do
      throw (.userError "this task carries kleis_narrow, but no kleis is configured")
    return none
  | none, c =>
    throw (.userError s!"this task names a kleis source ({c.describe}), but no kleis is configured")
  | some _, .none =>
    unless narrow.isEmpty do
      throw (.userError "this task carries kleis_narrow but has no kleis source to derive from, \
and the legacy mint (kleis.grants) cannot narrow; set kleis.default_spec or a kleis_spec")
    return none
  | some cfg, c =>
    let some src ← resolve c identityToken | return none
    some <$> prepare cfg c src facts narrow

/-- The bundle a derived token's sandbox trusts: the system's roots and the CA its setup named,
    else the one `ca_file` or the daemon gives. -/
def setupBundle (cfg : KleisConfig) (s : Setup) : IO System.FilePath :=
  match s.caPem with
  | some pem => bundleOf pem
  | none => caBundle cfg

/-- Write the setup's files into the session's home, and the grants that let the agent read them
    there (`Exec.Session.putHomeFile`). Home-relative and supplied by the environment, as far as a
    backend that carries paths is concerned: they are already where the agent will look. -/
def installFiles (session : Exec.Session) (files : Array SetupFile) : IO (Array Exec.PathGrant) :=
  files.mapM fun f => do
    session.putHomeFile f.path f.mode f.content
    return { path := f.path, access := .ro, scope := .home }

/-- The environment and files for a sandbox whose token was derived. Unlike `launch`, nothing
    about any one service: no `GH_TOKEN`, no git configuration — those come in `setup.env` from
    the extensions that need them. -/
def launchDerived (cfg : KleisConfig) (p : Prepared) (bundle : System.FilePath)
    (fileGrants : Array Exec.PathGrant) : Launch :=
  { env := launch.proxyEnv cfg p.minted.token bundle p.setup.noProxy
      ++ #[("GIT_TERMINAL_PROMPT", "0")] ++ p.setup.env
    files := #[bundleGrant bundle] ++ fileGrants
    port := proxyPort cfg }

/-- The card as a system-prompt section. -/
def cardPrompt (card : String) : String :=
  "## What you can reach\n\n" ++
  "Everything you do over the network goes through a proxy, kleis, which holds the credentials " ++
  "and decides each request; use ordinary command-line tools against the real addresses. A " ++
  "refusal is HTTP 403 and says which rule refused it. This is what your token allows:\n\n" ++
  card.trimAscii.toString

end Orchestra.Kleis
