import Lean.Data.Json
import Orchestra.Config
import Orchestra.Dirs
import Orchestra.Exec.Spec
import Orchestra.Utils.Http

/-!
# Reaching GitHub through kleis

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
  /-- When it expires, in seconds since the epoch. -/
  expires : Nat

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
def caBundle (cfg : KleisConfig) : IO System.FilePath := do
  let kleisCa ← caPem cfg
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

/-- The environment and files for a sandbox whose task holds `m`.

    `HTTPS_PROXY` in both spellings, because curl reads only the lowercase one for `http_proxy` and
    Go programs read either; the CA in each of the variables the common tools read, since there is
    no one variable they all honour. `GH_TOKEN` is a placeholder: `gh` refuses to run without one,
    and kleis strips whatever `Authorization` a client sends before attaching the real credential.
    git is told to answer the proxy's challenge with basic authentication straight away rather
    than probing for a scheme first, through the environment so nothing in the sandbox's git
    configuration has to change. -/
def launch (cfg : KleisConfig) (m : Minted) (bundle : System.FilePath) : Launch :=
  -- With the port always written out: the sandbox is let through to `proxyPort` and no
  -- other, and a URL without one would have clients dial 80 whatever that says.
  let host := match (proxyAuthority cfg).splitOn ":" with
    | h :: _ :: _ => h
    | _ => proxyAuthority cfg
  let proxyUrl := s!"http://orchestra:{percentEncode m.token}@{host}:{proxyPort cfg}"
  let noProxy := ",".intercalate (["localhost", "127.0.0.1", "::1"] ++ cfg.noProxy)
  let ca := bundle.toString
  { env := #[
      ("HTTPS_PROXY", proxyUrl), ("https_proxy", proxyUrl),
      ("HTTP_PROXY", proxyUrl), ("http_proxy", proxyUrl),
      ("NO_PROXY", noProxy), ("no_proxy", noProxy),
      ("SSL_CERT_FILE", ca), ("GIT_SSL_CAINFO", ca), ("CURL_CA_BUNDLE", ca),
      ("REQUESTS_CA_BUNDLE", ca), ("NODE_EXTRA_CA_CERTS", ca),
      ("GH_TOKEN", "kleis"),
      ("GIT_CONFIG_COUNT", "1"),
      ("GIT_CONFIG_KEY_0", "http.proxyAuthMethod"), ("GIT_CONFIG_VALUE_0", "basic"),
      ("GIT_TERMINAL_PROMPT", "0")]
    files := #[{ path := ca, access := .ro, from_ := .orchestra, required := true }]
    port := proxyPort cfg }

end Orchestra.Kleis
