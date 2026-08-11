import Lean.Data.Json
import Std.Time
import Orchestra.Config

open Lean (Json ToJson FromJson toJson)

namespace Orchestra.Deploy

/-! # Preview deployments

Runs a repository's own `docker-compose.yaml` on a separate machine and hands back a URL a human
can open. It backs the `deploy_preview` / `destroy_preview` / `list_deployments` MCP tools, and
the same functions are what the daemon's sweeper calls to expire what it created.

The design decision that shapes everything here: **the compose file is never inspected**. Not
validated, not rewritten, not restricted. A pull request may ask for `privileged: true`, a
socket mount, host networking — whatever it likes. That is only a sane thing to allow because of
where it runs: a pod on the previews cluster with `runtimeClassName: kata`, which is a VM with
its own kernel, on a machine that holds none of orchestra's credentials
(`container/previews-vm/`). The boundary is the hypervisor, not a denylist.

Three consequences follow, and they explain most of what looks unusual below:

* **The build happens inside the sandbox, not here.** An arbitrary `RUN` line is arbitrary code,
  so building on the daemon's host would hand it the very machine the sandbox exists to protect.
  The pod runs its own Docker daemon and does its own `docker compose up --build`.
* **The pod is never given a credential.** It cannot clone the repository — it has no token and
  no business holding one. Instead the source is exported from the daemon's *existing* clone with
  `git archive` and copied in. Nothing in the sandbox can reach GitHub as anybody.
* **Every deployment expires.** A preview whose pull request is forgotten is the normal case, not
  the exception, so the expiry is written onto the pod as an annotation at creation time and
  `gc` believes the annotation rather than any bookkeeping of ours. -/

/-- What to deploy: a tree, a ref inside it, and which port of the compose project is the one a
    reviewer should land on. -/
structure Spec where
  /-- Repository the preview belongs to. Only used to name and label the deployment. -/
  repo : Repository
  /-- Ref to export from `sourcePath`. Whatever the agent has just pushed. -/
  ref : String
  /-- The daemon's existing clone. The source is taken from here with `git archive`, so the pod
      needs no credential and no network to get it. -/
  sourcePath : System.FilePath
  /-- Compose file within the tree, relative to its root. -/
  composeFile : String := "docker-compose.yaml"
  /-- Port the compose project publishes that the ingress should route to. -/
  port : Nat := 80
  /-- Pull request number, when the preview belongs to one. Part of the name, so re-deploying a
      pull request replaces its preview instead of accumulating another. -/
  prNumber : Option Nat := none
deriving Repr, Inhabited

/-- A live preview, as reported back to the agent and recorded on the issue. -/
structure Deployment where
  name : String
  url : String
  /-- Pod phase as Kubernetes reports it: `Pending`, `Running`, `Succeeded`, `Failed`. -/
  status : String
  /-- ISO-8601, from the pod's own annotation — so a deployment created by a previous daemon
      still expires on time. -/
  expiresAt : String
deriving Repr, Inhabited

instance : ToJson Deployment where
  toJson d := Json.mkObj
    [ ("name", .str d.name)
    , ("url", .str d.url)
    , ("status", .str d.status)
    , ("expires_at", .str d.expiresAt) ]

/-! ## Naming

A deployment's name is its identity: the pod, the service, the ingress, the hostname and the
compose project all use it, and re-deploying the same pull request must land on the same name so
the old preview is replaced rather than joined. It also has to be a DNS-1123 label, because it
ends up in a hostname. -/

private def isNameChar (c : Char) : Bool :=
  (c.isAlpha && c.isLower) || c.isDigit

/-- Lowercase, non-alphanumerics collapsed to single dashes, no leading or trailing dash.

    Character lists rather than `String.take` / `String.dropRight` throughout: those return
    slices in current Lean, and a name that is silently a `String.Slice` is a type error waiting
    at every call site. -/
private def slug (s : String) : String :=
  let chars := s.toList.map Char.toLower
  let collapsed := chars.foldl (init := ([] : List Char)) fun acc c =>
    if isNameChar c then acc ++ [c]
    else match acc.getLast? with
      | some '-' => acc
      | _ => acc ++ ['-']
  let noLead := match collapsed with
    | '-' :: rest => rest
    | other => other
  let noTrail := match noLead.getLast? with
    | some '-' => noLead.dropLast
    | _ => noLead
  String.mk noTrail

private def hexDigits : List Char := "0123456789abcdef".toList

/-- FNV-1a, rendered as eight hex digits. Only ever used to keep two different inputs that slug
    to the same string apart, so a non-cryptographic hash is the right tool. -/
private def shortHash (s : String) : String := Id.run do
  let mut h : UInt64 := 14695981039346656037
  for b in s.toUTF8 do
    h := (h ^^^ b.toUInt64) * 1099511628211
  let mut out : List Char := []
  for i in [0:8] do
    let nibble := (h >>> (UInt64.ofNat (4 * (7 - i)))) &&& 15
    out := out ++ [hexDigits.getD nibble.toNat '0']
  return String.mk out

/-- The name a spec deploys under.

    Built from the repository and either the pull request number or the ref, then truncated and
    suffixed with a hash of the full identity. The truncation is what makes the hash necessary:
    two long branches on the same repository can slug to the same prefix, and silently
    redeploying over someone else's preview is worse than an ugly name. -/
def deploymentName (spec : Spec) : String :=
  let subject := match spec.prNumber with
    | some n => s!"pr-{n}"
    | none => spec.ref
  let identity := s!"{spec.repo.owner}/{spec.repo.name}/{subject}"
  let base := (slug s!"{spec.repo.name}-{subject}").toList.take 40
  -- Clipping can leave the dash that `slug` was careful to remove.
  let clipped := match base.getLast? with
    | some '-' => base.dropLast
    | _ => base
  -- A repository and ref made entirely of characters the slug drops — every non-Latin script,
  -- for one — leave nothing behind, and `-<hash>` is not a DNS label. The hash still identifies
  -- the deployment; only the human-readable half is gone, which is the correct thing to lose.
  let stem := if clipped.isEmpty then "preview" else String.mk clipped
  s!"{stem}-{shortHash identity}"

/-- Hostname a deployment answers on. -/
def deploymentUrl (cfg : DeployConfig) (name : String) : String :=
  s!"https://{name}.{cfg.baseDomain}"

/-! ## Refs

The ref arrives as a tool argument written by an agent, and it is handed to `git`, so it is
checked before it is used rather than trusted because it usually looks like a branch name. -/

private def isRefChar (c : Char) : Bool :=
  c.isAlphanum || c == '/' || c == '.' || c == '_' || c == '-'

/-- Accept a ref, or say why not.

    An allowlist rather than a denylist: what a legal branch, tag or commit id may contain is a
    short and well-known set, and everything outside it is either an attempt at an option or a
    typo. The leading dash is called out separately because that is the one that turns a ref into
    a `git` flag — a ref of `--output=…` makes `git archive` write its tarball wherever it is
    pointed, on the daemon's own filesystem. -/
def validateRef (ref : String) : Except String String :=
  let trimmed := ref.trim
  if trimmed.isEmpty then
    .error "ref must not be empty"
  else if trimmed.startsWith "-" then
    .error s!"ref {repr trimmed} must not start with a dash: git would read it as an option"
  else if !trimmed.all isRefChar then
    .error s!"ref {repr trimmed} contains characters that are not allowed in a branch, tag or \
      commit id (letters, digits, and / . _ - only)"
  else if (trimmed.splitOn "..").length > 1 then
    -- `a..b` is a range to git, not a ref, and `..` in a path is the other thing nobody means.
    .error s!"ref {repr trimmed} must not contain '..'"
  else
    .ok trimmed

/-! ## Time -/

private def nowSeconds : IO Int := do
  return (← Std.Time.Timestamp.now).toSecondsSinceUnixEpoch.val

private def toIso (epoch : Int) : IO String := do
  let ts := Std.Time.Timestamp.ofSecondsSinceUnixEpoch (Std.Time.Second.Offset.ofInt epoch)
  return (Std.Time.DateTime.ofTimestamp ts Std.Time.TimeZone.UTC).format "uuuu-MM-dd'T'HH:mm:ss'Z'"

private def parseIso (s : String) : Option Int :=
  -- Only ever reads back what `toIso` wrote: `uuuu-MM-ddTHH:mm:ssZ`, always UTC.
  match Std.Time.ZonedDateTime.fromISO8601String s with
  | .ok zdt => some zdt.toTimestamp.toSecondsSinceUnixEpoch.val
  | .error _ => none

/-! ## Manifests

Rendered as JSON rather than YAML: `kubectl apply` accepts either, and JSON is what Lean can
build without a serializer that has to get indentation right. -/

/-- Label every object carries, and the selector everything else is found by. -/
private def deploymentLabel : String := "orchestra.dev/deployment"

private def expiresAnnotation : String := "orchestra.dev/expires-at"

private def repoAnnotation : String := "orchestra.dev/repository"

private def objectMeta (cfg : DeployConfig) (name : String) (annotations : List (String × String)) : Json :=
  Json.mkObj
    [ ("name", .str name)
    , ("namespace", .str cfg.ns)
    , ("labels", Json.mkObj
        [ (deploymentLabel, .str name)
        , ("app.kubernetes.io/managed-by", .str "orchestra") ])
    , ("annotations", Json.mkObj (annotations.map fun (k, v) => (k, Json.str v))) ]

/-- The sandbox itself: one pod, one container, its own Docker daemon inside.

    `privileged` is on because dockerd needs it, and it is affordable for exactly the reason the
    module header gives — the pod is a VM. The previews cluster sets
    `privileged_without_host_devices` on the Kata runtime, so this grants the guest's devices,
    not the node's. -/
def podManifest (cfg : DeployConfig) (spec : Spec) (name : String) (expiresAt : String) : Json :=
  let container := Json.mkObj
    [ ("name", .str "sandbox")
    , ("image", .str cfg.image)
    , ("securityContext", Json.mkObj [("privileged", .bool true)])
    , ("env", .arr #[
        -- dind defaults to TLS on a generated CA. Nothing outside the pod ever talks to this
        -- daemon — every command reaches it through `kubectl exec` — so the plain local socket
        -- is both simpler and one less thing listening.
        Json.mkObj [("name", .str "DOCKER_TLS_CERTDIR"), ("value", .str "")] ])
    , ("ports", .arr #[Json.mkObj [("containerPort", toJson spec.port)]])
      -- Requests are stated rather than left to default, and they are equal to the limits on
      -- purpose. Kubernetes copies limits into requests when requests are absent, so the
      -- namespace's LimitRange defaults never apply here and the quota is charged the full
      -- limit either way — writing it down is the difference between a capacity that can be
      -- reasoned about and one discovered when the third preview is rejected. Equal is also the
      -- truth for memory: a Kata sandbox takes its guest's RAM for as long as it runs and gives
      -- none of it back.
    , ("resources", Json.mkObj
        [ ("limits", Json.mkObj
            [ ("cpu", .str cfg.cpuLimit)
            , ("memory", .str cfg.memoryLimit) ])
        , ("requests", Json.mkObj
            [ ("cpu", .str cfg.cpuLimit)
            , ("memory", .str cfg.memoryLimit) ]) ]) ]
  let podSpec := Json.mkObj <|
    [ ("containers", .arr #[container])
      -- Never restart: a compose project that fails to build should be reported to the agent as
      -- a failure it can read logs from, not looped over until the TTL expires.
    , ("restartPolicy", .str "Never") ]
    ++ (if cfg.runtimeClass.isEmpty then [] else [("runtimeClassName", Json.str cfg.runtimeClass)])
  Json.mkObj
    [ ("apiVersion", .str "v1")
    , ("kind", .str "Pod")
    , ("metadata", objectMeta cfg name
        [ (expiresAnnotation, expiresAt)
        , (repoAnnotation, spec.repo.toString) ])
    , ("spec", podSpec) ]

def serviceManifest (cfg : DeployConfig) (spec : Spec) (name : String) (expiresAt : String) : Json :=
  Json.mkObj
    [ ("apiVersion", .str "v1")
    , ("kind", .str "Service")
    , ("metadata", objectMeta cfg name [(expiresAnnotation, expiresAt)])
    , ("spec", Json.mkObj
        [ ("selector", Json.mkObj [(deploymentLabel, .str name)])
        , ("ports", .arr #[Json.mkObj
            [ ("port", toJson (80 : Nat))
            , ("targetPort", toJson spec.port)
            , ("protocol", .str "TCP") ]]) ]) ]

def ingressManifest (cfg : DeployConfig) (name : String) (expiresAt : String) : Json :=
  Json.mkObj
    [ ("apiVersion", .str "networking.k8s.io/v1")
    , ("kind", .str "Ingress")
    , ("metadata", objectMeta cfg name [(expiresAnnotation, expiresAt)])
    , ("spec", Json.mkObj <|
        (match cfg.ingressClass with
         | some c => [("ingressClassName", Json.str c)]
         | none => [])
        ++
        [ ("rules", .arr #[Json.mkObj
            [ ("host", .str s!"{name}.{cfg.baseDomain}")
            , ("http", Json.mkObj
                [ ("paths", .arr #[Json.mkObj
                    [ ("path", .str "/")
                    , ("pathType", .str "Prefix")
                    , ("backend", Json.mkObj
                        [ ("service", Json.mkObj
                            [ ("name", .str name)
                            , ("port", Json.mkObj [("number", toJson (80 : Nat))]) ]) ]) ]]) ]) ]]) ]) ]

/-- All three objects in one document, which is what gets piped to `kubectl apply -f -`. -/
def manifests (cfg : DeployConfig) (spec : Spec) (name : String) (expiresAt : String) : Json :=
  Json.mkObj
    [ ("apiVersion", .str "v1")
    , ("kind", .str "List")
    , ("items", .arr #[
        podManifest cfg spec name expiresAt,
        serviceManifest cfg spec name expiresAt,
        ingressManifest cfg name expiresAt ]) ]

/-! ## Running commands

Every failure below is returned as a message meant to be read by an agent that must decide what
to do next, so they carry the command's own stderr rather than a summary of it. -/

private structure CmdResult where
  exitCode : UInt32
  stdout : String
  stderr : String

private def run (cmd : String) (args : Array String) (input : Option String := none) :
    IO CmdResult := do
  let child ← IO.Process.spawn {
    cmd, args
    stdin := if input.isSome then .piped else .null
    stdout := .piped
    stderr := .piped
  }
  let child ← match input with
    | none => pure child
    | some s =>
      -- takeStdin so the handle can be dropped, which is what signals EOF to the child.
      let (h, child') ← child.takeStdin
      h.putStr s
      h.flush
      pure child'
  -- Both pipes are drained at once, and that is not a refinement: reading one to EOF before
  -- touching the other deadlocks as soon as the *other* fills its 64 KiB buffer. `docker compose
  -- up --build` writes BuildKit's progress to stderr, so every build past a trivial one hits
  -- that — the child blocks writing stderr, we block reading stdout, and nobody moves. A small
  -- demo project stays under the buffer and hides it, which is exactly how this got shipped.
  let stderrTask ← IO.asTask child.stderr.readToEnd
  let stdout ← child.stdout.readToEnd
  let stderr ← match ← IO.wait stderrTask with
    | .ok s => pure s
    | .error e => pure s!"(could not read stderr: {e})"
  let exitCode ← child.wait
  return { exitCode, stdout, stderr }

/-- Single-quote a string for `sh -c`. Every value interpolated into a shell command inside the
    sandbox goes through this — not because the shell is a boundary worth defending (it is the
    sandbox's own), but because a compose path with a space in it otherwise becomes a different
    command and a baffling error. -/
private def shQuote (s : String) : String :=
  "'" ++ s.replace "'" "'\\''" ++ "'"

private def failure (what : String) (r : CmdResult) : String :=
  let detail :=
    if !r.stderr.trim.isEmpty then r.stderr.trim
    else if !r.stdout.trim.isEmpty then r.stdout.trim
    else "(no output)"
  s!"{what} failed (exit {r.exitCode}):\n{detail}"

private def kubectl (cfg : DeployConfig) (args : Array String) (input : Option String := none) :
    IO CmdResult :=
  -- `--context` only when configured: naming one that a single-cluster kubeconfig does not
  -- contain is an error, so an unset context has to mean "whatever this file says" rather than
  -- a guess at a name.
  let base := #["--kubeconfig", cfg.kubeconfig, "-n", cfg.ns]
    ++ (match cfg.context with | some c => #["--context", c] | none => #[])
  run cfg.kubectl (base ++ args) input

/-- `kubectl exec` into a deployment's pod. Every command that touches the compose project goes
    through here, which is the reason the pod needs no network path back to us.

    Everything is run under busybox `timeout`, because the compose file is hostile by assumption:
    a `RUN sleep infinity` in an unread Dockerfile would otherwise hang this call, and with it the
    agent's tool call and the queue slot behind it, for as long as the sandbox's own TTL. Bounding
    it inside the sandbox rather than here also means the runaway is killed rather than merely
    abandoned. -/
private def execIn (cfg : DeployConfig) (name : String) (script : String)
    (timeoutSeconds : Nat) : IO CmdResult :=
  kubectl cfg #["exec", name, "--", "timeout", "-s", "KILL", toString timeoutSeconds,
                "sh", "-c", script]

/-! ## Operations -/

/-- The repository a deployment was created for, from the annotation `create` wrote. `none` when
    the deployment does not exist or predates the annotation. -/
private def repositoryOf (cfg : DeployConfig) (name : String) : IO (Option String) := do
  -- `-o json` rather than a jsonpath: the annotation key contains both a dot and a slash, which
  -- jsonpath needs escaped in a way that is easy to get subtly wrong and silently returns empty.
  let r ← kubectl cfg #["get", "pod", name, "-o", "json"]
  if r.exitCode != 0 then return none
  match Json.parse r.stdout with
  | .error _ => return none
  | .ok j =>
    return (do
      let md ← j.getObjVal? "metadata"
      let ann ← md.getObjVal? "annotations"
      ann.getObjValAs? String repoAnnotation) |>.toOption

/-- Refuse to touch a deployment belonging to a different repository.

    Previews from unrelated pull requests share one namespace, and `list_deployments` hands every
    name to whoever asks. Without this, a task working on one repository can tear down another's
    preview, or read its logs — which are the logs of a compose project whose environment is
    nobody else's business. `none` means the caller has no repository to check against (the CLI,
    driven by a person who can see the whole cluster anyway) and skips the check. -/
private def checkOwner (cfg : DeployConfig) (name : String) (expected : Option Repository) :
    IO (Except String Unit) := do
  match expected with
  | none => return .ok ()
  | some repo =>
    match ← repositoryOf cfg name with
    -- An unannotated or absent deployment is not claimed by anyone. `destroy` is idempotent on a
    -- name that does not exist, and refusing here would turn "already gone" into an error.
    | none => return .ok ()
    | some owner =>
      if owner == repo.toString then return .ok ()
      else return .error s!"deployment {name} belongs to {owner}, not {repo.toString}"

/-- Remove a deployment and everything belonging to it. Idempotent: deleting what is not there
    is success, because the caller's intent — that nothing of that name be running — is
    satisfied either way. -/
def destroy (cfg : DeployConfig) (name : String) (owner : Option Repository := none) :
    IO (Except String Unit) := do
  match ← checkOwner cfg name owner with
  | .error e => return .error e
  | .ok () =>
    let r ← kubectl cfg
      #["delete", "pod,service,ingress", "-l", s!"{deploymentLabel}={name}", "--ignore-not-found"]
    if r.exitCode != 0 then
      return .error (failure s!"deleting deployment {name}" r)
    return .ok ()

private def podPhase (cfg : DeployConfig) (name : String) : IO (Option String) := do
  let r ← kubectl cfg #["get", "pod", name, "-o", "jsonpath={.status.phase}"]
  if r.exitCode != 0 then return none
  let phase := r.stdout.trim
  return if phase.isEmpty then none else some phase

/-- Status of one deployment, or `none` if nothing of that name exists. -/
def status (cfg : DeployConfig) (name : String) : IO (Option Deployment) := do
  let r ← kubectl cfg #["get", "pod", name, "-o", "json"]
  if r.exitCode != 0 then return none
  match Json.parse r.stdout with
  | .error _ => return none
  | .ok j =>
    let phase := (do (← j.getObjVal? "status").getObjValAs? String "phase") |>.toOption |>.getD "Unknown"
    let expiresAt :=
      (do
        let md ← j.getObjVal? "metadata"
        let ann ← md.getObjVal? "annotations"
        ann.getObjValAs? String expiresAnnotation) |>.toOption |>.getD ""
    return some { name, url := deploymentUrl cfg name, status := phase, expiresAt }

/-- Every deployment orchestra has created in this namespace. -/
def list (cfg : DeployConfig) : IO (Except String (Array Deployment)) := do
  let r ← kubectl cfg
    #["get", "pods", "-l", "app.kubernetes.io/managed-by=orchestra", "-o", "json"]
  if r.exitCode != 0 then
    return .error (failure "listing deployments" r)
  match Json.parse r.stdout with
  | .error e => return .error s!"could not parse kubectl output: {e}"
  | .ok j =>
    let items := (j.getObjValAs? (Array Json) "items") |>.toOption |>.getD #[]
    return .ok <| items.filterMap fun item => do
      let md ← (item.getObjVal? "metadata").toOption
      let name ← (md.getObjValAs? String "name").toOption
      let phase := (do (← item.getObjVal? "status").getObjValAs? String "phase")
        |>.toOption |>.getD "Unknown"
      let expiresAt :=
        (do (← md.getObjVal? "annotations").getObjValAs? String expiresAnnotation)
          |>.toOption |>.getD ""
      some { name, url := deploymentUrl cfg name, status := phase, expiresAt }

/-- Destroy every deployment whose expiry has passed. Returns the names removed.

    Reads the expiry off the pod rather than from any state of our own: the daemon that created a
    preview is often not the one that outlives it, and a restarted daemon must still clean up. A
    pod with no annotation, or one whose annotation does not parse, is left alone — better a leak
    someone notices than a sweeper that deletes what it does not understand. -/
def gc (cfg : DeployConfig) : IO (Except String (Array String)) := do
  match ← list cfg with
  | .error e => return .error e
  | .ok deployments =>
    let now ← nowSeconds
    let mut removed : Array String := #[]
    for d in deployments do
      match parseIso d.expiresAt with
      | none => pure ()
      | some expiry =>
        if expiry <= now then
          match ← destroy cfg d.name with
          | .error _ => pure ()
          | .ok () => removed := removed.push d.name
    return .ok removed

/-- Export `spec.ref` from the daemon's clone into a tar file under `dir`.

    `git archive` rather than a copy of the working tree: it produces exactly the committed state
    of that ref, with no build output, no `.git`, and nothing the agent left lying around
    uncommitted.

    The ref reaches this from a tool argument, so it is treated as hostile all the way down.
    `validateRef` rejects anything that could be read as an option, `--end-of-options` stops git
    from looking for one anyway, and what is finally handed to `git archive` is the resolved
    commit id rather than the caller's string at all. Any one of the three would do; the reason
    for all three is that the thing being protected is the daemon's own filesystem — `git
    archive`'s `--output` happily overwrites whatever path it is given, and it wins over an
    earlier `-o`. -/
private def exportSource (spec : Spec) (dir : System.FilePath) : IO (Except String System.FilePath) := do
  match validateRef spec.ref with
  | .error e => return .error e
  | .ok ref =>
    let resolve ← run "git"
      #["-C", spec.sourcePath.toString, "rev-parse", "--verify", "--end-of-options",
        ref ++ "^{commit}"]
    if resolve.exitCode != 0 then
      return .error (failure s!"resolving {ref} in {spec.sourcePath}" resolve)
    let commit := resolve.stdout.trim
    let tarball := dir / "source.tar"
    let r ← run "git"
      #["-C", spec.sourcePath.toString, "archive", "--format=tar", "-o", tarball.toString,
        "--end-of-options", commit]
    if r.exitCode != 0 then
      return .error (failure s!"exporting {ref} ({commit}) from {spec.sourcePath}" r)
    return .ok tarball

/-- Create or replace a preview.

    Replacing rather than updating: the previous pod is destroyed first, so a re-deploy is the
    same code path as a first deploy and cannot half-apply onto a sandbox in an unknown state.
    The cost is that a preview blinks out for the length of a rebuild, which for something a
    human opens on a link is the right trade. -/
def create (cfg : DeployConfig) (spec : Spec) (startTimeoutSeconds : Nat := 600) :
    IO (Except String Deployment) := do
  let name := deploymentName spec
  let now ← nowSeconds
  let expiresAt ← toIso (now + Int.ofNat (cfg.ttlMinutes * 60))

  -- 1. Source first: a `git archive` that fails costs nothing, while a pod that starts before
  --    we know the ref exists has to be torn down again.
  let tmp ← IO.FS.createTempDir
  let sourceResult ← exportSource spec tmp
  let tarball ← match sourceResult with
    | .error e =>
      IO.FS.removeDirAll tmp
      return .error e
    | .ok p => pure p

  let cleanup : IO Unit := IO.FS.removeDirAll tmp
  -- Failures below fall into two kinds, and they are cleaned up differently on purpose.
  --
  -- Plumbing failures — the sandbox not starting, dockerd not coming up, the copy or the unpack
  -- going wrong — leave nothing worth looking at, so the pod goes with them. Leaving it would
  -- pin a multi-gigabyte Kata guest against the namespace quota until someone noticed, and the
  -- next `deploy_preview` in the namespace would be the one that failed for it.
  --
  -- A failed `docker compose up` is the other kind: the build output *is* the answer, and it is
  -- inside the sandbox. That one stays, and says so.
  let abandon (msg : String) : IO (Except String Deployment) := do
    cleanup
    let _ ← destroy cfg name
    return .error msg

  -- 2. Replace whatever was there.
  match ← destroy cfg name with
  | .error e => cleanup; return .error e
  | .ok () => pure ()

  -- 3. The sandbox.
  let applyResult ← kubectl cfg #["apply", "-f", "-"]
    (input := (manifests cfg spec name expiresAt).compress)
  if applyResult.exitCode != 0 then
    return ← abandon (failure "creating the deployment" applyResult)

  let waitResult ← kubectl cfg
    #["wait", "--for=condition=Ready", s!"pod/{name}", s!"--timeout={startTimeoutSeconds}s"]
  if waitResult.exitCode != 0 then
    return ← abandon (failure s!"waiting for the sandbox of {name} to start" waitResult)

  -- 4. Wait for the Docker daemon inside the sandbox. dind is ready when it says so and not when
  --    the pod is: the container is Running from the moment the entrypoint execs, which is well
  --    before the daemon accepts connections.
  let dockerReady ← execIn cfg name
    "for i in $(seq 1 60); do docker info >/dev/null 2>&1 && exit 0; sleep 2; done; exit 1"
    (timeoutSeconds := 180)
  if dockerReady.exitCode != 0 then
    return ← abandon (failure s!"the Docker daemon in {name} did not come up" dockerReady)

  -- 5. The source, over the same exec channel. `kubectl cp` rather than a stream through this
  --    process: a tar is binary, and Lean's strings are not.
  let mkdir ← execIn cfg name "mkdir -p /workspace" (timeoutSeconds := 60)
  if mkdir.exitCode != 0 then
    return ← abandon (failure s!"preparing /workspace in {name}" mkdir)

  let cpResult ← kubectl cfg
    #["cp", tarball.toString, s!"{cfg.ns}/{name}:/workspace/source.tar"]
  cleanup
  if cpResult.exitCode != 0 then
    let _ ← destroy cfg name
    return .error (failure s!"copying the source into {name}" cpResult)

  let untar ← execIn cfg name "cd /workspace && tar xf source.tar && rm source.tar"
    (timeoutSeconds := 300)
  if untar.exitCode != 0 then
    let _ ← destroy cfg name
    return .error (failure s!"unpacking the source in {name}" untar)

  -- 6. Hand over to the repository's own compose file, unread.
  let up ← execIn cfg name
    s!"cd /workspace && docker compose -f {shQuote spec.composeFile} -p preview up -d --build"
    (timeoutSeconds := cfg.buildTimeoutMinutes * 60)
  if up.exitCode != 0 then
    return .error <|
      failure "docker compose up" up ++
      s!"\n\nThe sandbox is still running so the build output above can be investigated; \
        destroy_preview with name {repr name} removes it."

  let phase := (← podPhase cfg name).getD "Unknown"
  return .ok { name, url := deploymentUrl cfg name, status := phase, expiresAt }

/-- Logs from a deployment's compose project, for an agent explaining why a preview is broken. -/
def logs (cfg : DeployConfig) (name : String) (tailLines : Nat := 200)
    (owner : Option Repository := none) : IO (Except String String) := do
  match ← checkOwner cfg name owner with
  | .error e => return .error e
  | .ok () =>
    let r ← execIn cfg name
      s!"cd /workspace && docker compose -p preview logs --tail={tailLines}"
      (timeoutSeconds := 120)
    if r.exitCode != 0 then
      return .error (failure s!"reading logs from {name}" r)
    return .ok r.stdout

end Orchestra.Deploy
