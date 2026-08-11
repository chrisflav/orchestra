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
  s!"{String.mk clipped}-{shortHash identity}"

/-- Hostname a deployment answers on. -/
def deploymentUrl (cfg : DeployConfig) (name : String) : String :=
  s!"https://{name}.{cfg.baseDomain}"

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
    , ("resources", Json.mkObj
        [ ("limits", Json.mkObj
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
    , ("spec", Json.mkObj
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
  let stdout ← child.stdout.readToEnd
  let stderr ← child.stderr.readToEnd
  let exitCode ← child.wait
  return { exitCode, stdout, stderr }

private def failure (what : String) (r : CmdResult) : String :=
  let detail :=
    if !r.stderr.trim.isEmpty then r.stderr.trim
    else if !r.stdout.trim.isEmpty then r.stdout.trim
    else "(no output)"
  s!"{what} failed (exit {r.exitCode}):\n{detail}"

private def kubectl (cfg : DeployConfig) (args : Array String) (input : Option String := none) :
    IO CmdResult :=
  run cfg.kubectl (#["--kubeconfig", cfg.kubeconfig, "-n", cfg.ns] ++ args) input

/-- `kubectl exec` into a deployment's pod. Every command that touches the compose project goes
    through here, which is the reason the pod needs no network path back to us. -/
private def execIn (cfg : DeployConfig) (name : String) (script : String) : IO CmdResult :=
  kubectl cfg #["exec", name, "--", "sh", "-c", script]

/-! ## Operations -/

/-- Remove a deployment and everything belonging to it. Idempotent: deleting what is not there
    is success, because the caller's intent — that nothing of that name be running — is
    satisfied either way. -/
def destroy (cfg : DeployConfig) (name : String) : IO (Except String Unit) := do
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
    uncommitted. -/
private def exportSource (spec : Spec) (dir : System.FilePath) : IO (Except String System.FilePath) := do
  let tarball := dir / "source.tar"
  let r ← run "git"
    #["-C", spec.sourcePath.toString, "archive", "--format=tar", "-o", tarball.toString, spec.ref]
  if r.exitCode != 0 then
    return .error (failure s!"exporting {spec.ref} from {spec.sourcePath}" r)
  return .ok tarball

/-- Create or replace a preview.

    Replacing rather than updating: the previous pod is destroyed first, so a re-deploy is the
    same code path as a first deploy and cannot half-apply onto a sandbox in an unknown state.
    The cost is that a preview blinks out for the length of a rebuild, which for something a
    human opens on a link is the right trade. -/
def create (cfg : DeployConfig) (spec : Spec) (timeoutSeconds : Nat := 600) :
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

  -- 2. Replace whatever was there.
  match ← destroy cfg name with
  | .error e => cleanup; return .error e
  | .ok () => pure ()

  -- 3. The sandbox.
  let applyResult ← kubectl cfg #["apply", "-f", "-"]
    (input := (manifests cfg spec name expiresAt).compress)
  if applyResult.exitCode != 0 then
    cleanup
    return .error (failure "creating the deployment" applyResult)

  let waitResult ← kubectl cfg
    #["wait", "--for=condition=Ready", s!"pod/{name}", s!"--timeout={timeoutSeconds}s"]
  if waitResult.exitCode != 0 then
    cleanup
    return .error <|
      failure s!"waiting for the sandbox of {name} to start" waitResult ++
      s!"\n\nThe pod is left in place for inspection; call destroy_preview with name \
        {repr name} when you are done with it."

  -- 4. Wait for the Docker daemon inside the sandbox. dind is ready when it says so and not when
  --    the pod is: the container is Running from the moment the entrypoint execs, which is well
  --    before the daemon accepts connections.
  let dockerReady ← execIn cfg name
    "for i in $(seq 1 60); do docker info >/dev/null 2>&1 && exit 0; sleep 2; done; exit 1"
  if dockerReady.exitCode != 0 then
    cleanup
    return .error (failure s!"the Docker daemon in {name} did not come up" dockerReady)

  -- 5. The source, over the same exec channel. `kubectl cp` rather than a stream through this
  --    process: a tar is binary, and Lean's strings are not.
  let mkdir ← execIn cfg name "mkdir -p /workspace"
  if mkdir.exitCode != 0 then
    cleanup
    return .error (failure s!"preparing /workspace in {name}" mkdir)

  let cpResult ← kubectl cfg
    #["cp", tarball.toString, s!"{cfg.ns}/{name}:/workspace/source.tar"]
  cleanup
  if cpResult.exitCode != 0 then
    return .error (failure s!"copying the source into {name}" cpResult)

  let untar ← execIn cfg name "cd /workspace && tar xf source.tar && rm source.tar"
  if untar.exitCode != 0 then
    return .error (failure s!"unpacking the source in {name}" untar)

  -- 6. Hand over to the repository's own compose file, unread.
  let up ← execIn cfg name
    s!"cd /workspace && docker compose -f {spec.composeFile} -p preview up -d --build"
  if up.exitCode != 0 then
    return .error <|
      failure "docker compose up" up ++
      s!"\n\nThe sandbox is still running so the build output above can be investigated; \
        destroy_preview with name {repr name} removes it."

  let phase := (← podPhase cfg name).getD "Unknown"
  return .ok { name, url := deploymentUrl cfg name, status := phase, expiresAt }

/-- Logs from a deployment's compose project, for an agent explaining why a preview is broken. -/
def logs (cfg : DeployConfig) (name : String) (tailLines : Nat := 200) :
    IO (Except String String) := do
  let r ← execIn cfg name s!"cd /workspace && docker compose -p preview logs --tail={tailLines}"
  if r.exitCode != 0 then
    return .error (failure s!"reading logs from {name}" r)
  return .ok r.stdout

end Orchestra.Deploy
