import Orchestra.Exec.Backend

/-!
# The Kubernetes backend: one pod per agent run

Runs each agent in a pod of its own, on a cluster the daemon talks to through `kubectl`. What
confines the agent is the pod: it sees the image's filesystem and nothing of the daemon's, its
egress is whatever the namespace's `NetworkPolicy` allows, and it is deleted when the run ends.

The daemon still does everything else. It clones, it holds the credentials, it runs the
repository's hooks and its validation script, and it parses what the agent says — so the pod has
to start from the checkout the daemon prepared and hand it back changed. That is what makes this
a backend rather than a rewrite of the task runner, and it is the one thing that costs real time:
the checkout goes in over `kubectl exec` and comes back the same way.

## the shape of a run

Two containers share the run:

* **`agent`** runs the agent. Its command waits for a go-file before starting, which is how the
  workspace gets staged into a container that is already running, and it writes its own stderr to
  a file so that stdout stays a clean event stream — a pod's log merges the two otherwise, and
  orchestra reads them for different things.
* **`workspace`** does nothing but stay alive until released. It exists because `kubectl exec`
  needs a running container, and the agent's is gone by the time there is anything to copy back.

```
  create secret + pod ─► wait Ready ─► stage checkout in ─► touch go
                                                              │
        stdout ◄── kubectl logs -f -c agent                    ▼
        stderr ◄── kubectl exec -c workspace -- tail -f     the agent runs
                                                              │
   exit code ◄── pod status ◄──────────────────────────────────┘
        │
        └─► copy the checkout back ─► touch release ─► delete pod + secret
```

Cancellation deletes the pod, which ends both streams; nothing is copied back, on the grounds
that a cancelled run's tree is not something anyone is waiting for.

## what the image has to have

`sh`, `tar` and `nc` (for the MCP transport), plus the agent CLI itself and whatever the
repository needs to build. This is the same list the daemon's own machine needs, minus `landrun`.
-/

namespace Orchestra.Exec.Kubernetes

open Lean (Json)
open Orchestra.Exec

/-! ## Configuration -/

/-- What this backend needs from `execution.options`.

    ```json
    "execution": {
      "backend": "kubernetes",
      "options": {
        "image": "ghcr.io/example/orchestra-agent:latest",
        "namespace": "orchestra",
        "mcp_host": "orchestra.orchestra.svc.cluster.local"
      }
    }
    ```
-/
structure Config where
  /-- The `kubectl` binary. Everything this backend does goes through it: implementing the API
      directly would mean implementing `exec`'s stream protocol, and `kubectl` is on every machine
      that already talks to a cluster. -/
  kubectl : String := "kubectl"
  /-- Namespace the run's pod and secret are created in. -/
  ns : String := "default"
  /-- Image the agent runs in. Required: there is no sensible default, and guessing one would mean
      a pod that starts and cannot run the agent. -/
  image : String
  /-- Image for the `workspace` container. Defaults to `image`, which already has `tar`. -/
  sidecarImage : Option String := none
  /-- Service account for the pod. Omitted means the namespace's default. -/
  serviceAccount : Option String := none
  /-- `imagePullSecrets` names. -/
  imagePullSecrets : Array String := #[]
  /-- `nodeSelector`, verbatim. -/
  nodeSelector : Option Json := none
  /-- `resources` for the agent container, verbatim. -/
  resources : Option Json := none
  /-- Extra `volumes`, verbatim — a PVC for a build cache, most usefully. -/
  extraVolumes : Array Json := #[]
  /-- Extra `volumeMounts` for the agent container, verbatim. -/
  extraMounts : Array Json := #[]
  /-- Where the pod reaches the daemon's MCP server. Required, and required to be routable *from
      the cluster*: a Service that fronts the daemon, or the address of the machine it runs on.
      Without it the agent has no tools — it cannot open a pull request, comment, or claim an
      issue — and nothing about the run says so. -/
  mcpHost : String
  /-- Address the daemon's MCP server binds so the cluster can reach it. `0.0.0.0` because the
      daemon is usually itself in a pod, where the interface it should bind is not knowable by
      name. Every connection is authenticated with a per-run token whatever this is set to. -/
  mcpBind : String := "0.0.0.0"
  /-- `activeDeadlineSeconds`: the cluster kills the pod after this, whatever the daemon is doing.
      The backstop for a daemon that dies mid-run and leaves a pod holding a token. -/
  deadlineSeconds : Nat := 14400
  /-- How long to wait for the pod to be ready before giving up on the run. Image pulls on a cold
      node are the reason this is minutes rather than seconds. -/
  startupTimeoutSeconds : Nat := 600
  /-- Whether the checkout is copied back out when the run ends. Off means the repository's
      validation script, and every `git` command the daemon runs afterwards, see the tree as it
      was before the agent touched it — so off is only right when the agent pushes and nothing
      local reads the result. -/
  syncBack : Bool := true
  /-- Paths not carried in either direction, as `tar --exclude` patterns. Build output is the
      usual candidate: `.lake`, `target`, `node_modules`. -/
  excludes : Array String := #[]
  /-- Where the agent's `$HOME` lives in the pod. -/
  homePath : String := "/home/agent"
  /-- A `PersistentVolumeClaim` to mount as the agent's `$HOME`, instead of an `emptyDir` that
      lives and dies with the pod.

      What this buys is the retry loop. A run that fails its repository's validation script is
      relaunched with `--resume <session>`, and the session it names is a file the agent CLI wrote
      under its home — in the previous pod, which no longer exists. With a claim, the second
      attempt picks up the conversation exactly as it does on a machine where every task shares
      one `$HOME`, which is what the landrun backend has always done.

      Needs `ReadWriteMany`, or `ReadWriteOnce` with every agent pod on one node: parallel runs
      mount it at the same time. -/
  homeClaim : Option String := none
deriving Inhabited

/-- Where the two containers meet: the go-file, the captured stderr, the exit code, the release
    flag. An `emptyDir` mounted in both. -/
def controlPath : String := "/orchestra"

private def jsonArr? (j : Json) (key : String) : Array Json :=
  match j.getObjVal? key with
  | .ok (.arr a) => a
  | _            => #[]

private def jsonStr? (j : Json) (key : String) : Option String :=
  j.getObjValAs? String key |>.toOption

/-- Read the backend's settings out of `execution.options`.

    Strict about the two settings that cannot be guessed and loose about the rest: a missing
    `image` or `mcp_host` is a configuration that cannot work, and finding that out at the first
    dispatched task — as a pod that runs an agent with no tools — is exactly the failure this
    refuses to allow. -/
def Config.fromJson (j : Json) : Except String Config := do
  let image ← match jsonStr? j "image" with
    | some i => pure i
    | none   => throw "kubernetes: execution.options.image is required — the image the agent runs \
in (it needs the agent CLI, sh, tar and nc)"
  let mcpHost ← match jsonStr? j "mcp_host" with
    | some h => pure h
    | none   => throw "kubernetes: execution.options.mcp_host is required — the address the pod \
reaches this daemon's MCP server at (a Service name, or the daemon's host). Without it the agent \
runs with no tools at all"
  let nat (key : String) (dflt : Nat) : Nat :=
    j.getObjValAs? Nat key |>.toOption |>.getD dflt
  return {
    kubectl := (jsonStr? j "kubectl").getD "kubectl"
    ns := (jsonStr? j "namespace").getD "default"
    image
    sidecarImage := jsonStr? j "sidecar_image"
    serviceAccount := jsonStr? j "service_account"
    imagePullSecrets := (jsonArr? j "image_pull_secrets").filterMap fun v =>
      match v with | .str s => some s | _ => none
    nodeSelector := j.getObjVal? "node_selector" |>.toOption
    resources := j.getObjVal? "resources" |>.toOption
    extraVolumes := jsonArr? j "volumes"
    extraMounts := jsonArr? j "volume_mounts"
    mcpHost
    mcpBind := (jsonStr? j "mcp_bind").getD "0.0.0.0"
    deadlineSeconds := nat "deadline_seconds" 14400
    startupTimeoutSeconds := nat "startup_timeout_seconds" 600
    syncBack := j.getObjValAs? Bool "sync_back" |>.toOption |>.getD true
    excludes := (jsonArr? j "excludes").filterMap fun v =>
      match v with | .str s => some s | _ => none
    homePath := (jsonStr? j "home_path").getD "/home/agent"
    homeClaim := jsonStr? j "home_claim"
  }

/-! ## The pod

Rendering is pure, and tested that way (`OrchestraTest/KubernetesTest.lean`). What a pod is
allowed to do is the same kind of statement as what a landrun ruleset is allowed to do, and
checking it should not need a cluster. -/

/-- The agent container's command.

    Three things it has to do that a bare `exec` would not. Wait for the workspace to be staged,
    which cannot happen before the container is running. Send stderr to a file, because a pod's
    log merges it with stdout and orchestra reads the two for different things — an event stream
    on one, a usage limit on the other. And record the exit code where the `workspace` container
    can still be asked for it. -/
def agentScript (ctl : String := controlPath) : String :=
  "set -u\n\
   while [ ! -f " ++ ctl ++ "/go ]; do sleep 0.2; done\n\
   \"$@\" 2>" ++ ctl ++ "/stderr\n\
   code=$?\n\
   echo \"$code\" > " ++ ctl ++ "/exit\n\
   exit $code\n"

/-- The `workspace` container's command: stay alive until the daemon has taken back what it
    needs. Without it the pod would be gone at the moment there is finally something to copy. -/
def sidecarScript (ctl : String := controlPath) : String :=
  "set -u\n\
   while [ ! -f " ++ ctl ++ "/release ]; do sleep 1; done\n"

/-- The command that streams the agent's stderr out of the pod.

    Ends itself when the agent does, rather than being killed from outside: the daemon waits for
    both streams to reach EOF before it collects the exit code, so a `tail -f` that outlived the
    run would hang the task. -/
def stderrScript (ctl : String := controlPath) : String :=
  "ctl=" ++ ctl ++ "\n\
   while [ ! -f \"$ctl/stderr\" ] && [ ! -f \"$ctl/exit\" ]; do sleep 0.2; done\n\
   [ -f \"$ctl/stderr\" ] || exit 0\n\
   tail -n +1 -f \"$ctl/stderr\" &\n\
   tp=$!\n\
   while [ ! -f \"$ctl/exit\" ]; do sleep 0.5; done\n\
   sleep 0.5\n\
   kill $tp 2>/dev/null\n\
   exit 0\n"

/-- A volume name for the `i`th staged path. Kubernetes names have to be a DNS label, and a
    filesystem path is not one. -/
def stageVolumeName (i : Nat) : String := s!"stage-{i}"

/-- One path orchestra carries into the pod, and what has to happen to it afterwards. -/
structure StagedPath where
  /-- Where it is on the daemon's disk. -/
  hostPath : String
  /-- Where it is mounted in the pod. The same string unless the grant was home-relative, since a
      checkout at `/var/lib/orchestra/work/...` mounted at that same path keeps every log line,
      message and prompt that mentions it true. -/
  podPath : String
  /-- Whether the agent may write to it, and so whether anything has to come back. -/
  writable : Bool
  /-- Whether this is the run's working directory — the checkout. It is the one path that is
      replaced wholesale on the way back rather than merged; see `syncOut`. -/
  isWorkspace : Bool
deriving Repr, BEq, Inhabited

/-- The paths orchestra has to carry into the pod: the checkout, and any plugin or memory
    directory the run was granted. Everything else a `RunSpec` names is the image's to provide. -/
def stagedPaths (cfg : Config) (hostHome : String) (spec : RunSpec) : Array StagedPath :=
  spec.orchestraGrants.map fun g =>
    let hostPath := (PathGrant.resolve hostHome g).path
    let podPath  := (PathGrant.resolve cfg.homePath g).path
    { hostPath, podPath
      writable := g.access == .rw || g.access == .rwx
      isWorkspace := podPath == spec.workdir.toString }

/-- The pod manifest for `spec`. -/
def podManifest (cfg : Config) (spec : RunSpec) (podName secretName : String)
    (staged : Array StagedPath) : Json :=
  let stageMounts : Array Json := staged.mapIdx fun i st =>
    Json.mkObj [("name", .str (stageVolumeName i)), ("mountPath", .str st.podPath)]
  let stageVolumes : Array Json := staged.mapIdx fun i _ =>
    Json.mkObj [("name", .str (stageVolumeName i)), ("emptyDir", Json.mkObj [])]
  let controlMount := Json.mkObj [("name", .str "control"), ("mountPath", .str controlPath)]
  let homeMount := Json.mkObj [("name", .str "home"), ("mountPath", .str cfg.homePath)]
  let homeVolume := match cfg.homeClaim with
    | some claim => Json.mkObj [("name", .str "home"),
        ("persistentVolumeClaim", Json.mkObj [("claimName", .str claim)])]
    | none       => Json.mkObj [("name", .str "home"), ("emptyDir", Json.mkObj [])]
  let volumes : Array Json :=
    stageVolumes
      ++ #[Json.mkObj [("name", .str "control"), ("emptyDir", Json.mkObj [])], homeVolume]
      ++ cfg.extraVolumes
  -- `HOME` is set here rather than passed through: the image's idea of home is not orchestra's,
  -- and every home-relative path the agent backend declared was resolved against `homePath`.
  let env : Array Json :=
    #[Json.mkObj [("name", .str "HOME"), ("value", .str cfg.homePath)]]
  let agentContainer := Json.mkObj ([
    ("name", .str "agent"),
    ("image", .str cfg.image),
    -- `$0` is a label for the shell's own error messages; the agent's argv starts at `$1`.
    ("command", .arr (#[.str "/bin/sh", .str "-c", .str agentScript, .str "orchestra-agent",
                        .str spec.command] ++ spec.args.map Json.str)),
    ("workingDir", .str spec.workdir.toString),
    ("env", .arr env),
    ("envFrom", .arr #[Json.mkObj [("secretRef", Json.mkObj [("name", .str secretName)])]]),
    ("volumeMounts", .arr (stageMounts ++ #[controlMount, homeMount] ++ cfg.extraMounts))
  ] ++ (match cfg.resources with | some r => [("resources", r)] | none => []))
  let sidecarContainer := Json.mkObj [
    ("name", .str "workspace"),
    ("image", .str (cfg.sidecarImage.getD cfg.image)),
    ("command", .arr #[.str "/bin/sh", .str "-c", .str sidecarScript]),
    ("volumeMounts", .arr (stageMounts ++ #[controlMount]))
  ]
  let podSpec := Json.mkObj ([
    ("restartPolicy", .str "Never"),
    ("activeDeadlineSeconds", .num cfg.deadlineSeconds),
    ("containers", .arr #[agentContainer, sidecarContainer]),
    ("volumes", .arr volumes)
  ] ++ (match cfg.serviceAccount with
        | some sa => [("serviceAccountName", Json.str sa)] | none => [])
     ++ (match cfg.nodeSelector with | some n => [("nodeSelector", n)] | none => [])
     ++ (if cfg.imagePullSecrets.isEmpty then [] else
          [("imagePullSecrets", .arr (cfg.imagePullSecrets.map fun n =>
            Json.mkObj [("name", .str n)]))]))
  Json.mkObj [
    ("apiVersion", .str "v1"),
    ("kind", .str "Pod"),
    ("metadata", Json.mkObj [
      ("name", .str podName),
      ("namespace", .str cfg.ns),
      ("labels", Json.mkObj [
        ("app.kubernetes.io/managed-by", .str "orchestra"),
        ("orchestra.dev/run", .str podName)])]),
    ("spec", podSpec)]

/-- The secret carrying the run's environment.

    The environment is where the credentials are — the installation token, the agent's API key —
    and a pod's own spec is readable by anything that can list pods in the namespace. A `Secret`
    is not much better on its own, but it is separately access-controlled, it stays out of
    `kubectl get pod -o yaml`, and it is deleted with the run. -/
def secretManifest (cfg : Config) (spec : RunSpec) (secretName podName : String) : Json :=
  Json.mkObj [
    ("apiVersion", .str "v1"),
    ("kind", .str "Secret"),
    ("type", .str "Opaque"),
    ("metadata", Json.mkObj [
      ("name", .str secretName),
      ("namespace", .str cfg.ns),
      ("labels", Json.mkObj [
        ("app.kubernetes.io/managed-by", .str "orchestra"),
        ("orchestra.dev/run", .str podName)])]),
    ("stringData", Json.mkObj (spec.env.toList.map fun (k, v) => (k, Json.str v)))]

/-- `tar` flags for the excluded paths, in the order they were configured. -/
def excludeArgs (cfg : Config) : Array String :=
  cfg.excludes.flatMap fun p => #["--exclude", p]

/-! ## Talking to the cluster -/

/-- Run `kubectl` and collect what it said. Nothing is passed on stdin anywhere in this backend —
    manifests go through a temporary file and the two `tar` pipelines are built inside a shell —
    because a Lean-side pipe would have to be closed to signal EOF and there is no way to say so. -/
private def kube (cfg : Config) (args : Array String) : IO (UInt32 × String × String) := do
  let out ← IO.Process.output { cmd := cfg.kubectl, args := #["-n", cfg.ns] ++ args }
  return (out.exitCode, out.stdout, out.stderr)

/-- Run a shell pipeline, for the two places one is genuinely needed: `tar` into `kubectl exec`
    and back out again. Every interpolated path goes through `shellEscape`. -/
private def shell (script : String) : IO (UInt32 × String × String) := do
  let out ← IO.Process.output { cmd := "/bin/sh", args := #["-c", script] }
  return (out.exitCode, out.stdout, out.stderr)

/-- Run `sh -c` inside the run's `workspace` container. -/
private def podShell (cfg : Config) (podName script : String) : IO (UInt32 × String × String) :=
  kube cfg #["exec", podName, "-c", "workspace", "--", "/bin/sh", "-c", script]

/-- Apply a manifest by writing it to a temporary file and handing `kubectl` the path.

    Through a file rather than stdin because a Lean-side pipe has no way to say EOF, and inside a
    directory narrowed to this user first because one of the two manifests is the run's `Secret`,
    which carries the installation token and the agent's API key. The narrowing happens before
    anything is written, so there is no moment when the file is both present and world-readable. -/
private def apply (cfg : Config) (manifest : Json) (what : String) : IO Unit := do
  let dir := System.FilePath.mk s!"/tmp/orchestra-k8s-{← randomHex 8}"
  IO.FS.createDirAll dir
  let _ ← IO.Process.output { cmd := "chmod", args := #["700", dir.toString] }
  let path := dir / "manifest.json"
  IO.FS.writeFile path manifest.compress
  let (code, _, err) ← kube cfg #["create", "-f", path.toString]
  try IO.FS.removeFile path catch _ => pure ()
  try IO.FS.removeDirAll dir catch _ => pure ()
  if code != 0 then
    throw (IO.userError s!"kubernetes: could not create the run's {what}: {err.trimAscii}")

/-- Delete the run's pod and secret. Best-effort and idempotent: it is called on the normal path,
    on cancellation, and on a failed start, and on two of those something may already be gone. -/
private def deleteRun (cfg : Config) (podName secretName : String) : IO Unit := do
  let _ ← try kube cfg #["delete", "pod", podName, "--now", "--wait=false",
                         "--ignore-not-found"] catch _ => pure (0, "", "")
  let _ ← try kube cfg #["delete", "secret", secretName, "--wait=false",
                         "--ignore-not-found"] catch _ => pure (0, "", "")

/-- Why a pod never became ready, in as much detail as the cluster will give. What a person needs
    here is the image pull error or the unschedulable message, not "timed out". -/
private def startupDiagnosis (cfg : Config) (podName : String) : IO String := do
  let (_, phase, _) ← kube cfg #["get", "pod", podName, "-o",
    "jsonpath={.status.phase}{\" \"}{.status.containerStatuses[*].state.waiting.reason}\
{\" \"}{.status.containerStatuses[*].state.waiting.message}\
{\" \"}{.status.conditions[?(@.type==\"PodScheduled\")].message}"]
  return phase.trimAscii.toString

/-- Copy one directory into the pod. Skipped, not failed, when the source is not there: an agent
    backend may declare a plugin directory this machine does not have, exactly as with landrun. -/
private def stageIn (cfg : Config) (podName hostPath podPath : String) : IO Unit := do
  unless ← System.FilePath.pathExists (System.FilePath.mk hostPath) do return ()
  let excludes := String.intercalate " " (excludeArgs cfg).toList
  let script := s!"set -o pipefail 2>/dev/null; tar -C {shellEscape hostPath} {excludes} -cf - . | \
{shellEscape cfg.kubectl} -n {shellEscape cfg.ns} exec -i {shellEscape podName} -c workspace -- \
tar -C {shellEscape podPath} -xf -"
  let (code, _, err) ← shell script
  if code != 0 then
    throw (IO.userError s!"kubernetes: could not copy {hostPath} into the pod: {err.trimAscii}")

/-- Copy one directory back out of the pod, replacing what is on disk.

    The workspace is swapped rather than extracted over: `tar` never deletes, so extracting on top
    of the old tree would resurrect every file the agent removed, and the validation script would
    then be run against a tree that never existed anywhere. The swap keeps the original in place
    until the new one is complete, so a transfer that fails leaves the checkout as it was.

    `merge` is for the paths where a swap would be wrong — memory directories, which other tasks
    may be writing to at the same time. There, a file the agent deleted survives, which is the
    lesser mistake. -/
private def syncOut (cfg : Config) (podName hostPath podPath : String) (merge : Bool) : IO Unit := do
  let kubectlTar := s!"{shellEscape cfg.kubectl} -n {shellEscape cfg.ns} exec {shellEscape podName} \
-c workspace -- tar -C {shellEscape podPath} {String.intercalate " " (excludeArgs cfg).toList} -cf - ."
  if merge then
    unless ← System.FilePath.pathExists (System.FilePath.mk hostPath) do return ()
    let (code, _, err) ← shell s!"{kubectlTar} | tar -C {shellEscape hostPath} -xf -"
    if code != 0 then
      IO.eprintln s!"  [k8s] warning: could not copy {podPath} back out of the pod: {err.trimAscii}"
    return ()
  let incoming := hostPath ++ ".orchestra-incoming"
  let previous := hostPath ++ ".orchestra-previous"
  let script := s!"set -e\n\
rm -rf {shellEscape incoming} {shellEscape previous}\n\
mkdir -p {shellEscape incoming}\n\
{kubectlTar} | tar -C {shellEscape incoming} -xf -\n\
if [ -d {shellEscape hostPath} ]; then mv {shellEscape hostPath} {shellEscape previous}; fi\n\
mv {shellEscape incoming} {shellEscape hostPath}\n\
rm -rf {shellEscape previous}\n"
  let (code, _, err) ← shell script
  if code != 0 then
    let _ ← shell s!"rm -rf {shellEscape incoming}"
    IO.eprintln s!"  [k8s] warning: could not copy the workspace back out of the pod, so \
{hostPath} still holds what the agent started from — validation and any git command the daemon \
runs will see the tree unchanged: {err.trimAscii}"

/-- The agent container's exit code, once the cluster has recorded it.

    Polled rather than waited on because the daemon gets here the moment the log stream ends, and
    the container status trails that by however long the kubelet takes. A pod that was killed
    outright — deleted, or over its `activeDeadlineSeconds` — never records one, which is reported
    as a failure rather than waited on forever. -/
private def awaitExitCode (cfg : Config) (podName : String) : IO UInt32 := do
  let mut waited := 0
  let mut misses := 0
  while waited < 120000 do
    let (code, out, _) ← kube cfg #["get", "pod", podName, "-o",
      "jsonpath={.status.containerStatuses[?(@.name==\"agent\")].state.terminated.exitCode}"]
    if code == 0 then
      misses := 0
      match out.trimAscii.toString.toNat? with
      | some n => return UInt32.ofNat n
      | none   =>
        let (_, phase, _) ← kube cfg #["get", "pod", podName, "-o", "jsonpath={.status.phase}"]
        if phase.trimAscii.toString == "Failed" then return 1
    else
      -- The pod is gone — deleted under us, or garbage-collected — and nothing more will ever be
      -- recorded about it. Three times, because a single failed `get` is as likely to be the API
      -- server having a moment as it is to be a missing pod.
      misses := misses + 1
      if misses >= 3 then return 137
    IO.sleep 500
    waited := waited + 500
  IO.eprintln s!"  [k8s] warning: pod {podName} never reported an exit code for its agent \
container; treating the run as failed."
  return 1

/-! ## The backend -/

/-- Start `spec` as a pod, and hand back the streams and the lifecycle. -/
def start (cfg : Config) (spec : RunSpec) : IO Handle := do
  if spec.stdio == .inherit then
    throw (IO.userError "kubernetes: this backend cannot host an interactive session — the agent runs in a pod, with no terminal to hand it. Run `orchestra interactive` on a machine configured for the landrun backend, or attach to a queued run's pod with `kubectl attach` yourself.")
  let home ← hostHome
  let staged := stagedPaths cfg home spec
  let suffix ← randomHex 6
  let podName := s!"orchestra-{suffix}"
  let secretName := s!"orchestra-{suffix}-env"
  apply cfg (secretManifest cfg spec secretName podName) "secret"
  try
    apply cfg (podManifest cfg spec podName secretName staged) "pod"
  catch e =>
    deleteRun cfg podName secretName
    throw e
  -- Ready, not Running: `kubectl exec` needs a container that has actually started, and the next
  -- thing this does is copy a repository through one.
  let (code, _, err) ← kube cfg #["wait", "--for=condition=Ready", s!"pod/{podName}",
    s!"--timeout={cfg.startupTimeoutSeconds}s"]
  if code != 0 then
    let why ← startupDiagnosis cfg podName
    deleteRun cfg podName secretName
    throw (IO.userError s!"kubernetes: pod {podName} did not become ready within \
{cfg.startupTimeoutSeconds}s ({why.trimAscii}): {err.trimAscii}")
  try
    for st in staged do
      stageIn cfg podName st.hostPath st.podPath
  catch e =>
    deleteRun cfg podName secretName
    throw e
  -- stdout is the agent container's log, which ends when that container does. stderr is the file
  -- the wrapper redirected it to, tailed out of the sidecar; that command ends itself when the
  -- agent exits, because the daemon drains both streams before it collects the exit code.
  let logs ← IO.Process.spawn {
    cmd := cfg.kubectl
    args := #["-n", cfg.ns, "logs", "-f", podName, "-c", "agent"]
    stdin := .null, stdout := .piped, stderr := .piped }
  let errs ← IO.Process.spawn {
    cmd := cfg.kubectl
    args := #["-n", cfg.ns, "exec", podName, "-c", "workspace", "--", "/bin/sh", "-c", stderrScript]
    stdin := .null, stdout := .piped, stderr := .piped }
  let (goCode, _, goErr) ← podShell cfg podName s!"touch {controlPath}/go"
  if goCode != 0 then
    -- The two stream readers were spawned a moment ago and would otherwise outlive the run they
    -- were reading, holding pipes nobody will ever read from.
    Handle.killPid logs.pid
    Handle.killPid errs.pid
    deleteRun cfg podName secretName
    throw (IO.userError s!"kubernetes: could not start the agent in pod {podName}: {goErr.trimAscii}")
  -- The stderr streamer ends itself when the wrapper records an exit code, which covers every
  -- way an agent can finish on its own. It does not cover a container that is killed outright —
  -- an OOM kill, the node going away — where nothing is ever written and a `tail -f` would wait
  -- for a file that is never touched, hanging the task at the point where the daemon drains the
  -- streams. The agent's log stream ending is the one signal that is always delivered, so it is
  -- what retires the streamer, a second later so the last lines still make it out.
  let _monitor ← IO.asTask (prio := .dedicated) do
    let _ ← logs.wait
    IO.sleep 1000
    Handle.killPid errs.pid
  let finished ← IO.mkRef false
  let killed ← IO.mkRef false
  return {
    stdout := some logs.stdout
    stderr := some errs.stdout
    id := s!"pod {cfg.ns}/{podName}"
    wait := do
      if ← finished.get then
        -- Already torn down, which at this point means cancelled: the pod was deleted before it
        -- could report anything, and 137 is what a killed process reports.
        return (if ← killed.get then 137 else 0)
      finished.set true
      let exitCode ← awaitExitCode cfg podName
      if cfg.syncBack then
        for st in staged do
          if st.writable then
            syncOut cfg podName st.hostPath st.podPath (merge := !st.isWorkspace)
      let _ ← podShell cfg podName s!"touch {controlPath}/release"
      deleteRun cfg podName secretName
      return exitCode
    kill := do
      -- Nothing is copied back: a cancelled run is one nobody is waiting on the tree of, and the
      -- pod has to go now rather than after a transfer that may take minutes. Deleting it is also
      -- what ends both streams, so the daemon's own teardown can proceed.
      killed.set true
      finished.set true
      deleteRun cfg podName secretName }

/-- The run as the commands that produce it, plus the manifest itself. `--debug` on this backend
    has to answer "what did you ask the cluster for", and the manifest is that answer. -/
def describe (cfg : Config) (spec : RunSpec) : IO String := do
  let home ← hostHome
  let staged := stagedPaths cfg home spec
  let manifest := podManifest cfg spec "orchestra-<run>" "orchestra-<run>-env" staged
  let stages := String.intercalate "\n" (staged.toList.map fun st =>
    s!"[debug]   tar -C {shellEscape st.hostPath} -cf - . | {cfg.kubectl} -n {cfg.ns} exec -i \
orchestra-<run> -c workspace -- tar -C {shellEscape st.podPath} -xf -")
  return s!"[debug] {cfg.kubectl} -n {cfg.ns} create -f - <<'EOF'\n\
{manifest.pretty}\n\
EOF\n\
[debug] staged into the pod:\n{stages}\n\
[debug] {cfg.kubectl} -n {cfg.ns} logs -f orchestra-<run> -c agent"

/-- Check that `kubectl` is here and that it may do what this backend does.

    The permission check is `create pods` alone, which is the one that fails first and the one an
    operator most often forgets; the rest of the verbs are named in the error so that fixing it is
    a single edit to a Role rather than a sequence of failed tasks. -/
def preflight (cfg : Config) : IO (Except String Unit) := do
  try
    let version ← IO.Process.output { cmd := cfg.kubectl, args := #["version", "--client=true"] }
    if version.exitCode != 0 then
      return .error s!"'{cfg.kubectl}' could not be run: {version.stderr.trimAscii}"
  catch _ =>
    return .error s!"'{cfg.kubectl}' is not on PATH. This backend drives the cluster through it."
  let (code, out, err) ← kube cfg #["auth", "can-i", "create", "pods"]
  if code != 0 || out.trimAscii.toString != "yes" then
    return .error s!"this daemon may not create pods in namespace '{cfg.ns}' \
({(out ++ err).trimAscii}). It needs create/get/list/delete on pods, create/delete on secrets, \
and get on pods/log and create on pods/exec."
  return .ok ()

/-- Kubernetes as an execution backend. -/
def factory : BackendFactory where
  name := "kubernetes"
  summary := "one pod per run, on a cluster reached through kubectl"
  make options := do
    let cfg ← Config.fromJson options
    return {
      name := "kubernetes"
      -- The agent is off this machine, so the MCP server has to listen somewhere it can reach —
      -- and every connection to it then carries a per-run token, minted with the server.
      exposure := .network cfg.mcpBind
      mcpEndpoint := fun e => pure { e with host := cfg.mcpHost }
      preflight := preflight cfg
      describe := describe cfg
      start := start cfg }

end Orchestra.Exec.Kubernetes
