import Orchestra.Exec.Backend

/-!
# The Kubernetes backend: one pod per task

Each task gets a pod, and everything the task does happens inside it: the repository's `init.sh`
and `before.sh`, the agent, `validation.sh`, the retry the agent gets when that fails, and
`after.sh`. The pod is a long-lived container that does nothing on its own; each of those is a
`kubectl exec` into it.

That the pod belongs to the *task* rather than to one agent launch is what makes the rest work:

* **`validation.sh` runs where the work happened.** It decides whether the agent is finished, and
  it is the repository's build — on the daemon's own machine there is no toolchain to run it with,
  and no reason to think a tree copied out would build the same way.
* **A retry resumes.** `--resume <session>` names a file the agent CLI wrote where it ran, so a
  second attempt has to run in the same place as the first.
* **The checkout is carried once**, not once per attempt.

An interactive session is the same mechanism with a terminal: `kubectl exec -it`, the daemon's own
streams handed straight through.

```
  open ─► create pod ─► wait Ready ─► stage the checkout in
                                              │
       init.sh / before.sh ── kubectl exec ────┤
       the agent ─────────── kubectl exec ────┤    (stdout and stderr arrive separately)
       validation.sh ─────── kubectl exec ────┤
       after.sh ──────────── kubectl exec ────┘
                                              │
  close ─► copy the checkout back ─► delete the pod
```

Cancelling a task deletes the pod, which ends whatever was running in it.

## what the image has to have

`sh`, `bash` (the repository's scripts are run with it), `tar`, `nc` for the MCP transport, `git`,
the agent CLI, and whatever the repositories being worked on need to build. The same list the
daemon's own machine needs, minus `landrun`.

## credentials

Nothing sensitive is passed on a command line or written into the pod's spec, both of which are
readable by anything that can watch the cluster. The environment for each command is written to a
file inside the pod — over the same `tar` channel the checkout travels on — and sourced there.
-/

namespace Orchestra.Exec.Kubernetes

open Lean (Json fromJson?)
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
  /-- Namespace the task's pod is created in. -/
  ns : String := "default"
  /-- Image a task runs in when nothing more specific applies. Required: there is no sensible
      default, and guessing one would mean a pod that starts and cannot run anything.

      Which build and dev dependencies a task needs varies by repository, so this is the last of
      three answers, not the only one. See `imageFor`. -/
  image : String
  /-- Image per repository, by `owner/name`, for an operator who would rather decide centrally than
      take what a repository asks for. Beats the repository's own choice. -/
  repoImages : Array (String × String) := #[]
  /-- Whether a repository may name its own image in its `.orchestra/config.json`.

      On by default, because the repository is what knows what it needs to build, and because
      allowing it grants nothing that is not already granted: the agent runs that repository's code
      with its own credentials in the environment either way. Turn it off to pin every task to what
      this configuration names — a fork whose `.orchestra/config.json` was edited then changes
      nothing about where its task runs. -/
  allowRepoImage : Bool := true
  /-- Service account for the pod. Omitted means the namespace's default. -/
  serviceAccount : Option String := none
  /-- `imagePullSecrets` names. The Secrets have to exist in the namespace already; orchestra never
      creates or reads them, and never talks to a registry itself. -/
  imagePullSecrets : Array String := #[]
  /-- `imagePullPolicy` for the container: `Always`, `IfNotPresent` or `Never`.

      Left unset by default, which means Kubernetes' own rule applies: `Always` for a `:latest` or
      untagged reference, `IfNotPresent` for anything else. Worth setting to `Always` on a floating
      tag that is not `:latest` — a `:main` that is rebuilt nightly is otherwise served from
      whatever each node happened to cache, so two tasks can run different code under one name. -/
  imagePullPolicy : Option String := none
  /-- Prefixes a *repository-declared* image has to start with, when the operator would rather
      allow the choice than the registry.

      Empty means any reference is accepted, which is the default and is what
      `allow_repo_image: false` is the other end of. A namespace that pulls only from a scanned
      mirror sets `["ghcr.io/acme/", "registry.internal/"]` and lets repositories pick within it.
      Never applied to a pin or to `image`: those are the operator's own. -/
  allowedImagePrefixes : Array String := #[]
  /-- `nodeSelector`, verbatim. -/
  nodeSelector : Option Json := none
  /-- `resources` for the container, verbatim. -/
  resources : Option Json := none
  /-- Extra `volumes`, verbatim — a PVC for a build cache, most usefully. -/
  extraVolumes : Array Json := #[]
  /-- Extra `volumeMounts`, verbatim. -/
  extraMounts : Array Json := #[]
  /-- Where the pod reaches the daemon's MCP server. Required, and required to be routable *from
      the cluster*: a Service that fronts the daemon, or the address of the machine it runs on.
      Without it the agent has no tools — it cannot open a pull request, comment, or claim an
      issue — and nothing about the run says so. -/
  mcpHost : String
  /-- Address the daemon's MCP server binds so the cluster can reach it. `0.0.0.0` because the
      daemon is usually itself in a pod, where the interface it should bind is not knowable by
      name. Every connection is authenticated with a per-task token whatever this is set to. -/
  mcpBind : String := "0.0.0.0"
  /-- Ports the daemon's MCP server may listen on, as `[from, to]` inclusive.

      Only needed when something between the pods and the daemon has to be told the port before the
      task that uses it exists — a firewall rule, a port-forward, an SSH tunnel — which is the case
      for a daemon that runs outside the cluster its pods are in. A daemon inside the cluster needs
      nothing here: pods reach its address on any port.

      One server is started per task, so the range has to be at least as wide as the queue's
      parallelism. Unset means any free port, which is what orchestra has always done. -/
  mcpPorts : Option (UInt16 × UInt16) := none
  /-- `activeDeadlineSeconds`: the cluster kills the pod after this, whatever the daemon is doing.
      The backstop for a daemon that dies mid-task and leaves a pod holding a checkout. Counted
      over the whole task, hooks and retries included. -/
  deadlineSeconds : Nat := 14400
  /-- How long to wait for the pod to be ready before giving up on the task. Image pulls on a cold
      node are the reason this is minutes rather than seconds. -/
  startupTimeoutSeconds : Nat := 600
  /-- Whether the checkout is copied back out when the task ends. Off means the daemon's copy stays
      as the agent found it — which is only right when nothing local reads it afterwards. -/
  syncBack : Bool := true
  /-- Paths not carried in either direction, as `tar --exclude` patterns. Build output is the usual
      candidate: `.lake`, `target`, `node_modules`. -/
  excludes : Array String := #[]
  /-- Where the agent's `$HOME` is in the pod. An `emptyDir` by default, so the agent's own state
      directories are writable without the image having to make them so, and so that everything it
      writes is there for the whole task and gone after it. -/
  homePath : String := "/home/agent"
  /-- A `PersistentVolumeClaim` to mount as `$HOME` instead of an `emptyDir`.

      What this buys is the cost of `init.sh`. Every task starts from a new pod, so a hook that
      installs a toolchain or warms a build cache pays in full each time unless what it installs
      survives — and `~/.elan`, `~/.cargo`, `~/.cache` and the rest are all under `$HOME`. With a
      claim the first task on an image pays and the ones after it do not, which is the same
      arrangement the landrun backend gets for free from the machine it runs on.

      Needs `ReadWriteMany`, or `ReadWriteOnce` with every agent pod on one node: tasks run in
      parallel and would mount it at the same time. -/
  homeClaim : Option String := none
deriving Inhabited

/-- Where orchestra keeps its own files in the pod: the environment for each command. An
    `emptyDir`, so nothing survives the task. -/
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
    | none   => throw "kubernetes: execution.options.image is required — the image tasks run in \
(it needs the agent CLI, bash, tar, git and nc)"
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
    serviceAccount := jsonStr? j "service_account"
    imagePullSecrets := (jsonArr? j "image_pull_secrets").filterMap fun v =>
      match v with | .str s => some s | _ => none
    imagePullPolicy := jsonStr? j "image_pull_policy"
    allowedImagePrefixes := (jsonArr? j "allowed_image_prefixes").filterMap fun v =>
      match v with | .str s => some s | _ => none
    nodeSelector := j.getObjVal? "node_selector" |>.toOption
    resources := j.getObjVal? "resources" |>.toOption
    extraVolumes := jsonArr? j "volumes"
    extraMounts := jsonArr? j "volume_mounts"
    mcpHost
    mcpBind := (jsonStr? j "mcp_bind").getD "0.0.0.0"
    mcpPorts := match j.getObjVal? "mcp_ports" with
      | .ok (.arr #[lo, hi]) => do
        let l ← (fromJson? lo : Except String Nat).toOption
        let h ← (fromJson? hi : Except String Nat).toOption
        if l == 0 || h < l || h > 65535 then none else
          some (UInt16.ofNat l, UInt16.ofNat h)
      | _ => none
    deadlineSeconds := nat "deadline_seconds" 14400
    startupTimeoutSeconds := nat "startup_timeout_seconds" 600
    syncBack := j.getObjValAs? Bool "sync_back" |>.toOption |>.getD true
    excludes := (jsonArr? j "excludes").filterMap fun v =>
      match v with | .str s => some s | _ => none
    homePath := (jsonStr? j "home_path").getD "/home/agent"
    homeClaim := jsonStr? j "home_claim"
    repoImages := match j.getObjVal? "images" with
      | .ok (.obj kvs) => kvs.toArray.filterMap fun (k, v) =>
          match v with | .str i => some (k, i) | _ => none
      | _              => #[]
    allowRepoImage := j.getObjValAs? Bool "allow_repo_image" |>.toOption |>.getD true
  }

/-- Whether `ref` is something that can be an image reference at all.

    Not a parse of the OCI grammar — the registry is the authority on that, and a reference this
    accepts and the registry rejects fails as an unstartable pod with the registry's own message.
    What this is for is the one case where the string is not the operator's: a repository names its
    own image in a file in the repository, so a name with a space, a quote or a newline in it
    should be refused here rather than turned into a manifest field. -/
def validImageRef (ref : String) : Bool :=
  !ref.isEmpty && ref.length ≤ 512 &&
    ref.all fun c =>
      c.isAlphanum || c == '.' || c == '_' || c == '-' || c == '/' || c == ':' || c == '@'

/-- The image a task runs in, or why the one it asked for cannot be used.

    An operator's per-repository pin first, because that is the one someone chose deliberately for
    this repository and nothing in the repository should be able to override it. Then what the
    repository itself asked for, which is where the answer usually belongs — the repository is what
    knows whether its tests need a JDK or a browser. Then the configured default, for everything
    that has not said otherwise.

    The middle one is the only one checked, because it is the only one that did not come from this
    daemon's own configuration. Refused rather than quietly replaced by the default: a repository
    that asked for a JDK image and silently got one without would fail its validation script for a
    reason nothing in the log points at. -/
def imageFor (cfg : Config) (spec : SessionSpec) : Except String String :=
  match spec.repo.bind (fun r => cfg.repoImages.find? (·.1 == r)) with
  | some (_, pinned) => .ok pinned
  | none =>
    match (if cfg.allowRepoImage then spec.image else none) with
    | none          => .ok cfg.image
    | some declared =>
      if !validImageRef declared then
        .error s!"the repository's .orchestra/config.json asks to run in '{declared}', which is not a usable image reference"
      else if cfg.allowedImagePrefixes.isEmpty
              || cfg.allowedImagePrefixes.any (fun p => declared.startsWith p) then
        .ok declared
      else
        .error s!"the repository's .orchestra/config.json asks to run in '{declared}', which is not under any of the image prefixes this daemon allows a repository to name ({String.intercalate ", " cfg.allowedImagePrefixes.toList}). Pin the repository under execution.options.images instead, or widen allowed_image_prefixes."

/-! ## The pod

Rendering is pure, and tested that way (`OrchestraTest/KubernetesTest.lean`). What a pod is allowed
to do is the same kind of statement as what a landrun ruleset is allowed to do, and checking it
should not need a cluster. -/

/-- The container's command: stay up and do nothing, so that everything the task consists of can be
    `exec`ed into it. Ends when the pod is deleted, which is what closing the session does. -/
def idleScript : String := "while true; do sleep 5; done\n"

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
  /-- Whether this is the task's checkout. It is the one path replaced wholesale on the way back
      rather than merged; see `syncOut`. -/
  isWorkspace : Bool
deriving Repr, BEq, Inhabited

/-- The paths orchestra has to carry into the pod: the checkout, and any plugin or memory directory
    the task was granted. Everything else a session names is the image's to provide. -/
def stagedPaths (cfg : Config) (hostHome : String) (spec : SessionSpec) : Array StagedPath :=
  spec.grants.filter (·.from_ == .orchestra) |>.map fun g =>
    let hostPath := (PathGrant.resolve hostHome g).path
    let podPath  := (PathGrant.resolve cfg.homePath g).path
    { hostPath, podPath
      writable := g.access == .rw || g.access == .rwx
      isWorkspace := podPath == spec.workdir.toString }

/-- The pod manifest for a task. -/
def podManifest (cfg : Config) (spec : SessionSpec) (podName image : String)
    (staged : Array StagedPath) : Json :=
  let stageMounts : Array Json := staged.mapIdx fun i st =>
    Json.mkObj [("name", .str (stageVolumeName i)), ("mountPath", .str st.podPath)]
  let stageVolumes : Array Json := staged.mapIdx fun i _ =>
    Json.mkObj [("name", .str (stageVolumeName i)), ("emptyDir", Json.mkObj [])]
  let homeVolume := match cfg.homeClaim with
    | some claim => Json.mkObj [("name", .str "home"),
        ("persistentVolumeClaim", Json.mkObj [("claimName", .str claim)])]
    | none       => Json.mkObj [("name", .str "home"), ("emptyDir", Json.mkObj [])]
  let volumes : Array Json :=
    stageVolumes
      ++ #[Json.mkObj [("name", .str "control"), ("emptyDir", Json.mkObj [])], homeVolume]
      ++ cfg.extraVolumes
  let mounts : Array Json :=
    stageMounts
      ++ #[Json.mkObj [("name", .str "control"), ("mountPath", .str controlPath)],
           Json.mkObj [("name", .str "home"), ("mountPath", .str cfg.homePath)]]
      ++ cfg.extraMounts
  -- `HOME` is set here rather than passed through: the image's idea of home is not orchestra's,
  -- and every home-relative path the agent backend declared was resolved against `homePath`.
  -- Nothing else is set on the pod. Credentials reach each command through a file (see
  -- `envFilePath`), because a pod's spec is readable by anything that can list pods.
  let container := Json.mkObj ([
    ("name", .str "agent"),
    ("image", .str image),
    ("command", .arr #[.str "/bin/sh", .str "-c", .str idleScript]),
    ("workingDir", .str spec.workdir.toString),
    ("env", .arr #[Json.mkObj [("name", .str "HOME"), ("value", .str cfg.homePath)]]),
    ("volumeMounts", .arr mounts)
  ] ++ (match cfg.imagePullPolicy with
        | some p => [("imagePullPolicy", Json.str p)] | none => [])
     ++ (match cfg.resources with | some r => [("resources", r)] | none => []))
  let podSpec := Json.mkObj ([
    ("restartPolicy", .str "Never"),
    ("activeDeadlineSeconds", .num cfg.deadlineSeconds),
    ("containers", .arr #[container]),
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
      ("labels", Json.mkObj ([
        ("app.kubernetes.io/managed-by", .str "orchestra"),
        ("orchestra.dev/task", .str spec.label)]
        -- A label value cannot hold a `/`, so `owner/name` is written the way Kubernetes writes
        -- its own two-part names.
        ++ (match spec.repo with
            | some r => [("orchestra.dev/repo", Json.str (r.replace "/" "."))]
            | none   => [])))]),
    ("spec", podSpec)]

/-- `tar` flags for the excluded paths, in the order they were configured. -/
def excludeArgs (cfg : Config) : Array String :=
  cfg.excludes.flatMap fun p => #["--exclude", p]

/-- Where the environment for the `n`th command in a session is written inside the pod. -/
def envFilePath (n : Nat) : String := s!"{controlPath}/env-{n}"

/-- A command's environment as a file to be sourced.

    A file rather than `kubectl exec -- env K=V ...` or an exported prefix, because the
    installation token and the agent's API key are in here: a command line is visible in the
    cluster's audit log and to anything reading `/proc` in the pod. Quoted with `shellEscape`, so a
    value containing a quote or a newline cannot end the assignment early. -/
def envFileContents (env : Array (String × String)) : String :=
  String.join (env.toList.map fun (k, v) => s!"export {k}={shellEscape v}\n")

/-- Where a running agent records its process id in the pod. -/
def agentPidPath : String := s!"{controlPath}/agent.pid"

/-- The shell a command runs under in the pod: source the environment, go to the checkout, and
    become the command — `exec`, so that the process the pod holds is the command itself and not a
    shell wrapping it.

    `guard` is for the agent, and for nothing else. A `kubectl exec` connection can die without the
    process at the far end dying with it — the kubelet does not kill it, and the daemon cannot tell
    that from the agent having exited. The next attempt would then start a second agent in the same
    checkout as the first, both editing it. So each agent records its process id and shoots
    whatever the last one left behind, which costs nothing when there is nothing there. -/
def runnerScript (envFile : String) (workdir : String) (guard : Bool := false) : String :=
  let guardLines :=
    if guard then
      s!"if [ -f {agentPidPath} ]; then kill -9 \"$(cat {agentPidPath} 2>/dev/null)\" 2>/dev/null || true; fi\necho $$ > {agentPidPath}\n"
    else ""
  s!". {envFile}\ncd {shellEscape workdir}\n{guardLines}exec \"$@\"\n"

/-! ## Talking to the cluster -/

/-- Run `kubectl` and collect what it said. -/
private def kube (cfg : Config) (args : Array String) : IO (UInt32 × String × String) := do
  let out ← IO.Process.output { cmd := cfg.kubectl, args := #["-n", cfg.ns] ++ args }
  return (out.exitCode, out.stdout, out.stderr)

/-- Run a shell pipeline, for the places one is genuinely needed: `tar` into `kubectl exec` and
    back out again. Every interpolated path goes through `shellEscape`. -/
private def shell (script : String) : IO (UInt32 × String × String) := do
  let out ← IO.Process.output { cmd := "/bin/sh", args := #["-c", script] }
  return (out.exitCode, out.stdout, out.stderr)

/-- The `kubectl exec` argument vector for running `command` with `args` in the pod.

    `-i -t` only for an interactive session, where a terminal is the point; a queued run wants the
    opposite, since `kubectl exec` merges stdout and stderr as soon as there is a TTY and orchestra
    reads the two for different things. -/
def execArgs (cfg : Config) (podName : String) (interactive : Bool)
    (script : String) (command : String) (args : Array String) : Array String :=
  #["-n", cfg.ns, "exec"] ++ (if interactive then #["-i", "-t"] else #[])
    ++ #[podName, "--", "/bin/sh", "-c", script, "orchestra", command] ++ args

/-- Copy a directory into the pod. Skipped, not failed, when the source is not there: an agent
    backend may declare a plugin directory this machine does not have, exactly as with landrun. -/
private def stageIn (cfg : Config) (podName hostPath podPath : String) : IO Unit := do
  unless ← System.FilePath.pathExists (System.FilePath.mk hostPath) do return ()
  let excludes := String.intercalate " " (excludeArgs cfg).toList
  let script := s!"tar -C {shellEscape hostPath} {excludes} -cf - . | \
{shellEscape cfg.kubectl} -n {shellEscape cfg.ns} exec -i {shellEscape podName} -- \
tar -C {shellEscape podPath} -xf -"
  let (code, _, err) ← shell script
  if code != 0 then
    throw (IO.userError s!"kubernetes: could not copy {hostPath} into the pod: {err.trimAscii}")

/-- Write a small file into the pod, without it ever appearing on a command line. -/
private def putFile (cfg : Config) (podName path contents : String) : IO Unit := do
  let dir := System.FilePath.mk s!"/tmp/orchestra-k8s-{← randomHex 8}"
  IO.FS.createDirAll dir
  -- Narrowed before anything is written: what goes through here is an environment file, and the
  -- installation token is in it.
  let _ ← IO.Process.output { cmd := "chmod", args := #["700", dir.toString] }
  let name := (System.FilePath.mk path).fileName.getD "file"
  IO.FS.writeFile (dir / name) contents
  let podDir := (System.FilePath.mk path).parent.map (·.toString) |>.getD "/"
  let script := s!"tar -C {shellEscape dir.toString} -cf - {shellEscape name} | \
{shellEscape cfg.kubectl} -n {shellEscape cfg.ns} exec -i {shellEscape podName} -- \
tar -C {shellEscape podDir} -xf -"
  let (code, _, err) ← shell script
  try IO.FS.removeDirAll dir catch _ => pure ()
  if code != 0 then
    throw (IO.userError s!"kubernetes: could not write {path} in the pod: {err.trimAscii}")

/-- Copy one directory back out of the pod, replacing what is on disk.

    The checkout is swapped rather than extracted over: `tar` never deletes, so extracting on top
    of the old tree would resurrect every file the agent removed. The new tree is assembled beside
    the old one and moved into place, so a transfer that fails leaves the checkout as it was.

    `merge` is for the paths where a swap would be wrong — memory directories, which other tasks
    may be writing to at the same time. There, a file the agent deleted survives, which is the
    lesser mistake. -/
private def syncOut (cfg : Config) (podName hostPath podPath : String) (merge : Bool)
    : IO Unit := do
  let kubectlTar := s!"{shellEscape cfg.kubectl} -n {shellEscape cfg.ns} exec {shellEscape podName} \
-- tar -C {shellEscape podPath} {String.intercalate " " (excludeArgs cfg).toList} -cf - ."
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
    IO.eprintln s!"  [k8s] warning: could not copy the checkout back out of the pod, so \
{hostPath} still holds what the agent started from: {err.trimAscii}"

/-- Why a pod never became ready, in as much detail as the cluster will give. What a person needs
    here is the image pull error or the unschedulable message, not "timed out". -/
private def startupDiagnosis (cfg : Config) (podName : String) : IO String := do
  let (_, phase, _) ← kube cfg #["get", "pod", podName, "-o",
    "jsonpath={.status.phase} {.status.containerStatuses[*].state.waiting.reason} \
{.status.containerStatuses[*].state.waiting.message} \
{.status.conditions[?(@.type=='PodScheduled')].message}"]
  return phase.trimAscii.toString

/-! ## The session -/

/-- Open a pod for one task, and hand back the handle on it. -/
def openSession (cfg : Config) (spec : SessionSpec) : IO Session := do
  let home ← hostHome
  let staged := stagedPaths cfg home spec
  let podName := s!"orchestra-{← randomHex 6}"
  let image ← match imageFor cfg spec with
    | .ok i    => pure i
    | .error e => throw (IO.userError s!"kubernetes: {e}")
  let manifest := podManifest cfg spec podName image staged
  let dir := System.FilePath.mk s!"/tmp/orchestra-k8s-{← randomHex 8}"
  IO.FS.createDirAll dir
  let manifestPath := dir / "pod.json"
  IO.FS.writeFile manifestPath manifest.compress
  let (code, _, err) ← kube cfg #["create", "-f", manifestPath.toString]
  try IO.FS.removeDirAll dir catch _ => pure ()
  if code != 0 then
    throw (IO.userError s!"kubernetes: could not create pod {podName}: {err.trimAscii}")
  let deletePod : IO Unit := do
    let _ ← try kube cfg #["delete", "pod", podName, "--now", "--wait=false",
                           "--ignore-not-found"] catch _ => pure (0, "", "")
  -- Ready, not merely created: the next thing this does is copy a repository through
  -- `kubectl exec`, which needs a container that has actually started.
  let (waitCode, _, waitErr) ← kube cfg #["wait", "--for=condition=Ready", s!"pod/{podName}",
    s!"--timeout={cfg.startupTimeoutSeconds}s"]
  if waitCode != 0 then
    let why ← startupDiagnosis cfg podName
    deletePod
    throw (IO.userError s!"kubernetes: pod {podName} did not become ready within \
{cfg.startupTimeoutSeconds}s ({why}): {waitErr.trimAscii}")
  try
    for st in staged do
      stageIn cfg podName st.hostPath st.podPath
  catch e =>
    deletePod
    throw e
  -- Each command gets its own environment file, so a later one cannot be handed an earlier one's
  -- variables by accident.
  let commandCount ← IO.mkRef 0
  let nextEnvFile (env : Array (String × String)) : IO String := do
    let n ← commandCount.modifyGet fun n => (n, n + 1)
    let path := envFilePath n
    putFile cfg podName path (envFileContents env)
    return path
  return {
    id := s!"pod {cfg.ns}/{podName} ({image})"
    -- Every task starts from a new pod, so the repository's `init.sh` cannot rely on what a
    -- previous one installed — and the marker it leaves in the checkout, which is carried in here,
    -- would otherwise say it can.
    freshEnvironment := true
    -- The agent's conversation is a file under its `$HOME`. With an `emptyDir` home that is gone
    -- when the pod is, so a task cannot continue one an earlier task started; with a claim, home
    -- outlives the pod and it can.
    carriesAgentState := cfg.homeClaim.isSome
    mcpEndpoint := fun e => pure { e with host := cfg.mcpHost }
    describe := fun run => do
      let envFile := envFilePath 0
      let rendered := String.intercalate " "
        ((#[cfg.kubectl] ++ execArgs cfg podName (run.stdio == .inherit)
           (runnerScript envFile run.workdir.toString) run.command run.args).toList.map shellEscape)
      return s!"[debug] {cfg.kubectl} -n {cfg.ns} create -f - <<'EOF'\n{manifest.pretty}\nEOF\n\
[debug] {rendered}"
    start := fun run => do
      let envFile ← nextEnvFile run.env
      let args := execArgs cfg podName (run.stdio == .inherit)
        (runnerScript envFile run.workdir.toString (guard := true)) run.command run.args
      match run.stdio with
      | .inherit =>
        -- An interactive session: `kubectl exec -i -t` puts a terminal on the connection, and the
        -- daemon's own streams go straight through, so the agent's TUI behaves as it does locally.
        let child ← IO.Process.spawn {
          cmd := cfg.kubectl, args
          stdin := .inherit, stdout := .inherit, stderr := .inherit }
        return { Handle.ofInheritChild child with
                 id := s!"pod {cfg.ns}/{podName}", kill := deletePod }
      | .piped =>
        -- Without a terminal, `kubectl exec` keeps the two streams apart, which is what orchestra
        -- needs: the agent's events are on one and everything else is on the other.
        let child ← IO.Process.spawn {
          cmd := cfg.kubectl, args
          stdin := .null, stdout := .piped, stderr := .piped }
        return { Handle.ofPipedChild child with
                 id := s!"pod {cfg.ns}/{podName}", kill := deletePod }
    runScript := fun script => do
      -- The repository's own scripts, run where the agent works. `bash` because that is what the
      -- daemon used when it ran them itself, and repositories were written against it.
      let envFile ← nextEnvFile #[]
      let args := execArgs cfg podName false
        (runnerScript envFile script.workdir.toString) "bash" #[script.path]
      match script.stdio with
      | .inherit =>
        let child ← IO.Process.spawn {
          cmd := cfg.kubectl, args, stdin := .null, stdout := .inherit, stderr := .inherit }
        return { exitCode := ← child.wait }
      | .piped =>
        let child ← IO.Process.spawn {
          cmd := cfg.kubectl, args, stdin := .null, stdout := .piped, stderr := .piped }
        let stdout ← child.stdout.readToEnd
        let stderr ← child.stderr.readToEnd
        let exitCode ← child.wait
        return { exitCode, output := (stdout ++ stderr).trimAscii.toString }
    close := do
      if cfg.syncBack then
        -- A pod that is already gone is the interesting case: it means something outside this
        -- daemon ended the task's environment — `deadline_seconds` expiring, an eviction, a node
        -- going away — and the difference between that and a transfer that failed is the
        -- difference between "retry it" and "the work is not there to retry".
        let (code, out, _) ← kube cfg #["get", "pod", podName, "-o", "jsonpath={.status.phase}"]
        if code != 0 then
          IO.eprintln s!"  [k8s] pod {podName} is gone before the task finished with it — \
deleted, evicted, or past its {cfg.deadlineSeconds}s deadline_seconds. Nothing was copied back, \
so {spec.workdir} still holds what the agent started from."
        else
          for st in staged do
            if st.writable then
              syncOut cfg podName st.hostPath st.podPath (merge := !st.isWorkspace)
          if out.trimAscii.toString == "Failed" then
            IO.eprintln s!"  [k8s] pod {podName} ended in Failed — if the task itself looked fine, \
check whether it ran past deadline_seconds ({cfg.deadlineSeconds}s)."
      deletePod }

/-- Check that `kubectl` is here and that it may do what this backend does.

    The permission check is `create pods` alone, which is the one that fails first and the one an
    operator most often forgets; the rest of the verbs are named in the error so that fixing it is
    a single edit to a Role rather than a sequence of failed tasks. -/
def preflight (cfg : Config) : IO (Except String Unit) := do
  if let some p := cfg.imagePullPolicy then
    unless ["Always", "IfNotPresent", "Never"].contains p do
      return .error s!"execution.options.image_pull_policy is '{p}'; Kubernetes accepts only \
Always, IfNotPresent or Never"
  try
    let version ← IO.Process.output { cmd := cfg.kubectl, args := #["version", "--client=true"] }
    if version.exitCode != 0 then
      return .error s!"'{cfg.kubectl}' could not be run: {version.stderr.trimAscii}"
  catch _ =>
    return .error s!"'{cfg.kubectl}' is not on PATH. This backend drives the cluster through it."
  let (code, out, err) ← kube cfg #["auth", "can-i", "create", "pods"]
  if code != 0 || out.trimAscii.toString != "yes" then
    return .error s!"this daemon may not create pods in namespace '{cfg.ns}' \
({(out ++ err).trimAscii}). It needs create/get/list/watch/delete on pods, create on pods/exec, \
and get on pods/log."
  return .ok ()

/-- Kubernetes as an execution backend. -/
def factory : BackendFactory where
  name := "kubernetes"
  summary := "one pod per task, on a cluster reached through kubectl"
  make options := do
    let cfg ← Config.fromJson options
    return {
      name := "kubernetes"
      -- The agent is off this machine, so the MCP server has to listen somewhere it can reach —
      -- and every connection to it then carries a per-run token, minted with the server.
      exposure := .network cfg.mcpBind cfg.mcpPorts
      mcpEndpoint := fun e => pure { e with host := cfg.mcpHost }
      preflight := preflight cfg
      openSession := openSession cfg }

end Orchestra.Exec.Kubernetes
