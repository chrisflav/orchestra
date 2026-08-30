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
  /-- Whether the *checkout* is copied back out when the task ends. Off means the daemon's copy
      stays as the agent found it — which is only right when nothing local reads it afterwards.

      Memory directories are not covered by this and always come back: they are the record of what
      earlier tasks learned, and a memory that does not outlive its pod is not one. -/
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

    Strict about the settings that cannot be guessed and loose about the rest: a missing `image` or
    `mcp_host`, or an `mcp_ports` that cannot be read, is a configuration that cannot work, and
    finding that out at the first dispatched task — as a pod that runs an agent with no tools — is
    exactly the failure this refuses to allow. -/
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
  -- Checked here, where the key can be named, rather than trusted at the two places it is
  -- rendered: this string goes into a shell pipeline (`McpEndpoint.stdioCommand`) and into the
  -- agent's own configuration file in JSON and in TOML.
  let mcpHost ← match Exec.McpEndpoint.validHost? mcpHost with
    | .ok h    => pure h
    | .error e => throw s!"kubernetes: execution.options.mcp_host {e}"
  -- A malformed range is refused rather than dropped. Dropping it means an ephemeral port, which
  -- is the one outcome the setting exists to prevent — whatever routes the pods to this daemon
  -- was told a fixed range in advance, and a task on a port outside it reaches nothing.
  let mcpPorts ← match j.getObjVal? "mcp_ports" with
    | .error _ => pure none
    | .ok (.arr #[lo, hi]) =>
      match (fromJson? lo : Except String Nat), (fromJson? hi : Except String Nat) with
      | .ok l, .ok h =>
        if l == 0 || h < l || h > 65535 then
          throw s!"kubernetes: execution.options.mcp_ports is [{l}, {h}], which is not a port \
range — it must be [from, to] with 0 < from ≤ to ≤ 65535"
        else pure (some (UInt16.ofNat l, UInt16.ofNat h))
      | _, _ => throw "kubernetes: execution.options.mcp_ports must be two port numbers, \
[from, to]"
    | .ok _ => throw "kubernetes: execution.options.mcp_ports must be an array of two port \
numbers, [from, to]"
  -- These reach a shell as globs rather than as quoted words — `syncOut` matches them against the
  -- old checkout to carry the excluded paths across a wholesale swap — so the pattern language is
  -- pinned to what a path and a glob are spelled with. A leading `/` or a `..` is refused as well:
  -- both name something outside the checkout, which `tar --exclude` would ignore and the preserve
  -- loop would not.
  let excludes ← (jsonArr? j "excludes").filterMap (fun v =>
      match v with | .str s => some s | _ => none)
    |>.mapM fun p =>
      if p.isEmpty || p.startsWith "/" || (p.splitOn "..").length > 1 then
        throw s!"kubernetes: execution.options.excludes has '{p}', which is not a path inside the \
checkout"
      else if p.all (fun c => c.isAlphanum || "._-*?/[]".any (· == c)) then
        pure p
      else
        throw s!"kubernetes: execution.options.excludes has '{p}', which is not a path or a glob \
(letters, digits, '.', '_', '-', '/', '*', '?' and '[]' only)"
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
    mcpPorts
    deadlineSeconds := nat "deadline_seconds" 14400
    startupTimeoutSeconds := nat "startup_timeout_seconds" 600
    syncBack := j.getObjValAs? Bool "sync_back" |>.toOption |>.getD true
    excludes
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
    (script : String) (command : String) (args : Array String)
    (stdinOpen : Bool := false) : Array String :=
  let flags := (if stdinOpen || interactive then #["-i"] else #[]) ++
               (if interactive then #["-t"] else #[])
  #["-n", cfg.ns, "exec"] ++ flags
    ++ #[podName, "--", "/bin/sh", "-c", script, "orchestra", command] ++ args

/-- `tar` flags for every extraction that happens *inside* the pod.

    The agent does not run as root there — it cannot, since the CLIs refuse
    `--dangerously-skip-permissions` under uid 0, and the pod carries no `securityContext` for
    orchestra to say otherwise with. So the user extracting owns neither the mount points nor the
    archive's recorded ownership, and plain `tar -x` fails on the first of the three:

      * `--no-overwrite-dir` leaves the metadata of directories that already exist alone. Without
        it the archive's own `./` entry makes `tar` try to `chmod` and `utime` the mount point,
        which belongs to root, and the whole extraction fails there.
      * `--no-same-owner` and `--no-same-permissions` are what an unprivileged `tar` does by
        default; named anyway, because it is the *daemon's* `tar` on the other side of the pipe
        that decides what is recorded, and this end should not depend on who runs it. -/
def extractFlags : String := "--no-overwrite-dir --no-same-owner --no-same-permissions"

/-- Copy a directory into the pod. Skipped, not failed, when the source is not there: an agent
    backend may declare a plugin directory this machine does not have, exactly as with landrun.

    The archive holds the directory's *entries*, not the directory. `tar -cf - .` would record a
    `./` member, and restoring that member means `chmod` and `utime` on the extraction root — which
    here is a mount point owned by root, under a `tar` that is not root, so the whole extraction
    fails on the first thing it does. Nothing wants that member: the mount point already exists,
    and its mode is the kubelet's business rather than the daemon's. -/
private def stageIn (cfg : Config) (podName hostPath podPath : String) : IO Unit := do
  unless ← System.FilePath.pathExists (System.FilePath.mk hostPath) do return ()
  -- An empty directory has no entries to list, and `tar` refuses to create an empty archive.
  -- There is also nothing to carry: the mount point is already there.
  if (← (System.FilePath.mk hostPath).readDir).isEmpty then return ()
  let excludes := String.intercalate " " (excludeArgs cfg).toList
  -- `--exclude` before `-T`, not after: it applies only to names that come after it on the
  -- command line, so the other order silently carries everything `excludes` names.
  let script := s!"cd {shellEscape hostPath} && \
find . -mindepth 1 -maxdepth 1 -print0 | \
tar {excludes} --null -T - -cf - | \
{shellEscape cfg.kubectl} -n {shellEscape cfg.ns} exec -i {shellEscape podName} -- \
tar -C {shellEscape podPath} {extractFlags} -xf -"
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
tar -C {shellEscape podDir} {extractFlags} -xf -"
  let (code, _, err) ← shell script
  try IO.FS.removeDirAll dir catch _ => pure ()
  if code != 0 then
    throw (IO.userError s!"kubernetes: could not write {path} in the pod: {err.trimAscii}")

/-- Create a directory in the pod, and everything above it. -/
private def mkdirInPod (cfg : Config) (podName dir : String) : IO Unit := do
  let (code, _, err) ← kube cfg #["exec", podName, "--", "mkdir", "-p", dir]
  if code != 0 then
    throw (IO.userError s!"kubernetes: could not create {dir} in the pod: {err.trimAscii}")

/-- Carry one path into a pod that is already running, whether it is a file or a directory.

    `stageIn` handles the directories the session was opened with, all of which exist by the time
    the pod does and all of which are mounted, so their mount point is already there. This is for
    what arrives afterwards — the agent's MCP configuration, which is often a single file under a
    directory the image never had a reason to create — so the destination is made first. -/
private def stagePath (cfg : Config) (podName hostPath podPath : String) : IO Unit := do
  let host := System.FilePath.mk hostPath
  -- Missing is not an error, on the same terms as `stageIn`: an agent backend can declare a path
  -- it only writes under some conditions.
  unless ← host.pathExists do return ()
  if ← host.isDir then
    mkdirInPod cfg podName podPath
    stageIn cfg podName hostPath podPath
  else
    mkdirInPod cfg podName ((System.FilePath.mk podPath).parent.map (·.toString) |>.getD "/")
    putFile cfg podName podPath (← IO.FS.readFile host)

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
  -- `excludes` means "not carried in either direction". On a tree that is replaced wholesale that
  -- would read as "deleted here", which is the opposite of what the setting is for: what people
  -- put in it is build output, and `orchestra prepare` exists to warm exactly that. The old tree
  -- still has it, so it is moved across into the new one before the swap. Nothing crosses the
  -- network and nothing is lost.
  --
  -- Unescaped on purpose — these are globs, and `Config.fromJson` has already refused any pattern
  -- with a character that could be anything but one.
  let preserve := String.join (cfg.excludes.toList.map fun pat =>
    s!"for src in \"$prev\"/{pat}; do\n\
  [ -e \"$src\" ] || continue\n\
  dst=\"$inc/$\{src#\"$prev\"/}\"\n\
  mkdir -p \"$(dirname \"$dst\")\"\n\
  rm -rf \"$dst\"\n\
  mv \"$src\" \"$dst\"\n\
done\n")
  let script := s!"set -e\n\
inc={shellEscape incoming}\n\
prev={shellEscape previous}\n\
rm -rf \"$inc\" \"$prev\"\n\
mkdir -p \"$inc\"\n\
{kubectlTar} | tar -C \"$inc\" -xf -\n\
if [ -d {shellEscape hostPath} ]; then mv {shellEscape hostPath} \"$prev\"; fi\n\
{preserve}\
mv \"$inc\" {shellEscape hostPath}\n\
rm -rf \"$prev\"\n"
  let (code, _, err) ← shell script
  if code != 0 then
    -- Put the checkout back if the failure landed between the two moves. There, `hostPath` does
    -- not exist at all, and the tree the agent started from is sitting intact under `previous` —
    -- so restoring it is both possible and the only thing that leaves the slot usable.
    let recovery := s!"if [ ! -e {shellEscape hostPath} ] && [ -d {shellEscape previous} ]; then \
mv {shellEscape previous} {shellEscape hostPath}; fi\n\
rm -rf {shellEscape incoming} {shellEscape previous}\n"
    let _ ← shell recovery
    if ← System.FilePath.pathExists (System.FilePath.mk hostPath) then
      IO.eprintln s!"  [k8s] warning: could not copy the checkout back out of the pod, so \
{hostPath} still holds what the agent started from: {err.trimAscii}"
    else
      IO.eprintln s!"  [k8s] error: could not copy the checkout back out of the pod, and \
{hostPath} could not be restored either — the slot has to be prepared again before it is used: \
{err.trimAscii}"

/-- Why a pod never became ready, in as much detail as the cluster will give. What a person needs
    here is the image pull error or the unschedulable message, not "timed out". -/
private def startupDiagnosis (cfg : Config) (podName : String) : IO String := do
  let (_, phase, _) ← kube cfg #["get", "pod", podName, "-o",
    "jsonpath={.status.phase} {.status.containerStatuses[*].state.waiting.reason} \
{.status.containerStatuses[*].state.waiting.message} \
{.status.conditions[?(@.type=='PodScheduled')].message}"]
  return phase.trimAscii.toString

/-- What the cluster says about a pod's existence, as the three answers that call for three
    different things.

    Kept apart because `kubectl` exits non-zero for "no such pod" and for "the API server did not
    answer" alike, and those are opposite situations: the first means the task's environment is
    gone and there is nothing to copy back, the second means we do not know, and treating not
    knowing as the first silently discards the work of a run that finished. -/
inductive PodState where
  /-- The pod is there, in this phase. -/
  | present (phase : String)
  /-- The cluster answered, and there is no such pod. -/
  | gone
  /-- The cluster could not be asked. -/
  | unknown (why : String)

/-- Ask whether the pod is still there.

    `--ignore-not-found` is what makes the distinction possible at all: with it, a pod that does
    not exist is exit 0 and empty output, so a non-zero exit means the query itself failed. -/
private def podState (cfg : Config) (podName : String) : IO PodState := do
  let (code, out, err) ← kube cfg
    #["get", "pod", podName, "--ignore-not-found", "-o", "jsonpath={.status.phase}"]
  if code != 0 then
    return .unknown err.trimAscii.toString
  else if out.trimAscii.isEmpty then
    return .gone
  else
    return .present out.trimAscii.toString

/-- Whether the process the agent's guard recorded is still running in the pod.

    Asked only once the local `kubectl exec` has exited, and only to tell its two meanings apart:
    the agent finished, or the connection to it dropped. `runnerScript`'s guard writes the pid for
    exactly this. A pod that cannot be reached at all answers `false` — the run is not observably
    alive, and reporting it as running forever would hang the caller. -/
private def agentAlive (cfg : Config) (podName : String) : IO Bool := do
  let script := s!"[ -f {agentPidPath} ] || exit 1\n\
kill -0 \"$(cat {agentPidPath} 2>/dev/null)\" 2>/dev/null\n"
  try
    let out ← IO.Process.output {
      cmd := cfg.kubectl
      args := #["-n", cfg.ns, "exec", podName, "--", "/bin/sh", "-c", script] }
    return out.exitCode == 0
  catch _ => return false

/-- End the run inside the pod, leaving the pod itself alone.

    Cancelling a task is not the same as ending its environment. `after.sh` still has to run, the
    checkout still has to come back, and the task still has to record that it was cancelled — all
    of which are `kubectl exec`s into a pod that has to still be there. So this kills the process
    the guard recorded and lets `close` take the pod down, which is where every other way a task
    can end already takes it down.

    The local `kubectl` is killed too: its connection would otherwise stay open reading from a
    process that is gone, and the supervisor is waiting on that. -/
private def killAgent (cfg : Config) (podName : String) (localPid : UInt32) : IO Unit := do
  let script := s!"if [ -f {agentPidPath} ]; then\n\
  pid=\"$(cat {agentPidPath} 2>/dev/null)\"\n\
  kill -TERM \"$pid\" 2>/dev/null || true\n\
  for _ in 1 2 3 4 5; do kill -0 \"$pid\" 2>/dev/null || exit 0; sleep 1; done\n\
  kill -9 \"$pid\" 2>/dev/null || true\n\
fi\n"
  try
    let child ← IO.Process.spawn {
      cmd := cfg.kubectl
      args := #["-n", cfg.ns, "exec", podName, "--", "/bin/sh", "-c", script]
      stdin := .null, stdout := .null, stderr := .null }
    let _ ← child.wait
  catch _ => pure ()
  Handle.killPid localPid

/-- `localTryWait` corrected for the one thing it cannot see.

    `Handle.tryWait` is specified to answer for the run, and a `kubectl exec` child answers for the
    connection to it — which can die on its own while the agent keeps going. Taking that as "the
    run is over" is how an interactive conversation gets reaped mid-turn, which is the failure the
    field's own documentation describes.

    So a local exit is a question rather than an answer, and the pod is asked. The result is
    remembered: once the agent is known to be gone it stays gone, and the poll costs one `exec`
    rather than one per call forever. -/
private def guardedTryWait (cfg : Config) (podName : String) (settled : IO.Ref Bool)
    (localTryWait : IO (Option UInt32)) : IO (Option UInt32) := do
  match ← localTryWait with
  | none      => return none
  | some code =>
    if ← settled.get then return some code
    if ← agentAlive cfg podName then
      return none
    settled.set true
    return some code

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
      -- `-i` for a stream as well as for a terminal: both need our end of stdin open, and only
      -- the interactive one wants a TTY.
      let args := execArgs cfg podName (interactive := run.stdio == .inherit)
        (stdinOpen := run.stdio != .piped)
        (runnerScript envFile run.workdir.toString (guard := true)) run.command run.args
      -- Cancellation ends the agent, not the pod: `close` is the only thing that takes the pod
      -- down, because everything that has to happen after a cancelled task — `after.sh`, the
      -- checkout coming back, the status being written — is another `exec` into it.
      let settled ← IO.mkRef false
      match run.stdio with
      | .inherit =>
        -- An interactive session: `kubectl exec -i -t` puts a terminal on the connection, and the
        -- daemon's own streams go straight through, so the agent's TUI behaves as it does locally.
        let child ← IO.Process.spawn {
          cmd := cfg.kubectl, args
          stdin := .inherit, stdout := .inherit, stderr := .inherit }
        return { Handle.ofInheritChild child with
                 id := s!"pod {cfg.ns}/{podName}"
                 tryWait := guardedTryWait cfg podName settled child.tryWait
                 kill := killAgent cfg podName child.pid }
      | .piped =>
        -- Without a terminal, `kubectl exec` keeps the two streams apart, which is what orchestra
        -- needs: the agent's events are on one and everything else is on the other.
        let child ← IO.Process.spawn {
          cmd := cfg.kubectl, args
          stdin := .null, stdout := .piped, stderr := .piped }
        return { Handle.ofPipedChild child with
                 id := s!"pod {cfg.ns}/{podName}"
                 tryWait := guardedTryWait cfg podName settled child.tryWait
                 kill := killAgent cfg podName child.pid }
      | .stream =>
        -- An interactive session's agent: up for hours, one turn written in at a time. `-i`
        -- without `-t` is what carries a pipe rather than a terminal, so closing our end reaches
        -- the agent's stdin as the EOF that tells it there are no more turns — and stdout and
        -- stderr still arrive apart, which a TTY would merge.
        let child ← IO.Process.spawn {
          cmd := cfg.kubectl, args
          stdin := .piped, stdout := .piped, stderr := .piped }
        let localPid := child.pid
        let handle ← Handle.ofStreamChild child
        return { handle with
                 id := s!"pod {cfg.ns}/{podName}"
                 tryWait := guardedTryWait cfg podName settled handle.tryWait
                 kill := killAgent cfg podName localPid }
    provide := fun grants => do
      -- Only orchestra's own content travels; a grant naming something the image supplies has
      -- nothing to carry, exactly as when the session was opened.
      for g in grants do
        if g.from_ == .orchestra then
          stagePath cfg podName (PathGrant.resolve home g).path
            (PathGrant.resolve cfg.homePath g).path
    runScript := fun script => do
      -- The repository's own scripts, run where the agent works. `bash` because that is what the
      -- daemon used when it ran them itself, and repositories were written against it.
      let envFile ← nextEnvFile #[]
      let args := execArgs cfg podName false
        (runnerScript envFile script.workdir.toString) "bash" #[script.path]
      match script.stdio with
      -- A repository's script is run, not conversed with; `.stream` is captured like `.piped`.
      | .inherit =>
        let child ← IO.Process.spawn {
          cmd := cfg.kubectl, args, stdin := .null, stdout := .inherit, stderr := .inherit }
        return { exitCode := ← child.wait }
      | .piped | .stream =>
        let child ← IO.Process.spawn {
          cmd := cfg.kubectl, args, stdin := .null, stdout := .piped, stderr := .piped }
        let stdout ← child.stdout.readToEnd
        let stderr ← child.stderr.readToEnd
        let exitCode ← child.wait
        return { exitCode, output := (stdout ++ stderr).trimAscii.toString }
    close := do
      -- A pod that is already gone is the interesting case: it means something outside this daemon
      -- ended the task's environment — `deadline_seconds` expiring, an eviction, a node going
      -- away — and the difference between that and a transfer that failed is the difference
      -- between "retry it" and "the work is not there to retry".
      --
      -- Which is why "gone" and "could not ask" are not the same answer. A `kubectl` that fails
      -- for an API-server hiccup or an expired credential says nothing about whether the pod is
      -- there, and taking it as "gone" throws away a completed run's checkout and memories
      -- without ever attempting the copy. So the copy is attempted whenever the pod was not
      -- positively reported absent; `syncOut` already warns rather than throws if it cannot.
      let state ← podState cfg podName
      match state with
      | .gone =>
        IO.eprintln s!"  [k8s] pod {podName} is gone before the task finished with it — deleted, \
evicted, or past its {cfg.deadlineSeconds}s deadline_seconds. Nothing was copied back, so \
{spec.workdir} still holds what the agent started from, and anything the agent wrote to a memory \
directory is lost."
      | .unknown why =>
        IO.eprintln s!"  [k8s] could not ask the cluster whether pod {podName} is still there \
({why}) — trying to copy the task's work back anyway, on the chance that it is."
      | .present _ => pure ()
      unless state matches .gone do
        for st in staged do
          if st.writable then
            if st.isWorkspace then
              -- `sync_back` is about the checkout, and only the checkout: an operator turns it off
              -- because the agent pushes its work and nothing local reads the tree afterwards.
              if cfg.syncBack then
                syncOut cfg podName st.hostPath st.podPath (merge := false)
            else
              -- Memory directories come back either way. "The agent pushes its code" is not a
              -- reason to throw away what it learned, and a memory that does not outlive the pod
              -- is not a memory.
              syncOut cfg podName st.hostPath st.podPath (merge := true)
        if state matches .present "Failed" then
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
