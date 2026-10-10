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

And a `USER` that is not root: Claude Code refuses `--dangerously-skip-permissions` under uid 0,
which is how orchestra runs it, so a root image fails every task before the agent starts. Nothing
here sets a `securityContext` — the image's own `USER` is what decides, and the volumes mounted
below are `emptyDir`s, which the kubelet creates world-writable, so an unprivileged user needs
nothing granted to write the checkout, its home, or the control directory. `docs/kubernetes.md`
has the reasoning.

## credentials

Nothing sensitive is passed on a command line or written into the pod's spec, both of which are
readable by anything that can watch the cluster. The environment for each command is written to a
file inside the pod — over the same `tar` channel the checkout travels on — and sourced there.
-/

namespace Orchestra.Exec.Kubernetes

open Lean (Json fromJson?)
open Orchestra.Exec

/-- The image a task runs in when the configuration names none: the one this repository builds and
    publishes (`docker/agent.Dockerfile`, `.github/workflows/docker-agent.yml`). It carries the
    agent CLI and the six tools the backend invokes, plus `npm`, `elan` and `uv` so a repository's
    `init.sh` can install a toolchain — and none of the toolchains themselves.

    A floating tag, deliberately. Pinning a version here would be a lie within a week: the CLI
    inside is rebuilt weekly and this file is not, so the pin would name an image older than the
    one every other part of the deployment is using. An operator who wants a fixed image says so
    (`execution.options.image`, or a `claude-<version>` tag) — which `docs/kubernetes.md`
    recommends, because that is a decision about a deployment rather than a default orchestra can
    make on its behalf. -/
def defaultImage : String := "ghcr.io/chrisflav/orchestra-agent:latest"

/-! ## Configuration -/

/-- Per-task volumes: the checkout and the agent's `$HOME` on a claim of their own, kept after the
    task so that a continuation can pick up exactly where it stopped.

    Without them every pod starts from a copy of a checkout on the daemon's disk and a home that
    dies with it. A continuation then needs two things the pod no longer has: the conversation,
    which the agent CLI wrote under `$HOME`, and the tree that conversation is about — the edits,
    the branch, the build. The daemon's clone slots can hold the second only until an unrelated
    task takes the slot and resets it, which with a busy queue is within a few tasks.

    With them, a task that starts fresh gets a new claim, filled from the checkout the daemon
    prepared for it; a task that continues another one is handed its predecessor's claim, untouched,
    in a new pod. The claim is found by a label naming the last task that used it, so a chain of
    continuations — a series — keeps one volume for as long as it runs. Agents stay isolated from
    one another: no two chains share a claim, and `$HOME` is per chain rather than per daemon.

    The claim is mounted at `mountPath`; the checkout is `<mountPath>/work` and `$HOME` is
    `<mountPath>/home`, whatever the checkout is called on the daemon. So a continuation runs in the
    same directory as its predecessor — which is what the agent CLIs key their saved sessions by —
    even though each task's daemon-side checkout is a new one. Both are created by the agent's own
    user rather than being mount points, which the kubelet would make root's: git refuses a work
    tree whose top-level directory another user owns. -/
structure TaskVolumes where
  /-- `storageClassName` for the claims; unset means the cluster's default class. -/
  storageClass : Option String := none
  /-- The size each claim requests. -/
  size : String := "20Gi"
  /-- A claim no task has used for this many days is deleted, the next time a task starts. -/
  retentionDays : Nat := 14
  /-- Where the claim is mounted in the pod. The checkout is `work` and `$HOME` is `home` under it. -/
  mountPath : String := "/task"
  /-- Paths inside the checkout carried *between chains*: copied into a fresh claim from the
      daemon's seed directory for the repository, and copied back there when a task ends. Build
      output is the point — `.lake/build`, `target` — so a task that starts fresh does not rebuild
      the project from nothing. Never applied to a continuation, which has its own. -/
  seedPaths : Array String := #[]
deriving Repr, Inhabited

/-- What this backend needs from `execution.options`.

    `mcp_host` is the only one without a default — nothing can guess where a pod reaches this
    daemon, and a wrong answer is an agent that runs with no tools and says nothing about it:

    ```json
    "execution": {
      "backend": "kubernetes",
      "options": { "mcp_host": "orchestra.orchestra.svc.cluster.local" }
    }
    ```

    A deployment past its first task usually names more — the image pinned to a version, the
    namespace, and per-task volumes so a continuation finds the tree and the conversation it left:

    ```json
    "execution": {
      "backend": "kubernetes",
      "options": {
        "image": "ghcr.io/chrisflav/orchestra-agent:claude-2.1.251",
        "namespace": "orchestra",
        "mcp_host": "orchestra.orchestra.svc.cluster.local",
        "task_volumes": { "size": "20Gi", "retention_days": 14 }
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
  /-- Image a task runs in when nothing more specific applies. Defaults to `defaultImage`.

      This was required, on the reasoning that guessing would mean a pod that starts and cannot run
      anything. That was true while there was no image to guess: an operator's first act was to
      work out what "the agent CLI, sh, bash, tar, nc and git" means as a Dockerfile, and to
      discover the non-root requirement by watching every task fail at launch. Now that orchestra
      publishes one, the default is an image that does run something, and the failure it used to
      prevent is a configuration error nobody has to hit first.

      Which build and dev dependencies a task needs varies by repository, so this is the last of
      three answers, not the only one. See `imageFor`. -/
  image : String := defaultImage
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
  /-- Give each task chain a `PersistentVolumeClaim` of its own, holding its checkout and its
      `$HOME`, and keep it after the pod is gone. See `TaskVolumes`. -/
  taskVolumes : Option TaskVolumes := none
  /-- Which daemon's pods these are: the value of `instanceLabel` on every pod this backend
      creates, and what `reclaim` selects on when a daemon starts and removes what the last one
      left behind.

      A pod outlives the daemon that made it — its PID 1 is a sleep loop, and only
      `activeDeadlineSeconds` ever ends it on its own — so a daemon killed mid-task leaves pods
      that hold namespace quota for hours and, with task volumes, the workspace claim a
      continuation of the same task is refused for (`acquireWorkspaceClaim`). At startup nothing
      of the new daemon runs yet, so every pod carrying its instance is a leftover; the label is
      what makes "every pod" mean *this daemon's* and not every orchestra pod in the namespace.

      `default` unless configured. Not derived from anything on the daemon's machine — a
      containerised daemon's paths and hostname are the same in every container, or new in every
      one, and neither is what "the same daemon" means across a restart. One daemon per namespace
      is the usual deployment, and needs nothing here; two daemons sharing a namespace must set
      different values, or each one's start removes the other's running pods. -/
  inst : String := "default"
deriving Inhabited

/-- Where orchestra keeps its own files in the pod: the environment for each command. An
    `emptyDir`, so nothing survives the task. -/
def controlPath : String := "/orchestra"

/-- Whether `s` is a Kubernetes label value as one this backend selects on: 1–63 characters of
    `[A-Za-z0-9._-]`, starting and ending alphanumeric. Kubernetes itself also allows the empty
    value; a selector `key=` matching every unlabelled pod is not something to configure by
    accident, so it is refused here. -/
def validLabelValue (s : String) : Bool :=
  !s.isEmpty && s.length ≤ 63
    && s.all (fun c => c.isAlphanum || c == '.' || c == '_' || c == '-')
    && s.front.isAlphanum && s.back.isAlphanum

/-- The label naming the daemon a pod belongs to (`Config.inst`). -/
def instanceLabel : String := "orchestra.dev/instance"

/-- The selector for every pod this daemon's configuration creates, tasks and interactive sessions
    alike: what `reclaim` removes at startup. Both halves, because `managed-by` alone is every
    orchestra in the namespace and `instance` alone is a label someone else may use too. -/
def instanceSelector (cfg : Config) : String :=
  s!"app.kubernetes.io/managed-by=orchestra,{instanceLabel}={cfg.inst}"

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
  -- Absent means the image orchestra publishes, which is a working answer rather than a guess.
  -- Present-and-unreadable is still an error: a number or an object here is somebody trying to
  -- name an image, and quietly running a different one is the failure this whole field is about.
  let image ← match j.getObjVal? "image" with
    | .error _      => pure defaultImage
    | .ok (.str i)  => pure i
    | .ok other     => throw s!"kubernetes: execution.options.image must be an image reference, \
not {other.compress}"
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
  -- Present means wanted, so a value that cannot be read is refused rather than quietly turned
  -- into "no volumes": that would be every continuation failing for a reason the log never names.
  let taskVolumes ← match j.getObjVal? "task_volumes" with
    | .error _ => pure none
    | .ok (.obj _) =>
      let tv := (j.getObjVal? "task_volumes").toOption.getD Json.null
      let seedPaths ← (jsonArr? tv "seed_paths").filterMap (fun v =>
          match v with | .str s => some s | _ => none)
        |>.mapM fun p =>
          -- Same rules as `excludes`, for the same reason: these become paths under the
          -- checkout on both sides, and a `..` or a leading `/` would name something else.
          if p.isEmpty || p.startsWith "/" || (p.splitOn "..").length > 1
              || !p.all (fun c => c.isAlphanum || "._-/".any (· == c)) then
            throw s!"kubernetes: execution.options.task_volumes.seed_paths has '{p}', which is not \
a plain path inside the checkout"
          else pure p
      let mountPath := (jsonStr? tv "mount_path").getD "/task"
      unless mountPath.startsWith "/" do
        throw s!"kubernetes: execution.options.task_volumes.mount_path must be absolute, not \
'{mountPath}'"
      pure (some {
        storageClass := jsonStr? tv "storage_class"
        size := (jsonStr? tv "size").getD "20Gi"
        -- At least a day: zero would delete every other chain's claim at the next task start.
        retentionDays := max 1 (tv.getObjValAs? Nat "retention_days" |>.toOption |>.getD 14)
        mountPath
        seedPaths : TaskVolumes })
    | .ok other => throw s!"kubernetes: execution.options.task_volumes must be an object, not \
{other.compress}"
  -- Validated rather than passed through `labelValue`: two daemons configured with `a/b` and
  -- `a.b` would otherwise both run as `a.b` and remove each other's pods at every start, and
  -- nothing in either log would say why.
  let inst ← match j.getObjVal? "instance" with
    | .error _      => pure "default"
    | .ok (.str s)  =>
      if validLabelValue s then pure s
      else throw s!"kubernetes: execution.options.instance is '{s}', which is not a Kubernetes \
label value (1–63 of letters, digits, '.', '_' and '-', starting and ending with a letter or digit)"
    | .ok other     => throw s!"kubernetes: execution.options.instance must be a string, not \
{other.compress}"
  if j.getObjVal? "home_claim" |>.toOption |>.isSome then
    throw "kubernetes: execution.options.home_claim is gone: it gave every agent one shared \
$HOME. Use task_volumes, which gives each task chain its own checkout and home"
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
    taskVolumes
    repoImages := match j.getObjVal? "images" with
      | .ok (.obj kvs) => kvs.toArray.filterMap fun (k, v) =>
          match v with | .str i => some (k, i) | _ => none
      | _              => #[]
    allowRepoImage := j.getObjValAs? Bool "allow_repo_image" |>.toOption |>.getD true
    inst
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
  /-- Whether it is a single file rather than a directory: kleis's CA bundle, say. A file cannot
      be a mount point of its own — an `emptyDir` is a directory — so it is carried into one
      mounted on the directory above it, and it never comes back. -/
  isFile : Bool := false
deriving Repr, BEq, Inhabited

/-- The paths orchestra has to carry into the pod: the checkout, any plugin or memory directory
    the task was granted, and files of orchestra's own such as kleis's CA bundle (see
    `markFiles`). Everything else a session names is the image's to provide. -/
def stagedPaths (cfg : Config) (hostHome : String) (spec : SessionSpec)
    (workspaceMount : Option String := none) : Array StagedPath :=
  spec.grants.filter (·.from_ == .orchestra) |>.map fun g =>
    let hostPath := (PathGrant.resolve hostHome g).path
    let podPath  := (PathGrant.resolve cfg.homePath g).path
    let isWorkspace := podPath == spec.workdir.toString
    -- On a task volume the checkout lives at one fixed path, whatever it is called here: see
    -- `TaskVolumes`.
    let podPath := if isWorkspace then workspaceMount.getD podPath else podPath
    { hostPath, podPath
      writable := g.access == .rw || g.access == .rwx
      isWorkspace }

/-- Mark the staged paths that are files on the daemon's disk. `stagedPaths` reads only the grants,
    and a grant does not say whether its path is a file; the disk does. A path that does not exist is
    left as a directory, which `stageIn` already skips. -/
def markFiles (staged : Array StagedPath) : IO (Array StagedPath) :=
  staged.mapM fun st => do
    let p := System.FilePath.mk st.hostPath
    if (← p.pathExists) && !(← p.isDir) then return { st with isFile := true }
    return st

/-- `path` as the pod sees it. The identity unless the checkout is on a task volume, where anything
    under the daemon's checkout is under `workspaceMount` instead — the agent's working directory,
    a hook's path, an argument naming a file in the tree. -/
def podPathOf (spec : SessionSpec) (workspaceMount : Option String) (path : String) : String :=
  match workspaceMount with
  | none => path
  | some m =>
    let w := spec.workdir.toString
    if path == w then m
    else if path.startsWith (w ++ "/") then m ++ path.drop w.length
    else path

/-- The name of the volume the task's claim is mounted from. -/
def workspaceVolumeName : String := "workspace"

/-- A string as a Kubernetes label value or name part: at most `max` characters of
    `[A-Za-z0-9._-]`, starting and ending alphanumeric. Task ids already are one; this is the guard
    for anything that is not. Never empty, since an empty name part is not a valid key. -/
def labelValue (s : String) (max : Nat := 63) : String :=
  let kept := s.toList.filter (fun c => c.isAlphanum || c == '.' || c == '_' || c == '-')
  let trimmed := (kept.drop (kept.length - max)).dropWhile (!·.isAlphanum)
  let v := String.ofList (trimmed.reverse.dropWhile (!·.isAlphanum)).reverse
  if v.isEmpty then "unnamed" else v

/-- The pod manifest for a task. -/
def podManifest (cfg : Config) (spec : SessionSpec) (podName image : String)
    (staged : Array StagedPath) (workspaceClaim : Option String := none) : Json :=
  -- Every directory the agent works in has to be the agent's own, mount point included: git refuses
  -- a work tree whose top-level directory another user owns, and the kubelet creates every mount
  -- point as root. So the checkout is never a mount point itself. It is a directory the agent
  -- creates (see `openSession`) inside one: inside the claim's root with task volumes, where
  -- `$HOME` lives too, and otherwise inside an `emptyDir` mounted on the directory above it.
  -- The directory above the checkout, unless that is `/` (mounting over the root filesystem is not
  -- an option): there the checkout itself is the mount point, and a root-owned one.
  let parentOf (p : String) : String :=
    match (System.FilePath.mk p).parent.map (·.toString) with
    | some q => if q == "/" || q.isEmpty then p else q
    | none   => p
  -- Where each staged path's `emptyDir` goes, if it gets one. A file goes into one on the directory
  -- above it, the way the checkout does; two files in one directory share the first one's, since
  -- a pod with two volumes on one mount path is refused.
  let wanted : Array (Option String) := staged.map fun st =>
    if st.isWorkspace then
      if workspaceClaim.isSome then none else some (parentOf st.podPath)
    else if st.isFile then some (parentOf st.podPath)
    else some st.podPath
  let mountAt : Array (Option String) := wanted.mapIdx fun i w =>
    w.bind fun p =>
      if (wanted.extract 0 i).any (· == some p) then none else some p
  let stageMounts : Array Json := (mountAt.mapIdx fun i m =>
    m.map fun p => Json.mkObj [("name", .str (stageVolumeName i)), ("mountPath", .str p)])
    |>.filterMap id
  let stageVolumes : Array Json := (mountAt.mapIdx fun i m =>
    m.map fun _ => Json.mkObj [("name", .str (stageVolumeName i)), ("emptyDir", Json.mkObj [])])
    |>.filterMap id
  -- The claim, mounted whole: the checkout and `$HOME` are directories in it, made by the agent.
  -- Without one, `$HOME` is a scratch `emptyDir` of its own.
  let (homeVolumes, homeMounts) := match workspaceClaim, cfg.taskVolumes with
    | some claim, some tv =>
      (#[Json.mkObj [("name", .str workspaceVolumeName),
          ("persistentVolumeClaim", Json.mkObj [("claimName", .str claim)])]],
       #[Json.mkObj [("name", .str workspaceVolumeName), ("mountPath", .str tv.mountPath)]])
    | _, _ =>
      (#[Json.mkObj [("name", .str "home"), ("emptyDir", Json.mkObj [])]],
       #[Json.mkObj [("name", .str "home"), ("mountPath", .str cfg.homePath)]])
  let volumes : Array Json :=
    stageVolumes
      ++ #[Json.mkObj [("name", .str "control"), ("emptyDir", Json.mkObj [])]] ++ homeVolumes
      ++ cfg.extraVolumes
  let mounts : Array Json :=
    stageMounts
      ++ #[Json.mkObj [("name", .str "control"), ("mountPath", .str controlPath)]] ++ homeMounts
      ++ cfg.extraMounts
  -- `HOME` is set here rather than passed through: the image's idea of home is not orchestra's,
  -- and every home-relative path the agent backend declared was resolved against `homePath`.
  -- Nothing else is set on the pod. Credentials reach each command through a file (see
  -- `envFilePath`), because a pod's spec is readable by anything that can list pods.
  --
  -- The working directory is the control directory, not the checkout: a container runtime creates
  -- a missing working directory itself, as root, which is exactly the ownership this avoids. Every
  -- command `cd`s to the checkout anyway (`runnerScript`).
  let container := Json.mkObj ([
    ("name", .str "agent"),
    ("image", .str image),
    ("command", .arr #[.str "/bin/sh", .str "-c", .str idleScript]),
    ("workingDir", .str controlPath),
    ("env", .arr #[Json.mkObj [("name", .str "HOME"), ("value", .str cfg.homePath)]]),
    ("volumeMounts", .arr mounts)
  ] ++ (match cfg.imagePullPolicy with
        | some p => [("imagePullPolicy", Json.str p)] | none => [])
     ++ (match cfg.resources with | some r => [("resources", r)] | none => []))
  -- A claim's root is whatever its storage class makes it: k3s's `local-path` hands out a
  -- world-writable directory, a block-backed class an ext4 root that only root may write. The agent
  -- has to create `work` and `home` in it as its own user, so a root init container opens the
  -- root up first — sticky, like `/tmp`, so no agent could remove what another made. It needs no
  -- uid to be named; only the claim, and only once per pod.
  let initContainers : List (String × Json) := match workspaceClaim, cfg.taskVolumes with
    | some _, some tv =>
      [("initContainers", .arr #[Json.mkObj ([
        ("name", .str "open-workspace"),
        ("image", .str image),
        ("command", .arr #[.str "/bin/sh", .str "-c", .str s!"chmod 1777 {shellEscape tv.mountPath}"]),
        ("securityContext", Json.mkObj [("runAsUser", .num 0)]),
        ("volumeMounts", .arr #[Json.mkObj [("name", .str workspaceVolumeName),
                                            ("mountPath", .str tv.mountPath)]])
      ] ++ (match cfg.imagePullPolicy with
            | some p => [("imagePullPolicy", Json.str p)] | none => []))])]
    | _, _ => []
  let podSpec := Json.mkObj ([
    ("restartPolicy", .str "Never"),
    ("activeDeadlineSeconds", .num cfg.deadlineSeconds),
    ("containers", .arr #[container]),
    ("volumes", .arr volumes)
  ] ++ initContainers ++ (match cfg.serviceAccount with
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
        (instanceLabel, .str cfg.inst),
        ("orchestra.dev/task", .str (labelValue spec.label))]
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

/-- The staged paths that sit strictly inside `hostPath`, as names relative to it (`./name`).

    A task granted `memory: both` is granted two memory directories, and the project one lives
    *inside* the global one (`<data>/memory` and `<data>/memory/<project>`, see
    `TaskRunner.resolveMemoryDirs`). Each staged path becomes an `emptyDir` of its own, so in the
    pod the second mount point sits inside the first — and the kubelet creates it root-owned and
    world-writable, like every other one.

    That is what makes the outer copy fail. `--no-overwrite-dir` preserves the metadata of
    directories that already exist: `tar` records the mount point's mode before extracting and
    `chmod`s it back afterwards, which the unprivileged user extracting does not own and may not
    do. The flag that protects the extraction root breaks on a mount point *below* it:

      tar: ./<project>: Cannot change mode to rwxrwxrwx: Operation not permitted

    So the inner path is left out of the outer archive. Nothing is lost by it: the nested path is
    staged in its own right, into the mount that is its actual home, and copying it twice was only
    ever writing the same bytes through a second door. -/
def nestedUnder (hostPath : String) (paths : Array String) : Array String :=
  let root := if hostPath.endsWith "/" then hostPath.dropRight 1 else hostPath
  paths.filterMap fun p =>
    if p.startsWith (root ++ "/") then some ("." ++ p.drop root.length) else none

/-- Copy a directory into the pod. Skipped, not failed, when the source is not there: an agent
    backend may declare a plugin directory this machine does not have, exactly as with landrun.

    The archive holds the directory's *entries*, not the directory. `tar -cf - .` would record a
    `./` member, and restoring that member means `chmod` and `utime` on the extraction root — which
    here is a mount point owned by root, under a `tar` that is not root, so the whole extraction
    fails on the first thing it does. Nothing wants that member: the mount point already exists,
    and its mode is the kubelet's business rather than the daemon's. -/
private def stageIn (cfg : Config) (podName hostPath podPath : String)
    (alsoStaged : Array String := #[]) : IO Unit := do
  unless ← System.FilePath.pathExists (System.FilePath.mk hostPath) do return ()
  -- An empty directory has no entries to list, and `tar` refuses to create an empty archive.
  -- There is also nothing to carry: the mount point is already there.
  if (← (System.FilePath.mk hostPath).readDir).isEmpty then return ()
  -- The configured excludes, plus any staged path nested under this one: see `nestedUnder`.
  let nested := (nestedUnder hostPath alsoStaged).flatMap fun n => #["--exclude", shellEscape n]
  let excludes := String.intercalate " " ((excludeArgs cfg) ++ nested).toList
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

/-! ## Task volumes

A claim per task chain (`TaskVolumes`). Each task that works on a claim adds a label naming itself,
which is how a continuation — of any task in the chain, including a retry of one that failed —
finds it. While a pod holds the claim it is annotated with that task, so a second continuation of
the same task is refused rather than mounted beside the first. The last-use time is an annotation
in epoch seconds, which is what retention reads. -/

/-- The label marking a workspace claim as used by `taskId`. The name part of a label key is at most
    63 characters, two of which are the `t-`. -/
def taskLabel (taskId : String) : String := s!"orchestra.dev/t-{labelValue taskId 61}"

/-- The annotation naming the task whose pod holds a workspace claim right now, and since when:
    `<task>@<epoch seconds>`. The time matters for the window between taking the claim and the pod
    existing, when the holder has no pod to show for it yet. -/
def inUseAnnotation : String := "orchestra.dev/in-use"

/-- The annotation recording when a workspace claim was last used, in epoch seconds. -/
def lastUsedAnnotation : String := "orchestra.dev/last-used"

/-- The value of `inUseAnnotation` for `taskId`, taken at `now`. -/
def inUseValue (taskId : String) (now : Nat) : String := s!"{labelValue taskId}@{now}"

/-- The claim a new task chain is given: labelled for the task that made it, and marked in use by it
    from the moment it exists, so there is no instant at which another task could take it. -/
def workspaceClaimManifest (cfg : Config) (tv : TaskVolumes) (spec : SessionSpec)
    (name taskId : String) (now : Nat) : Json :=
  Json.mkObj [
    ("apiVersion", .str "v1"),
    ("kind", .str "PersistentVolumeClaim"),
    ("metadata", Json.mkObj [
      ("name", .str name),
      ("namespace", .str cfg.ns),
      ("labels", Json.mkObj ([
        ("app.kubernetes.io/managed-by", .str "orchestra"),
        ("orchestra.dev/kind", .str "workspace"),
        (taskLabel taskId, .str "1")]
        ++ (match spec.repo with
            | some r => [("orchestra.dev/repo", Json.str (labelValue (r.replace "/" ".")))]
            | none   => []))),
      ("annotations", Json.mkObj [(lastUsedAnnotation, .str (toString now)),
        (inUseAnnotation, .str (inUseValue taskId now))])]),
    ("spec", Json.mkObj ([
      ("accessModes", .arr #[.str "ReadWriteOnce"]),
      ("resources", Json.mkObj [("requests", Json.mkObj [("storage", .str tv.size)])])]
      ++ (match tv.storageClass with
          | some c => [("storageClassName", Json.str c)] | none => [])))]

/-- The wall clock, in epoch seconds. `date` rather than a Lean API: the standard library's clocks
    are monotonic, and this is compared against a timestamp another daemon run wrote. -/
private def epochNow : IO Nat := do
  let out ← IO.Process.output { cmd := "date", args := #["+%s"] }
  return out.stdout.trimAscii.toNat?.getD 0

/-- One workspace claim as the cluster reports it. -/
structure ClaimRow where
  name : String
  resourceVersion : String
  /-- `inUseAnnotation`, empty when nobody holds it. -/
  inUse : String
  /-- `lastUsedAnnotation`. -/
  lastUsed : Option Nat
  /-- Set once the claim is being deleted; such a claim is never handed out. -/
  terminating : Bool

/-- The workspace claims matching `selector`. -/
private def listClaims (cfg : Config) (selector : String) : IO (Except String (List ClaimRow)) := do
  let (code, out, err) ← kube cfg #["get", "pvc", "-l", selector, "-o",
    "jsonpath={range .items[*]}{.metadata.name}{\"\\t\"}{.metadata.resourceVersion}{\"\\t\"}\
{.metadata.annotations.orchestra\\.dev/in-use}{\"\\t\"}{.metadata.annotations.orchestra\\.dev/last-used}\
{\"\\t\"}{.metadata.deletionTimestamp}{\"\\n\"}{end}"]
  if code != 0 then return .error err.trimAscii.toString
  return .ok <| (out.splitOn "\n").filterMap fun line =>
    match line.splitOn "\t" with
    | [n, rv, use, used, del] =>
      if n.trimAscii.isEmpty then none
      else some { name := n.trimAscii.toString, resourceVersion := rv.trimAscii.toString
                  inUse := use.trimAscii.toString, lastUsed := used.trimAscii.toString.toNat?
                  terminating := !del.trimAscii.isEmpty }
    | _ => none

/-- The task named in an in-use mark (`inUseValue`), and when it took the claim. A mark that cannot
    be read names itself, taken at the epoch — so it is judged by its pods alone. -/
def parseInUse (inUse : String) : String × Nat :=
  match inUse.splitOn "@" with
  | [h, t] => (h, t.toNat?.getD 0)
  | _      => (inUse, 0)

/-- Whether the holder named in an in-use mark is still working, as a decision over what is known.

    `podsAlive` is the answer from the cluster — `some true` when the holder has a pod that is up
    (not finished, not failed), `some false` when it has none, `none` when the cluster could not
    be asked; not knowing is not the same as knowing it is gone, so that counts as alive, since an
    API hiccup must not hand one tree to two agents. Before that, a holder that took the claim
    less than `grace` seconds ago is alive whatever its pods say: its pod may simply not exist yet.

    `deadPredecessor` is a task the caller knows to be dead (`SessionSpec.predecessorDead`): a
    restart resume's predecessor, swept and reclaimed at startup. When the mark names it, the grace
    is skipped and only live pods count — the grace exists for a holder that might be about to
    have a pod, and this one never will. Without that, a task killed within its first few minutes
    could never be resumed. The pods still decide: a live one refuses, as always. -/
def holderStillWorking (inUse : String) (now grace : Nat) (podsAlive : Option Bool)
    (deadPredecessor : Option String := none) : Bool :=
  if inUse.isEmpty then false
  else
    let (holder, since) := parseInUse inUse
    let knownDead := deadPredecessor.any (labelValue · == holder)
    if !knownDead && now < since + grace then true
    else podsAlive.getD true

/-- How long after taking a claim a holder counts as alive without a pod: long enough for its pod
    to be created and become ready. -/
def holderGraceSeconds (cfg : Config) : Nat := cfg.startupTimeoutSeconds + 120

/-- Whether the holder named in an in-use annotation is still working (`holderStillWorking`), asking
    the cluster for its pods only when the decision needs them. A holder whose daemon died has no
    pod up and, once the grace is past, its claim can be taken over. -/
private def holderAlive (cfg : Config) (inUse : String) (now : Nat)
    (deadPredecessor : Option String := none) : IO Bool := do
  if inUse.isEmpty then return false
  -- Decided without the cluster when it can be: within the grace the pods are not consulted.
  -- (Asked with "no pods", a `true` can only have come from the grace.)
  if holderStillWorking inUse now (holderGraceSeconds cfg) (some false) deadPredecessor then
    return true
  let (holder, _) := parseInUse inUse
  let (code, pods, _) ← kube cfg #["get", "pods", "-l",
    s!"app.kubernetes.io/managed-by=orchestra,orchestra.dev/task={labelValue holder}",
    "--field-selector=status.phase!=Failed,status.phase!=Succeeded", "-o", "name"]
  let podsAlive := if code != 0 then none else some !pods.trimAscii.isEmpty
  return holderStillWorking inUse now (holderGraceSeconds cfg) podsAlive deadPredecessor

/-- Delete workspace claims nobody has used for `retentionDays`. Best effort, and quiet about it:
    a sweep that fails costs disk, not a task. A claim whose holder is still working is skipped
    however old its last-use stamp, and so is one already on its way out. -/
private def sweepWorkspaceClaims (cfg : Config) (tv : TaskVolumes) : IO Unit := do
  try
    let .ok rows ← listClaims cfg "app.kubernetes.io/managed-by=orchestra,orchestra.dev/kind=workspace"
      | return
    let now ← epochNow
    let cutoff := max 1 tv.retentionDays * 86400
    for r in rows do
      if r.terminating then continue
      let some t := r.lastUsed | continue
      if now ≤ t + cutoff then continue
      if ← holderAlive cfg r.inUse now then continue
      IO.println s!"  [k8s] deleting workspace claim {r.name}: unused for over {tv.retentionDays} days"
      -- Marked first, by the same compare-and-swap a task takes a claim with, so a continuation that
      -- took it a moment ago keeps it: the delete only follows if nobody changed it since the read.
      let (mc, _, _) ← kube cfg #["annotate", "pvc", r.name, "--overwrite",
        s!"--resource-version={r.resourceVersion}", s!"{inUseAnnotation}=retention@{now}"]
      if mc == 0 then
        let _ ← kube cfg #["delete", "pvc", r.name, "--wait=false", "--ignore-not-found"]
  catch _ => pure ()

/-- Take a claim for `taskId`: one compare-and-swap on the in-use annotation, against the version
    the caller read, so two tasks reading "free" at once cannot both win. Then the label a
    continuation of this task will look it up by. -/
private def takeClaim (cfg : Config) (row : ClaimRow) (taskId : String) : IO Bool := do
  let now ← epochNow
  let (code, _, _) ← kube cfg #["annotate", "pvc", row.name, "--overwrite",
    s!"--resource-version={row.resourceVersion}",
    s!"{inUseAnnotation}={inUseValue taskId now}", s!"{lastUsedAnnotation}={now}"]
  if code != 0 then return false
  let (lcode, _, lerr) ← kube cfg #["label", "pvc", row.name, "--overwrite", s!"{taskLabel taskId}=1"]
  if lcode != 0 then
    throw (IO.userError s!"kubernetes: could not label workspace claim {row.name}: {lerr.trimAscii}")
  return true

/-- Let go of a claim once the task's pod is gone — if it is still this task's to let go of. Another
    task may have taken it over in the meantime (`holderAlive` judged this one gone), and clearing
    *its* mark would let a third mount the claim beside it; so the mark is removed only if it still
    names `taskId`, as a compare-and-swap on the version just read. Best effort. -/
private def releaseWorkspace (cfg : Config) (name taskId : String) : IO Unit := do
  try
    let (code, out, _) ← kube cfg #["get", "pvc", name, "-o",
      "jsonpath={.metadata.resourceVersion}{\"\\t\"}{.metadata.annotations.orchestra\\.dev/in-use}"]
    if code != 0 then return
    match out.splitOn "\t" with
    | [rv, inUse] =>
      unless inUse.trimAscii.toString.startsWith s!"{labelValue taskId}@" do return
      let now ← epochNow
      let _ ← kube cfg #["annotate", "pvc", name, "--overwrite",
        s!"--resource-version={rv.trimAscii}", s!"{lastUsedAnnotation}={now}", s!"{inUseAnnotation}-"]
    | _ => pure ()
  catch _ => pure ()

/-- Re-stamp this task's in-use mark with the time now, if the mark is still its own: for a task
    that holds a claim while it waits for a pod, which `holderAlive` would otherwise judge gone once
    the startup window passed with no pod to show for it. The same compare-and-swap as
    `releaseWorkspace`. Best effort. -/
private def renewWorkspace (cfg : Config) (name taskId : String) : IO Unit := do
  try
    let (code, out, _) ← kube cfg #["get", "pvc", name, "-o",
      "jsonpath={.metadata.resourceVersion}{\"\\t\"}{.metadata.annotations.orchestra\\.dev/in-use}"]
    if code != 0 then return
    match out.splitOn "\t" with
    | [rv, inUse] =>
      unless inUse.trimAscii.toString.startsWith s!"{labelValue taskId}@" do return
      let _ ← kube cfg #["annotate", "pvc", name, "--overwrite",
        s!"--resource-version={rv.trimAscii}", s!"{inUseAnnotation}={inUseValue taskId (← epochNow)}"]
    | _ => pure ()
  catch _ => pure ()

/-- Whether `kubectl create` was refused for want of room in the namespace's `ResourceQuota`: a
    state that passes as pods finish, unlike a manifest the API will never accept. -/
def quotaExceeded (err : String) : Bool :=
  (err.splitOn "exceeded quota").length > 1

/-- How often a task waiting for room in the namespace looks again. How long it waits is
    `SessionSpec.roomWaitSeconds`. -/
def quotaPollSeconds : Nat := 15

/-- Make a new claim for `taskId`, already marked in use by it. -/
private def createClaim (cfg : Config) (tv : TaskVolumes) (spec : SessionSpec) (taskId : String)
    : IO String := do
  let name := s!"orchestra-ws-{← randomHex 8}"
  let dir := System.FilePath.mk s!"/tmp/orchestra-k8s-{← randomHex 8}"
  IO.FS.createDirAll dir
  let path := dir / "pvc.json"
  IO.FS.writeFile path (workspaceClaimManifest cfg tv spec name taskId (← epochNow)).compress
  let (code, _, err) ← kube cfg #["create", "-f", path.toString]
  try IO.FS.removeDirAll dir catch _ => pure ()
  if code != 0 then
    throw (IO.userError s!"kubernetes: could not create workspace claim {name}: {err.trimAscii}")
  return name

/-- The claim this task works on, and whether it is one a predecessor left (so its tree is already
    there and must not be overwritten).

    A continuation is handed the claim its predecessor used, or refused: starting it on a fresh
    tree would answer a follow-up prompt with a model whose context describes edits that are not
    there — unless the caller said a fresh one will do (`SessionSpec.continuationOptional`, for a
    chat session whose agent never got as far as starting). A claim another task is still working
    in is refused too: two agents in one tree is how each undoes the other's work. Anything that
    continues nothing gets a new claim. -/
private def acquireWorkspaceClaim (cfg : Config) (tv : TaskVolumes) (spec : SessionSpec)
    (taskId : String) : IO (String × Bool) := do
  sweepWorkspaceClaims cfg tv
  let some prev := spec.continuesFrom | return (← createClaim cfg tv spec taskId, false)
  -- A few rounds, for the case where another task changes the claim between our read and our
  -- swap: that one is either taking it (and the next read says so) or letting it go.
  for _ in [0:3] do
    let rows ← match ← listClaims cfg s!"app.kubernetes.io/managed-by=orchestra,{taskLabel prev}" with
      | .ok rs   => pure (rs.filter (!·.terminating))
      | .error e => throw (IO.userError s!"kubernetes: could not look up the workspace of {prev}: {e}")
    match rows with
    | [] =>
      if spec.continuationOptional then return (← createClaim cfg tv spec taskId, false)
      throw (IO.userError s!"kubernetes: {taskId} continues {prev}, but no workspace volume is \
left for {prev} — it ran before task volumes were enabled, or went unused for more than \
{tv.retentionDays} days and was deleted. Queue the work as a task of its own.")
    | [row] =>
      let now ← epochNow
      let ownHold := row.inUse.startsWith s!"{labelValue taskId}@"
      -- A restart resume's predecessor is known dead (`SessionSpec.predecessorDead`), so a mark
      -- naming it is judged by live pods alone, without the startup grace; any other holder —
      -- including some third task that took the claim since — keeps the grace.
      let deadPredecessor := if spec.predecessorDead then some prev else none
      if !ownHold && (← holderAlive cfg row.inUse now deadPredecessor) then
        throw (IO.userError s!"kubernetes: {taskId} continues {prev}, whose workspace volume \
{row.name} is in use by {(row.inUse.splitOn "@").headD row.inUse} right now. Two agents in one \
tree would undo each other's work; continue that task instead, or wait for it to finish.")
      -- Its own mark is a waking session whose last pod may have outlived a daemon that died:
      -- that pod's agent can still be running, and a second one in the same tree would undo the
      -- first's work. So whatever of its own is left is removed before the claim is taken back.
      if ownHold then
        let (dc, _, derr) ← kube cfg #["delete", "pods", "-l",
          s!"app.kubernetes.io/managed-by=orchestra,orchestra.dev/task={labelValue taskId}",
          "--now", "--ignore-not-found", "--timeout=120s"]
        if dc != 0 then
          throw (IO.userError s!"kubernetes: {taskId} has a pod left over from before that could \
not be removed ({derr.trimAscii}); refusing to start a second agent in its workspace")
      if ← takeClaim cfg row taskId then return (row.name, true)
    | many =>
      throw (IO.userError s!"kubernetes: more than one workspace volume is labelled as used by \
{prev} ({String.intercalate ", " (many.map (·.name))}); refusing to guess which one {taskId} continues")
  throw (IO.userError s!"kubernetes: the workspace volume of {prev} kept changing while {taskId} \
tried to take it; another task is contending for it")

/-- Copy the seed paths (`TaskVolumes.seedPaths`) the daemon holds for this repository into a fresh
    workspace. Missing ones are skipped — the first task on a repository has nothing to seed from —
    and a failed copy is a warning, not a failed task: a seed is a head start, not a result. -/
private def stageSeeds (cfg : Config) (tv : TaskVolumes) (podName : String)
    (seedDir : System.FilePath) (mount : String) : IO Unit := do
  for p in tv.seedPaths do
    let src := seedDir / p
    try
      if ← src.isDir then
        mkdirInPod cfg podName s!"{mount}/{p}"
        stageIn cfg podName src.toString s!"{mount}/{p}"
    catch e =>
      IO.eprintln s!"  [k8s] warning: could not seed {p} into the new workspace: {e}"

/-- Copy the seed paths back out of the pod into the daemon's seed directory, each replacing what
    was there. Best effort, like staging them. Serialised per repository with `flock`, so two tasks
    ending at once cannot interleave their swaps; the archive is written to a file before anything
    is moved, so a broken stream leaves the previous seed whole rather than installing half of one. -/
private def exportSeeds (cfg : Config) (tv : TaskVolumes) (podName : String)
    (seedDir : System.FilePath) (mount : String) : IO Unit := do
  for p in tv.seedPaths do
    let target := (seedDir / p).toString
    let tag ← randomHex 4
    let incoming := s!"{target}.orchestra-incoming-{tag}"
    let previous := s!"{target}.orchestra-previous-{tag}"
    let archive := s!"{target}.orchestra-{tag}.tar"
    let kubectl := s!"{shellEscape cfg.kubectl} -n {shellEscape cfg.ns} exec {shellEscape podName} --"
    let script := s!"set -e\n\
mkdir -p {shellEscape seedDir.toString} \"$(dirname {shellEscape target})\"\n\
exec 9> {shellEscape (seedDir / ".lock").toString}\n\
flock 9\n\
{kubectl} test -d {shellEscape s!"{mount}/{p}"} || exit 0\n\
trap 'rm -rf {shellEscape incoming} {shellEscape archive}' EXIT\n\
{kubectl} tar -C {shellEscape s!"{mount}/{p}"} -cf - . > {shellEscape archive}\n\
mkdir -p {shellEscape incoming}\n\
tar -C {shellEscape incoming} -xf {shellEscape archive}\n\
if [ -e {shellEscape target} ]; then mv -T {shellEscape target} {shellEscape previous}; fi\n\
mv -T {shellEscape incoming} {shellEscape target}\n\
rm -rf {shellEscape previous}\n"
    let (code, _, err) ← shell script
    if code != 0 then
      let _ ← shell s!"if [ ! -e {shellEscape target} ] && [ -d {shellEscape previous} ]; then \
mv -T {shellEscape previous} {shellEscape target}; fi"
      IO.eprintln s!"  [k8s] warning: could not keep {p} as the seed for the next task: {err.trimAscii}"

/-! ## The session -/

/-- Open a pod for one task, and hand back the handle on it. -/
def openSession (cfg : Config) (spec : SessionSpec) : IO Session := do
  let home ← hostHome
  let image ← match imageFor cfg spec with
    | .ok i    => pure i
    | .error e => throw (IO.userError s!"kubernetes: {e}")
  -- A task volume only for what asks for one (`taskId`): the merger and `prepare` have nothing
  -- to continue and nothing worth keeping, and get the throwaway pod they always had.
  let volume : Option (TaskVolumes × String × Bool) ← match cfg.taskVolumes, spec.taskId with
    | some tv, some tid =>
      let (claim, reused) ← acquireWorkspaceClaim cfg tv spec tid
      if reused then
        IO.println s!"  [k8s] continuing on workspace volume {claim} ({spec.continuesFrom.getD "?"}'s)"
      else
        IO.println s!"  [k8s] new workspace volume {claim}"
      pure (some (tv, claim, reused))
    | _, _ => pure none
  -- On a task volume the checkout and `$HOME` are directories on the claim (see `TaskVolumes`);
  -- `ecfg` is the configuration with `homePath` pointing there, so every home-relative path the
  -- agent backend declared lands on the claim as well.
  let ecfg : Config := match volume with
    | some (tv, _, _) => { cfg with homePath := s!"{tv.mountPath}/home" }
    | none            => cfg
  let workspaceMount := volume.map (fun (tv, _, _) => s!"{tv.mountPath}/work")
  let staged ← markFiles (stagedPaths ecfg home spec workspaceMount)
  let toPod := podPathOf spec workspaceMount
  let podName := s!"orchestra-{← randomHex 6}"
  let manifest := podManifest ecfg spec podName image staged (volume.map (·.2.1))
  let dir := System.FilePath.mk s!"/tmp/orchestra-k8s-{← randomHex 8}"
  IO.FS.createDirAll dir
  let manifestPath := dir / "pod.json"
  IO.FS.writeFile manifestPath manifest.compress
  -- The claim is let go of whenever the pod is: when the task is done with it, and on every way
  -- of failing to start one. A claim made for this task and never used is deleted rather than kept:
  -- it holds an empty or half-copied tree, and a start that keeps failing would otherwise leave one
  -- behind per attempt.
  let releaseClaim : IO Unit := match volume with
    | some (_, claim, _) => releaseWorkspace cfg claim (spec.taskId.getD "")
    | none => pure ()
  let abandonClaim : IO Unit := match volume with
    | some (_, claim, false) =>
      discard <| (try kube cfg #["delete", "pvc", claim, "--wait=false", "--ignore-not-found"]
                  catch _ => pure (0, "", ""))
    | _ => releaseClaim
  -- A full quota is not the task's fault. It fills when something other than this daemon's own
  -- tasks holds pods there — a previous daemon's, an operator's — and failing every entry the
  -- queue reaches meanwhile empties the queue in seconds: on 2026-10-10 three hundred entries were
  -- spent that way in a quarter of an hour. So it is waited on, briefly, and then answered with
  -- `noRoom`, which puts the entry back to `pending`. Briefly because the task's credentials
  -- were minted before this and are ticking, and because a stop must not wait on it: a task with
  -- no pod yet is not one a drain should finish.
  let gaveUp : IO Bool := do return (← spec.cancelled) || (← Exec.stopRequested)
  let result ← try
      let mut waited := 0
      let mut announced := false
      let mut result ← kube cfg #["create", "-f", manifestPath.toString]
      while result.1 != 0 && quotaExceeded result.2.2 && waited < spec.roomWaitSeconds do
        unless announced do
          announced := true
          IO.println s!"  [k8s] namespace {cfg.ns} is at its quota; waiting for room \
(up to {spec.roomWaitSeconds}s): {result.2.2.trimAscii}"
        for _ in [0:quotaPollSeconds] do
          if ← gaveUp then break
          IO.sleep 1000
        if ← gaveUp then break
        waited := waited + quotaPollSeconds
        if let some (_, claim, _) := volume then renewWorkspace cfg claim (spec.taskId.getD "")
        result ← kube cfg #["create", "-f", manifestPath.toString]
      if announced && result.1 == 0 then
        IO.println s!"  [k8s] room in {cfg.ns} after {waited}s; pod {podName} created"
      pure result
    catch e =>
      try IO.FS.removeDirAll dir catch _ => pure ()
      abandonClaim
      throw e
  try IO.FS.removeDirAll dir catch _ => pure ()
  let (code, _, err) := result
  if code != 0 then
    abandonClaim
    if ← spec.cancelled then
      throw (IO.userError s!"kubernetes: cancelled while waiting for room in namespace {cfg.ns}")
    if quotaExceeded err then
      throw (Exec.noRoom s!"namespace {cfg.ns} is at its quota: {err.trimAscii}")
    throw (IO.userError s!"kubernetes: could not create pod {podName}: {err.trimAscii}")
  -- On a task volume the pod is waited for before the claim is let go of: a continuation that
  -- mounted it while a cancelled agent was still writing would be two agents in one tree.
  -- Whether the pod is known to be gone. If the delete did not finish in time, the claim is left
  -- marked: `holderAlive` frees it once the pod really is gone, and releasing it now would let a
  -- continuation mount it beside an agent still writing.
  let removePod : IO Bool := do
    let (code, _, _) ← try kube cfg #["delete", "pod", podName, "--now", "--ignore-not-found",
                           if volume.isSome then "--timeout=120s" else "--wait=false"]
            catch _ => pure (1, "", "")
    return code == 0
  let deletePod : IO Unit := do
    if ← removePod then releaseClaim
    else IO.eprintln s!"  [k8s] pod {podName} did not go away in time; its workspace claim stays \
marked until it does"
  let failPod : IO Unit := do
    if ← removePod then abandonClaim
  -- Ready, not merely created: the next thing this does is copy a repository through
  -- `kubectl exec`, which needs a container that has actually started.
  let (waitCode, _, waitErr) ← kube cfg #["wait", "--for=condition=Ready", s!"pod/{podName}",
    s!"--timeout={cfg.startupTimeoutSeconds}s"]
  if waitCode != 0 then
    let why ← startupDiagnosis cfg podName
    failPod
    throw (IO.userError s!"kubernetes: pod {podName} did not become ready within \
{cfg.startupTimeoutSeconds}s ({why}): {waitErr.trimAscii}")
  try
    -- The directories the agent works in, made by the agent's own user (every `exec` runs as the
    -- image's user) before anything lands in them, so that they are its own and not root's — the
    -- checkout and, on a claim, `$HOME`. A no-op on a claim a predecessor left, where they exist.
    if let some ws := staged.find? (·.isWorkspace) then
      mkdirInPod cfg podName ws.podPath
    if volume.isSome then
      mkdirInPod cfg podName ecfg.homePath
    for st in staged do
      -- A file is written into the `emptyDir` above it; `stageIn` carries a directory's entries.
      if st.isFile then
        putFile cfg podName st.podPath (← IO.FS.readFile st.hostPath)
        continue
      match volume with
      | some (tv, _, reused) =>
        if st.isWorkspace then
          -- A continuation's tree is already on the claim, exactly as its predecessor left it;
          -- copying the daemon's checkout over it would undo the very thing it was kept for.
          unless reused do
            stageIn cfg podName st.hostPath st.podPath (staged.map (·.hostPath))
            if let some seedDir := spec.seedDir then
              stageSeeds cfg tv podName seedDir st.podPath
        else
          stageIn cfg podName st.hostPath st.podPath (staged.map (·.hostPath))
      | none =>
        stageIn cfg podName st.hostPath st.podPath (staged.map (·.hostPath))
  catch e =>
    failPod
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
    -- when the pod is, so a task cannot continue one an earlier task started; on a task volume,
    -- home is the chain's own and outlives the pod.
    carriesAgentState := volume.isSome
    mcpEndpoint := fun e => pure { e with host := cfg.mcpHost }
    describe := fun run => do
      let envFile := envFilePath 0
      let rendered := String.intercalate " "
        ((#[cfg.kubectl] ++ execArgs cfg podName (run.stdio == .inherit)
           (runnerScript envFile (toPod run.workdir.toString)) run.command (run.args.map toPod)).toList.map shellEscape)
      return s!"[debug] {cfg.kubectl} -n {cfg.ns} create -f - <<'EOF'\n{manifest.pretty}\nEOF\n\
[debug] {rendered}"
    start := fun run => do
      let envFile ← nextEnvFile run.env
      -- `-i` for a stream as well as for a terminal: both need our end of stdin open, and only
      -- the interactive one wants a TTY.
      let args := execArgs cfg podName (interactive := run.stdio == .inherit)
        (stdinOpen := run.stdio != .piped)
        (runnerScript envFile (toPod run.workdir.toString) (guard := true)) run.command (run.args.map toPod)
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
            (PathGrant.resolve ecfg.homePath g).path
    runScript := fun script => do
      -- The repository's own scripts, run where the agent works. `bash` because that is what the
      -- daemon used when it ran them itself, and repositories were written against it.
      let envFile ← nextEnvFile #[]
      let args := execArgs cfg podName false
        (runnerScript envFile (toPod script.workdir.toString)) "bash" #[toPod script.path]
      match script.stdio with
      -- A repository's script is run, not conversed with; `.stream` is captured like `.piped`.
      | .inherit =>
        let child ← IO.Process.spawn {
          cmd := cfg.kubectl, args, stdin := .null, stdout := .inherit, stderr := .inherit }
        return { exitCode := ← child.wait }
      | .piped | .stream =>
        let child ← IO.Process.spawn {
          cmd := cfg.kubectl, args, stdin := .null, stdout := .piped, stderr := .piped }
        -- Both pipes drained at once: a build writing more than a pipe buffer to stderr would
        -- otherwise block there while stdout is read to the end, and never finish.
        let stderrTask ← IO.asTask (prio := .dedicated) child.stderr.readToEnd
        let stdout ← child.stdout.readToEnd
        let stderr ← match ← IO.wait stderrTask with
          | .ok s => pure s
          | .error e => throw e
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
          -- A file is something orchestra handed the agent, never something it hands back.
          if st.writable && !st.isFile then
            if st.isWorkspace then
              match volume with
              | some (tv, _, _) =>
                -- The tree stays where it is, on the claim, for whatever continues this task: the
                -- daemon's checkout was only what a fresh workspace was filled from, and nothing
                -- reads it afterwards. What does come back is the build, as the head start for the
                -- next task that starts fresh on this repository.
                if let some seedDir := spec.seedDir then
                  exportSeeds cfg tv podName seedDir st.podPath
              | none =>
                -- `sync_back` is about the checkout, and only the checkout: an operator turns it
                -- off because the agent pushes its work and nothing local reads the tree afterwards.
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
  -- Task volumes need their own verbs, and a Role written for the pods alone is the usual way to
  -- be missing them: every task would then fail at its first `kubectl create pvc`.
  if cfg.taskVolumes.isSome then
    for verb in ["get", "list", "create", "patch", "delete"] do
      let (code, out, err) ← kube cfg #["auth", "can-i", verb, "persistentvolumeclaims"]
      if code != 0 || out.trimAscii.toString != "yes" then
        return .error s!"task_volumes is set, but this daemon may not {verb} \
persistentvolumeclaims in namespace '{cfg.ns}' ({(out ++ err).trimAscii}). It needs \
get/list/create/patch/delete on persistentvolumeclaims as well as what pods need."
  return .ok ()

/-- Remove every pod a previous daemon with this configuration left behind (`Backend.reclaim`).

    Called at startup, before the first worker or interactive session exists, so every pod carrying
    this daemon's instance (`instanceSelector`) belongs to a process that is gone: queued tasks
    whose agents died with their `kubectl exec` streams, mergers, and interactive sessions — whose
    records `Interactive.reconcile` puts to sleep a moment later, and whose next turn opens a new
    pod on the same claim (and would remove a leftover of its own anyway: see the `ownHold` case
    of `acquireWorkspaceClaim`). Nothing in them is lost by removing them. With task volumes the
    tree and the conversation are on the claim, not in the pod; without them the pod's `emptyDir`
    held the only copy of what the agent did since the last sync, and no daemon can reach it again
    either way.

    Waited for with task volumes, as `removePod` waits: the pod's agent may outlive the stream
    that started it, and the continuations the daemon is about to queue (`Queue.resumeEntryFor`)
    and the sessions about to wake take over these very claims. `holderAlive` judges a pod that is
    still terminating to be alive, so a continuation that came too soon would be refused the
    workspace it exists to continue in. Without task volumes nothing waits on them, and the
    delete is fire-and-forget.

    Best effort, like the retention sweep: a cluster that cannot be reached here is logged, and
    the pods fall back to `activeDeadlineSeconds`. -/
def reclaim (cfg : Config) : IO Unit := do
  try
    let selector := instanceSelector cfg
    let (code, out, err) ← kube cfg #["get", "pods", "-l", selector, "-o", "name"]
    if code != 0 then
      IO.eprintln s!"  [k8s] could not list leftover pods ({selector}): {err.trimAscii}"
      return
    let pods := (out.splitOn "\n").map (·.trimAscii.toString) |>.filter (!·.isEmpty)
    if pods.isEmpty then
      IO.println s!"  [k8s] no pods left over from a previous daemon ({cfg.inst})"
      return
    IO.println s!"  [k8s] removing {pods.length} pod(s) left over from a previous daemon \
({cfg.inst}): {String.intercalate ", " pods}"
    let waitArgs := if cfg.taskVolumes.isSome then #["--timeout=120s"] else #["--wait=false"]
    let (dc, _, derr) ← kube cfg (#["delete", "pods", "-l", selector, "--now",
      "--ignore-not-found"] ++ waitArgs)
    if dc != 0 then
      IO.eprintln s!"  [k8s] could not remove every leftover pod: {derr.trimAscii}"
  catch e =>
    IO.eprintln s!"  [k8s] could not remove leftover pods: {e}"

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
      persistentWorkspaces := cfg.taskVolumes.isSome
      reclaim := reclaim cfg
      openSession := openSession cfg }

end Orchestra.Exec.Kubernetes
