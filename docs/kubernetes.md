# running agents on a Kubernetes cluster

The `kubernetes` execution backend gives each task a pod, and runs everything the task consists of
inside it: the repository's `init.sh` and `before.sh`, the agent, `validation.sh`, the retry the
agent gets when that fails, and `after.sh`. The pod is a container that does nothing on its own;
each of those is a `kubectl exec` into it.

That the pod belongs to the *task* rather than to one agent launch is what makes the rest work:

- **`validation.sh` runs where the work happened.** It is the repository's build, and it decides
  whether the agent is finished — the daemon's own machine has no toolchain to run it with and no
  reason to produce the same answer.
- **A retry resumes.** `--resume <session>` names a file the agent CLI wrote where it ran.
- **The checkout is carried once**, not once per attempt.

The daemon still clones, holds the credentials, mints the tokens, and parses what the agent says —
see [the execution model](execution.md) for how the pieces divide.

```json
{
  "execution": {
    "backend": "kubernetes",
    "options": {
      "image": "ghcr.io/chrisflav/orchestra-agent:latest",
      "namespace": "orchestra",
      "mcp_host": "orchestra.orchestra.svc.cluster.local",
      "service_account": "orchestra-agent",
      "excludes": [".lake", "node_modules"]
    }
  }
}
```

## what a task looks like

```
  open ─► create pod ─► wait Ready ─► stage the checkout in
                                              │
       init.sh / before.sh ── kubectl exec ────┤
       the agent ─────────── kubectl exec ────┤   (stdout and stderr arrive separately)
       validation.sh ─────── kubectl exec ────┤
       after.sh ──────────── kubectl exec ────┘
                                              │
  close ─► copy the checkout back ─► delete the pod
```

`orchestra interactive` is the same mechanism with a terminal: `kubectl exec -i -t`, with the
daemon's own streams handed straight through, so the agent's TUI behaves as it does locally. A
queued run deliberately gets no TTY — `kubectl exec` merges stdout and stderr as soon as there is
one, and orchestra reads the two for different things.

An interactive **session** — `orchestra chat`, and the dashboard's chat page — is the third shape:
one `kubectl exec -i` held open for as long as the conversation, with each turn written to the
agent's stdin as a line and its events streaming back. The pod lives as long as the session does,
and closing that stdin is how the conversation ends. A session that goes dormant and is later
woken gets a new pod, so waking one needs `home_claim` for the same reason a continuation does;
without it the wake is refused rather than starting the conversation over behind the transcript.

Cancelling a task deletes the pod, which ends whatever was running in it. Nothing is copied back
from a cancelled task.

## what you need

**On the daemon:** `kubectl`, configured for the cluster and namespace. The backend checks at the
start of every task that it is there and that it may create pods, and fails the task with one line
if not, rather than dispatching into a cluster that will refuse it.

**In the image:** the agent CLI (`claude`, `vibe`, `opencode` or `pi`), `sh`, `bash` (the
repository's scripts are run with it), `tar`, `nc` and `git`. That is the fixed part — the same
list the daemon's own machine needs, minus `landrun`. What each repository needs on top of it is
the subject of the next section.

**And a `USER` that is not root.** This is not a preference. Claude Code refuses
`--dangerously-skip-permissions` under uid 0 — "cannot be used with root/sudo privileges for
security reasons" — and that flag is how orchestra runs it, so an image that leaves the default
root user fails every task at the first launch, before the agent has done anything:

```dockerfile
RUN useradd --create-home agent
USER agent
```

The pod carries no `securityContext`, deliberately: `runAsUser` in the manifest would be
orchestra overriding a decision the image already makes, and getting it wrong means a uid with no
passwd entry, which several toolchains will not start under. So the image's own `USER` is the only
thing that decides, and it is the image's job to set it.

Nothing has to be granted for that to work. Every directory orchestra mounts — the checkout,
`$HOME`, the control directory — is an `emptyDir`, which the kubelet creates world-writable, so an
unprivileged user can write all three without a `fsGroup` or an init container. `$HOME` in
particular is empty at that point and the CLI expects to create its own state directory in it.

The daemon does not have to run as that user, or as anyone in particular. Everything it does to
the pod is a `kubectl exec`, which lands as the image's user whoever the daemon is.

### the image orchestra publishes

You do not have to build one to start. CI builds `docker/agent.Dockerfile` and publishes it, and
it is what the examples here name:

```
ghcr.io/chrisflav/orchestra-agent:latest
ghcr.io/chrisflav/orchestra-agent:claude-2.1.251     # pin the CLI version
```

It is the floor and nothing more: the Claude CLI, `sh`, `bash`, `tar`, `nc`, `git`, and a non-root
`agent` user whose home is `/home/agent` — the `home_path` default. Everything above is what the
list above requires, so `claude` tasks against a repository that needs no toolchain of its own work
against it unmodified.

**Prefer the `claude-<version>` tag to `latest`.** What changes between two of these images is
almost always the CLI rather than the Dockerfile, and a weekly build picks up new releases without
a commit here — so `latest` is a moving target, and pinning is how a run stays reproducible.

**What it deliberately does not carry** is anything a repository needs to build: no Lean, no JDK,
no Python, no browser — and no Node or npm either, since the CLI ships as a self-contained binary
and carrying a toolchain nothing in the image uses is exactly the bloat "minimal" is meant to
avoid. A repository that needs any of that names its own image, or has one pinned for it; see
[what is installed, when repositories disagree](#what-is-installed-when-repositories-disagree).
The other three agent CLIs (`vibe`, `opencode`, `pi`) are not in it either — a deployment that runs
those builds its own, and `docker/agent.Dockerfile` is a reasonable thing to copy.

To build it yourself — a private registry, a different CLI version, extra tooling:

```sh
docker build -f docker/agent.Dockerfile \
  --build-arg CLAUDE_CODE_VERSION=2.1.251 \
  -t registry.internal/orchestra-agent:2.1.251 .
```

**RBAC** for the daemon's service account, in the namespace the pods run in:

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: orchestra-runner
  namespace: orchestra
rules:
  - apiGroups: [""]
    resources: ["pods"]
    verbs: ["create", "get", "list", "watch", "delete"]
  - apiGroups: [""]
    resources: ["pods/exec"]
    verbs: ["create"]
  - apiGroups: [""]
    resources: ["pods/log"]
    verbs: ["get"]
```

### where the daemon runs

The cluster does not have to be the machine the daemon is on, and usually is not. Everything
orchestra does to the cluster goes through `kubectl` and therefore through the API server, so
creating the pod, copying the checkout in and out, running each command and reading its output all
work against a cluster anywhere. What decides the topology is the *other* direction: the pod has to
reach the daemon's MCP server, or the agent has no tools.

**The daemon in the cluster** is the arrangement with nothing to configure. Pods reach it at its
Service address on whatever port each task's server happens to take, so `mcp_host` is that Service
and there is nothing else to do.

**The daemon outside the cluster, with a route in** — a workstation on the same network as an
on-prem cluster, a VM peered with a cloud VPC. Set `mcp_host` to an address of that machine the
pods can route to, and set `mcp_ports` to a range that whatever is in between can be told about:

```json
"options": { "mcp_host": "10.0.4.11", "mcp_ports": [31000, 31015] }
```

Without a range the port is ephemeral and different for every task, which no firewall rule, no
port-forward and no tunnel can be written against. A range that cannot be read as one — reversed,
out of range, not a pair of numbers — is refused when the configuration is read rather than
dropped, since dropping it would silently give you the ephemeral port the setting exists to avoid.
One server runs per task, so the range has to be at least as wide as the queue's parallelism;
orchestra takes the first free port in it and fails the task with a clear message when there is
none left.

**The daemon outside, with no route in** — a laptop behind NAT, a managed cluster with no path back
to it. There is nothing to set here: the agent's tools need an inbound connection. The usual
answers are to run the daemon in the cluster, or to give it one — a reverse tunnel or a VPN that
puts the daemon on a routable address, and then the case above.

Two things cost more the further away the cluster is. The checkout is copied in when a task starts
and out when it ends, over the API server, so `excludes` for build output earns its keep on a
distant cluster. And each command is a single long-lived `kubectl exec` connection, which anything
in between may drop if it goes quiet; see the note on long silences below.

**A route from the pods to the daemon's MCP server.** This is the part that is easy to get wrong
and silent when it is: without it the agent starts, finds no tools, and does the task by hand — it
cannot open a pull request, comment, or claim an issue. `mcp_host` is where the pod should look. If
the daemon runs in the cluster, that is a Service in front of it:

```yaml
apiVersion: v1
kind: Service
metadata:
  name: orchestra
  namespace: orchestra
spec:
  selector: { app: orchestra }
  clusterIP: None          # headless: the port is per-task, so only the address matters here
  ports: [{ port: 1, name: placeholder }]
```

The port is not fixed and does not need to be: the daemon starts one MCP server per task on an
ephemeral port and tells that task's agent which one. What has to be reachable is the daemon's pod,
on arbitrary high ports, from the agent pods. If the daemon runs outside the cluster, set
`mcp_host` to an address of that machine the cluster can route to.

**Authentication happens on its own.** Because the agent is off this machine, the MCP server binds
`0.0.0.0` rather than loopback, and a one-task token is minted with it. The agent's MCP client
sends the token as its first line; a connection presenting anything else is closed. The token is
written only into the agent's own configuration inside its pod. There is nothing to configure, and
nothing that turns it off.

## what is installed, when repositories disagree

Build and dev dependencies are a property of the repository, so `image` is a default rather than
the answer. Three sources, settled in this order:

1. **An operator's pin**, `images` in the options, keyed by `owner/name`. Deliberate, and nothing
   inside the repository can talk it out of it.
2. **What the repository asked for**, `execution.image` in its own `.orchestra/config.json`. The
   repository already writes this down for its own CI, and it is the thing that knows whether its
   tests need a JDK, a browser or a database client. Set `allow_repo_image: false` to ignore it.
3. **`image`**, for everything that has not said otherwise.

```json
{
  "execution": {
    "backend": "kubernetes",
    "options": {
      "image": "ghcr.io/chrisflav/orchestra-agent:latest",
      "images": {
        "acme/widgets": "ghcr.io/acme/widgets-ci:latest",
        "acme/mobile":  "ghcr.io/acme/android-ci:latest"
      },
      "home_claim": "orchestra-agent-home"
    }
  }
}
```

Allowing a repository to name its image grants nothing that is not already granted: the agent runs
that repository's code, with this task's credentials in the environment, either way. It is worth
turning off when the images that may run in the namespace are a decision of their own — an
allowlisted registry, a scanned base — rather than a matter of convenience.

### how a reference is written, and where it is pulled from

An image is a plain OCI reference — `ghcr.io/acme/widgets-ci:latest`,
`registry.internal:5000/team/img@sha256:…` — and it goes into the pod's `image` field exactly as
written. Orchestra never contacts a registry: the kubelet on the node resolves the reference and
pulls it, by the same rules as any other pod. A reference with no registry host is Docker Hub, or
whatever the node's container runtime has been configured to mirror it to; one with a host is that
host.

**Credentials** are the cluster's, not orchestra's. `image_pull_secrets` names `Secret`s that must
already exist in the namespace — orchestra never creates or reads them — and the service account
the pods run under (`service_account`) contributes any `imagePullSecrets` attached to it, as does
whatever node-level credential the platform provides (an instance role for ECR, a metadata-server
token for GCR). If a task fails to start with `ImagePullBackOff`, the reason is in the pod's events
and the fix is a registry credential, not an orchestra setting.

**Pull policy** is unset by default, so Kubernetes' own rule applies: `Always` for a `:latest` or
untagged reference, `IfNotPresent` for everything else. That last one is worth knowing about a
floating tag that is *not* `:latest` — a `:main` rebuilt nightly is served from whatever each node
happened to cache, so two tasks can run different code under one name. Set
`image_pull_policy: "Always"` for that, or refer to images by digest.

**A repository's own choice is checked** before it becomes a manifest, since it is the one
reference that did not come from this daemon's configuration: a name with a space or a quote in it
is refused. `allowed_image_prefixes` narrows it further — a namespace that pulls only from a
scanned mirror sets `["ghcr.io/acme/", "registry.internal/"]` and lets repositories pick within it.
Either refusal fails the task with the reason, rather than quietly falling back to the default: a
repository that asked for a JDK image and silently got one without would fail its validation script
for a reason nothing in the log points at.

**What the image does not have, `init.sh` installs.** Every task starts from a new pod, so the hook
runs on every task rather than once per checkout: the marker it writes lives in the checkout, and
the checkout is exactly what gets carried into a pod that has nothing installed. Repository hooks
that install a toolchain or warm a build cache are already idempotent for their own reasons — they
check before they fetch — and that is what is expected of them here.

**`home_claim` is what makes that cheap.** Point it at a `PersistentVolumeClaim` and `$HOME` — with
`~/.elan`, `~/.cargo`, `~/.cache`, `~/.npm` and the rest under it — survives between tasks, so the
first task on an image pays for the toolchain and the ones after it do not. That is the same
arrangement the landrun backend gets for free from the machine it runs on. The claim needs
`ReadWriteMany`, or `ReadWriteOnce` with every agent pod on one node, since tasks run in parallel.
The checkout's own build output (`.lake`, `target`, `node_modules`) travels with the checkout
instead; `excludes` keeps it off the wire without removing it from the daemon's copy.

## memory, continuations and series

**Memory directories travel with the task.** Orchestra's memory system is a directory on the
daemon's disk — one global, one per upstream repository — that the agent is told about and writes
files into. Each is copied into the pod when the task starts, mounted at the same absolute path,
and merged back when it ends. Merged, not swapped: several tasks hold a copy at once, and the
repository-per-file convention the agent is instructed to follow (one new file per memory, never
appending to another run's) is what makes those merges disjoint.

Two differences from a daemon that runs its agents locally, both consequences of the copy:

- a memory written by a task while another is already running is **not** visible to that other
  task; it sees the snapshot taken when it started, and picks the new file up on its next task;
- a file the agent *deletes* comes back, since a merge never deletes. Correcting a memory by adding
  a file that supersedes it — which is what the agent is told to do — works; deleting one does not.

Memories are copied back even when `sync_back` is off. That setting is about the checkout, for the
case where the agent pushes its work and nothing local reads the tree afterwards; it is not a
statement about what the agent learned.

**Continuations and series are the same mechanism.** A series resolves to the latest task in it and
runs the next one with `continues_from` pointing at it, so both need the agent's earlier
conversation — which lives under the agent's `$HOME`, in the pod that ran it. With a scratch home
that pod is gone and the task is refused at the start, naming the setting that keeps it. With
`home_claim` the home outlives the pod and both work.

The checkout side of a continuation already works either way: the daemon hands a continuation its
predecessor's clone slot, and that is the tree copied into the new pod.

## what a pod's lifetime covers

**One pod per task, reused for everything in it.** `init.sh`, `before.sh`, the agent, the
validation script, the agent again on a retry, `after.sh` — all of it is a `kubectl exec` into the
same container. Nothing is reused *between* tasks: the pod is created when the task starts and
deleted when it ends, including when it fails or is cancelled.

**The agent's conversation lives as long as its `$HOME`.** A session id is a file the agent CLI
wrote there, and `--resume <id>` is a request to read it back. Within a task that always works: the
retry after a failed validation runs in the same pod as the attempt before it. Across tasks it
depends on `home_claim`:

| | scratch home (default) | `home_claim` set |
| --- | --- | --- |
| retry after failed validation | resumes | resumes |
| `continues_from` / a series | **refused** | resumes |
| waking a dormant chat session | **refused** | resumes |
| memory directories | copied back | copied back |
| toolchain `init.sh` installed | reinstalled each task | kept |

A task that continues another one, in an environment that cannot have its conversation, fails at
the start and says which setting keeps it. Running anyway is the worse outcome: a follow-up prompt
— "now also handle the timeout case" — answered by a model that has never seen what came before,
which reads as a plausible pull request and is not one.

**If the pod goes away mid-task** — `deadline_seconds` expiring, an eviction, a node lost — the
`kubectl exec` carrying whatever was running dies with it, and the task fails. The checkout is
*not* brought back, since there is nothing left to copy it from; the daemon says so plainly rather
than reporting a failed transfer, because the difference matters: the work is not there to retry,
only to run again. `deadline_seconds` counts the whole task, hooks and retries included, so a
repository whose build is slow wants it raised rather than discovered.

**If a session id no longer resolves** for any other reason — the agent CLI expired it, a claim was
wiped — the CLI is what says so, on the run it was passed to. Orchestra records that as the task's
failure; it does not silently start a fresh conversation in its place.

## the checkout, and what comes back

The daemon prepares the checkout as it always has — a clone slot per concurrent task — and the
backend copies it into the pod when the session opens. Plugin and memory directories go the same
way, since they too live on the daemon's disk and in no image.

When the task ends:

- the **checkout** is replaced wholesale, so that a file the agent deleted is gone. The new tree is
  assembled beside the old one and swapped in, so a transfer that fails leaves the checkout as it
  was;
- **memory directories** are merged, because other tasks may be writing to them at the same time. A
  file the agent deleted survives, which is the lesser mistake;
- **read-only paths**, such as plugin directories, are not copied back at all.

`excludes` keeps build output off the wire in both directions (`.lake`, `target`, `node_modules`).
It costs a rebuild in the pod and saves copying hundreds of megabytes twice. It does not cost you
the daemon's copy: an excluded path is not transferred, but the checkout is replaced wholesale on
the way back, so what the old tree had is moved across into the new one before the swap. A warmed
`.lake` that `orchestra prepare` built survives every task that excludes it. Patterns are matched
as globs against the checkout root, so they have to be paths or globs — a leading `/` or a `..` is
refused when the configuration is read. `sync_back: false` turns the
return trip off entirely — reasonable when the agent pushes and nothing local reads the result,
since with the hooks and validation now running in the pod, the daemon's copy is mostly there for
the next task and for anyone looking.

## credentials

Nothing sensitive is passed on a command line or written into the pod's spec — the first is visible
in the cluster's audit log and in `/proc` inside the pod, the second to anything that can list
pods. The environment for each command is written to a file inside the pod, over the same channel
the checkout travels on, and sourced there. Values are quoted, so one containing a quote or a
newline cannot end its own assignment.

## operating it

**Orphans.** The pod is deleted when the task ends, including when it fails or is cancelled. A
daemon that dies mid-task leaves one; `activeDeadlineSeconds` (`deadline_seconds`, four hours by
default, counted over the whole task) is what eventually stops it, and everything this backend
creates carries `app.kubernetes.io/managed-by=orchestra`:

```sh
kubectl -n orchestra get pod -l app.kubernetes.io/managed-by=orchestra
kubectl -n orchestra delete pod -l app.kubernetes.io/managed-by=orchestra
```

Pods are also labelled `orchestra.dev/task=<task id>`, so a running task can be found from its id
in the dashboard — and `kubectl exec` into for a look around while it works.

**Long silences.** An agent run is a single `kubectl exec` connection, and some load balancers and
API-server configurations drop streams that go idle. The agent's output keeps the connection busy
most of the time, but a long pause between tool calls is the thing to watch if runs die mid-task
with no error from the agent itself. If the connection does drop, the agent process in the pod
outlives it — the next attempt kills whatever the last one left behind before starting, so two
agents never share a checkout.

**Every task failing the same way at the first launch** is almost always the image running as
root — see "what you need". Two symptoms, depending on how far it gets:
`--dangerously-skip-permissions cannot be used with root/sudo privileges` from the agent, or
`tar: .: Cannot change mode` while the checkout is being staged, if something else in the image
has dropped privileges. Both are the image's `USER`, not the cluster.

**`--debug`** prints the pod manifest and the exec command, which is the quickest way to see what
the cluster was actually asked for.

## all options

| key | default | what it does |
| --- | --- | --- |
| `image` | *required* | image tasks run in, unless something more specific applies |
| `images` | `{}` | image per repository, by `owner/name`; beats what the repository asks for |
| `allow_repo_image` | `true` | whether a repository's own `execution.image` is honoured |
| `mcp_host` | *required* | where the pod reaches this daemon's MCP server; a hostname or IP, refused otherwise |
| `namespace` | `default` | namespace the pod is created in |
| `kubectl` | `kubectl` | path to the binary |
| `service_account` | *(namespace default)* | `serviceAccountName` for the pod |
| `image_pull_secrets` | `[]` | `imagePullSecrets` names; the Secrets must already exist in the namespace |
| `image_pull_policy` | *(cluster default)* | `Always`, `IfNotPresent` or `Never` |
| `allowed_image_prefixes` | `[]` | prefixes a repository-declared image must start with; empty allows any |
| `node_selector` | *(none)* | `nodeSelector`, verbatim |
| `resources` | *(none)* | `resources` for the container, verbatim |
| `volumes` / `volume_mounts` | `[]` | extra volumes and mounts, verbatim — a build cache, most usefully |
| `home_path` | `/home/agent` | where the agent's `$HOME` is in the pod |
| `home_claim` | *(none)* | PVC to mount as `$HOME`, so toolchains and caches survive between tasks |
| `mcp_bind` | `0.0.0.0` | address the daemon's MCP server binds |
| `mcp_ports` | *(any free port)* | `[from, to]` the MCP server may listen on, for a daemon something has to route to |
| `deadline_seconds` | `14400` | `activeDeadlineSeconds` on the pod |
| `startup_timeout_seconds` | `600` | how long to wait for the pod to be ready |
| `sync_back` | `true` | whether the *checkout* is copied back out; memory directories always are |
| `excludes` | `[]` | `tar --exclude` patterns, applied in both directions; excluded paths stay as they are in the daemon's checkout |

## keeping the MCP server to one namespace

`mcp_bind` and the firewall decide which *machines* can reach the daemon's MCP server. They cannot
decide which *namespace* can: every pod's traffic leaves the cluster behind the node's address, so
a rule written on the daemon's host sees one source for the whole cluster.

That matters because the server is not a small thing to expose. It hands out the authority of the
PAT the daemon holds — opening pull requests, commenting, claiming issues — and the per-task token
is the only thing in front of it. A pod in some unrelated namespace should not be able to try.

Inside the cluster it is a `NetworkPolicy`, and the useful shape is the inverse of the obvious one:
egress is default-allow, so the restriction is written in the namespaces that should *not* reach
the daemon, as "everywhere except the networks the cluster sits on".

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: default-egress
  namespace: default          # every namespace that is not the runners'
spec:
  podSelector: {}
  policyTypes: [Egress]
  egress:
    - to:
        - ipBlock:
            cidr: 0.0.0.0/0
            except: ["10.0.0.0/8", "172.16.0.0/12", "192.168.0.0/16", "169.254.0.0/16"]
    - to: [{ namespaceSelector: { matchLabels: { kubernetes.io/metadata.name: kube-system } } }]
      ports: [{ port: 53, protocol: UDP }, { port: 53, protocol: TCP }]
```

Excluding the private ranges wholesale rather than the daemon's address alone is worth doing: it
covers the API server, the kubelet and the cloud metadata service in the same breath, and it does
not have to be revisited when the daemon moves.

Two things this does not do, both worth knowing before relying on it:

- **It has to be repeated per namespace.** There is no cluster-wide `NetworkPolicy`; a namespace
  created later starts default-allow and can reach the daemon until it is given one of these. If
  that is not a property you want to maintain by hand, an admission policy that requires it, or a
  CNI with cluster-scoped policies, is the thing to reach for.
- **It needs a CNI that enforces policy.** k3s does, through its embedded kube-router; plain
  flannel elsewhere does not, and a policy no one enforces reads exactly like one that works. Check
  before trusting it: apply a deny-all-egress policy in a scratch namespace and confirm a pod there
  loses the network.

Leave `kube-system` alone. It is cluster infrastructure rather than anywhere tasks run, and an
egress policy there buys nothing against a threat model where the agent is the thing being confined.

## testing it against a real cluster

`OrchestraTest/KubernetesTest.lean` checks what the pod spec *says*, which needs no cluster.
`OrchestraTest/KubernetesLiveTest.lean` checks what a pod *does* — that a session opens, the
checkout arrives and comes back, memories merge, `excludes` costs a transfer and not the daemon's
copy, and that a cancelled task and an evicted pod leave the disk in the state this document
claims. It opts in through the environment and skips itself otherwise, so the suite stays green on
a machine with no cluster:

```sh
ORCHESTRA_TEST_K8S_IMAGE=debian:stable-slim \
ORCHESTRA_TEST_K8S_NAMESPACE=orchestra-runners \
lake test
```

The image needs only `sh`, `bash` and `tar` — the whole of what the session lifecycle uses. `git`
and `nc` are the agent's own requirements and no test there launches an agent, since that needs the
pod to reach the daemon's MCP server, which is a fact about a particular network rather than about
this code. `ORCHESTRA_TEST_K8S_KUBECTL` names the binary when it is not on `PATH`.

Give the tests a namespace of their own, with the RBAC above and nothing else. Each test deletes
its pod, but a namespace is the cheap way to be sure of what a failing run left behind.
