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
      "image": "ghcr.io/example/orchestra-agent:latest",
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

Cancelling a task deletes the pod, which ends whatever was running in it. Nothing is copied back
from a cancelled task.

## what you need

**On the daemon:** `kubectl`, configured for the cluster and namespace. The backend checks at the
start of every task that it is there and that it may create pods, and fails the task with one line
if not, rather than dispatching into a cluster that will refuse it.

**In the image:** the agent CLI (`claude`, `vibe`, `opencode` or `pi`), `sh`, `bash` (the
repository's scripts are run with it), `tar`, `nc`, `git`, and whatever the repositories being
worked on need to build. The same list the daemon's own machine needs, minus `landrun`.

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

`excludes` keeps build output out of both directions (`.lake`, `target`, `node_modules`). It costs
a rebuild in the pod and saves copying hundreds of megabytes twice. `sync_back: false` turns the
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

**`--debug`** prints the pod manifest and the exec command, which is the quickest way to see what
the cluster was actually asked for.

## all options

| key | default | what it does |
| --- | --- | --- |
| `image` | *required* | image tasks run in |
| `mcp_host` | *required* | where the pod reaches this daemon's MCP server |
| `namespace` | `default` | namespace the pod is created in |
| `kubectl` | `kubectl` | path to the binary |
| `service_account` | *(namespace default)* | `serviceAccountName` for the pod |
| `image_pull_secrets` | `[]` | `imagePullSecrets` names |
| `node_selector` | *(none)* | `nodeSelector`, verbatim |
| `resources` | *(none)* | `resources` for the container, verbatim |
| `volumes` / `volume_mounts` | `[]` | extra volumes and mounts, verbatim — a build cache, most usefully |
| `home_path` | `/home/agent` | where the agent's `$HOME` is in the pod |
| `mcp_bind` | `0.0.0.0` | address the daemon's MCP server binds |
| `deadline_seconds` | `14400` | `activeDeadlineSeconds` on the pod |
| `startup_timeout_seconds` | `600` | how long to wait for the pod to be ready |
| `sync_back` | `true` | whether the checkout is copied back out |
| `excludes` | `[]` | `tar --exclude` patterns, applied in both directions |
