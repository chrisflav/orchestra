# running agents on a Kubernetes cluster

The `kubernetes` execution backend runs each agent in a pod of its own, on a cluster the daemon
reaches through `kubectl`. What confines the agent is the pod: it sees the image's filesystem and
nothing of the daemon's, its egress is whatever the namespace allows, and it is deleted when the
run ends.

Everything else stays where it was. The daemon still clones, holds the credentials, runs the
repository's hooks and validation script, and parses what the agent says — see
[the execution model](execution.md) for how the pieces divide. That is also why the checkout
travels: it goes into the pod before the agent starts and comes back changed when it finishes.

```json
{
  "execution": {
    "backend": "kubernetes",
    "options": {
      "image": "ghcr.io/example/orchestra-agent:latest",
      "namespace": "orchestra",
      "mcp_host": "orchestra.orchestra.svc.cluster.local",
      "service_account": "orchestra-agent",
      "home_claim": "orchestra-agent-home",
      "excludes": [".lake", "node_modules"]
    }
  }
}
```

## what a run looks like

```
  create secret + pod ─► wait Ready ─► stage the checkout in ─► touch go
                                                                  │
        stdout ◄── kubectl logs -f -c agent                        ▼
        stderr ◄── kubectl exec -c workspace -- tail -f       the agent runs
                                                                  │
   exit code ◄── pod status ◄──────────────────────────────────────┘
        │
        └─► copy the checkout back ─► touch release ─► delete pod + secret
```

Each pod has two containers. **`agent`** runs the agent CLI, after waiting for a go-file — which
is how a workspace gets into a container that is already running — and with its stderr redirected
to a file, so the pod's log stays a clean event stream. **`workspace`** does nothing but stay
alive until released, because `kubectl exec` needs a running container and the agent's is gone at
the moment there is finally something to copy back.

Cancelling a task deletes the pod, which ends both streams. Nothing is copied back from a
cancelled run.

## what you need

**On the daemon:** `kubectl`, configured for the cluster and namespace. The backend checks at the
start of every task that it is there and that it may create pods, and fails the task with one line
if not, rather than dispatching into a cluster that will refuse it.

**In the image:** the agent CLI (`claude`, `vibe`, `opencode` or `pi`), `sh`, `tar`, `nc`, `git`,
and whatever the repositories being worked on need to build. `tar` moves the checkout in and out;
`nc` is the agent's MCP transport.

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
  - apiGroups: [""]
    resources: ["secrets"]
    verbs: ["create", "delete"]
```

**A route from the pods to the daemon's MCP server.** This is the part that is easy to get wrong
and silent when it is: without it the agent starts, finds no tools, and does the task by hand — it
cannot open a pull request, comment, or claim an issue. `mcp_host` is where the pod should look.
If the daemon runs in the cluster, that is a Service in front of it:

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
ephemeral port and tells that task's agent which one. What has to be reachable is the daemon's
pod, on arbitrary high ports, from the agent pods. If the daemon runs outside the cluster, set
`mcp_host` to an address of that machine which the cluster can route to, and expect to open a port
range through whatever is in between.

**Authentication happens on its own.** Because the agent is off this machine, the MCP server binds
`0.0.0.0` rather than loopback, and a one-run token is minted with it. The agent's MCP client
sends the token as its first line; a connection that presents anything else is closed. The token
is written only into the agent's own configuration inside its pod. There is nothing to configure,
and nothing that turns it off.

## the checkout, and what comes back

The daemon prepares the checkout as it always has — a clone slot per concurrent task — and the
backend copies it into the pod. Plugin and memory directories go the same way, since they too live
on the daemon's disk and in no image.

On the way back:

- the **checkout** is replaced wholesale, so that a file the agent deleted is gone. The new tree is
  assembled beside the old one and swapped in, so a transfer that fails leaves the checkout as it
  was;
- **memory directories** are merged, because other tasks may be writing to them at the same time.
  A file the agent deleted survives, which is the lesser mistake;
- **read-only paths**, such as plugin directories, are not copied back at all.

`excludes` keeps build output out of both directions (`.lake`, `target`, `node_modules`). It costs
a rebuild in the pod and saves copying hundreds of megabytes twice. `sync_back: false` turns the
return trip off entirely — only right when the agent pushes and nothing local reads the result,
since the repository's validation script and every `git` command the daemon runs afterwards would
otherwise see the tree unchanged.

## operating it

**Orphans.** Pods and secrets are deleted when a run ends, including a cancelled one. A daemon that
dies mid-run leaves them; `activeDeadlineSeconds` (`deadline_seconds`, four hours by default) is
what eventually stops the pod, and everything this backend creates carries
`app.kubernetes.io/managed-by=orchestra`:

```sh
kubectl -n orchestra get pod,secret -l app.kubernetes.io/managed-by=orchestra
kubectl -n orchestra delete pod,secret -l app.kubernetes.io/managed-by=orchestra
```

**Credentials** — the installation token and the agent's API key — travel in a per-run `Secret`
rather than in the pod's own spec, which anything that can list pods in the namespace can read.
The secret is deleted with the run.

**Retries.** A run that fails its repository's validation script is relaunched with `--resume`, and
the session it resumes is a file the agent CLI wrote under its home. Set `home_claim` to a
`PersistentVolumeClaim` if you use validation retries: without it each attempt gets a fresh
`emptyDir` home and has nothing to resume. The claim needs `ReadWriteMany`, or `ReadWriteOnce` with
all agent pods on one node.

**`orchestra interactive` does not work on this backend** and says so: there is no terminal to hand
a pod. Run it on a machine configured for `landrun`.

**`--debug`** prints the pod manifest and the commands that produce the run, which is the quickest
way to see what the cluster was actually asked for.

## all options

| key | default | what it does |
| --- | --- | --- |
| `image` | *required* | image the agent runs in |
| `mcp_host` | *required* | where the pod reaches this daemon's MCP server |
| `namespace` | `default` | namespace for the run's pod and secret |
| `kubectl` | `kubectl` | path to the binary |
| `sidecar_image` | `image` | image for the `workspace` container |
| `service_account` | *(namespace default)* | `serviceAccountName` for the pod |
| `image_pull_secrets` | `[]` | `imagePullSecrets` names |
| `node_selector` | *(none)* | `nodeSelector`, verbatim |
| `resources` | *(none)* | `resources` for the agent container, verbatim |
| `volumes` / `volume_mounts` | `[]` | extra volumes and mounts, verbatim — a build cache, most usefully |
| `home_path` | `/home/agent` | where the agent's `$HOME` is |
| `home_claim` | *(none)* | PVC to use as `$HOME` instead of an `emptyDir` |
| `mcp_bind` | `0.0.0.0` | address the daemon's MCP server binds |
| `deadline_seconds` | `14400` | `activeDeadlineSeconds` on the pod |
| `startup_timeout_seconds` | `600` | how long to wait for the pod to be ready |
| `sync_back` | `true` | whether the checkout is copied back out |
| `excludes` | `[]` | `tar --exclude` patterns, applied in both directions |
