# previews VM — k3s with Kata Containers, on Incus

A NixOS Incus **VM** running a single-node k3s cluster where every pod gets its own kernel via
Kata Containers. It is the deployment target for preview environments: arbitrary Dockerfiles and
compose files out of pull requests, built and run without anyone reading them first.

Two properties make that acceptable, and both are structural rather than procedural:

- **This machine holds nothing.** No GitHub App key, no PAT, no agent tokens, no clones. It is a
  separate machine from the one running orchestra precisely so that a preview escaping its
  sandbox lands somewhere worthless.
- **Every preview gets a kernel.** Kata runs each pod as a lightweight VM, so `privileged: true`
  in an uninspected compose file is a statement about a throwaway guest, not about this host.

Files:

| Path | What it is |
| --- | --- |
| `configuration.nix` | The VM: hostname, ssh, and the two modules below |
| `modules/kata.nix` | Kata runtime, its config, the kernel modules it needs, a preflight check |
| `modules/k3s.nix` | k3s server, the containerd stanza that routes pods to Kata, namespace + quotas + network policies |
| `default.nix` | Image build against a pinned nixpkgs |

`modules/` is self-contained: import both files into an existing NixOS VM's configuration and you
get the same cluster without using `default.nix` at all.

## Before anything else: nested virtualisation

Kata pods are VMs. Running them inside an Incus VM makes them **L2 guests**, which the Incus host
must permit. This is the single most likely thing to be wrong, and it fails in a way that looks
like a hang (pods stuck in `ContainerCreating`) rather than an error — which is why
`kata-preflight.service` checks for it at boot and says so in the journal.

On a NixOS Incus host:

```nix
boot.extraModprobeConfig = "options kvm_intel nested=1";   # kvm_amd on AMD
```

Verify on the **host**:

```sh
cat /sys/module/kvm_intel/parameters/nested     # Y or 1
```

Incus passes the host CPU through to its VMs, so once nesting is on the guest sees the
virtualisation extensions. Verify inside the **VM** after it boots:

```sh
grep -cE 'vmx|svm' /proc/cpuinfo    # non-zero
ls -l /dev/kvm                      # exists
kata-runtime check
```

If the Incus host is itself a VM (a cloud instance rather than your own metal), this is where it
stops: you need a provider and instance type that exposes nested virtualisation, or a machine you
own. Without `/dev/kvm` in the guest, k3s still comes up and ordinary pods still run — but every
pod requesting `runtimeClassName: kata` fails, which is every preview.

## Build and launch

```sh
cd container/previews-vm
nix-build -A qemuImage -A metadata           # result -> qcow2, result-2 -> metadata tarball

incus image import result-2/tarball/*.tar.xz result/nixos.qcow2 --alias previews

incus launch previews previews --vm \
  -c limits.cpu=8 \
  -c limits.memory=32GiB \
  -d root,size=200GiB
```

Size the VM for what it actually does. Two things dominate:

- **Builds.** Every deployment builds its own images inside its own sandbox. This is the spiky
  part — CPU, disk and page cache all at once.
- **Guest RAM per preview.** A Kata sandbox takes `default_memory` (2 GiB by default) or the
  pod's memory limit, whichever is larger, plus ~160 MiB of qemu and shim overhead on the host.
  Ten concurrent previews is therefore ~22 GiB before anything else.

## Or: apply to a VM you already have

Copy `modules/` into that VM's configuration and import both files:

```nix
imports = [ ./modules/kata.nix ./modules/k3s.nix ];
previews.k3s.tlsSans = [ "previews.internal" ];
```

Then `nixos-rebuild switch`. Nothing in `modules/` assumes the image build — only that it runs on
a machine with `/dev/kvm`.

## Verify the boundary is real

```sh
kubectl get nodes
kubectl get runtimeclass kata

kubectl -n previews run kata-smoke --image=alpine:3 --restart=Never \
  --overrides='{"spec":{"runtimeClassName":"kata"}}' -- sleep 600

kubectl -n previews exec kata-smoke -- uname -r    # guest kernel
uname -r                                            # this VM's kernel
```

Two different kernel versions is the proof. If they match, the pod landed on runc and is sharing
this VM's kernel — check `kubectl describe pod` for the runtime class and
`journalctl -u k3s | grep kata`.

**A pod that does not set `runtimeClassName: kata` gets runc.** There is no cluster-wide default
runtime class, so the deployer must always set it; nothing here will catch the omission for you.
Worth an admission policy once a deployer exists.

## What is already configured

- **Namespace `previews`** with a `ResourceQuota` (the ceiling on all previews together) and a
  `LimitRange` (so a compose file that declares no limits still gets some, instead of being
  rejected by the quota).
- **Egress network policy**: the internet, and nothing on the private side — not this VM, not the
  orchestra host, not the cluster's own service network, not link-local metadata endpoints. Only
  cluster DNS is carved back in. k3s's kube-router enforces this at the pod.
- **Ingress network policy**: only from `kube-system`, i.e. the Traefik that k3s ships. Previews
  cannot reach each other, which matters when they are unrelated pull requests sharing a cluster.
- **Traefik and ServiceLB** are k3s defaults and deliberately left enabled — Traefik is the edge
  the per-deployment hostnames will point at.

Tuning: `previews.k3s.quota` for the namespace ceiling, and `previews.kata.configFile` for
sandbox sizing (`default_vcpus = 1`, `default_memory = 2048` in the package's
`configuration.toml`). Override the latter with a `pkgs.runCommand` that patches the package's
file — do not hand-write one, or the store paths for qemu, the guest kernel and the guest image
go with it.

## What drives it

`Orchestra.Deploy`, over the API server and nothing else — there is no orchestra agent on this
box. See [preview deployments](../../README.md#preview-deployments) in the main README for the
`deploy` config section, the tools and the CLI. Deployments are created with an expiry annotation
and the daemon sweeps what has passed it; `orchestra deploy gc` does the same by hand.

### the credential orchestra should hold

Not `/etc/rancher/k3s/k3s.yaml`. That one is `O=system:masters, CN=system:admin` — cluster-admin,
and `exec` into any pod on the node. Fine to have here, wrong to put on the machine holding
orchestra's other credentials, and worse to put on a network.

This configuration creates an `orchestra-deployer` ServiceAccount in the `previews` namespace
whose Role grants exactly what the deployer issues — get/list/watch/create/patch/delete on pods,
services and ingresses, and `create` on `pods/exec` — and a non-expiring token for it. Emit a
kubeconfig for that account with:

```sh
incus exec nixvm -- previews-kubeconfig https://10.0.100.50:6443 > previews.kubeconfig
```

Check what you got:

```sh
kubectl --kubeconfig previews.kubeconfig auth whoami
# system:serviceaccount:previews:orchestra-deployer
kubectl --kubeconfig previews.kubeconfig get nodes
# Error from server (Forbidden)
```

Reaching it from another machine also needs that address in `previews.k3s.tlsSans`, and — if the
address is public rather than private — in `previews.k3s.extraEgressExcept`, or previews can
reach the API server that schedules them.

## What is deliberately not here

- **DNS and TLS for preview hostnames.** Traefik is running; wildcard DNS, certificates and the
  hostname scheme are not configured. Until they are, a preview is reachable by pointing at the
  node with a `Host` header.
- **Authentication in front of previews.** An unlisted URL is not access control.
- **Deletion on pull-request close.** Previews expire on a timer; nothing yet reacts to the pull
  request itself being closed or merged.
