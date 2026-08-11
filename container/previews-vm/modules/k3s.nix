# A single-node k3s server that schedules pods onto Kata.
#
# k3s rather than full Kubernetes because the cluster has one job — hand out a sandbox per
# preview deployment and route a hostname to it — and k3s brings the two other pieces that job
# needs in the box: Traefik as the ingress, and kube-router enforcing NetworkPolicy. Both are
# used below; neither is optional here.

{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.previews.k3s;

  # Why this is written by hand instead of via `services.k3s.containerdConfigTemplate`:
  #
  # k3s 1.34 ships containerd 2.1, which reads its template from `config-v3.toml.tmpl` and uses
  # the `io.containerd.cri.v1.runtime` plugin key. The NixOS option still writes the containerd
  # 1.x filename (`config.toml.tmpl`), which k3s honours only by falling back to rendering a
  # deprecated version-2 config — working today, removed in containerd 3.0. So the template goes
  # in under the v3 name, with the v3 key, and the NixOS option stays unused.
  containerdTemplate = pkgs.writeText "config-v3.toml.tmpl" ''
    {{ template "base" . }}

    [plugins.'io.containerd.cri.v1.runtime'.containerd.runtimes.'kata']
      runtime_type = "io.containerd.kata.v2"
      # A Kata sandbox *is* a VM, so honouring `privileged: true` inside one hands out the
      # guest's devices, not this host's. This is the line that makes running an uninspected
      # compose file a bounded decision.
      privileged_without_host_devices = true
      pod_annotations = ["io.katacontainers.*"]

    [plugins.'io.containerd.cri.v1.runtime'.containerd.runtimes.'kata'.options]
      ConfigPath = "/etc/kata-containers/configuration.toml"
  '';

  containerdDir = "/var/lib/rancher/k3s/agent/etc/containerd";
in
{
  options.previews.k3s = {
    namespace = lib.mkOption {
      type = lib.types.str;
      default = "previews";
      description = "Namespace every preview deployment is created in, and the one the quotas and network policies below apply to.";
    };

    tlsSans = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [ "previews.internal" ];
      description = ''
        Extra names to put in the API server certificate. Needed for whatever address the
        deployer reaches the cluster on — without it, a kubeconfig pointing at anything other
        than this VM's own IP fails certificate verification.
      '';
    };

    quota = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = {
        "requests.cpu" = "8";
        "requests.memory" = "16Gi";
        "limits.cpu" = "16";
        "limits.memory" = "32Gi";
        "count/pods" = "40";
      };
      description = ''
        ResourceQuota for the previews namespace: the ceiling on what all previews together may
        consume, so one runaway deployment cannot starve the rest or the node. Size it to the
        VM, and remember a Kata pod costs its guest's memory plus roughly 160Mi of overhead.
      '';
    };
  };

  config = {
    services.k3s = {
      enable = true;
      # Pinned rather than tracking `pkgs.k3s`: a k3s minor bump can change the containerd major
      # version, and with it the template above.
      package = pkgs.k3s_1_34;
      role = "server";

      extraFlags = map (san: "--tls-san=${san}") cfg.tlsSans;

      manifests = {
        # The handler name must match the containerd runtime name in the template above. A pod
        # opts in with `runtimeClassName: kata`; anything that does not name it lands on runc and
        # shares this VM's kernel, so the deployer must always set it.
        kata-runtimeclass.content = {
          apiVersion = "node.k8s.io/v1";
          kind = "RuntimeClass";
          metadata.name = "kata";
          handler = "kata";
          # The host-side cost of the sandbox — the qemu process and the shim — charged to the
          # quota on top of the pod's own requests so the scheduler sees it instead of
          # discovering it under memory pressure. The *guest's* RAM is separate and is the
          # larger number: `default_memory` in the Kata config (2 GiB) or the pod's memory
          # limit, whichever is greater. Budget both when sizing `quota` below.
          overhead.podFixed = {
            cpu = "250m";
            memory = "160Mi";
          };
        };

        previews-namespace.content = [
          {
            apiVersion = "v1";
            kind = "Namespace";
            metadata.name = cfg.namespace;
          }

          {
            apiVersion = "v1";
            kind = "ResourceQuota";
            metadata = {
              name = "previews-quota";
              namespace = cfg.namespace;
            };
            spec.hard = cfg.quota;
          }

          # Every container gets a limit whether the compose file asked for one or not: without
          # this, a pod with no limits is quota-rejected, and the deployer would have to inject
          # limits into user-authored compose.
          {
            apiVersion = "v1";
            kind = "LimitRange";
            metadata = {
              name = "previews-defaults";
              namespace = cfg.namespace;
            };
            spec.limits = [
              {
                type = "Container";
                default = {
                  cpu = "2";
                  memory = "2Gi";
                };
                defaultRequest = {
                  cpu = "100m";
                  memory = "256Mi";
                };
              }
            ];
          }

          # Egress: the internet, and nothing on the private side of the network. A preview may
          # pull from a registry and call an API; it may not reach this VM, the orchestra host,
          # the cluster's own service network, or anything else on the LAN. kube-router (shipped
          # and enabled by k3s) enforces this at the pod, before the host firewall is involved.
          {
            apiVersion = "networking.k8s.io/v1";
            kind = "NetworkPolicy";
            metadata = {
              name = "previews-egress";
              namespace = cfg.namespace;
            };
            spec = {
              podSelector = { };
              policyTypes = [ "Egress" ];
              egress = [
                {
                  to = [
                    {
                      ipBlock = {
                        cidr = "0.0.0.0/0";
                        except = [
                          "10.0.0.0/8" # covers the pod (10.42/16) and service (10.43/16) CIDRs
                          "172.16.0.0/12"
                          "192.168.0.0/16"
                          "169.254.0.0/16" # link-local, i.e. cloud metadata endpoints
                        ];
                      };
                    }
                  ];
                }
                {
                  # The one exception carved back in: cluster DNS, or nothing resolves.
                  to = [
                    {
                      namespaceSelector.matchLabels."kubernetes.io/metadata.name" = "kube-system";
                    }
                  ];
                  ports = [
                    {
                      protocol = "UDP";
                      port = 53;
                    }
                    {
                      protocol = "TCP";
                      port = 53;
                    }
                  ];
                }
              ];
            };
          }

          # Ingress: only from the edge proxy. Previews cannot reach each other, which matters
          # because they are unrelated pull requests that happen to share a cluster.
          {
            apiVersion = "networking.k8s.io/v1";
            kind = "NetworkPolicy";
            metadata = {
              name = "previews-ingress";
              namespace = cfg.namespace;
            };
            spec = {
              podSelector = { };
              policyTypes = [ "Ingress" ];
              ingress = [
                {
                  from = [
                    {
                      namespaceSelector.matchLabels."kubernetes.io/metadata.name" = "kube-system";
                    }
                  ];
                }
              ];
            };
          }
        ];
      };
    };

    # containerd execs `containerd-shim-kata-v2` by name off its own PATH — this is what makes
    # the runtime handler above resolve to anything.
    systemd.services.k3s.path = [ config.previews.kata.package ];

    systemd.tmpfiles.settings."10-k3s-kata" = {
      ${containerdDir}.d.mode = "0700";
      "${containerdDir}/config-v3.toml.tmpl"."L+".argument = "${containerdTemplate}";
    };

    networking.firewall = {
      allowedTCPPorts = [
        6443 # kube-apiserver, for the deployer's kubeconfig
        80
        443 # Traefik
      ];
      allowedUDPPorts = [ 8472 ]; # flannel vxlan
      # Plumbing, not policy: the cluster's own traffic has to cross these interfaces for DNS and
      # the API server to work at all. What a preview pod may reach is decided by the
      # NetworkPolicies above, which apply before the host firewall sees the packet.
      trustedInterfaces = [
        "cni0"
        "flannel.1"
      ];
    };
  };
}
