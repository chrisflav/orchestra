# This machine: the previews host as it actually runs, as the incus instance `nixvm`.
#
# ./configuration.nix describes the design — k3s on Kata, the namespace, the policies. Everything
# here is true of this one instance and of nothing else: its address, its size, and the state
# version of the image it was created from. Keeping the two apart is what lets the same modules
# build a fresh image (../default.nix, pinned to nixos-25.11) and drive an already-running VM on
# a different channel.
#
# On the VM this is reached through /etc/nixos/configuration.nix, which imports it.

{ lib, ... }:

{
  imports = [ ./configuration.nix ];

  networking.hostName = lib.mkForce "nixvm";

  # The instance was created from a 26.05 image. ./configuration.nix carries the state version of
  # the nixpkgs the *image* is built from, which is a different question and a different answer.
  system.stateVersion = lib.mkForce "26.05";

  # Static, and deliberately so. incusbr0 runs dnsmasq with a lease range, but the host's firewall
  # drops DHCP (and DNS) on that bridge before dnsmasq is reached, so the guest asks and never
  # hears back. The proper fix is one line in the *host's* configuration:
  #
  #     networking.firewall.trustedInterfaces = [ "incusbr0" ];
  #
  # after which this block can become `networkConfig.DHCP = "ipv4"` and the public resolvers below
  # can give way to incus's own on 10.0.100.1. Until then, a fixed address is also what k3s wants:
  # a node whose IP moves is a node whose certificates and etcd records have to be rewritten.
  networking.useNetworkd = true;
  networking.useDHCP = false;

  systemd.network.networks."10-lan" = {
    matchConfig.Name = "en*";
    address = [ "10.0.100.50/24" ];
    routes = [ { Gateway = "10.0.100.1"; } ];
    dns = [
      "1.1.1.1"
      "9.9.9.9"
    ];
    linkConfig.RequiredForOnline = "routable";
  };

  previews.k3s = {
    tlsSans = [ "10.0.100.50" ];

    # Sized for this VM: 8 vCPU, 16 GiB, 100 GiB disk.
    #
    # The binding constraint is guest RAM. A Kata sandbox takes its memory limit out of the VM's
    # 16 GiB and gives none of it back while it runs, and orchestra asks for 4 GiB per preview by
    # default (`deploy.memory_limit`), stated as both request and limit — Kubernetes copies
    # limits into requests anyway, so the LimitRange defaults never apply to these pods and the
    # quota is charged the full amount either way.
    #
    # 12 GiB of requests is therefore three concurrent previews, leaving 4 GiB for k3s and the
    # system. `count/pods` is set to match rather than to a larger number that memory would never
    # let you reach: a quota that rejects at a limit nobody can predict is worse than a low one.
    quota = {
      "requests.cpu" = "6";
      "requests.memory" = "12Gi";
      "limits.cpu" = "6";
      "limits.memory" = "12Gi";
      "count/pods" = "3";
    };
  };
}
