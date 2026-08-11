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

    # Sized for this VM: 8 vCPU, 16 GiB, 100 GiB disk. The binding constraint is guest RAM — a
    # Kata sandbox takes its memory limit (2 GiB by default here) out of the VM's 16 GiB and
    # gives none of it back while it runs, so the ceiling is roughly five concurrent previews
    # with headroom for k3s itself. `count/pods` is the honest expression of that; the memory
    # numbers stop one preview from eating the budget alone.
    quota = {
      "requests.cpu" = "4";
      "requests.memory" = "8Gi";
      "limits.cpu" = "8";
      "limits.memory" = "12Gi";
      "count/pods" = "10";
    };
  };
}
