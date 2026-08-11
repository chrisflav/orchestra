# The previews VM: a NixOS Incus VM running k3s with Kata Containers.
#
# It exists to run deployments nobody has read — arbitrary Dockerfiles and compose files from
# pull requests — and it holds nothing worth stealing. No GitHub App key, no PAT, no agent
# tokens, no clones. That is the point of it being a separate machine from the one running
# orchestra: the isolation story is the hypervisor boundary between the two, and Kata inside is
# the second boundary between one preview and the next.
#
# Build it with ../default.nix (see README.md), or import ./modules/* into the configuration of
# a NixOS VM you already have.

{ modulesPath, lib, ... }:

{
  imports = [
    # Builds `system.build.qemuImage` and `system.build.metadata` — the pair `incus image import`
    # wants — and turns on the Incus guest agent so `incus exec` works.
    "${modulesPath}/virtualisation/incus-virtual-machine.nix"

    ./modules/kata.nix
    ./modules/k3s.nix
  ];

  # mkDefault throughout this file: a machine-specific overlay (./nixvm.nix is the one that runs
  # here) sets the things that are true of one instance rather than of the design — its address,
  # its size, the state version of the image it was created from.
  networking.hostName = lib.mkDefault "previews";

  # `previews.k3s.tlsSans` is deliberately left empty here. k3s already puts the node's own IP in
  # the API certificate; only an extra name the deployer will use needs adding, and that is a
  # property of a particular deployment, not of this configuration.

  # The upstream Incus module leaves root with an empty password, on the assumption that console
  # access is `incus console` on a trusted host. Fine — but this box does face preview traffic,
  # so make sure that empty password can never be used over the network.
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = lib.mkForce "prohibit-password";
    };
  };

  users.users.root.openssh.authorizedKeys.keys = [
    # Add the key that will run `nixos-rebuild switch --target-host` against this VM.
  ];

  # Deliberately close to empty. Anything installed here is attack surface on the machine whose
  # job is to be attacked; the workloads bring their own userland inside their sandboxes.
  environment.systemPackages = [ ];

  system.stateVersion = "25.11";
}
