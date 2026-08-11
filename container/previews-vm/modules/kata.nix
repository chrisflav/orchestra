# Kata Containers: every pod in its own lightweight VM.
#
# This is the whole reason the previews host exists. The compose files it runs are never
# inspected — that is a deliberate design decision, not an oversight — so the boundary around
# them cannot be a shared kernel. Kata gives each sandbox its own kernel and its own qemu
# process, which is what makes `privileged: true` in an unreviewed compose file a statement
# about a throwaway VM rather than about this host.
#
# nixpkgs has no NixOS module for Kata: it ships the `kata-runtime` package and stops there.
# So the wiring is by hand, and it is worth knowing which piece lives where:
#
#   * the guest kernel, guest rootfs, qemu and virtiofsd paths are already rewritten to the
#     Nix store inside the package's own `configuration.toml` — that is why `/etc` below just
#     points at the package's copy instead of templating a new file.
#   * the shim binary (`containerd-shim-kata-v2`) has to be on *containerd's* PATH, because
#     containerd resolves `runtime_type = "io.containerd.kata.v2"` to that binary name and
#     execs it. That half lives in ./k3s.nix, next to the containerd config that names the
#     runtime, so the two halves are read together.
#   * vhost_vsock carries the runtime-to-guest-agent channel and vhost_net the guest's
#     networking. Without vhost_vsock every sandbox dies with a vsock connect timeout, which
#     reads like a hang rather than a missing module — hence the preflight unit below.

{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.previews.kata;
in
{
  options.previews.kata = {
    package = lib.mkPackageOption pkgs "kata-runtime" { };

    configFile = lib.mkOption {
      type = lib.types.path;
      default = "${cfg.package}/share/defaults/kata-containers/configuration.toml";
      defaultText = lib.literalExpression ''"''${previews.kata.package}/share/defaults/kata-containers/configuration.toml"'';
      description = ''
        The Kata `configuration.toml` placed at {file}`/etc/kata-containers/configuration.toml`,
        which is also the path {option}`previews.k3s`'s containerd stanza names explicitly.

        The default is the package's own file, whose hypervisor, guest kernel, guest image and
        virtiofsd paths nixpkgs has already pointed at the store. Override it with a
        {command}`pkgs.runCommand` that patches that file if you need to change sandbox sizing
        (`default_vcpus`, `default_memory`) — do not hand-write one from scratch, or the store
        paths go with it.
      '';
    };
  };

  config = {
    environment.etc."kata-containers/configuration.toml".source = cfg.configFile;

    # `kata-runtime check` and `kata-monitor` are the two things worth having on the host when a
    # sandbox will not start.
    environment.systemPackages = [ cfg.package ];

    boot.kernelModules = [
      "vhost_vsock"
      "vhost_net"
    ];

    # Fails loudly and early when this VM cannot actually run VMs — the single most likely way
    # for this host to be misconfigured, and otherwise only visible as pods stuck in
    # ContainerCreating. Ordered before k3s but deliberately *not* required by it: a cluster
    # that comes up without Kata is still worth having a shell on to diagnose.
    systemd.services.kata-preflight = {
      description = "Check that this host can run Kata sandboxes";
      wantedBy = [ "multi-user.target" ];
      before = [ "k3s.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
      script = ''
        if [ ! -e /dev/kvm ]; then
          echo "/dev/kvm is missing — this VM has no hardware virtualisation." >&2
          echo "Kata sandboxes cannot start. Enable nested virtualisation on the Incus host:" >&2
          echo "  boot.extraModprobeConfig = \"options kvm_intel nested=1\";  # kvm_amd on AMD" >&2
          echo "and check /sys/module/kvm_intel/parameters/nested there. See ../README.md." >&2
          exit 1
        fi
        exec ${cfg.package}/bin/kata-runtime check
      '';
    };
  };
}
