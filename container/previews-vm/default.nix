# Builds the Incus VM image for ./configuration.nix against a pinned nixpkgs.
#
#   nix-build -A qemuImage -A metadata
#
# The pin is the whole point: the same commit produces the same image, so a rebuilt previews host
# is the one you tested rather than whatever the channel moved to. Bump `rev` and `sha256`
# together (`nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz`).

{
  system ? builtins.currentSystem,

  # nixos-25.11 as of 2026-08-11.
  nixpkgs ? builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/b6018f87da91d19d0ab4cf979885689b469cdd41.tar.gz";
    sha256 = "0ln4yw7z3g9lb0x081hc0pd2j1wsx2qqf6bgmwwvdbkcl4bcy1dp";
  },
}:

let
  eval = import "${nixpkgs}/nixos" {
    inherit system;
    configuration = ./configuration.nix;
  };
in
{
  # The qcow2 disk and the metadata tarball, in that order, are the two arguments to
  # `incus image import`.
  inherit (eval.config.system.build) qemuImage metadata;

  # The system closure, for `nixos-rebuild --target-host` against an already-running VM.
  toplevel = eval.system;
}
