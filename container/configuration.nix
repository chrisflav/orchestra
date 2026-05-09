{
  lib,
  config,
  pkgs,
  ...
}:

let
  unstable = import
    (builtins.fetchTarball https://github.com/nixos/nixpkgs/tarball/31ca9ee01d22aafd34495977ab009e2984afa99d)
    # reuse the current configuration
    { config = config.nixpkgs.config; };
in
{
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    vim
    man
    git
    gh
    yq
    jwt-cli
    jq
    openssl
    gcc
    # Lean version manager
    elan
    landrun
    # Coding agents
    unstable.opencode
    unstable.claude-code
    unstable.mistral-vibe
  ];

  users.users.orchestra = {
    isNormalUser = true;
  };

  services.openssh.enable = true;
}
