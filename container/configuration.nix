{
  lib,
  config,
  pkgs,
  ...
}:

let
  unstable = import
    (builtins.fetchTarball https://github.com/nixos/nixpkgs/tarball/5b2c2d84341b2afb5647081c1386a80d7a8d8605)
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
    # Lean version manager
    elan
    landrun
    # Coding agents
    opencode
    unstable.claude-code
    unstable.mistral-vibe
  ];

  users.users.orchestra = {
    isNormalUser = true;
  };

  users.users."orchestra".openssh.authorizedKeys.keys = [
    "your public key here"
  ];

  services.openssh.enable = true;
}
