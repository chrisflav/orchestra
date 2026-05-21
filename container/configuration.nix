{
  lib,
  config,
  pkgs,
  ...
}:

let
  unstable = import
    (builtins.fetchTarball https://github.com/nixos/nixpkgs/tarball/071434384885966c13bb5a4fd4e6d16788c8247f)
    # reuse the current configuration
    { config = config.nixpkgs.config; };

  pi-coding-agent-wrapped = pkgs.symlinkJoin {
    name = "pi-coding-agent";
    buildInputs = [ pkgs.makeWrapper ];
    paths = [ unstable.pi-coding-agent ];
    postBuild = ''
      wrapProgram $out/bin/pi \
        --run 'export NPM_CONFIG_PREFIX="$HOME/.pi/npm/"' \
        --prefix PATH : ${lib.makeBinPath [ pkgs.nodejs_latest ]}
    '';
  };
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
    pi-coding-agent-wrapped
  ];

  users.users.orchestra = {
    isNormalUser = true;
  };

  services.openssh.enable = true;
}
