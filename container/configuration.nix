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
    # zlib, for taxis's gzip shim: an agent building orchestra itself, or any Lean project
    # depending on taxis, compiles `bindings/gzip.c` and cannot without it. Mirrors
    # zlib1g-dev in docker/Dockerfile — but three things rather than one, because on NixOS a
    # package in this list is not a package on a compiler's search path: the headers are in
    # `zlib.dev` (the default output installs none), the system profile links no `/include` at
    # all unless told to, and `cc` reads neither location without being pointed at it.
    zlib.dev
    zlib
    # `nc` is the MCP transport, not a debugging tool: every agent backend points its MCP client
    # at the built-in server with `nc 127.0.0.1 <port>` (see setupMcp in Orchestra/Agents/).
    # Without it the agent sees the "agent" MCP server stuck connecting and gets no tools.
    netcat-openbsd
    # Lean version manager
    elan
    landrun
    # Coding agents
    unstable.opencode
    unstable.claude-code
    unstable.mistral-vibe
    pi-coding-agent-wrapped
  ];

  environment.pathsToLink = [ "/include" ];
  environment.variables = {
    C_INCLUDE_PATH = "/run/current-system/sw/include";
    LIBRARY_PATH = "/run/current-system/sw/lib";
  };

  users.users.orchestra = {
    isNormalUser = true;
  };

  services.openssh.enable = true;
}
