# Build context is the repository root (see docker-compose.yaml's `build.context: ..`), so every
# COPY below is relative to the repo root, not this file's directory.
#
# The dependency set here mirrors container/configuration.nix, which is the existing NixOS/Incus
# packaging of the same thing — keep the two in sync when either changes.

# ---- landrun ----
# Orchestra launches every agent through landrun (Orchestra/Sandbox.lean); there is no bypass, so
# this is a hard runtime dependency rather than an optional hardening step.
FROM golang:1.24-bookworm AS landrun
ARG LANDRUN_VERSION=v0.1.15
RUN GOBIN=/out go install github.com/zouuup/landrun/cmd/landrun@${LANDRUN_VERSION}

# ---- orchestra build ----
# elan is installed directly rather than using a Lean base image, so the toolchain pinned by
# lean-toolchain is fetched exactly as `lake` would locally. `build-essential` provides the `cc`
# that ffi/UnixSocket.c (the daemon's Unix-socket shim) and leansqlite's amalgamation need.
FROM debian:bookworm-slim AS build
RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates curl git build-essential \
    && rm -rf /var/lib/apt/lists/*
ENV ELAN_HOME=/root/.elan
ENV PATH="${ELAN_HOME}/bin:${PATH}"
RUN curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf \
      | sh -s -- -y --no-modify-path --default-toolchain none
WORKDIR /src
COPY lean-toolchain lakefile.lean lake-manifest.json Orchestra.lean Main.lean ./
COPY OrchestraTest.lean TestDriver.lean ./
COPY Orchestra ./Orchestra
COPY OrchestraTest ./OrchestraTest
COPY ffi ./ffi
# `lake build orchestra` (not the bare `lake build`) so this doesn't also compile the test driver.
RUN lake build orchestra

# ---- runtime ----
FROM debian:bookworm-slim AS runtime

# git/gh: repo and PR operations. openssl: signs the GitHub App JWT (Orchestra/GitHub.lean).
# jq: used by the entrypoint to emit config.json. build-essential + elan (below): agent tasks
# build the Lean projects they are working on, so the toolchain has to be present at *runtime*,
# not just at build time.
# netcat-openbsd provides `nc`, which is the MCP transport, not a debugging convenience: every
# agent backend points Claude/opencode/pi at the built-in MCP server with `nc 127.0.0.1 <port>`
# (see each `setupMcp` in Orchestra/Agents/). Without it the agent reports the "agent" MCP server
# as perpetually connecting and gets no orchestra tools at all.
RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates curl gnupg git openssl jq procps build-essential netcat-openbsd \
    && curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg \
         -o /usr/share/keyrings/githubcli-archive-keyring.gpg \
    && chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg \
    && echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" \
         > /etc/apt/sources.list.d/github-cli.list \
    && curl -fsSL https://deb.nodesource.com/setup_22.x | bash - \
    && apt-get install -y --no-install-recommends gh nodejs \
    && rm -rf /var/lib/apt/lists/*

RUN npm install -g @anthropic-ai/claude-code

# Skills shipped with orchestra, seeded into /config on first start by the entrypoint. Baked in
# at a fixed path because /config is a bind mount and starts empty.
COPY skills /opt/orchestra/skills

COPY --from=landrun /out/landrun /usr/local/bin/landrun
COPY --from=build /src/.lake/build/bin/orchestra /usr/local/bin/orchestra
COPY docker/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

# Agents run as this user, not root: Landlock is an unprivileged LSM, so nothing here needs root,
# and a sandboxed agent that escapes landrun should not land on a root shell.
RUN useradd --create-home --shell /bin/bash orchestra

USER orchestra

# Orchestra resolves config/data through XDG (Orchestra/Dirs.lean), appending `orchestra` to each:
# config.json lands at /config/orchestra/config.json, state under /data/orchestra/.
# HOME is set explicitly because the entrypoint starts as root (see below) — without it, the
# elan toolchain installed here would be looked up under root's home after the privilege drop.
ENV HOME=/home/orchestra \
    XDG_CONFIG_HOME=/config \
    XDG_DATA_HOME=/data \
    ELAN_HOME=/home/orchestra/.elan
ENV PATH="${ELAN_HOME}/bin:${PATH}"
RUN curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf \
      | sh -s -- -y --no-modify-path --default-toolchain none

# Pre-create every $HOME path the agent sandbox grants access to.
#
# Landlock rules can only attach to paths that already exist, so Sandbox.lean drops any that
# don't (`if ← p.pathExists`) rather than letting landrun hard-error. In a fresh container none
# of these exist, so their rules are silently dropped — and since $HOME itself is never granted,
# the agent cannot create them either: it reports that it can't write ~/.claude, then hangs.
#
# Keep this list in sync with `sandboxPaths` in Orchestra/Agents/Claude.lean (`homeRox` +
# `homeRw`); `.elan` is already created by the elan installer above.
#
# `.claude.json` is seeded with an empty object rather than touched: it is Claude Code's config
# file, and a zero-length file is not valid JSON.
RUN mkdir -p /home/orchestra/.claude \
             /home/orchestra/.cache \
             /home/orchestra/.local \
             /home/orchestra/.config/claude \
             /home/orchestra/.config/gh \
             /home/orchestra/.config/git \
    && echo '{}' > /home/orchestra/.claude.json \
    && touch /home/orchestra/.gitconfig

# Drop the shell dotfiles useradd copied from /etc/skel. $HOME is not granted to the sandbox and
# these are not in any rule, so every interactive shell the agent starts fails to source them and
# prints "/home/orchestra/.bashrc: Permission denied". Harmless to the shell, but it lands in the
# output of every command the agent runs, which is noise it then tries to act on.
#
# Deleting rather than granting read: nothing in Debian's default .bashrc (prompt, aliases,
# colours) matters to an agent, PATH and HOME are passed into the sandbox explicitly, and a
# rule would hand the agent read access to a file people habitually put secrets in. Humans who
# exec into the container still get the system-wide /etc/bash.bashrc, since /etc is granted.
RUN rm -f /home/orchestra/.bashrc /home/orchestra/.bash_logout /home/orchestra/.profile

# Back to root *only* so the entrypoint can take ownership of the bind-mounted /config and /data
# (Docker creates missing bind-mount sources as root) before re-execing itself as `orchestra` via
# setpriv. Nothing beyond those two chowns ever runs as root.
#
# No VOLUME declaration on purpose: docker-compose.yaml bind-mounts host directories, and a VOLUME
# here would additionally spawn anonymous volumes for anyone running the image without mounts.
USER root

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["queue", "start"]
