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
RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates curl gnupg git openssl jq procps build-essential \
    && curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg \
         -o /usr/share/keyrings/githubcli-archive-keyring.gpg \
    && chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg \
    && echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" \
         > /etc/apt/sources.list.d/github-cli.list \
    && curl -fsSL https://deb.nodesource.com/setup_22.x | bash - \
    && apt-get install -y --no-install-recommends gh nodejs \
    && rm -rf /var/lib/apt/lists/*

RUN npm install -g @anthropic-ai/claude-code

COPY --from=landrun /out/landrun /usr/local/bin/landrun
COPY --from=build /src/.lake/build/bin/orchestra /usr/local/bin/orchestra
COPY docker/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

# Agents run as this user, not root: Landlock is an unprivileged LSM, so nothing here needs root,
# and a sandboxed agent that escapes landrun should not land on a root shell.
RUN useradd --create-home --shell /bin/bash orchestra \
    && mkdir -p /config /data \
    && chown orchestra:orchestra /config /data
USER orchestra

# Orchestra resolves config/data through XDG (Orchestra/Dirs.lean), appending `orchestra` to each:
# config.json lands at /config/orchestra/config.json, state under /data/orchestra/.
ENV XDG_CONFIG_HOME=/config \
    XDG_DATA_HOME=/data \
    ELAN_HOME=/home/orchestra/.elan
ENV PATH="${ELAN_HOME}/bin:${PATH}"
RUN curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf \
      | sh -s -- -y --no-modify-path --default-toolchain none

VOLUME ["/config", "/data"]

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["queue", "start"]
