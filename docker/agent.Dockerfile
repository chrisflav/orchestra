# The image a task runs in under the kubernetes execution backend.
#
# Not the daemon image. `docker/Dockerfile` builds orchestra itself — the thing that holds the
# credentials and decides what runs; this builds the far smaller thing a *task* runs inside, one
# pod per task, holding nothing but the agent and the tools it needs. Keeping them apart is the
# point: this image is handed a checkout and a per-task token and nothing else, so everything it
# does not contain is something an agent cannot reach.
#
# What has to be here is fixed by the backend, not by taste (docs/kubernetes.md, "what you need"):
#
#   claude   the agent CLI orchestra spawns; `RunSpec.command` is the bare name, resolved on
#            *this* image's PATH rather than the daemon's
#   sh       every command runs as `sh -c` (Kubernetes.runnerScript)
#   bash     the repository's own hooks — init.sh, before.sh, validation.sh, after.sh
#   tar      how the checkout travels, in both directions
#   nc       the MCP transport; the agent's tools are a socket on the daemon
#   git      how the agent commits and pushes its work
#
# On top of that it carries the *package managers* a repository's `init.sh` reaches for — npm,
# elan, uv — but none of the toolchains they install. That line is where the size is: a Lean
# toolchain, a JDK and a CPython are hundreds of megabytes each and only one repository wants any
# given one, while the managers are a few tens of megabytes and every repository that needs a
# toolchain needs one of them first. A repository still names its own image (`execution.image` in
# its .orchestra/config.json) when it needs something this cannot install.
#
# Where what they install *goes* is the other half of the design. `$HOME` is an `emptyDir` the
# kubelet mounts over whatever the image put at that path, so a toolchain baked in here would be
# masked at runtime and one installed at runtime does not survive the pod — unless `home_claim`
# points that mount at a volume, which is exactly what it is for. So these managers are installed
# to /usr/local (outside `$HOME`, never masked) and left to put their content under `$HOME`, where
# a claim makes it persist and `init.sh` gets cheap from the second task onward. It is also where
# `Sandbox.grantsFor` already expects it: `.elan` and `.cache` are home-relative writable grants.

FROM node:22-slim

# Resolved to exact versions by CI before the build (.github/workflows/docker-agent.yml), so the
# image is reproducible and its tag says what is in it. `latest` is the default for a local
# `docker build`, where nobody has resolved it.
ARG CLAUDE_CODE_VERSION=latest
# Pinned rather than resolved: these move slowly, and a bump should be a commit somebody made
# rather than something a Monday build did.
ARG ELAN_VERSION=v4.2.4
ARG UV_VERSION=0.12.7

# One layer, lists dropped in the same one — a separate `rm` leaves them in the layer below, where
# they still cost what they weigh. `bash` and `tar` come with the base; naming them keeps the
# required list in one place and costs nothing when they are already there. `curl` and `xz-utils`
# are for the two downloads below and stay: `init.sh` scripts use both constantly.
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      bash \
      ca-certificates \
      curl \
      git \
      netcat-openbsd \
      tar \
      xz-utils \
 && rm -rf /var/lib/apt/lists/*

# The agent CLI. npm comes with the base image and stays — a JS repository's `init.sh` wants it,
# and it is how this is installed in the first place.
RUN npm install -g "@anthropic-ai/claude-code@${CLAUDE_CODE_VERSION}" \
 && npm cache clean --force

# elan, the Lean toolchain manager. `elan-init` is the manager binary under its installer name
# (elan is a rustup fork and keeps that property), so it is installed under the name it is *used*
# by rather than run against this filesystem: a normal `elan-init` would build an ELAN_HOME here,
# at a path the pod mounts `$HOME` over, and everything in it would vanish at runtime. Toolchains
# land in $HOME/.elan instead; see the note at the top.
#
# `lean`, `lake` and the rest are shims — copies of elan that dispatch on the name they were
# invoked as — and elan only writes them as part of that self-install. So they are recreated here
# as symlinks, and which names to create is asked of elan (installed into a throwaway ELAN_HOME
# with no toolchain) rather than hardcoded, so a release that adds a shim is picked up by the next
# build instead of going missing.
RUN curl -fsSL "https://github.com/leanprover/elan/releases/download/${ELAN_VERSION}/elan-x86_64-unknown-linux-gnu.tar.gz" \
      | tar -xz -C /tmp \
 && install -m 0755 /tmp/elan-init /usr/local/bin/elan \
 && ELAN_HOME=/tmp/elan-probe /tmp/elan-init -y --no-modify-path --default-toolchain none >/dev/null \
 && for shim in /tmp/elan-probe/bin/*; do \
      name="$(basename "$shim")"; \
      [ "$name" = elan ] || ln -sf elan "/usr/local/bin/$name"; \
    done \
 && rm -rf /tmp/elan-probe /tmp/elan-init \
 && elan --version \
 && test -L /usr/local/bin/lake

# uv, the Python package and version manager. A static binary plus its `uvx` runner; the Pythons
# and virtualenvs it installs go under $HOME.
RUN curl -fsSL "https://github.com/astral-sh/uv/releases/download/${UV_VERSION}/uv-x86_64-unknown-linux-gnu.tar.gz" \
      | tar -xz -C /tmp \
 && install -m 0755 /tmp/uv-x86_64-unknown-linux-gnu/uv /tmp/uv-x86_64-unknown-linux-gnu/uvx /usr/local/bin/ \
 && rm -rf /tmp/uv-x86_64-unknown-linux-gnu \
 && uv --version

# Not root. Claude Code refuses `--dangerously-skip-permissions` under uid 0 — which is how
# orchestra runs it — and the pod carries no securityContext, so the image's own USER is the only
# thing that decides who the agent is. See docs/kubernetes.md.
#
# Nothing has to be granted for this to work: every directory orchestra mounts (the checkout,
# $HOME, the control directory) is an emptyDir, which the kubelet creates world-writable.
#
# `/home/agent` matches the `home_path` default, so the mount lands on a directory that already
# exists and belongs to this user. uid 1001 because the base image already spends 1000 on `node`;
# the number is not load-bearing.
RUN useradd --uid 1001 --create-home --home-dir /home/agent --shell /bin/bash agent
USER agent
ENV HOME=/home/agent

# Where the two managers put executables under `$HOME`: elan's own bin if a repository runs a
# self-install, and uv's target for `uv tool install`. Neither directory exists yet — both are
# created under the mounted `$HOME` at runtime — and a PATH entry that does not exist is simply
# skipped, so naming them here saves every repository's `init.sh` from exporting them.
#
# `lean` and `lake` do not depend on this: those are the shims in /usr/local/bin above, which work
# from the moment a toolchain is installed (or, for a repository with a `lean-toolchain` file, are
# what installs it).
ENV PATH="/home/agent/.elan/bin:/home/agent/.local/bin:${PATH}"

# The pod overrides this with the checkout's path; it matters only for a bare `docker run`.
WORKDIR /home/agent
