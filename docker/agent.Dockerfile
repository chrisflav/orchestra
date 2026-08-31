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
# Whatever a *repository* needs to build is deliberately absent — no Lean, no JDK, no browser, and
# (see below) no Node either. A repository that needs those names its own image (`execution.image`
# in its .orchestra/config.json) or gets one pinned for it (`execution.options.images`); see
# docs/kubernetes.md. This is the floor everything else is built on, and the image an operator can
# point at on day one without building anything.

# ---- build: get the CLI out of npm --------------------------------------------------------
#
# Resolved to an exact version by CI before the build (.github/workflows/docker-agent.yml), so the
# image is reproducible and its tag says what is in it. `latest` is the default for a local
# `docker build`, where nobody has resolved it.
FROM node:22-slim AS build
ARG CLAUDE_CODE_VERSION=latest

# The npm package ships the CLI as a self-contained native executable per platform, alongside a
# `cli.js` wrapper that needs Node to run. Taking the executable is what lets the runtime stage
# below carry no Node at all.
#
# That is a fact about how the package is currently built rather than a documented interface, so
# it is pinned down here rather than trusted: `find` failing leaves `install` with no source and
# the build stops. A release that goes back to a Node-only CLI therefore breaks *this line*, at
# build time, instead of shipping an image whose `claude` does not start.
RUN npm install -g "@anthropic-ai/claude-code@${CLAUDE_CODE_VERSION}" \
 && bin="$(find /usr/local/lib/node_modules/@anthropic-ai/claude-code -type f -perm -u+x \
             -name claude ! -name '*.js' ! -name '*.cjs' | head -1)" \
 && test -n "$bin" || { echo "no native claude executable in the package" >&2; exit 1; } \
 && install -D "$bin" /out/claude \
 && /out/claude --version

# ---- runtime: the floor, and nothing else -------------------------------------------------
#
# Debian rather than Alpine: this image is also where a repository's own hooks run when it has not
# named an image of its own, and toolchain installers overwhelmingly expect glibc. Alpine measured
# 563MB against this stage's 585MB — 4%, for a class of failure that shows up as somebody's
# `init.sh` dying on a cluster rather than here.
FROM debian:trixie-slim

# One layer, lists dropped in the same one — a separate `rm` leaves them in the layer below, where
# they still cost what they weigh. `bash` and `tar` are usually already present; naming them keeps
# the required list in one place and costs nothing when they are.
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      bash \
      ca-certificates \
      git \
      netcat-openbsd \
      tar \
 && rm -rf /var/lib/apt/lists/*

COPY --from=build /out/claude /usr/local/bin/claude

# Not root. Claude Code refuses `--dangerously-skip-permissions` under uid 0 — which is how
# orchestra runs it — and the pod carries no securityContext, so the image's own USER is the only
# thing that decides who the agent is. See docs/kubernetes.md.
#
# Nothing has to be granted for this to work: every directory orchestra mounts (the checkout,
# $HOME, the control directory) is an emptyDir, which the kubelet creates world-writable.
#
# `/home/agent` matches the `home_path` default, so the mount lands on a directory that already
# exists and belongs to this user.
RUN useradd --uid 1001 --create-home --home-dir /home/agent --shell /bin/bash agent
USER agent
ENV HOME=/home/agent

# The pod overrides this with the checkout's path; it matters only for a bare `docker run`.
WORKDIR /home/agent
