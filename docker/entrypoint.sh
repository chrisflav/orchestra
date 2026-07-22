#!/bin/sh
# Renders config.json from environment variables, then execs orchestra with whatever arguments
# the image was given (`queue start` by default — see the Dockerfile's CMD).
#
# A config.json mounted into the image always wins: the generated one only covers the handful of
# settings worth exposing as environment variables. Anything richer — listeners, agent auth
# blocks, additional_sandbox_paths — should be mounted instead. See examples/ for the full shape.
set -eu

RUN_AS=orchestra
CONFIG_ROOT="${XDG_CONFIG_HOME:-/config}"
DATA_ROOT="${XDG_DATA_HOME:-/data}"
CONFIG_DIR="$CONFIG_ROOT/orchestra"
DATA_DIR="$DATA_ROOT/orchestra"
CONFIG_FILE="$CONFIG_DIR/config.json"

# /config and /data are bind-mounted host directories, and Docker creates a missing bind-mount
# source as root — so on a fresh checkout they arrive unwritable by the unprivileged user agents
# run as. Fix that here, then immediately drop privileges by re-execing this same script as
# `orchestra`; everything below the drop runs unprivileged.
#
# The ownership test keeps this O(1) after first start: a plain `chown -R` would walk every cloned
# repository under /data on every restart.
if [ "$(id -u)" = "0" ]; then
  target_uid="$(id -u "$RUN_AS")"
  for d in "$CONFIG_ROOT" "$DATA_ROOT"; do
    mkdir -p "$d"
    if [ "$(stat -c %u "$d")" != "$target_uid" ]; then
      echo "[entrypoint] taking ownership of $d for $RUN_AS"
      chown -R "$RUN_AS:$RUN_AS" "$d"
    fi
  done
  exec setpriv --reuid="$RUN_AS" --regid="$RUN_AS" --init-groups "$0" "$@"
fi

mkdir -p "$CONFIG_DIR" "$DATA_DIR"

if [ -f "$CONFIG_FILE" ]; then
  echo "[entrypoint] using existing $CONFIG_FILE (env vars ignored)"
else
  if [ -z "${ORCHESTRA_TAXIS_URL:-}" ]; then
    echo "[entrypoint] error: no $CONFIG_FILE and ORCHESTRA_TAXIS_URL is unset." >&2
    echo "[entrypoint] Set ORCHESTRA_TAXIS_URL/ORCHESTRA_TAXIS_TOKEN, or mount a config.json." >&2
    exit 1
  fi

  # Built with jq rather than a heredoc so tokens containing quotes or backslashes can't break
  # the JSON. `github_app` is emitted unconditionally because AppConfig's FromJson requires it
  # (Orchestra/Config.lean) even when GitHub App auth is unused — a PAT is the usual alternative.
  #
  # Written to a per-process temporary file and renamed into place, so a reader never sees a
  # half-written config: on a first `up` the daemon and the dashboard containers start together
  # and both find the file missing. `mv` within one directory is atomic, so the loser simply
  # replaces identical content.
  tmp="$CONFIG_FILE.$$.tmp"
  jq -n \
    --arg taxis_url    "$ORCHESTRA_TAXIS_URL" \
    --arg taxis_token  "${ORCHESTRA_TAXIS_TOKEN:-}" \
    --arg pat          "${ORCHESTRA_GITHUB_PAT:-}" \
    --arg app_id       "${ORCHESTRA_GITHUB_APP_ID:-0}" \
    --arg key_path     "${ORCHESTRA_GITHUB_PRIVATE_KEY_PATH:-}" \
    --arg claude_token "${ORCHESTRA_CLAUDE_TOKEN:-}" \
    --arg api_key      "${ORCHESTRA_ANTHROPIC_API_KEY:-}" \
    '{
       github_app: { app_id: ($app_id | tonumber), private_key_path: $key_path },
       github: { pat: $pat },
       taxis: ({ url: $taxis_url }
               + (if $taxis_token == "" then {} else { token: $taxis_token } end))
     }
     + (if $claude_token == "" then {} else { claude_token: $claude_token } end)
     + (if $api_key == "" then {} else { anthropic_api_key: $api_key } end)' \
    > "$tmp"
  mv "$tmp" "$CONFIG_FILE"

  echo "[entrypoint] wrote $CONFIG_FILE (taxis: $ORCHESTRA_TAXIS_URL)"
fi

# Seed the bundled skills into the config volume. They tell agents to use orchestra's MCP tools
# instead of `gh` for pull requests and taxis issues. Only when absent, so local edits survive
# restarts; delete the directory to get the shipped copy back.
# Under CONFIG_DIR, not CONFIG_ROOT: Dirs.skillsDir is <config>/skills where <config> is
# already the orchestra-suffixed XDG path (Orchestra/Dirs.lean).
SKILLS_DIR="$CONFIG_DIR/skills"
if [ ! -e "$SKILLS_DIR" ] && [ -d /opt/orchestra/skills ]; then
  cp -r /opt/orchestra/skills "$SKILLS_DIR"
  echo "[entrypoint] installed bundled skills into $SKILLS_DIR"
fi

# The two checks below are about *running tasks*: signing a GitHub JWT and sandboxing an agent.
# The dashboard container shares this entrypoint and this environment but does neither — it has
# no /secrets mount and never launches anything — so running them there produces warnings that
# are false for that container and would teach people to ignore the real ones.
case "${1:-}" in
  dashboard) exec orchestra "$@" ;;
esac

# GitHub App auth reads this key from disk to sign a JWT (Orchestra/GitHub.lean). Check it up
# front: an unreadable key surfaces otherwise as an opaque openssl failure the first time a task
# needs a GitHub token. Only checked when set — a PAT is the alternative and needs no key.
if [ -n "${ORCHESTRA_GITHUB_PRIVATE_KEY_PATH:-}" ]; then
  if [ -d "$ORCHESTRA_GITHUB_PRIVATE_KEY_PATH" ]; then
    echo "[entrypoint] error: $ORCHESTRA_GITHUB_PRIVATE_KEY_PATH is a directory, not a key." >&2
    echo "[entrypoint] Docker creates a directory when a bind-mounted file is missing on the" >&2
    echo "[entrypoint] host. Put the .pem in the mounted secrets directory and recreate." >&2
    exit 1
  elif [ ! -r "$ORCHESTRA_GITHUB_PRIVATE_KEY_PATH" ]; then
    echo "[entrypoint] warning: GitHub App key $ORCHESTRA_GITHUB_PRIVATE_KEY_PATH is missing" >&2
    echo "[entrypoint] or unreadable; GitHub App auth will fail. See docker/README.md." >&2
  fi
fi

# Probe landrun by actually building a Landlock ruleset and running something inside it — a bare
# `landrun --help` proves nothing, since it never reaches the landlock_* syscalls. Every agent
# launch goes through landrun with no bypass, so if this fails, every dispatched task will too;
# warn now rather than surfacing it as an opaque sandbox error on the first task.
probe_args=""
for d in /usr /bin /lib /lib64; do
  [ -d "$d" ] && probe_args="$probe_args --rox $d"
done
# shellcheck disable=SC2086
if ! landrun $probe_args -- /bin/true >/dev/null 2>&1; then
  echo "[entrypoint] warning: landrun could not create a Landlock sandbox here." >&2
  echo "[entrypoint] Agent tasks will fail. Check that the host kernel has Landlock enabled" >&2
  echo "[entrypoint] (grep landlock /sys/kernel/security/lsm) and see docker/README.md." >&2
fi

exec orchestra "$@"
