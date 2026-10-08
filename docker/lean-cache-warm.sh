#!/usr/bin/env bash
# Fill a node-level Lean cache for lean-cache-shim (docker/lean-cache-shim.sh) to link from.
#
#     lean-cache-warm [MATHLIB_REV ...]
#
# Warms each Mathlib revision named on the command line, plus every revision a pod has asked for
# in $LEAN_CACHE_REQUESTS, then prunes old ones. Meant to run on a schedule, as the only writer of
# a cache that every task pod mounts read-only; see "a shared Lean cache" in docs/kubernetes.md.
#
# Per revision it builds a throwaway project that requires nothing but Mathlib at that revision,
# so it needs no access to the repositories the agents work on -- only to Mathlib's, which is
# public. What it keeps is everything Mathlib's cache provides: the toolchain, Mathlib, the
# packages Mathlib's own manifest pins, and the downloaded archives. A project's other
# dependencies are not Mathlib's to build and are left to the task.
#
# The tree is built *before* it becomes read-only: `lake exe cache get` alone leaves some trace
# and lock files for Lake to write on first use (a widget package lock, a few .hash files), which a
# read-only mount would then refuse. One `lake build` of `import Mathlib` writes them all.
#
# LAYOUT (the shim reads exactly this):
#
#   elan/toolchains/<elan dir>          toolchains, shared by every revision that uses them
#   packages/<name>/<rev>/              a built package checkout
#   packages/<name>/<rev>.toolchain     the toolchain it was built with; the shim links a package
#                                       only into a project on the same toolchain, since Lake would
#                                       otherwise rebuild it -- into a tree it cannot write
#   mathlib-cache/<mathlib rev>/        `cache get`'s archives for that revision (MATHLIB_CACHE_DIR)
#   warmed/<mathlib rev>                when that revision was last warmed *or used*
#
# REQUESTS. A pod writes $LEAN_CACHE_REQUESTS/<mathlib rev> both when the revision is missing and
# when it linked it, so the directory doubles as a record of use: a request for a revision already
# here just refreshes `warmed/<rev>`, and pruning keeps the most recently *used* revisions. A
# request is removed only once it has been honoured, so a failed warm is retried next run.
#
# Environment:
#   LEAN_CACHE_DIR       the cache, writable here          (default /lean-cache)
#   LEAN_CACHE_REQUESTS  revisions pods have asked for     (default /lean-cache-requests)
#   LEAN_CACHE_KEEP      how many unpinned revisions to keep, most recently used first (default 4);
#                        the ones named on the command line are kept on top of these
#   LEAN_CACHE_GRACE     seconds a pruned tree stays in trash/ before it is deleted, so a task that
#                        linked it just before the prune can finish (default 86400)

# No `set -e` here, deliberately: each revision's warm runs in a subshell with its own errexit, and
# errexit is silently ignored in any function or subshell called from an `if`, `&&` or `||`. So the
# calls below are plain statements and their status is read afterwards.
set -uo pipefail

cache=${LEAN_CACHE_DIR:-/lean-cache}
requests=${LEAN_CACHE_REQUESTS:-/lean-cache-requests}
keep=${LEAN_CACHE_KEEP:-4}
grace=${LEAN_CACHE_GRACE:-86400}
export ELAN_HOME=$cache/elan
mkdir -p "$ELAN_HOME" "$cache/mathlib-cache" "$cache/packages" "$cache/tmp" "$cache/trash" "$cache/failed" \
  "$cache/warmed" || exit 1

# One writer. A second run -- an overlapping schedule, a manual `kubectl create job` -- would wipe
# this one's scratch directory below and race it on every rename.
exec 9> "$cache/.lock"
if ! flock -n 9; then
  echo "another lean-cache-warm holds $cache/.lock; leaving it to finish"
  exit 0
fi

# A previous run that died part-way leaves its scratch directory behind; nothing else uses tmp.
rm -rf "${cache:?}/tmp/"*

valid_rev() { [[ $1 =~ ^[0-9a-f]{40}$ ]]; }

warm() {
  local rev=$1 work rc
  if [ -e "$cache/packages/mathlib/$rev" ]; then
    echo "have mathlib@$rev"
    date +%s > "$cache/warmed/$rev"
    return 0
  fi
  echo "warming mathlib@$rev"
  work=$(mktemp -d "$cache/tmp/warm.XXXXXX") || return 1
  (
    set -e
    cd "$work"
    export MATHLIB_CACHE_DIR=$cache/mathlib-cache/$rev
    mkdir -p "$MATHLIB_CACHE_DIR"
    curl -fsSL "https://raw.githubusercontent.com/leanprover-community/mathlib4/$rev/lean-toolchain" > lean-toolchain
    toolchain=$(tr -d '[:space:]' < lean-toolchain)
    [ -n "$toolchain" ]
    cat > lakefile.toml <<TOML
name = "warm"
defaultTargets = ["Warm"]
[[lean_lib]]
name = "Warm"
[[require]]
name = "mathlib"
git = "https://github.com/leanprover-community/mathlib4"
rev = "$rev"
TOML
    echo 'import Mathlib' > Warm.lean
    elan toolchain install "$toolchain"
    # GitHub turns away anonymous clones of a repository this size now and then; it passes.
    for i in 1 2 3 4 5; do
      if lake update && [ -d .lake/packages/mathlib ]; then break; fi
      echo "lake update failed (attempt $i); retrying in $((i * 30))s" >&2
      rm -rf .lake; sleep $((i * 30))
    done
    [ -d .lake/packages/mathlib ]
    [ "$(git -C .lake/packages/mathlib rev-parse HEAD)" = "$rev" ]
    lake exe cache get
    lake build
    chmod -R a+rX .lake/packages "$MATHLIB_CACHE_DIR" "$ELAN_HOME"
    for p in .lake/packages/*; do
      name=$(basename "$p")
      [ "$name" = mathlib ] && continue
      prev=$(git -C "$p" rev-parse HEAD)
      dest=$cache/packages/$name/$prev
      [ -e "$dest" ] && continue
      mkdir -p "$(dirname "$dest")"
      # Sidecar first, tree second: a tree without its sidecar is never linked, so a pod sees the
      # whole entry or none of it. One rename on one filesystem.
      echo "$toolchain" > "$dest.toolchain"
      mv "$p" "$dest"
    done
    # Mathlib last: its presence is what the shim and this script test for.
    mkdir -p "$cache/packages/mathlib"
    echo "$toolchain" > "$cache/packages/mathlib/$rev.toolchain"
    mv .lake/packages/mathlib "$cache/packages/mathlib/$rev"
  )
  rc=$?
  rm -rf "$work"
  if [ $rc -ne 0 ]; then
    echo "warming mathlib@$rev failed (exit $rc)" >&2
    rm -rf "${cache:?}/mathlib-cache/$rev"
    return 1
  fi
  date +%s > "$cache/warmed/$rev"
}

# Revisions to warm: the pinned ones, then whatever pods asked for.
pinned=()
for r in "$@"; do valid_rev "$r" && pinned+=("$r"); done
requested=()
if [ -d "$requests" ]; then
  for f in "$requests"/*; do
    [ -e "$f" ] || continue
    r=$(basename "$f")
    # Only a revision is accepted: it is fetched from Mathlib's repository by that name, so a
    # request decides *which* revision is warmed and nothing about what ends up in the cache.
    if valid_rev "$r"; then requested+=("$r"); else rm -f "$f"; fi
  done
fi

status=0
for rev in "${pinned[@]}" "${requested[@]}"; do
  warm "$rev"
  if [ $? -eq 0 ]; then
    rm -f "$requests/$rev" "$cache/failed/$rev"
  else
    status=1
    # A request is retried on the next run, but not for ever: one naming a revision that does not
    # exist upstream would otherwise fail every run from now on.
    fails=$(( $(cat "$cache/failed/$rev" 2>/dev/null || echo 0) + 1 ))
    echo "$fails" > "$cache/failed/$rev"
    if [ "$fails" -ge 3 ]; then
      echo "giving up on mathlib@$rev after $fails attempts" >&2
      rm -f "$requests/$rev" "$cache/failed/$rev"
    fi
  fi
done

# Prune: keep the pinned revisions and the $keep most recently used others. Pruned trees go to
# trash/ rather than straight to `rm -rf`, and are deleted only after $grace seconds, so a task that
# linked one just before the prune does not have it vanish under its build.
now=$(date +%s)
declare -A keepset=()
for r in "${pinned[@]}"; do keepset[$r]=1; done
n=0
for r in $(find "$cache/warmed" -type f -printf '%T@ %f\n' 2>/dev/null | sort -rn | cut -d" " -f2); do
  [ -n "${keepset[$r]:-}" ] && continue
  [ $n -lt "$keep" ] || break
  keepset[$r]=1; n=$((n + 1))
done
trash() {
  mv "$1" "$cache/trash/$(basename "$1").$now.$RANDOM" 2>/dev/null || rm -rf "$1"
}
for d in "$cache/packages/mathlib"/*/; do
  [ -d "$d" ] || continue
  r=$(basename "$d")
  [ -n "${keepset[$r]:-}" ] && continue
  echo "pruning mathlib@$r"
  rm -f "$cache/packages/mathlib/$r.toolchain" "$cache/warmed/$r"
  trash "$cache/packages/mathlib/$r"
  trash "$cache/mathlib-cache/$r"
done
# Then any package, and any toolchain, that no kept Mathlib uses.
declare -A used=() usedtc=()
for r in "${!keepset[@]}"; do
  d=$cache/packages/mathlib/$r
  [ -f "$d/lake-manifest.json" ] || continue
  while IFS=$'\t' read -r name prev; do used["$name/$prev"]=1; done \
    < <(jq -r '.packages[]? | [.name, .rev] | @tsv' "$d/lake-manifest.json")
  tc=$(cat "$d.toolchain" 2>/dev/null) && usedtc[$(printf '%s' "$tc" | sed -e 's|:|---|g' -e 's|/|--|g')]=1
done
for d in "$cache/packages"/*/*/; do
  [ -d "$d" ] || continue
  key=${d#"$cache/packages/"}; key=${key%/}
  case "$key" in mathlib/*) continue ;; esac
  if [ -z "${used[$key]:-}" ]; then
    echo "pruning $key"
    rm -f "$cache/packages/$key.toolchain"
    trash "$cache/packages/$key"
  fi
done
for d in "$ELAN_HOME/toolchains"/*/; do
  [ -d "$d" ] || continue
  t=$(basename "$d")
  [ -n "${usedtc[$t]:-}" ] || { echo "pruning toolchain $t"; trash "$ELAN_HOME/toolchains/$t"; }
done
for d in "$cache/trash"/*; do
  [ -e "$d" ] || continue
  ts=$(basename "$d" | awk -F. '{print $(NF-1)}')
  if [[ $ts =~ ^[0-9]+$ ]] && [ $((now - ts)) -ge "$grace" ]; then rm -rf "$d"; fi
done

du -sh "$cache/packages" "$ELAN_HOME" "$cache/mathlib-cache" "$cache/trash" 2>/dev/null || true
exit $status
