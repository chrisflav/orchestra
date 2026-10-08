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
# Environment:
#   LEAN_CACHE_DIR       the cache, writable here          (default /lean-cache)
#   LEAN_CACHE_REQUESTS  revisions pods have asked for     (default /lean-cache-requests)
#   LEAN_CACHE_KEEP      how many requested revisions to keep, newest first (default 4); the
#                        ones named on the command line are always kept

set -euo pipefail

cache=${LEAN_CACHE_DIR:-/lean-cache}
requests=${LEAN_CACHE_REQUESTS:-/lean-cache-requests}
keep=${LEAN_CACHE_KEEP:-4}
export ELAN_HOME=$cache/elan
export MATHLIB_CACHE_DIR=$cache/mathlib-cache
mkdir -p "$ELAN_HOME" "$MATHLIB_CACHE_DIR" "$cache/packages" "$cache/tmp" "$cache/warmed"

# A previous run that died part-way leaves its scratch directory behind; nothing else uses tmp.
rm -rf "${cache:?}/tmp/"*

warm() {
  local rev=$1 work name prev dest i
  if [ -e "$cache/packages/mathlib/$rev" ]; then
    echo "have mathlib@$rev"
    return 0
  fi
  echo "warming mathlib@$rev"
  work=$(mktemp -d "$cache/tmp/warm.XXXXXX")
  (
    cd "$work"
    curl -fsSL "https://raw.githubusercontent.com/leanprover-community/mathlib4/$rev/lean-toolchain" > lean-toolchain
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
    elan toolchain install "$(tr -d '[:space:]' < lean-toolchain)"
    # GitHub turns away anonymous clones of a repository this size now and then; it passes.
    for i in 1 2 3 4 5; do
      lake update && [ -d .lake/packages/mathlib ] && break
      echo "lake update failed (attempt $i); retrying in $((i * 30))s" >&2
      rm -rf .lake; sleep $((i * 30))
    done
    [ -d .lake/packages/mathlib ]
    lake exe cache get
    lake build
    for p in .lake/packages/*; do
      name=$(basename "$p")
      prev=$(git -C "$p" rev-parse HEAD)
      dest=$cache/packages/$name/$prev
      if [ -e "$dest" ]; then continue; fi
      chmod -R a+rX "$p"
      mkdir -p "$(dirname "$dest")"
      # A rename on one filesystem: a pod sees the whole package or none of it. Mathlib last, so
      # its presence -- which is what the shim and this script test -- implies the rest.
      [ "$name" = mathlib ] && continue
      mv "$p" "$dest"
    done
    chmod -R a+rX .lake/packages/mathlib
    mkdir -p "$cache/packages/mathlib"
    mv .lake/packages/mathlib "$cache/packages/mathlib/$rev"
  ) || { echo "warming mathlib@$rev failed" >&2; rm -rf "$work"; return 1; }
  rm -rf "$work"
  date +%s > "$cache/warmed/$rev"
  chmod -R a+rX "$ELAN_HOME" "$MATHLIB_CACHE_DIR"
}

pinned=("$@")
requested=()
if [ -d "$requests" ]; then
  for f in "$requests"/*; do
    [ -e "$f" ] || continue
    r=$(basename "$f")
    # Only a revision is accepted: it is fetched from Mathlib's repository by that name, so a
    # request decides *which* revision is warmed and nothing about what ends up in the cache.
    if [[ $r =~ ^[0-9a-f]{40}$ ]]; then requested+=("$r"); fi
    rm -f "$f"
  done
fi

status=0
for rev in "${pinned[@]}" "${requested[@]}"; do
  warm "$rev" || status=1
done

# Prune: keep the pinned revisions and the $keep most recently warmed others. A revision is
# dropped only once $keep newer ones exist, so one a running task still links is not pulled out
# from under it in practice.
keepset=" ${pinned[*]} $(ls -t "$cache/warmed" 2>/dev/null | head -n "$keep" | tr '\n' ' ') "
for d in "$cache/packages/mathlib"/*; do
  [ -e "$d" ] || continue
  r=$(basename "$d")
  case "$keepset" in *" $r "*) continue ;; esac
  echo "pruning mathlib@$r"
  rm -rf "$d" "$cache/warmed/$r"
done
# Then any package no kept Mathlib pins.
declare -A used=()
for d in "$cache/packages/mathlib"/*; do
  [ -f "$d/lake-manifest.json" ] || continue
  while IFS=$'\t' read -r name prev; do used["$name/$prev"]=1; done \
    < <(jq -r '.packages[] | [.name, .rev] | @tsv' "$d/lake-manifest.json")
done
for d in "$cache/packages"/*/*; do
  [ -e "$d" ] || continue
  key=${d#"$cache/packages/"}
  case "$key" in mathlib/*) continue ;; esac
  [ -n "${used[$key]:-}" ] || { echo "pruning $key"; rm -rf "$d"; }
done

du -sh "$cache/packages" "$ELAN_HOME" "$MATHLIB_CACHE_DIR" 2>/dev/null || true
exit $status
