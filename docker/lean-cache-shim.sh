#!/usr/bin/env bash
# Every elan proxy in this image (`lake`, `lean`, `leanc`, ...) is a symlink to this script. It
# links whatever a node-level Lean cache already holds into the places elan and Lake look, then
# hands over to elan under the name it was invoked as.
#
# WHY. Under the kubernetes backend each task is a fresh pod, so a Mathlib project starts with no
# toolchain and no `.lake/packages`: every task downloads and unpacks ~7.5 GB, and every pod on a
# node holds its own copy of the same oleans in the page cache. A cache mounted read-only at
# $LEAN_CACHE_DIR (default /lean-cache) and *symlinked* in fixes both -- one copy on disk, one in
# memory, shared by every pod on the node -- without the project or its scripts knowing.
#
# The cache's layout is lean-cache-warm's (docker/lean-cache-warm.sh), which fills it.
#
# RULES, so that a cache can only ever save time and never change a result:
#
#   - A package is linked only at exactly the rev the manifest pins, and only from the entry built
#     for the toolchain this run uses (the warmer keys every package by both) -- otherwise Lake's
#     traces would not match and it would try to rebuild into a tree it cannot write. The toolchain
#     is the one elan would pick: `+toolchain`, then ELAN_TOOLCHAIN, then lean-toolchain.
#   - Nothing real is touched: a directory a task created is left alone. Only links into the cache
#     are ever replaced or removed (a rev that no longer matches, or one the warmer pruned).
#   - `lake update` removes every cache link first, because it rewrites packages in place and the
#     cache is read-only. The packages it fetches are then the pod's own for the rest of its life.
#   - Nothing happens inside the cache or inside another package's directory.
#   - With no cache mounted, or LEAN_CACHE_DISABLE set, this is just `exec elan`.
#
# It also records which Mathlib revision the project uses in $LEAN_CACHE_REQUESTS (default
# /lean-cache-requests), cached or not: an uncached one gets warmed, a cached one counts as used
# and is kept by the warmer's pruning. Only a 40-hex revision is ever written there.
#
# Failures here are reported and ignored: the fallback is what would have happened anyway. Note
# that an `elan-init` self-install puts proxies in ~/.elan/bin, which is first on PATH and bypasses
# this; `elan run` bypasses it too.

name=$(basename "$0")
cache=${LEAN_CACHE_DIR:-/lean-cache}
requests=${LEAN_CACHE_REQUESTS:-/lean-cache-requests}

elan_dir() { printf '%s' "$1" | sed -e 's|:|---|g' -e 's|/|--|g'; }

lean_cache_link() {
  local start=$PWD sub="" arg dir root="" toolchain="${ELAN_TOOLCHAIN:-}" manifest pkgdir

  # elan's `+toolchain` override, for any proxy: `lake +leanprover/lean4:v4.33.1 build`.
  case "${1:-}" in +?*) toolchain=${1#+}; shift ;; esac

  # For `lake`: where it will run (`-d`/`--dir`), and which subcommand it is.
  if [ "$name" = lake ]; then
    while [ $# -gt 0 ]; do
      arg=$1; shift
      case "$arg" in
        --) break ;;
        -d|--dir) start=${1:-$start}; shift ;;
        --dir=*) start=${arg#--dir=} ;;
        -f|--file|-K) shift ;;
        -*) ;;
        *) sub=$arg; break ;;
      esac
    done
    case "$start" in /*) ;; *) start=$PWD/$start ;; esac
  fi

  # The project is the nearest ancestor holding a lean-toolchain, which is also what elan uses.
  dir=$start
  while [ -n "$dir" ] && [ "$dir" != / ]; do
    if [ -f "$dir/lean-toolchain" ]; then root=$dir; break; fi
    dir=$(dirname "$dir")
  done
  [ -n "$root" ] || return 0
  case "$root" in "$cache"|"$cache"/*|*/.lake/packages/*) return 0 ;; esac

  # An explicit choice (`+toolchain`, ELAN_TOOLCHAIN) wins over the file, as it does for elan.
  [ -n "$toolchain" ] || toolchain=$(tr -d '[:space:]' < "$root/lean-toolchain")
  [ -n "$toolchain" ] || return 0

  # elan's directory name for a toolchain: "leanprover/lean4:v4.33.1" -> "leanprover--lean4---v4.33.1".
  local elan_home=${ELAN_HOME:-${HOME:-}/.elan} tcdir
  tcdir=$(elan_dir "$toolchain")
  if [ "$elan_home" != /.elan ] && [ -d "$cache/elan/toolchains/$tcdir" ] \
     && [ ! -e "$elan_home/toolchains/$tcdir" ]; then
    mkdir -p "$elan_home/toolchains" && ln -sfn "$cache/elan/toolchains/$tcdir" "$elan_home/toolchains/$tcdir"
  fi

  manifest=$root/lake-manifest.json
  [ -f "$manifest" ] || return 0
  local entries
  entries=$(jq -r '(.packagesDir // ".lake/packages"),
                   (.packages[]? | select(.type == "git") | [.name, .rev] | @tsv)' "$manifest") || return 1
  pkgdir=$(head -n1 <<< "$entries")
  case "$pkgdir" in /*) ;; *) pkgdir=$root/$pkgdir ;; esac

  if [ "$name" = lake ] && [ "$sub" = update ]; then
    find "$pkgdir" -maxdepth 1 -type l -lname "$cache/*" -delete 2>/dev/null
    return 0
  fi

  local pkg rev target src link mathlib_rev="" mathlib_linked=""
  while IFS=$'\t' read -r pkg rev; do
    [ -n "$pkg" ] && [ -n "$rev" ] || continue
    target=$pkgdir/$pkg
    src=$cache/packages/$pkg/$rev@$tcdir
    [ "$pkg" = mathlib ] && mathlib_rev=$rev
    # A link of ours that points at another rev, or at one the warmer has since pruned.
    if [ -L "$target" ]; then
      link=$(readlink "$target")
      case "$link" in
        "$cache"/*) { [ "$link" != "$src" ] || [ ! -e "$target" ]; } && rm -f "$target" ;;
      esac
    fi
    if [ ! -e "$target" ] && [ ! -L "$target" ] && [ -d "$src" ]; then
      mkdir -p "$pkgdir" && ln -s "$src" "$target" 2>/dev/null
    fi
    if [ "$pkg" = mathlib ] && [ -L "$target" ] && [ "$(readlink "$target")" = "$src" ]; then
      mathlib_linked=yes
    fi
  done < <(tail -n +2 <<< "$entries")

  if [ -n "$mathlib_rev" ] && [[ $mathlib_rev =~ ^[0-9a-f]{40}$ ]] && [ -d "$requests" ] && [ -w "$requests" ]; then
    : > "$requests/$mathlib_rev"
  fi

  # Mathlib's `cache get` keeps its downloaded archives in MATHLIB_CACHE_DIR and fetches any that
  # are missing there, whether or not the build they unpack to already exists -- so with a fresh
  # $HOME it re-downloads the lot on every task. Pointing it at the warmer's archives for this
  # revision makes it a no-op. Only when Mathlib itself came from the cache: for any other
  # revision `cache get` has real work to do, and the read-only mount would refuse it.
  if [ -n "$mathlib_linked" ] && [ -d "$cache/mathlib-cache/$mathlib_rev" ] && [ -z "${MATHLIB_CACHE_DIR:-}" ]; then
    export MATHLIB_CACHE_DIR=$cache/mathlib-cache/$mathlib_rev
  fi
}

if [ -d "$cache" ] && [ -z "${LEAN_CACHE_DISABLE:-}" ] && command -v jq >/dev/null; then
  lean_cache_link "$@" || echo "lean-cache: linking failed; continuing without the cache" >&2
fi

exec -a "$name" /usr/local/bin/elan "$@"
