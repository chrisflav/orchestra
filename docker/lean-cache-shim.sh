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
  # Remembered for the exec at the end: see there.
  cached_toolchain=$cache/elan/toolchains/$tcdir
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

  lake_artifact_cache "$root" "$pkgdir" "$tcdir" "$mathlib_rev" "$mathlib_linked" "$sub"
}

# Lake's artifact cache, on a node that has one: a directory shared and writable by every pod on
# the node ($LAKE_NODE_CACHE, default /lake-cache), with an S3 bucket behind it shared by every
# node (Lake's system configuration, $LAKE_CONFIG). Every module any task on the cluster has built
# is then reused by any other whose inputs match -- on any branch, in any fresh checkout -- instead
# of the per-task seed copied into every new workspace.
#
# Turned on only where it cannot break a build. Lake caches a package's outputs on first use by
# writing a `.hash` file beside each of them, which fails for a dependency linked read-only from
# the Lean cache. So the warmer copies the outputs of each Mathlib revision it holds into the node's
# cache first, and records that in $LAKE_NODE_CACHE/.seeded/<mathlib rev>@<toolchain>; with nothing
# linked from the Lean cache there is nothing read-only to trip over.
#
# Before the first build of a checkout at a given HEAD, the mappings for that revision (or the
# nearest ancestor that has some) are fetched from the bucket; the outputs themselves come when the
# build asks for them. The key for uploads ($LAKE_CACHE_KEY) is read from $LAKE_CACHE_KEY_FILE.
lake_artifact_cache() {
  local root=$1 pkgdir=$2 tcdir=$3 mathlib_rev=$4 mathlib_linked=$5 sub=$6
  local node=${LAKE_NODE_CACHE:-/lake-cache}
  [ -n "${LAKE_NODE_CACHE_DISABLE:-}" ] && return 0
  [ -d "$node" ] && [ -w "$node" ] || return 0
  if find "$pkgdir" -maxdepth 1 -type l -lname "$cache/*" 2>/dev/null | grep -q .; then
    [ -n "$mathlib_linked" ] && [ -e "$node/.seeded/$mathlib_rev@$tcdir" ] || return 0
  fi
  export LAKE_ARTIFACT_CACHE=true LAKE_CACHE_DIR=$node
  if [ -z "${LAKE_CONFIG:-}" ] && [ -f /etc/lake/config.toml ]; then export LAKE_CONFIG=/etc/lake/config.toml; fi
  local keyfile=${LAKE_CACHE_KEY_FILE:-/etc/lake-key/LAKE_CACHE_KEY}
  if [ -z "${LAKE_CACHE_KEY:-}" ] && [ -r "$keyfile" ]; then LAKE_CACHE_KEY=$(cat "$keyfile"); export LAKE_CACHE_KEY; fi

  # The remote mappings: once per checkout and HEAD, for the commands that build.
  [ "$name" = lake ] || return 0
  case "$sub" in build|exe|env|test|lint|"") ;; *) return 0 ;; esac
  [ -n "${LAKE_CONFIG:-}" ] && [ -z "${LAKE_CACHE_GET_RUNNING:-}" ] || return 0
  local head repo mark
  head=$(git -C "$root" rev-parse HEAD 2>/dev/null) || return 0
  repo=$(lake_cache_repo "$root") || return 0
  mark=$root/.lake/lake-cache-got/$head
  [ -e "$mark" ] && return 0
  mkdir -p "$(dirname "$mark")" || return 0
  # Through this script again, so the same toolchain and environment apply; the variable keeps
  # that inner run from coming back here.
  if LAKE_CACHE_GET_RUNNING=1 timeout 300 "$0" cache get --mappings-only --repo "$repo" -d "$root" \
       >"$root/.lake/lake-cache-get.log" 2>&1; then
    echo "lake-cache: mappings for $repo@${head:0:9} fetched" >&2
  else
    echo "lake-cache: no mappings for $repo@${head:0:9} or an ancestor (see .lake/lake-cache-get.log)" >&2
  fi
  : > "$mark"
}

# `owner/name` of the repository a checkout is a clone of: the upstream if the task has one, which
# is where a fork's master builds are filed, else origin.
lake_cache_repo() {
  local url
  url=$(git -C "$1" remote get-url upstream 2>/dev/null || git -C "$1" remote get-url origin 2>/dev/null) || return 1
  url=${url%.git}
  case "$url" in
    https://github.com/*) printf '%s' "${url#https://github.com/}" ;;
    git@github.com:*) printf '%s' "${url#git@github.com:}" ;;
    *) return 1 ;;
  esac
}

if [ -d "$cache" ] && [ -z "${LEAN_CACHE_DISABLE:-}" ] && command -v jq >/dev/null; then
  lean_cache_link "$@" || echo "lean-cache: linking failed; continuing without the cache" >&2
fi

# A cached toolchain is run from the cache itself, not through elan and the link in ~/.elan. The
# warmer built everything with the toolchain at this very path, and Lake's traces for native code
# record it (the compiler's include and library paths): run from anywhere else, `lake exe mk_all`
# would see a different command and try to rebuild Mathlib's executables -- into a read-only tree.
# elan's `+toolchain` argument has done its job by now (it chose `toolchain` above) and is dropped.
if [ -n "${cached_toolchain:-}" ] && [ -x "$cached_toolchain/bin/$name" ]; then
  case "${1:-}" in +?*) shift ;; esac
  exec "$cached_toolchain/bin/$name" "$@"
fi

exec -a "$name" /usr/local/bin/elan "$@"
