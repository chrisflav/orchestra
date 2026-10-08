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
# LAYOUT of the cache, as the warmer writes it:
#
#   $LEAN_CACHE_DIR/elan/toolchains/<elan toolchain dir>      e.g. leanprover--lean4---v4.33.1
#   $LEAN_CACHE_DIR/packages/<package name>/<git rev>/        a built checkout, as Lake leaves it
#   $LEAN_CACHE_REQUESTS/<mathlib rev>   (default /lean-cache-requests, optional, writable)
#                                        revisions to warm next; lean-cache-warm reads them
#
# RULES, so that a cache can only ever save time and never change a result:
#
#   - Only a package whose manifest rev matches exactly is linked, keyed by the full git rev, so
#     what is linked is byte-for-byte what Lake would have checked out.
#   - Nothing that already exists is touched: a real directory a task created is left alone.
#   - A link whose rev no longer matches the manifest is removed, and Lake clones as usual.
#   - `lake update` removes every cache link first, because it rewrites packages in place and the
#     cache is read-only.
#   - With no cache mounted, or LEAN_CACHE_DISABLE set, this is just `exec elan`.
#
# Failures here are reported and ignored: the fallback is what would have happened anyway.

set -u

name=$(basename "$0")
cache=${LEAN_CACHE_DIR:-/lean-cache}
requests=${LEAN_CACHE_REQUESTS:-/lean-cache-requests}

lean_cache_link() {
  local dir root="" toolchain tcdir manifest pkgdir

  # The project is the nearest ancestor holding a lean-toolchain, which is also what elan uses.
  dir=$PWD
  while [ "$dir" != / ]; do
    if [ -f "$dir/lean-toolchain" ]; then root=$dir; break; fi
    dir=$(dirname "$dir")
  done
  [ -n "$root" ] || return 0

  # elan's directory name for a toolchain: "leanprover/lean4:v4.33.1" -> "leanprover--lean4---v4.33.1".
  toolchain=$(tr -d '[:space:]' < "$root/lean-toolchain")
  tcdir=$(printf '%s' "$toolchain" | sed -e 's|:|---|g' -e 's|/|--|g')
  local elan_home=${ELAN_HOME:-$HOME/.elan}
  if [ -n "$tcdir" ] && [ -d "$cache/elan/toolchains/$tcdir" ] \
     && [ ! -e "$elan_home/toolchains/$tcdir" ]; then
    mkdir -p "$elan_home/toolchains"
    ln -sfn "$cache/elan/toolchains/$tcdir" "$elan_home/toolchains/$tcdir"
  fi

  manifest=$root/lake-manifest.json
  [ -f "$manifest" ] || return 0
  pkgdir=$root/$(jq -r '.packagesDir // ".lake/packages"' "$manifest")

  if [ "$name" = lake ] && [ "${1:-}" = update ]; then
    find "$pkgdir" -maxdepth 1 -type l -lname "$cache/*" -delete 2>/dev/null
    return 0
  fi

  local pkg rev target src
  while IFS=$'\t' read -r pkg rev; do
    [ -n "$pkg" ] && [ -n "$rev" ] || continue
    target=$pkgdir/$pkg
    src=$cache/packages/$pkg/$rev
    if [ -L "$target" ] && [ "$(readlink "$target")" != "$src" ]; then
      case "$(readlink "$target")" in "$cache"/*) rm -f "$target" ;; esac
    fi
    if [ ! -e "$target" ] && [ ! -L "$target" ]; then
      if [ -d "$src" ]; then
        mkdir -p "$pkgdir"
        ln -s "$src" "$target"
      elif [ "$pkg" = mathlib ] && [ -d "$requests" ] \
           && [ -w "$requests" ] && [[ $rev =~ ^[0-9a-f]{40}$ ]]; then
        # Not cached yet: ask the warmer for it. Only a revision is passed -- the warmer fetches it
        # from Mathlib's own repository -- so a request cannot put anything into the cache.
        : > "$requests/$rev"
      fi
    fi
  done < <(jq -r '.packages[] | select(.type == "git") | [.name, .rev] | @tsv' "$manifest")

  # Mathlib's `cache get` keeps its downloaded archives in MATHLIB_CACHE_DIR and fetches any that
  # are missing there, whether or not the build they unpack to already exists -- so with a fresh
  # $HOME it re-downloads the lot on every task. The warmer keeps them in the cache, and pointing
  # at it turns `cache get` into a no-op. Only when Mathlib itself came from the cache: for any
  # other revision `cache get` has real work to do, and the read-only mount would refuse it.
  if [ -L "$pkgdir/mathlib" ] && [ "$(readlink "$pkgdir/mathlib")" = "$cache/packages/mathlib/$(jq -r '.packages[] | select(.name == "mathlib") | .rev' "$manifest")" ] \
     && [ -d "$cache/mathlib-cache" ] && [ -z "${MATHLIB_CACHE_DIR:-}" ]; then
    export MATHLIB_CACHE_DIR=$cache/mathlib-cache
  fi
}

if [ -d "$cache" ] && [ -z "${LEAN_CACHE_DISABLE:-}" ] && command -v jq >/dev/null; then
  lean_cache_link "$@" || echo "lean-cache: linking failed; continuing without the cache" >&2
fi

exec -a "$name" /usr/local/bin/elan "$@"
