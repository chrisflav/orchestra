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
# LAYOUT (the shim reads exactly this; <tc> is elan's directory name for a toolchain, e.g.
# leanprover--lean4---v4.33.1):
#
#   elan/toolchains/<tc>/               toolchains
#   packages/<name>/<rev>@<tc>/         a built package checkout. Keyed by toolchain as well as rev:
#                                       packages such as Qq or aesop often keep a rev across a
#                                       toolchain bump, and a build for one toolchain is not one for
#                                       another -- Lake would rebuild it, into a tree it cannot write
#   mathlib-cache/<mathlib rev>/        `cache get`'s archives for that revision (MATHLIB_CACHE_DIR)
#   warmed/<mathlib rev>                when that revision was last warmed *or used*
#   failed/<mathlib rev>                failed attempts; three within a week and requests are ignored
#   orphaned/<tc>                       when a toolchain stopped being used by any kept Mathlib
#
# REQUESTS. A pod writes $LEAN_CACHE_REQUESTS/<mathlib rev> both when the revision is missing and
# when it linked it, so the directory doubles as a record of use: a request for a revision already
# here just refreshes `warmed/<rev>`, and pruning keeps the most recently *used* revisions. A
# request is removed once it has been honoured, so a failed warm is retried on the next run.
#
# PRUNING never removes anything a task may still be using. Pods reach the cache through symlinks
# to these exact paths, so moving a tree aside would break them as surely as deleting it; instead a
# revision becomes eligible only when it is outside the keep set *and* nobody has used it for
# $LEAN_CACHE_GRACE seconds, and is then deleted outright, with whatever only it used.
#
# Environment:
#   LEAN_CACHE_DIR       the cache, writable here          (default /lean-cache)
#   LEAN_CACHE_REQUESTS  revisions pods have asked for     (default /lean-cache-requests)
#   LEAN_CACHE_KEEP      how many unpinned revisions to keep, most recently used first (default 4);
#                        the ones named on the command line are kept on top of these
#   LEAN_CACHE_GRACE     seconds since last use before a revision outside the keep set may go
#                        (default 86400, longer than any task)
#
# Upgrading from a cache written by an earlier layout or an older warmer: empty the directory and
# let this refill it. A revision already here is not warmed again, so what a newer warmer adds (the
# executables, say) only reaches revisions it warms itself.

# No `set -e` here, deliberately: each revision's warm runs in a subshell with its own errexit, and
# errexit is silently ignored in any function or subshell called from an `if`, `&&` or `||`. So the
# calls below are plain statements and their status is read afterwards.
set -uo pipefail

cache=${LEAN_CACHE_DIR:-/lean-cache}
requests=${LEAN_CACHE_REQUESTS:-/lean-cache-requests}
keep=${LEAN_CACHE_KEEP:-4}
grace=${LEAN_CACHE_GRACE:-86400}
export ELAN_HOME=$cache/elan
mkdir -p "$ELAN_HOME" "$cache/mathlib-cache" "$cache/packages/mathlib" "$cache/tmp" \
  "$cache/warmed" "$cache/failed" || exit 1

# One writer. A second run -- an overlapping schedule, a manual `kubectl create job` -- would wipe
# this one's scratch directory below and race it on every rename.
exec 9> "$cache/.lock"
if ! flock -n 9; then
  echo "another lean-cache-warm holds $cache/.lock; leaving it to finish"
  exit 0
fi

# A previous run that died part-way leaves its scratch directory behind; nothing else uses tmp.
rm -rf "${cache:?}/tmp/"*

now=$(date +%s)
valid_rev() { [[ $1 =~ ^[0-9a-f]{40}$ ]]; }
elan_dir() { printf '%s' "$1" | sed -e 's|:|---|g' -e 's|/|--|g'; }
have_mathlib() { compgen -G "$cache/packages/mathlib/$1@*" >/dev/null; }

warm() {
  local rev=$1 work rc
  if have_mathlib "$rev"; then
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
    tc=$(elan_dir "$toolchain")
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
    # Every executable the cached packages declare -- Mathlib's `mk_all` and `cache`, Batteries'
    # `runLinter`, and so on. A project's own scripts run these with `lake exe`, which builds them,
    # native code and all, into the package's tree on first use; read-only in a pod, that fails
    # the script. Built here instead, each on its own, so one that does not build costs only itself.
    for p in .lake/packages/*; do
      pkg=$(basename "$p")
      exes=$( { sed -n -E 's/^lean_exe[[:space:]]+(«)?([A-Za-z0-9_-]+)(»)?.*/\2/p' \
                  "$p/lakefile.lean" 2>/dev/null || true
                awk '/^\[\[lean_exe\]\]/ {e=1; next} /^\[/ {e=0} e && /^name *=/ {gsub(/[" ]/, "", $0); sub(/^name=/, ""); print}' \
                  "$p/lakefile.toml" 2>/dev/null || true; } | sort -u)
      for exe in $exes; do
        # `:exe`, not the bare name: an executable whose root module has the same name (`mk_all`)
        # would otherwise resolve to the module, build its olean, and report success.
        lake build "@$pkg/$exe:exe" || echo "warning: could not build $pkg/$exe" >&2
      done
    done
    chmod -R a+rX .lake/packages "$MATHLIB_CACHE_DIR" "$ELAN_HOME"
    for p in .lake/packages/*; do
      name=$(basename "$p")
      [ "$name" = mathlib ] && continue
      dest=$cache/packages/$name/$(git -C "$p" rev-parse HEAD)@$tc
      [ -e "$dest" ] && continue
      mkdir -p "$(dirname "$dest")"
      mv "$p" "$dest"      # one rename on one filesystem: a pod sees all of it or none
    done
    # Dated before it appears, so an entry can never exist without a record of when it was used.
    date +%s > "$cache/warmed/$rev"
    # Mathlib last: its presence is what the shim and this script test for.
    mv .lake/packages/mathlib "$cache/packages/mathlib/$rev@$tc"
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
    if ! valid_rev "$r"; then rm -f "$f"; continue; fi
    # A revision that failed three times in the last week -- one that does not exist upstream, a
    # fork's commit -- is not retried on every run just because pods keep asking for it.
    fails=$(cat "$cache/failed/$r" 2>/dev/null || echo 0)
    if [ "$fails" -ge 3 ] && [ $((now - $(stat -c %Y "$cache/failed/$r"))) -lt 604800 ]; then
      rm -f "$f"; continue
    fi
    # A pinned revision is warmed anyway; its request only says it is in use, which that records.
    case " ${pinned[*]} " in *" $r "*) rm -f "$f"; date +%s > "$cache/warmed/$r" 2>/dev/null; continue ;; esac
    requested+=("$r")
  done
fi

status=0
for rev in "${pinned[@]}" "${requested[@]}"; do
  warm "$rev"
  if [ $? -eq 0 ]; then
    rm -f "$requests/$rev" "$cache/failed/$rev"
  else
    status=1
    fails=$(( $(cat "$cache/failed/$rev" 2>/dev/null || echo 0) + 1 ))
    echo "$fails" > "$cache/failed/$rev"
    [ "$fails" -ge 3 ] && { echo "ignoring mathlib@$rev for a week after $fails failures" >&2; rm -f "$requests/$rev"; }
  fi
done

# Prune. Keep: the pinned revisions, the $keep most recently used others, and anything used within
# $grace. What is left is deleted, along with packages and toolchains only it used.
declare -A keepset=()
for r in "${pinned[@]}"; do keepset[$r]=1; done
n=0
for r in $(find "$cache/warmed" -type f -printf '%T@ %f\n' 2>/dev/null | sort -rn | cut -d" " -f2); do
  [ -n "${keepset[$r]:-}" ] && continue
  if [ $n -lt "$keep" ] || [ $((now - $(stat -c %Y "$cache/warmed/$r"))) -lt "$grace" ]; then
    keepset[$r]=1; n=$((n + 1))
  fi
done
declare -A used=() usedtc=()
for d in "$cache/packages/mathlib"/*@*/; do
  [ -d "$d" ] || continue
  entry=$(basename "$d"); r=${entry%%@*}; tc=${entry#*@}
  # A revision with no warmed/ record at all is one this run cannot date; keep it.
  if [ -z "${keepset[$r]:-}" ] && [ -e "$cache/warmed/$r" ]; then
    echo "pruning mathlib@$r"
    rm -rf "$d" "$cache/mathlib-cache/$r" "$cache/warmed/$r"
    continue
  fi
  usedtc[$tc]=1
  while IFS=$'\t' read -r name prev; do used["$name/$prev@$tc"]=1; done \
    < <(jq -r '.packages[]? | [.name, .rev] | @tsv' "$d/lake-manifest.json" 2>/dev/null)
done
for d in "$cache/packages"/*/*/; do
  [ -d "$d" ] || continue
  key=${d#"$cache/packages/"}; key=${key%/}
  case "$key" in mathlib/*) continue ;; esac
  [ -n "${used[$key]:-}" ] || { echo "pruning $key"; rm -rf "$d"; }
done
# A toolchain no kept Mathlib uses may still be linked by a task on a project without Mathlib, so it
# is only marked at first and deleted once it has stayed unused for the grace period.
mkdir -p "$cache/orphaned"
for d in "$ELAN_HOME/toolchains"/*/; do
  [ -d "$d" ] || continue
  t=$(basename "$d")
  if [ -n "${usedtc[$t]:-}" ]; then rm -f "$cache/orphaned/$t"; continue; fi
  if [ ! -e "$cache/orphaned/$t" ]; then touch "$cache/orphaned/$t"; continue; fi
  if [ $((now - $(stat -c %Y "$cache/orphaned/$t"))) -ge "$grace" ]; then
    echo "pruning toolchain $t"; rm -rf "$d" "$cache/orphaned/$t"
  fi
done

du -sh "$cache/packages" "$ELAN_HOME" "$cache/mathlib-cache" 2>/dev/null || true

# ------------------------------------------------------------------ Lake's artifact cache --
#
# With $LAKE_NODE_CACHE mounted (the node's writable artifact cache, see lean-cache-shim), two more
# jobs, both best effort -- a failure here costs build time, never a task:
#
#   SEEDING. Each Mathlib revision kept above has its outputs, and those of every package it pins,
#   copied into the node's cache, by building `import Mathlib` over the cached packages with the
#   artifact cache on. That is what lets a pod turn the cache on at all: Lake caches a package's
#   outputs on first use by writing a `.hash` beside each, which fails for one linked read-only;
#   here the package trees are writable. Recorded in .seeded/<rev>@<tc>, which the shim checks.
#   Redone daily, and at once after pruning removed anything, so a pruned output comes back.
#
#   MASTER BUILDS. With $LAKE_CACHE_MASTER_REPOS (space-separated owner/name) and Lake's system
#   configuration ($LAKE_CONFIG) and key ($LAKE_CACHE_KEY or $LAKE_CACHE_KEY_FILE): each repository's
#   default branch is fetched, and when it has moved since the last upload, built with `-o` and its
#   mappings uploaded under that commit -- the revision every fresh task starts from. A private
#   repository is cloned over SSH with $LAKE_CACHE_DEPLOY_KEYS/<owner>_<name>, a read-only deploy
#   key, when there is one. The checkouts persist in $LAKE_NODE_CACHE/.master, so each build is
#   incremental.
#
# Environment:
#   LAKE_NODE_CACHE        the node's artifact cache            (default /lake-cache)
#   LAKE_NODE_CACHE_DAYS   prune outputs unread for this long   (default 14)
node=${LAKE_NODE_CACHE:-/lake-cache}
if [ -d "$node" ] && [ -w "$node" ]; then
  mkdir -p "$node/.seeded" "$node/.master"
  pruned=$(find "$node/artifacts" -type f -atime +"${LAKE_NODE_CACHE_DAYS:-14}" -print -delete 2>/dev/null | wc -l)
  [ "$pruned" -gt 0 ] && echo "pruned $pruned outputs from $node unread for ${LAKE_NODE_CACHE_DAYS:-14} days"

  seed() {
    local rev=$1 tc=$2 m=$cache/packages/mathlib/$1@$2 lake=$ELAN_HOME/toolchains/$2/bin/lake work rc
    work=$(mktemp -d "$cache/tmp/seed.XXXXXX") || return 1
    (
      set -e
      cd "$work"
      cp "$m/lean-toolchain" .
      printf 'name = "seed"\ndefaultTargets = ["Seed"]\n[[lean_lib]]\nname = "Seed"\n[[require]]\nname = "mathlib"\ngit = "https://github.com/leanprover-community/mathlib4"\nrev = "%s"\n' "$rev" > lakefile.toml
      echo 'import Mathlib' > Seed.lean
      # The manifest a project requiring this Mathlib would have, so Lake takes the linked
      # packages as they are rather than updating them.
      jq --arg rev "$rev" '{version, packagesDir: ".lake/packages", name: "seed", lakeDir: ".lake",
          packages: ([{url: "https://github.com/leanprover-community/mathlib4", type: "git", subDir: null,
                       scope: "", rev: $rev, name: "mathlib", manifestFile: "lake-manifest.json",
                       inputRev: $rev, inherited: false, configFile: "lakefile.lean"}]
                     + [.packages[] | .inherited = true])}' "$m/lake-manifest.json" > lake-manifest.json
      mkdir -p .lake/packages
      ln -s "$m" .lake/packages/mathlib
      while IFS=$'\t' read -r n r; do
        ln -s "$cache/packages/$n/$r@$tc" ".lake/packages/$n"
      done < <(jq -r '.packages[] | [.name, .rev] | @tsv' "$m/lake-manifest.json")
      export LAKE_ARTIFACT_CACHE=true LAKE_CACHE_DIR=$node
      "$lake" build
      # The executables too: `lake exe` checks them the same way, and they were built above
      # without the cache.
      for p in .lake/packages/*; do
        pkg=$(basename "$p")
        for exe in $( { sed -n -E 's/^lean_exe[[:space:]]+(«)?([A-Za-z0-9_-]+)(»)?.*/\2/p' "$p/lakefile.lean" 2>/dev/null || true
                        awk '/^\[\[lean_exe\]\]/ {e=1; next} /^\[/ {e=0} e && /^name *=/ {gsub(/[" ]/, "", $0); sub(/^name=/, ""); print}' \
                          "$p/lakefile.toml" 2>/dev/null || true; } | sort -u); do
          "$lake" build "@$pkg/$exe:exe" || echo "warning: could not cache $pkg/$exe" >&2
        done
      done
    )
    rc=$?
    rm -rf "$work"
    [ $rc -eq 0 ] && date +%s > "$node/.seeded/$rev@$tc"
    return $rc
  }

  for d in "$cache/packages/mathlib"/*@*/; do
    [ -d "$d" ] || continue
    entry=$(basename "$d"); r=${entry%%@*}; tc=${entry#*@}
    mark=$node/.seeded/$entry
    if [ "$pruned" -gt 0 ] || [ ! -e "$mark" ] || [ $((now - $(cat "$mark" 2>/dev/null || echo 0))) -ge 86400 ]; then
      echo "seeding $node with mathlib@$r"
      seed "$r" "$tc" || { echo "seeding mathlib@$r failed" >&2; rm -f "$mark"; status=1; }
    fi
  done
  # Markers of revisions no longer held: nothing to keep in step with.
  for f in "$node/.seeded"/*; do
    [ -e "$f" ] || continue
    [ -d "$cache/packages/mathlib/$(basename "$f")" ] || rm -f "$f"
  done

  if [ -n "${LAKE_CACHE_MASTER_REPOS:-}" ] && [ -n "${LAKE_CONFIG:-}" ]; then
    keyfile=${LAKE_CACHE_KEY_FILE:-/etc/lake-key/LAKE_CACHE_KEY}
    if [ -z "${LAKE_CACHE_KEY:-}" ] && [ -r "$keyfile" ]; then LAKE_CACHE_KEY=$(cat "$keyfile"); export LAKE_CACHE_KEY; fi
    for repo in $LAKE_CACHE_MASTER_REPOS; do
      [[ $repo =~ ^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$ ]] || { echo "ignoring repository '$repo'" >&2; continue; }
      dir=$node/.master/${repo/\//_}
      dkey=${LAKE_CACHE_DEPLOY_KEYS:-/etc/lake-deploy}/${repo/\//_}
      if [ -r "$dkey" ]; then
        url=git@github.com:$repo.git
        export GIT_SSH_COMMAND="ssh -i $dkey -o IdentitiesOnly=yes -o StrictHostKeyChecking=accept-new -o UserKnownHostsFile=$node/.master/known_hosts"
      else
        url=https://github.com/$repo.git
        unset GIT_SSH_COMMAND
      fi
      (
        set -e
        if [ ! -d "$dir/.git" ]; then rm -rf "$dir"; git clone -q "$url" "$dir"; fi
        cd "$dir"
        git fetch -q origin
        branch=$(git symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null || echo origin/master)
        git checkout -q --detach "$branch"
        head=$(git rev-parse HEAD)
        if [ "$(cat .lake/lake-cache-put 2>/dev/null)" = "$head" ]; then echo "have $repo@${head:0:9}"; exit 0; fi
        echo "building $repo@${head:0:9}"
        map=$(mktemp)
        # Through the shim, which links the cached packages and turns the artifact cache on.
        nice -n 10 lake build -o "$map"
        lake cache put "$map" --repo "$repo"
        mkdir -p .lake && echo "$head" > .lake/lake-cache-put
        echo "uploaded $(( $(grep -c . "$map") - 1 )) modules of $repo@${head:0:9}"
        rm -f "$map"
      ) || { echo "master build of $repo failed" >&2; status=1; }
    done
  fi
  du -sh "$node" 2>/dev/null || true
fi
exit $status
