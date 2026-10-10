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
#   lake-seeded/<mathlib rev>@<tc>      what that revision's seed of Lake's artifact cache covers
#   master/<owner>/<name>/              checkouts for the master builds
#   (the last two only with a node artifact cache: see the end of this script)
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
# With $LAKE_NODE_CACHE mounted (the node's writable artifact cache, see lean-cache-shim), more
# jobs, all best effort -- a failure here costs build time, never a task:
#
#   SEEDING. Each Mathlib revision held above has its outputs, and those of exactly the packages
#   its manifest pins, copied into the node's cache. That is what lets a pod turn the cache on at
#   all: Lake caches a package's outputs on first use by writing a `.hash` beside each (and making
#   it read-only), which fails for one linked read-only. The seed does that first use here, with
#   `lake build --no-build` -- so it can only ever *cache* outputs that are up to date, never
#   rebuild one -- and not in the cached trees themselves but in a scratch copy of them made of
#   hard links, in which every file Lake writes in place (`.hash`, `.trace`, locks) is a copy of
#   its own: no file of the trees pods have open is written to, not even the identical `.hash` a
#   first seed rewrites. (Their metadata is: caching an output makes it read-only, r--r--r--, and
#   the scratch copy shares its inode with the cached tree's file -- which changes nothing a reader
#   of it sees.) A seed that would have had to write a trace -- a tree not up to date as built --
#   fails. What it covered is listed in $LEAN_CACHE_DIR/lake-seeded/<rev>@<tc> (the artifacts and
#   mappings in the node's cache named by the trees' hashes), and recorded for the shim in
#   $LAKE_NODE_CACHE/.seeded/<rev>@<tc> -- that marker last, once everything is there.
#
#   VALIDATION. Lake writes into the node's cache without renaming into place: an artifact it
#   copies across volumes (`writeBinFileIfNew`), a mapping (`writeFile`), a revision's mappings
#   it downloads (`revisions/<scope>/<rev>.jsonl`, never downloaded again once there). One whose
#   writer died part-way stays, and every later build trusts it. So each run checks what was
#   written since the last one (by modification time, against $LEAN_CACHE_DIR/lake-verified):
#   every artifact's content against its name with Lake's own hash (docker/lake-verify.lean, run
#   with the Lean of each held seed's toolchain that passes one of its seed's artifacts, so the
#   hash is exactly Lake's; damaged only if all of them say so), every mapping and `.jsonl` as
#   JSON; what fails is deleted. At most $LAKE_NODE_CACHE_VERIFY_MAX files per run, oldest first,
#   the rest next time. If more than a tenth of the artifacts checked fail, nothing is deleted:
#   that is a hash this Lean does not share with the one that wrote them, not damage.
#
#   PRUNING. Files in the node's cache unread for $LAKE_NODE_CACHE_DAYS days go. Not those a seed
#   covers: on every run, *before* pruning, each held seed's files have their access time set to
#   now, so they never age out while the revision is held; and a seed some of whose files are gone
#   anyway (another pod's `lake cache clean`, say) loses its marker and is done again. If the
#   refresh itself fails, every marker goes before anything is pruned. The access times are set
#   explicitly, so this works on a `noatime` mount too -- there everything else ages from when it
#   was written. The refresh needs the warmer to own the files (they are read-only to everyone),
#   so warmer and pods must run as the same user -- the image's, uid 1001.
#
#   MASTER BUILDS. With $LAKE_CACHE_MASTER_REPOS (space-separated owner/name) and Lake's system
#   configuration ($LAKE_CONFIG) and key ($LAKE_CACHE_KEY or $LAKE_CACHE_KEY_FILE): each repository's
#   default branch is fetched, and when it has moved since the last upload, built with `-o` and its
#   mappings uploaded under that commit -- the revision every fresh task starts from. A build
#   that fails still uploads what it built, but its commit is not recorded as done, and is tried
#   again no sooner than six hours later (or when the branch moves). Only once the
#   Mathlib revision it pins is seeded on this node (it is requested meanwhile): before that, the
#   build would be all of Mathlib from source. A private repository is cloned over SSH with
#   $LAKE_CACHE_DEPLOY_KEYS/<owner>_<name>, a read-only deploy key, accepting only GitHub's host
#   keys as shipped in the image. The checkouts persist in $LEAN_CACHE_DIR/master/<owner>/<name>,
#   so each build is incremental -- in the Lean cache because only the warmer can write there: what
#   it builds there runs with the deploy keys, the upload key and write access to the Lean cache,
#   so no pod may edit it. (Pods can *read* it, the source of private repositories included, as
#   they can read every output in the node's cache.)
#
# Environment:
#   LAKE_NODE_CACHE           the node's artifact cache                 (default /lake-cache)
#   LAKE_NODE_CACHE_DAYS      prune files unread for this long          (default 14)
#   LAKE_CACHE_MASTER_TIMEOUT seconds a master build may take           (default 3600)
#   LAKE_NODE_CACHE_VERIFY_MAX files validated per run                    (default 50000)
#   LAKE_VERIFY_SCRIPT        docker/lake-verify.lean in the image
#                             (default /usr/local/share/lake-verify.lean)
node=${LAKE_NODE_CACHE:-/lake-cache}
if [ -d "$node" ] && [ -w "$node" ]; then
  # What this writes into the node's cache is the pods' as much as its own: group-writable, for a
  # deployment that gives them a group rather than the same user.
  umask 002
  days=${LAKE_NODE_CACHE_DAYS:-14}
  seeded=$cache/lake-seeded
  mkdir -p "$node/.seeded" "$node/artifacts" "$node/outputs" "$seeded" "$cache/master" || exit 1
  # Where an earlier version kept the master checkouts -- writable by every pod, so never used.
  rm -rf "${node:?}/.master"

  # A scratch copy of a cached package tree at $2: hard links to every file, except the ones Lake
  # writes in place, which are copies of their own (writable, whatever the cached file's mode).
  scratch_copy() {
    local src=$1 dst=$2
    local pat=(-name '*.hash' -o -name '*.trace' -o -name '*.nobuild' -o -name '*.lock')
    cp -al "$src" "$dst"
    find "$dst" -type d -exec chmod u+w {} +   # directories are the copy's own already
    (cd "$dst" && find . -type f \( "${pat[@]}" \) -delete)
    (cd "$src" && find . -type f \( "${pat[@]}" \) -print0 | tar --null -T - -cf -) | tar -C "$dst" -xf -
    find "$dst" -type f \( "${pat[@]}" \) -exec chmod u+w {} +
  }

  # The executables a package declares, by name: `lean_exe foo` in a lakefile.lean, `[[lean_exe]]`
  # tables in a lakefile.toml.
  package_exes() {
    { sed -n -E 's/^lean_exe[[:space:]]+(«)?([A-Za-z0-9_-]+)(»)?.*/\2/p' "$1/lakefile.lean" 2>/dev/null || true
      awk '/^\[\[lean_exe\]\]/ {e=1; next} /^\[/ {e=0} e && /^name *=/ {gsub(/[" ]/, "", $0); sub(/^name=/, ""); print}' \
        "$1/lakefile.toml" 2>/dev/null || true; } | sort -u
  }

  seed() {
    local rev=$1 tc=$2 m=$cache/packages/mathlib/$1@$2 lake=$ELAN_HOME/toolchains/$2/bin/lake work rc
    work=$(mktemp -d "$cache/tmp/seed.XXXXXX") || return 1
    (
      set -e
      cd "$work"
      cp "$m/lean-toolchain" .
      printf 'name = "seed"\ndefaultTargets = ["Seed"]\n[[lean_lib]]\nname = "Seed"\n[[require]]\nname = "mathlib"\ngit = "https://github.com/leanprover-community/mathlib4"\nrev = "%s"\n' "$rev" > lakefile.toml
      echo 'import Mathlib' > Seed.lean
      # The manifest a project requiring this Mathlib would have, so Lake takes the packages as
      # they are rather than updating them.
      jq --arg rev "$rev" '{version, packagesDir: ".lake/packages", name: "seed", lakeDir: ".lake",
          packages: ([{url: "https://github.com/leanprover-community/mathlib4", type: "git", subDir: null,
                       scope: "", rev: $rev, name: "mathlib", manifestFile: "lake-manifest.json",
                       inputRev: $rev, inherited: false, configFile: "lakefile.lean"}]
                     + [.packages[] | .inherited = true])}' "$m/lake-manifest.json" > lake-manifest.json
      pkgs=$(jq -r '.packages[] | [.name, .rev] | @tsv' "$m/lake-manifest.json")
      mkdir -p .lake/packages
      scratch_copy "$m" .lake/packages/mathlib
      while IFS=$'\t' read -r n r; do
        [ -n "$n" ] || continue
        scratch_copy "$cache/packages/$n/$r@$tc" ".lake/packages/$n"
      done <<< "$pkgs"
      touch .stamp
      export LAKE_ARTIFACT_CACHE=true LAKE_CACHE_DIR=$node
      unset LAKE_CACHE_KEY
      # Every module `import Mathlib` reaches, in every package -- `+Mathlib` rather than this
      # project's default target, which is a module of its own and would need building.
      "$lake" build --no-build +Mathlib
      # The executables too: `lake exe` caches them the same way. Exit 3 is `--no-build`'s "would
      # have to build": an executable the warmer could not build, which a pod could not run from
      # the read-only tree either. Anything else is a failure of the seed.
      for p in .lake/packages/*; do
        pkg=$(basename "$p")
        for exe in $(package_exes "$p"); do
          # One the warm itself could not build (Batteries' `test`, whose sources are not in the
          # release) has no binary to cache: --no-build fails on its missing inputs before it can
          # say "would have to build", so it is recognised by that instead.
          if [ ! -e "$p/.lake/build/bin/$exe" ]; then
            echo "note: $pkg/$exe was never built; not cached" >&2
            continue
          fi
          rc=0
          "$lake" build --no-build "@$pkg/$exe:exe" || rc=$?
          case $rc in
            0) ;;
            3) echo "warning: $pkg/$exe was never built; not cached" >&2 ;;
            *) echo "caching $pkg/$exe failed (exit $rc)" >&2; exit 1 ;;
          esac
        done
      done
      # Lake writes a trace only for an output it fetched or built anew. In a pod that would be a
      # write into the read-only tree; here it means the cached tree is not what it should be.
      written=$(find .lake/packages -type f \( -name '*.trace' -o -name '*.nobuild' \) -newer .stamp -print -quit)
      if [ -n "$written" ]; then
        echo "the cached packages are not up to date as built (Lake rewrote ${written#.lake/packages/})" >&2
        exit 1
      fi
      # What the seed covers: every artifact and mapping in the node's cache that the trees name
      # -- a `.hash` holds an output's content hash, which is its artifact's name up to the
      # extension, and a trace holds its outputs' hashes and its input hash, which names its
      # mapping. Hex runs of 16 are taken wherever they appear; one that names nothing costs nothing.
      find .lake/packages -path '*/.lake/build/*' -type f \( -name '*.hash' -o -name '*.trace' \) -print0 \
        | xargs -0 cat | grep -oE '[0-9a-f]{16}' | sort -u > .tokens
      (cd "$node" && find artifacts outputs -type f ! -name '*.tmp.*') \
        | awk -F/ 'FILENAME == ARGV[1] { want[$0] = 1; next } substr($NF, 1, 16) in want' .tokens - \
        > "$seeded/$rev@$tc.new"
      [ -s "$seeded/$rev@$tc.new" ]
      mv -f "$seeded/$rev@$tc.new" "$seeded/$rev@$tc"
    )
    rc=$?
    rm -rf "$work" "$seeded/$rev@$tc.new"
    [ $rc -eq 0 ] && date +%s > "$node/.seeded/$rev@$tc"
    return $rc
  }

  # Set the access time of every file the listed seeds cover to now, and drop the marker of a seed
  # some of whose files are gone. Fails if a refresh did.
  refresh_seeds() {
    local have list missing rc=0
    have=$(mktemp "$cache/tmp/have.XXXXXX") || return 1
    (cd "$node" && find artifacts outputs -type f ! -name '*.tmp.*' 2>/dev/null) > "$have"
    for list in "$@"; do
      missing=$(awk 'FILENAME == ARGV[1] { have[$0] = 1; next } !($0 in have) { n++ } END { print n + 0 }' "$have" "$list")
      if [ "$missing" -gt 0 ]; then
        echo "$missing files of the seed of mathlib@$(basename "$list") are gone from $node; seeding it again"
        rm -f "$node/.seeded/$(basename "$list")"
      fi
    done
    sort -u "$@" | awk 'FILENAME == ARGV[1] { have[$0] = 1; next } $0 in have' "$have" - \
      | (cd "$node" && xargs -r -d '\n' touch -a -c --) || rc=1
    rm -f "$have"
    return $rc
  }

  # VALIDATION (see above): what was written into the node's cache since the last pass, oldest
  # first and at most $LAKE_NODE_CACHE_VERIFY_MAX files. Not the last ten minutes' -- a writer may
  # still be at it -- which the next pass picks up instead.
  validate_node() {
    local stamp=$cache/lake-verified max=${LAKE_NODE_CACHE_VERIFY_MAX:-50000}
    local script=${LAKE_VERIFY_SCRIPT:-/usr/local/share/lake-verify.lean}
    local all todo bad probe d tc l nart nbad f rel deleted=0 leans=() newer=()
    all=$(mktemp "$cache/tmp/verify.XXXXXX") && todo=$(mktemp "$cache/tmp/verify.XXXXXX") \
      && bad=$(mktemp "$cache/tmp/verify.XXXXXX") || return 1
    touch -d '10 minutes ago' "$stamp.next"
    [ -e "$stamp" ] && newer=(-newer "$stamp")
    (cd "$node" && find artifacts outputs revisions -type f ! -name '*.tmp.*' ! -newer "$stamp.next" \
       "${newer[@]}" -printf '%T@ %p\n' 2>/dev/null) \
      | sort -n > "$all"
    # The first $max, and any more with the same time as the last of those: the stamp is set to
    # that time, and only what is newer is looked at next.
    awk -v max="$max" 'NR <= max { b = $1; print; next } $1 == b { print; next } { exit }' "$all" > "$todo"
    if [ "$(wc -l < "$todo")" -lt "$(wc -l < "$all")" ]; then
      touch -d "@$(tail -n 1 "$todo" | cut -d' ' -f1)" "$stamp.next"
    fi
    # Artifacts, by Lake's hash: with the Lean of each held seed's toolchain that passes an
    # artifact its own seed made (one that does not hashes differently from the Lake in use), and
    # damaged only by the verdict of all of them -- a toolchain's Lake must not judge another's.
    for d in "$cache/packages/mathlib"/*@*/; do
      [ -d "$d" ] || continue
      tc=${d%/}; tc=${tc##*@}
      l=$ELAN_HOME/toolchains/$tc/bin/lean
      [ -x "$l" ] && [ -r "$script" ] || continue
      case " ${leans[*]} " in *" $l "*) continue ;; esac
      probe=$(grep -m 1 '^artifacts/' "$seeded/$(basename "$d")" 2>/dev/null)
      [ -n "$probe" ] && [ -e "$node/$probe" ] || continue
      if [ -z "$(printf '%s\n' "$node/$probe" | "$l" --run "$script" 2>/dev/null)" ]; then leans+=("$l"); fi
    done
    nart=$(grep -c ' artifacts/' "$todo")
    if [ ${#leans[@]} -gt 0 ] && [ "$nart" -gt 0 ]; then
      sed -n 's|^[^ ]* \(artifacts/.*\)$|\1|p' "$todo" | sed "s|^|$node/|" > "$bad"
      for l in "${leans[@]}"; do
        # A Lean that fails to run passes judgement on nothing: then nothing is deleted.
        "$l" --run "$script" < "$bad" > "$bad.next" 2>/dev/null || : > "$bad.next"
        mv -f "$bad.next" "$bad"
      done
      nbad=$(grep -c . "$bad")
      if [ "$nbad" -gt 10 ] && [ $((nbad * 10)) -gt "$nart" ]; then
        echo "$nbad of $nart artifacts in $node do not match their names by the cached toolchains' hash; deleting none" >&2
        : > "$bad"
      fi
    elif [ "$nart" -gt 0 ]; then
      echo "no seeded toolchain to check $nart new artifacts in $node with; only empty ones go" >&2
      sed -n 's|^[^ ]* \(artifacts/.*\)$|\1|p' "$todo" | while IFS= read -r rel; do
        [ -s "$node/$rel" ] || printf '%s\n' "$node/$rel"
      done > "$bad"
    fi
    # Mappings and revisions' mappings: JSON (Lines) that parses to the end.
    sed -n 's#^[^ ]* \(\(outputs\|revisions\)/.*\)$#\1#p' "$todo" | while IFS= read -r rel; do
      f=$node/$rel
      [ -e "$f" ] || continue
      if [ ! -s "$f" ] || ! jq empty "$f" >/dev/null 2>&1; then printf '%s\n' "$f"; fi
    done >> "$bad"
    while IFS= read -r f; do
      [ -n "$f" ] || continue
      echo "deleting damaged ${f#"$node/"}"
      rm -f "$f" && deleted=$((deleted + 1))
    done < "$bad"
    [ "$deleted" -gt 0 ] && echo "deleted $deleted damaged files from $node"
    mv -f "$stamp.next" "$stamp"
    rm -f "$all" "$todo" "$bad"
  }
  validate_node || echo "could not validate $node" >&2

  # Which seeds hold: a marker with a list beside it. Markers from before lists were kept, and
  # markers and lists of revisions no longer held, go.
  lists=()
  for d in "$cache/packages/mathlib"/*@*/; do
    [ -d "$d" ] || continue
    entry=$(basename "$d")
    if [ -e "$node/.seeded/$entry" ] && [ -s "$seeded/$entry" ]; then lists+=("$seeded/$entry")
    else rm -f "$node/.seeded/$entry"; fi
  done
  for f in "$node/.seeded"/* "$seeded"/*; do
    [ -e "$f" ] || continue
    [ -d "$cache/packages/mathlib/$(basename "$f")" ] || rm -f "$f"
  done
  if [ ${#lists[@]} -gt 0 ]; then
    refresh_seeds "${lists[@]}"
    if [ $? -ne 0 ]; then
      echo "could not refresh the seeds' files in $node; dropping every seed before pruning" >&2
      rm -f "$node/.seeded"/*
      status=1
    fi
  fi

  # (Not with -prune: -delete implies -depth, under which -prune does nothing.)
  pruned=$(find "$node" -mindepth 1 -type f ! -path "$node/.seeded/*" -atime +"$days" -print -delete 2>/dev/null | wc -l)
  [ "$pruned" -gt 0 ] && echo "pruned $pruned files from $node unread for $days days"
  # Downloads lake-curl did not get to finish (their pod died first).
  find "$node/artifacts" -type f -name '*.tmp.*' -mmin +120 -delete 2>/dev/null

  for d in "$cache/packages/mathlib"/*@*/; do
    [ -d "$d" ] || continue
    entry=$(basename "$d"); r=${entry%%@*}; tc=${entry#*@}
    [ -e "$node/.seeded/$entry" ] && continue
    echo "seeding $node with mathlib@$r"
    seed "$r" "$tc"
    rc=$?
    if [ $rc -ne 0 ]; then echo "seeding mathlib@$r failed (exit $rc)" >&2; status=1; fi
  done

  # One repository's default branch, built in its checkout at $2 from $3 and uploaded. Any step
  # failing fails it, and only a completed upload is recorded.
  master_build() {
    local repo=$1 dir=$2 url=$3
    (
      set -e
      map=""
      trap 'rm -f "$map"' EXIT
      mkdir -p "$(dirname "$dir")"
      if [ ! -d "$dir/.git" ]; then rm -rf "$dir"; git clone -q "$url" "$dir"; fi
      cd "$dir"
      git remote set-url origin "$url"
      git fetch -q --prune origin
      git remote set-head origin --auto >/dev/null   # a renamed default branch
      branch=$(git symbolic-ref --short refs/remotes/origin/HEAD)
      # Whatever the last build left in the tree -- a tracked file it changed, an untracked one it
      # wrote -- must neither keep the checkout from moving nor end up in this build. `.lake` stays:
      # it is what makes the build incremental.
      git checkout -q -f --detach "$branch"
      git clean -q -ffdx -e /.lake
      head=$(git rev-parse HEAD)
      if [ "$(cat .lake/lake-cache-put 2>/dev/null)" = "$head" ]; then echo "have $repo@${head:0:9}"; exit 0; fi
      # A commit whose build failed is built again no sooner than six hours later: a broken master
      # would otherwise cost a full build on every run.
      if failed=$(cat .lake/lake-cache-failed 2>/dev/null) && [[ $failed =~ ^[0-9a-f]+\ [0-9]+$ ]] \
         && [ "${failed%% *}" = "$head" ] && [ $((now - ${failed#* })) -lt 21600 ]; then
        echo "not building $repo@${head:0:9} again yet: its build failed less than six hours ago"
        exit 0
      fi
      toolchain=$(tr -d '[:space:]' < lean-toolchain)
      tc=$(elan_dir "$toolchain")
      mrev=$(jq -r '[.packages[]? | select(.name == "mathlib") | .rev][0] // empty' lake-manifest.json)
      if [ -n "$mrev" ] && [ ! -e "$node/.seeded/$mrev@$tc" ]; then
        echo "not building $repo@${head:0:9} yet: the mathlib@${mrev:0:9} it pins is not seeded here for $toolchain"
        if valid_rev "$mrev" && [ -d "$requests" ] && [ -w "$requests" ]; then : > "$requests/$mrev"; fi
        exit 0
      fi
      # A package the Lean cache holds is the shim's to link, and it links only into an empty
      # place: a real directory left by an earlier build -- one from before that revision was
      # cached -- would stay, and be built from source, for good.
      pkgdir=$(jq -r '.packagesDir // ".lake/packages"' lake-manifest.json)
      pkgs=$(jq -r '.packages[]? | select(.type == "git") | [.name, .rev] | @tsv' lake-manifest.json)
      while IFS=$'\t' read -r n r; do
        [ -n "$n" ] || continue
        p=$pkgdir/$n
        if [ -d "$p" ] && [ ! -L "$p" ] && [ -d "$cache/packages/$n/$r@$tc" ]; then rm -rf "$p"; fi
      done <<< "$pkgs"
      # Through the shim, which links the cached packages and decides whether the artifact cache
      # can be on; without it there is nothing to upload.
      on=$(lake env printenv LAKE_ARTIFACT_CACHE | tail -n 1)
      if [ "$on" != true ]; then
        echo "$repo@${head:0:9}: Lake's artifact cache is off in its checkout (a package linked from the Lean cache that no seed covers?)" >&2
        exit 1
      fi
      echo "building $repo@${head:0:9}"
      map=$(mktemp "$cache/tmp/master-map.XXXXXX")
      # A build that fails still lists what it built (Lake writes `-o` before it reports the
      # failure: Lake/Build/Run.lean), and that is uploaded; only the commit is not recorded as done.
      build_rc=0
      timeout "${LAKE_CACHE_MASTER_TIMEOUT:-3600}" nice -n 10 lake build -o "$map" || build_rc=$?
      n=$(grep -c . "$map" || true)
      if [ "${n:-0}" -gt 1 ]; then
        lake cache put "$map" --repo "$repo"
        echo "uploaded $((n - 1)) mapping entries of $repo@${head:0:9}"
      else
        echo "$repo@${head:0:9} has no modules to upload"
      fi
      mkdir -p .lake
      if [ "$build_rc" -ne 0 ]; then
        echo "$head $(date +%s)" > .lake/lake-cache-failed
        echo "the build of $repo@${head:0:9} failed (exit $build_rc); trying it again in six hours" >&2
        exit "$build_rc"
      fi
      rm -f .lake/lake-cache-failed
      echo "$head" > .lake/lake-cache-put
    )
  }

  configured=" "
  if [ -n "${LAKE_CACHE_MASTER_REPOS:-}" ] && [ -n "${LAKE_CONFIG:-}" ]; then
    keyfile=${LAKE_CACHE_KEY_FILE:-/etc/lake-key/LAKE_CACHE_KEY}
    if [ -z "${LAKE_CACHE_KEY:-}" ] && [ -r "$keyfile" ]; then LAKE_CACHE_KEY=$(cat "$keyfile"); export LAKE_CACHE_KEY; fi
    for repo in $LAKE_CACHE_MASTER_REPOS; do
      if ! [[ $repo =~ ^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$ ]] || [[ /$repo/ == */./* || /$repo/ == */../* ]]; then
        echo "ignoring repository '$repo'" >&2; continue
      fi
      configured+="$repo "
      dkey=${LAKE_CACHE_DEPLOY_KEYS:-/etc/lake-deploy}/${repo/\//_}
      own=""
      if [ -r "$dkey" ]; then
        url=git@github.com:$repo.git
        # A copy only this process can read: ssh refuses a key its group can, which is how a
        # Secret volume hands it to a non-root container. GitHub's host keys are the image's,
        # and no other host key is accepted.
        if ! { own=$(mktemp) && cat "$dkey" > "$own" && chmod 600 "$own"; }; then
          echo "could not copy the deploy key for $repo" >&2; [ -n "$own" ] && rm -f "$own"; status=1; continue
        fi
        export GIT_SSH_COMMAND="ssh -i $own -o IdentitiesOnly=yes -o StrictHostKeyChecking=yes -o UserKnownHostsFile=${LAKE_CACHE_KNOWN_HOSTS:-/etc/ssh/github_known_hosts} -o GlobalKnownHostsFile=/dev/null"
      else
        url=https://github.com/$repo.git
        unset GIT_SSH_COMMAND
      fi
      master_build "$repo" "$cache/master/$repo" "$url"
      rc=$?
      [ -n "$own" ] && rm -f "$own"
      unset GIT_SSH_COMMAND
      if [ $rc -ne 0 ]; then echo "master build of $repo failed (exit $rc)" >&2; status=1; fi
    done
    # Checkouts of repositories no longer configured -- only here, with a list to go by: a run
    # without one (no LAKE_CACHE_MASTER_REPOS, no LAKE_CONFIG) is not a reason to drop them all.
    if [ "$configured" != " " ]; then
      for d in "$cache/master"/*/*/; do
        [ -d "$d" ] || continue
        r=${d#"$cache/master/"}; r=${r%/}
        case "$configured" in *" $r "*) ;; *) echo "removing the master checkout of $r"; rm -rf "$d" ;; esac
      done
      find "$cache/master" -mindepth 1 -maxdepth 1 -type d -empty -delete 2>/dev/null
    fi
  fi
  du -sh "$node" 2>/dev/null || true
fi
exit $status
