#!/usr/bin/env bash
# Upload a checkout's build to the cluster's Lake cache bucket, filed under its HEAD.
#
#     lake-cache-put [DIR]
#
# Run when a task ends (orchestra's `task_volumes.finish_command`), in the task's checkout. It
# rebuilds, with `-o`, exactly the modules the task built -- a replay, since they are up to date --
# which lists each with the outputs it has in the artifact cache, and hands that list to `lake cache
# put`. A module whose source changed after its last build is built again here; one that fails to
# build is simply not listed. Nothing the task did not build is compiled.
#
# Filed under HEAD, so it helps whatever later runs on that commit or a descendant: a continuation
# that starts fresh, a review fix on the same branch, another task picking the branch up. Master's
# own builds come from the Lean cache warmer, which uploads each new upstream master -- and so this
# uploads nothing for a HEAD that is on upstream's default branch already (equal to its tip or
# behind it). `lake cache put` *replaces* the mappings filed under a revision, and Lake's `cache get`
# stops at the first revision that has any: a task's few modules filed over the warmer's complete
# list would cost every later task on master most of its cache.
#
# Nor for a checkout with uncommitted changes to its Lean sources or its Lake configuration: what
# was built from those does not belong under HEAD. Untracked files do not count: a scratch file
# the task left lying around is no part of any module the project imports.
#
# Goes through `lake`, i.e. lean-cache-shim, which turns the artifact cache on only where it can
# (see there); where it is off there is nothing to upload and this says so. Never fails: a task's
# result does not depend on its build being shared.
#
# Environment:
#   LAKE_CACHE_PUT_TIMEOUT   seconds the build may take (default 1800)
set -uo pipefail

dir=${1:-$PWD}
cd "$dir" 2>/dev/null || exit 0
[ -f lean-toolchain ] && [ -f lake-manifest.json ] || exit 0

# Where the task's work is filed: the upstream if there is one (a fork's master builds are filed
# under the repository it is a fork of), else origin. The shim fetches from the same place.
remote=origin
git remote get-url upstream >/dev/null 2>&1 && remote=upstream
url=$(git remote get-url "$remote" 2>/dev/null) || exit 0
url=${url%.git}
case "$url" in
  https://github.com/*) repo=${url#https://github.com/} ;;
  git@github.com:*) repo=${url#git@github.com:} ;;
  *) echo "lake-cache-put: $url is not a GitHub repository; nothing to file it under"; exit 0 ;;
esac
head=$(git rev-parse HEAD 2>/dev/null) || exit 0

if [ -n "$(git status --porcelain -uno -- '*.lean' 'lakefile.*' lake-manifest.json lean-toolchain 2>/dev/null)" ]; then
  echo "lake-cache-put: uncommitted changes to Lean sources or Lake's configuration; not filing them under ${head:0:9}"
  exit 0
fi

# Upstream's default branch: what the remote says HEAD is, as recorded locally or else asked; a
# main or master branch failing both.
ref=$(git symbolic-ref -q --short "refs/remotes/$remote/HEAD" 2>/dev/null)
if [ -z "$ref" ]; then
  b=$(timeout 60 git ls-remote --symref "$remote" HEAD 2>/dev/null | sed -n 's|^ref: refs/heads/\(.*\)[[:space:]]HEAD$|\1|p')
  [ -n "$b" ] && ref=$remote/$b
fi
if [ -z "$ref" ]; then
  for b in main master; do
    git rev-parse -q --verify "refs/remotes/$remote/$b" >/dev/null && { ref=$remote/$b; break; }
  done
fi
if [ -n "$ref" ]; then
  # A branch only moves forward, so being behind a stale tip is being behind the current one; only
  # a "no" is worth asking the remote again about.
  if ! git merge-base --is-ancestor HEAD "$ref" 2>/dev/null; then
    timeout 120 git fetch -q "$remote" "+refs/heads/${ref#"$remote"/}:refs/remotes/$ref" 2>/dev/null || true
  fi
  if git merge-base --is-ancestor HEAD "$ref" 2>/dev/null; then
    echo "lake-cache-put: ${head:0:9} is on $ref, which the warmer uploads in full; nothing to add"
    exit 0
  fi
else
  echo "lake-cache-put: warning: could not tell $remote's default branch; uploading anyway"
fi

# Whether Lake's artifact cache is on here at all, as Lake itself sees it: the shim decides that in
# lake's environment, not this script's.
mapfile -t env < <(timeout 300 lake env printenv LAKE_ARTIFACT_CACHE LAKE_CACHE_DIR 2>/dev/null | tail -n 2)
on=${env[0]:-} cachedir=${env[1]:-}
if [ "$on" != true ]; then
  echo "lake-cache-put: Lake's artifact cache is off in this checkout (LAKE_ARTIFACT_CACHE=${on:-unset}); nothing to upload"
  exit 0
fi

# The modules this checkout has built -- one `.trace` each under .lake/build/lib/lean -- that it
# still has the source of, named `+Module.Name`: a bare name is resolved as a package or library
# before a module, so `Foo` could mean all of library Foo. Naming exactly those modules makes the
# build below a replay of what the task did: the default targets instead could mean compiling the
# rest of the project first.
#
# A library's sources need not be at the top (`srcDir`), so a module A.B is taken to have a source
# when some `<dir>/A/B.lean` exists outside .lake -- any <dir>, including none.
mapfile -t mods < <(
  awk 'NR == FNR { p = $0; have[p] = 1; while ((i = index(p, "/")) > 0) { p = substr(p, i + 1); have[p] = 1 }; next }
       ($0 ".lean") in have { m = $0; gsub("/", ".", m); print "+" m }' \
    <(find . \( -path ./.lake -o -path ./.git \) -prune -o -type f -name '*.lean' -print 2>/dev/null | sed 's|^\./||') \
    <(find .lake/build/lib/lean -name '*.trace' 2>/dev/null | sed -e 's|^\.lake/build/lib/lean/||' -e 's|\.trace$||'))
if [ ${#mods[@]} -eq 0 ]; then
  echo "lake-cache-put: nothing built here"
  exit 0
fi

map=$(mktemp /tmp/lake-cache-map.XXXXXX.jsonl)
log=$(mktemp /tmp/lake-cache-put.XXXXXX.log)
trap 'rm -f "$map"' EXIT
# Twice at most: a module Lake does not know after all (a source that only looked like one, under
# a directory no library covers) fails the whole command before it builds anything, and is
# dropped for the second try.
for attempt in 1 2; do
  timeout "${LAKE_CACHE_PUT_TIMEOUT:-1800}" nice -n 10 lake build -o "$map" "${mods[@]}" >"$log" 2>&1 && break
  mapfile -t unknown < <(sed -n 's/.*unknown module `\([^`]*\)`.*/+\1/p' "$log" | sort -u)
  if [ "$attempt" -eq 1 ] && [ ${#unknown[@]} -gt 0 ]; then
    mapfile -t mods < <(printf '%s\n' "${mods[@]}" | grep -vxF -f <(printf '%s\n' "${unknown[@]}"))
    [ ${#mods[@]} -gt 0 ] && continue
  fi
  echo "lake-cache-put: the build did not finish cleanly; uploading what built (log: $log)"
  break
done
# The first line of the map is its header; the rest are mapping entries, one per module.
n=$(grep -c . "$map" 2>/dev/null)
if [ "${n:-0}" -le 1 ]; then
  echo "lake-cache-put: nothing to upload (no module of this checkout built)"
  exit 0
fi
# What is filed under HEAD already -- an earlier task that ended on the same commit -- is kept:
# `put` replaces the revision's mappings, so they are merged into this upload first. Fetched with
# Lake's own `cache get --rev`, which uses the very URL `put` writes to and also brings the
# outputs those mappings name into the node's cache -- `put` uploads every output its map names
# and refuses one it does not have. Its local copy of the revision's mappings is removed first:
# `cache get` would otherwise take that rather than ask the bucket again.
if [ -n "$cachedir" ] && [ -d "$cachedir" ]; then
  find "$cachedir/revisions" -name "$head.jsonl" -delete 2>/dev/null
  if timeout 600 lake cache get --rev "$head" --repo "$repo" >>"$log" 2>&1; then
    existing=$(find "$cachedir/revisions" -name "$head.jsonl" -print -quit 2>/dev/null)
    if [ -n "$existing" ]; then
      before=$n
      # Entries `[input hash, outputs]`; the first line of each file is its header.
      jq -c -n --slurpfile mine <(tail -n +2 "$map") --slurpfile theirs <(tail -n +2 "$existing") \
        '($mine | map({key: (.[0] | tostring), value: true}) | from_entries) as $have
         | $theirs[] | select($have[.[0] | tostring] | not)' >> "$map"
      n=$(grep -c . "$map" 2>/dev/null)
      echo "lake-cache-put: kept $((n - before)) mapping entries already filed under ${head:0:9}"
    fi
  elif grep -q 'outputs not found' "$log"; then
    :   # nothing filed under HEAD yet
  else
    echo "lake-cache-put: could not read what is filed under ${head:0:9} already; not replacing it (log: $log)"
    exit 0
  fi
fi
if lake cache put "$map" --repo "$repo" >>"$log" 2>&1; then
  echo "lake-cache-put: $((n - 1)) mapping entries of $repo filed under ${head:0:9}"
else
  echo "lake-cache-put: upload failed (log: $log)"
fi
exit 0
