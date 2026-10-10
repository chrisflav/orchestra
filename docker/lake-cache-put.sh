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
# own builds come from the Lean cache warmer, which uploads each new upstream master.
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

url=$(git remote get-url upstream 2>/dev/null || git remote get-url origin 2>/dev/null) || exit 0
url=${url%.git}
case "$url" in
  https://github.com/*) repo=${url#https://github.com/} ;;
  git@github.com:*) repo=${url#git@github.com:} ;;
  *) echo "lake-cache-put: $url is not a GitHub repository; nothing to file it under"; exit 0 ;;
esac

# The modules this checkout has built -- one `.trace` each under .lake/build/lib/lean -- that it
# still has the source of. Naming exactly those makes the build below a replay of what the task
# did: the default targets instead could mean compiling the rest of the project first.
mapfile -t mods < <(find .lake/build/lib/lean -name '*.trace' 2>/dev/null \
  | sed -e 's|^\.lake/build/lib/lean/||' -e 's|\.trace$||' \
  | while read -r m; do [ -f "$m.lean" ] && printf '%s\n' "${m//\//.}"; done)
if [ ${#mods[@]} -eq 0 ]; then
  echo "lake-cache-put: nothing built here"
  exit 0
fi

map=$(mktemp /tmp/lake-cache-map.XXXXXX.jsonl)
log=$(mktemp /tmp/lake-cache-put.XXXXXX.log)
trap 'rm -f "$map"' EXIT
timeout "${LAKE_CACHE_PUT_TIMEOUT:-1800}" nice -n 10 lake build -o "$map" "${mods[@]}" >"$log" 2>&1 \
  || echo "lake-cache-put: the build did not finish cleanly; uploading what built (log: $log)"
# The shim sets the cache up in lake's environment, not this script's: the map is what tells.
if [ "$(grep -c . "$map" 2>/dev/null || echo 0)" -le 1 ]; then
  echo "lake-cache-put: nothing to upload (artifact cache off, or nothing built)"
  exit 0
fi
head=$(git rev-parse --short HEAD 2>/dev/null)
if lake cache put "$map" --repo "$repo" >>"$log" 2>&1; then
  echo "lake-cache-put: $(( $(grep -c . "$map") - 1 )) modules of $repo filed under $head"
else
  echo "lake-cache-put: upload failed (log: $log)"
fi
exit 0
