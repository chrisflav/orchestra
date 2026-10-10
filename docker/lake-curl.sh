#!/usr/bin/env bash
# `curl` on this image's PATH: the static curl Lake needs (/usr/local/libexec/curl-real, see
# agent.Dockerfile), unchanged -- except that a download into the node's shared Lake artifact
# cache lands under a temporary name and is renamed into place only once it is complete.
#
# WHY. Lake downloads an artifact with `curl -o <its final path in LAKE_CACHE_DIR>`, and only
# afterwards checks its hash (and deletes it on a mismatch). In a cache every pod on the node
# reads, that is a window in which a half-written file sits at a name others trust -- and one
# whose download died with its pod (the deadline, an eviction) stays there for good: Lake never
# looks at an artifact's content again once the file exists. A rename is atomic, so with this
# another reader sees the whole file or none.
#
# Two shapes of download are recognised, the two Lake uses (Lake/Build/Actions.lean `download`,
# Lake/Config/Cache.lean `transferArtifacts`):
#
#   - exactly one `-o PATH` among the arguments, no `--config`: PATH becomes PATH.tmp.<pid>, and
#     on exit 0 that is renamed to PATH; on anything else it is removed.
#   - `--config FILE` whose lines include `-o "PATH"` (one per URL, the URLs in the same order),
#     the parallel `-Z` download: a copy of FILE with each PATH so renamed is passed instead. Lake
#     reads each transfer's result from the JSON curl writes to stderr (`-w '%{stderr}%{json}'`)
#     *while* curl runs and then hashes the file at PATH -- so stderr is held back until curl has
#     exited and each transfer that answered 200/201 has been renamed into place; Lake then sees
#     the same lines, in the same order, with every file where it expects it.
#
# Anything else -- an upload, a `-o` outside the cache, an option this does not recognise in a
# position that matters -- is passed to the real curl untouched, as is everything when jq (needed
# to read curl's JSON) is missing. The exit status is always curl's own.
#
# It also points the static curl at Debian's CA bundle when nothing else does: a static build has
# no system default it can rely on, and without one every HTTPS request fails.

real=${LAKE_CURL_REAL:-/usr/local/libexec/curl-real}
if [ -z "${CURL_CA_BUNDLE:-}" ] && [ -z "${SSL_CERT_FILE:-}" ] && [ -r /etc/ssl/certs/ca-certificates.crt ]; then
  export CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
fi

# Where the shared cache is: Lake's own setting, which the shim exports, else the node's mount.
shared=${LAKE_CACHE_DIR:-${LAKE_NODE_CACHE:-/lake-cache}}
shared=${shared%/}
under_shared() { case "$1" in "$shared"/*) [[ $1 != *$'\n'* ]] ;; *) return 1 ;; esac; }

args=("$@")
out_idx=() config=""
for ((i = 0; i < ${#args[@]}; i++)); do
  case "${args[i]}" in
    -o|--output) out_idx+=("$((i + 1))"); i=$((i + 1)) ;;
    -K|--config) config=${args[i+1]:-}; i=$((i + 1)) ;;
    # The values of the options Lake passes, so that none is taken for an option itself.
    -H|--header|-w|--write-out|-X|--request|-u|--user|--retry|-T|--upload-file|-d|--data|--aws-sigv4)
      i=$((i + 1)) ;;
    # Joined forms (`-oFILE`, `--output=FILE`, `--config=FILE`) and the output-naming options:
    # not what Lake writes, so not worth guessing at.
    -o?*|--output=*|--config=*|-K?*|-O|--remote-name|--remote-name-all|--output-dir|-J|--remote-header-name)
      exec "$real" "$@" ;;
  esac
done

# The single download.
if [ ${#out_idx[@]} -eq 1 ] && [ -z "$config" ]; then
  i=${out_idx[0]}
  dest=${args[i]:-}
  under_shared "$dest" || exec "$real" "$@"
  tmp=$dest.tmp.$$
  args[i]=$tmp
  "$real" "${args[@]}"
  rc=$?
  if [ $rc -eq 0 ] && [ -e "$tmp" ]; then
    mv -f "$tmp" "$dest" || { rc=$?; rm -f "$tmp"; }
  else
    rm -f "$tmp"
  fi
  exit $rc
fi

# The parallel download, from a config file.
if [ ${#out_idx[@]} -eq 0 ] && [ -n "$config" ] && [ -r "$config" ] && command -v jq >/dev/null; then
  dests=() tmpcfg="" errlog=""
  # Only a config every `-o` line of which is the plain `-o "PATH"` Lake writes, with PATH in the
  # shared cache and nothing in it that would need unquoting. Anything else goes through as is.
  ok=yes
  while IFS= read -r line || [ -n "$line" ]; do
    case "$line" in
      '-o '*|'--output '*|'output'*)
        if [[ $line =~ ^-o\ \"([^\"\\]*)\"$ ]] && under_shared "${BASH_REMATCH[1]}"; then
          dests+=("${BASH_REMATCH[1]}")
        else
          ok=""; break
        fi ;;
    esac
  done < "$config"
  if [ -n "$ok" ] && [ ${#dests[@]} -gt 0 ]; then
    tmpcfg=$(mktemp) && errlog=$(mktemp) || exec "$real" "$@"
    sed -E "s|^-o \"([^\"]*)\"\$|-o \"\\1.tmp.$$\"|" "$config" > "$tmpcfg" || { rm -f "$tmpcfg" "$errlog"; exec "$real" "$@"; }
    for ((i = 0; i < ${#args[@]}; i++)); do
      case "${args[i]}" in -K|--config) args[i+1]=$tmpcfg; break ;; esac
    done
    "$real" "${args[@]}" 2>"$errlog"
    rc=$?
    # Each transfer's status, by its index among the URLs -- which is the index of its `-o` line,
    # Lake writing one of each per artifact, in order.
    declare -A code=()
    while read -r n c; do [ -n "$n" ] && code[$n]=$c; done < <(
      jq -Rr 'fromjson? | select(type == "object") | select(.urlnum != null) | "\(.urlnum) \(.http_code // 0)"' \
        "$errlog" 2>/dev/null)
    # Only a 200/201 is renamed into place. Any other answer's body stays out of the shared cache
    # -- but Lake reads a failed download's body from the final path to say what the server
    # answered (an S3 error document), so for anything but a 404 its start is passed on instead,
    # as a line of its own after curl's: Lake reports a line that is not JSON verbatim.
    diag=()
    for i in "${!dests[@]}"; do
      t=${dests[i]}.tmp.$$
      case "${code[$i]:-}" in
        200|201) if [ -e "$t" ]; then mv -f "$t" "${dests[i]}" || rm -f "$t"; fi ;;
        *)
          if [ -s "$t" ] && [ "${code[$i]:-}" != 404 ]; then
            diag+=("lake-curl: ${dests[i]##*/}: HTTP ${code[$i]:-?}: $(head -c 300 "$t" | tr -s '\r\n\t' '   ')")
          fi
          rm -f "$t" ;;
      esac
    done
    cat "$errlog" >&2
    [ ${#diag[@]} -eq 0 ] || printf '%s\n' "${diag[@]}" >&2
    rm -f "$tmpcfg" "$errlog"
    exit $rc
  fi
fi

exec "$real" "$@"
