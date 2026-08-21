# orchestra — codebase review

A review of the repository at `8fbad9d` from three angles: **security**, **performance**, and
**maintainability**. ~28k lines: ~21k Lean 4 across `Orchestra/`, `Main.lean` and `Orchestrad.lean`,
~4k Lean test code under `OrchestraTest/`, ~2k TypeScript/React under `web/`, and two small C shims
under `ffi/`.

Findings are numbered and carry a severity. Every one names the file and line it is about, and
where a claim is about behaviour that only shows up at run time, the code path that produces it is
spelled out so it can be checked without taking this document's word for it.

---

## 0. Summary

This is unusually careful code. The security model is stated up front and mostly held to: agents run
under Landlock with an explicit path and port allowlist, credentials are threaded per-invocation
rather than through process-global `gh auth login`, the git credential helper is scoped to
`credential.https://github.com.helper` so a push to another host cannot carry the installation
token, the dashboard's CSRF reasoning is written down and correct, and path traversal is checked
after percent-decoding with a store-level backstop underneath the HTTP layer. The docstrings
routinely explain *why* rather than *what*, and several of them document bugs that were found and
fixed. That is rare and worth saying before the list of problems.

The problems that matter cluster in three places:

1. **The sandbox has a hole in it.** Repository hook scripts (`.orchestra/init.sh`, `before.sh`,
   `validation.sh`, `after.sh`) run **outside landrun**, in the daemon's own process environment,
   and they are read from the working tree *after* the sandboxed agent has had write access to it —
   and, in the merger backend, from a checked-out pull request branch. Findings **S1** and **S2**.
2. **Secrets travel in `argv`.** The GitHub App token, the PAT and every agent API key are passed as
   command-line arguments to `landrun` and `curl`, where `/proc/<pid>/cmdline` makes them readable
   by any local process. Finding **S3**.
3. **The state store is a directory of JSON files, re-read in full on a 1-second and a 2-second
   loop, with no index, no cache and no retention.** Findings **P1**–**P4**.

Nothing here is a reason to stop using the system in the deployment it documents (a dedicated VM or
container, loopback-only dashboard, repositories you control). Several of them are reasons not to
point it at a repository that accepts pull requests from strangers.

**Priority order for fixes:** S1, S2 → S3, S4 → P1, P2 → S5, S6 → the rest.

---

## 1. Security

### S1 — Sandbox escape: repository hooks run unsandboxed, after the agent can write them
**Severity: critical.** `Orchestra/RepoConfig.lean:52-66`, `Orchestra/TaskRunner.lean:536-635`.

`RepoConfig.runHook` and `RepoConfig.runValidation` execute scripts from the *cloned repository*:

```lean
def runHook (repoPath : System.FilePath) (name : String) : IO Unit := do
  let hookPath := (← orchestraDir repoPath) / name
  if !(← hookPath.pathExists) then return
  let child ← IO.Process.spawn { cmd := "bash", args := #[hookPath.toString], cwd := repoPath, … }
```

No landrun. No restricted environment. The child inherits the daemon's environment and privileges.

The order in `runIOTask` is what turns that into an escape:

```
runInitIfNeeded              -- bash .orchestra/init.sh          (unsandboxed)
loop over attempts:
  runHook "before.sh"        -- bash .orchestra/before.sh        (unsandboxed)
  Sandbox.launchAgent        -- landrun --rwx <repoPath> …       (sandboxed)
  runValidation              -- bash .orchestra/validation.sh    (unsandboxed)
runHook "after.sh"           -- bash .orchestra/after.sh         (unsandboxed)
```

`Sandbox.launchAgent` grants the agent `--rwx <repoPath>` (`Orchestra/Sandbox.lean:131`), and
`.orchestra/` is inside `repoPath`. So a sandboxed agent that writes `.orchestra/validation.sh` — a
single `Write` tool call, in a directory it is explicitly granted — gets that file executed outside
the sandbox moments later, by the daemon, with:

- the GitHub PAT and the App private key path in the loaded `config.json`,
- `$HOME` including `~/.claude`, `~/.config/gh`, `~/.claude.json`,
- the whole of `/data` (task history, every clone, `dashboard.secret`) and `/config`
  (`secrets.json`, listener configs),
- the network, unrestricted.

The landrun sandbox is the project's central security claim ("Every agent runs under landrun …
nothing else"). This path defeats it entirely, and the agent does not need to be adversarial to
reach it — a model that decides a repository "needs a validation script" and writes one produces the
same effect by accident.

**Fix.** Either run hooks inside a landrun sandbox of their own (the same `paths` set the agent
gets, minus the credentials), or snapshot `.orchestra/` to a temp directory *before* the agent runs
and execute the snapshot afterwards, or refuse to run a hook whose file changed during the agent
run. The snapshot approach is the smallest change and preserves the retry loop's semantics — the
validation script that runs is the one the repository shipped, not the one the agent just wrote.

### S2 — Merger backend executes a pull request's validation script unsandboxed
**Severity: critical.** `Orchestra/TaskRunner.lean:118-137`.

`runMerger` does:

```lean
let coChild ← IO.Process.spawn
  { cmd := "gh", args := #["pr", "checkout", toString pr.number, "--repo", pr.repo.toString], … }
…
let (valid, validOutput) ← RepoConfig.runValidation repoPath
```

`gh pr checkout` fetches the PR's head — for a fork PR, that is a branch written by whoever opened
it. `runValidation` then runs `.orchestra/validation.sh` **from that branch**, unsandboxed, in the
daemon's environment (S1).

Anyone who can open a pull request against a repository orchestra merges can therefore run arbitrary
code on the orchestra host, reaching the PAT, the App private key and every stored credential. No
agent, no prompt injection, no interaction with a model is involved — the merger backend "Skips the
entire agent / sandbox / MCP path", as its own docstring says.

**Fix.** Same as S1, plus: the merger should never execute code from a branch it did not produce.
If pre-merge validation of contributor code is wanted, it belongs in CI on the GitHub side, or in
a sandbox with no credentials at all.

### S3 — Credentials passed in `argv`, readable via `/proc`
**Severity: high.** `Orchestra/Sandbox.lean:211`, `Orchestra/Utils/Http.lean:132,140,155`,
`Orchestra/GitHub.lean:163,190,198`, `Orchestra/Client.lean:118`.

```lean
-- Sandbox.lean:211
args := args.push "--env" |>.push s!"GH_TOKEN={ghToken}"
```

```lean
-- Utils/Http.lean
let mut args := #["-H", s!"Authorization: Bearer {token}"]
```

On Linux `/proc/<pid>/cmdline` is world-readable by default. Every GitHub App installation token,
the PAT, and every Anthropic/Mistral key handed to an agent is therefore visible to any process on
the host for the lifetime of the `landrun` or `curl` invocation — including, notably, to the
sandboxed agent itself if `/proc` is reachable, and to any other user account on a shared machine.

This is at odds with the care taken elsewhere: `Repo.tokenEnv` (`Orchestra/Repo.lean:13-16`) goes out
of its way to pass tokens through the *environment* of a single invocation precisely to avoid
process-global exposure, and `commandFailure` (`Orchestra/GitHub.lean:15-29`) deliberately never
quotes command arguments "because they carry the JWT and the installation token" — the argument
vector is already understood to be sensitive. It just isn't treated that way at the point where it
is built.

**Fix.** For `curl`: read the header from stdin with `--config -` (`header = "Authorization: Bearer …"`),
or from a `600` temp file with `-K`. For landrun: pass `--env GH_TOKEN` by *name* if landrun supports
inheriting from the parent's environment (it does for the `SHELL`/`PATH`/`HOME` cases two lines
below, which are passed by name already — the token is the only one interpolated by value).

### S4 — `--debug` prints every credential to stderr and into the log
**Severity: high.** `Orchestra/Sandbox.lean:259-261`.

```lean
if debug then
  let argsStr := String.intercalate " " (args.toList.map shellEscape)
  IO.eprintln s!"[debug] cd {shellEscape repoPath.toString} && landrun {argsStr}"
```

`args` at that point contains `--env GH_TOKEN=ghs_…` and every `--env KEY=value` from `agentEnv` and
`extraEnv` — that is, the Anthropic OAuth token or API key. `--debug` is a user-facing flag on
`orchestra run`, and the daemon's stderr is what `docker compose logs` shows and what CI captures.
A user turning on debug to diagnose a sandbox problem publishes their credentials into a log they
will then paste into an issue.

**Fix.** Redact `--env NAME=value` pairs in the debug line — print `--env NAME=<redacted>` for
anything whose name is not on a small allowlist (`PATH`, `HOME`, `TERM`, `USER`, `SHELL`,
`CLAUDE_CODE_DISABLE_AUTO_MEMORY`).

### S5 — Shell injection in the GitHub App JWT signer
**Severity: high (latent).** `Orchestra/GitHub.lean:184-192`.

```lean
let signature ← runCmd "sh" #["-c",
  s!"echo -n '{unsigned}' | openssl dgst -sha256 -sign {privateKeyPath} | openssl base64 -e -A | …"]
```

`privateKeyPath` is interpolated into a shell command **unquoted**. It comes from
`github_app.private_key_path` in `config.json`. A path containing a space breaks the command; a path
containing `;` or `$(…)` executes as the daemon.

Today `config.json` is written by the operator or by the Docker entrypoint (which builds it with
`jq`, correctly). But combined with S6 — the dashboard container has read-write `/config` — and with
the fact that config is a documented editing surface, this is one configuration-write away from
being a live remote-code-execution path rather than a latent one.

The same three `sh -c` calls are also fragile for a second reason: `echo -n` is not portable
(`/bin/sh` is dash on Debian, and POSIX leaves `echo -n` undefined), and the JWT header and payload
are interpolated into single-quoted shell strings too.

**Fix.** Drop the shell entirely. `openssl dgst -sha256 -sign <path>` reads from stdin, and `runCmd`
already supports an `input` parameter; base64url can be done in Lean over the raw bytes. That
removes three subprocesses per JWT as well (see P6).

### S6 — The dashboard secret is effectively remote code execution on the daemon host
**Severity: high (by design, but undocumented as such).**
`Orchestra/Listener.lean:49,180-183,1322-1334`, `docker/docker-compose.yaml`.

A listener source may be an arbitrary shell command:

```lean
| shell (command : String) (args : List String)
```

and the daemon runs it directly (`Listener.lean:1323`). Listener configs are writable over the HTTP
API — `PUT /api/v1/listeners/{name}` → `writeListener` → `saveListenerConfigRaw` — and
`validateListenerConfig` (`Listener.lean:491`) checks the name, the JSON shape and the poll interval,
but places no restriction on what a `shell` source may run.

So: anyone holding the dashboard password can write a listener whose command is anything, and the
daemon will execute it as the `orchestra` user within a poll interval.

The compose file draws a trust boundary that this crosses. It says of the dashboard container:

> Deliberately *not* `<<: *orchestra-env`. This is the one container reachable from outside, and it
> needs none of the daemon's credentials: it dispatches nothing, opens no PR, and runs no agent.

That reasoning holds for the dashboard *process*, but the dashboard writes `/config`, and the daemon
executes what is in `/config`. The isolation is therefore nominal: compromising the exposed
container yields code execution in the credentialed one.

**Fix.** At minimum, document this — "the dashboard password is host-root-equivalent" belongs next
to the password in `.env.example` and in the README's authentication section. Better: gate `shell`
listeners behind a daemon flag (`--allow-shell-listeners`) or an allowlist of commands in
`config.json`, so that the API cannot introduce one that the operator has not already permitted.

### S7 — Use-after-free in the Unix-socket send shim
**Severity: medium (memory safety).** `ffi/UnixSocket.c:68-84`.

```c
LEAN_EXPORT lean_obj_res lean_uds_send_line(uint32_t fd, lean_obj_arg str_obj, lean_obj_arg world) {
    const char *str = lean_string_cstr(str_obj);
    size_t len = strlen(str);
    lean_dec(str_obj);          // <-- may free the object backing `str`

    const char *p = str;        // <-- `str` is used after the decrement
    …
        ssize_t n = write((int)fd, p, rem);
```

`lean_string_cstr` returns a pointer *into* the string object. `lean_dec` on the last reference frees
it. Everything after that line reads freed memory. In practice the caller usually holds another
reference and it survives; when it does not, this writes whatever now occupies that heap block to
the control socket, or faults.

**Fix.** Move `lean_dec(str_obj)` to just before each `return`. (The `listen`/`connect` shims do this
correctly — they `strncpy` into `addr.sun_path` first, then `lean_dec`.)

Two smaller issues in the same file:

- `lean_uds_recv_line` (line 87) grows its buffer without any ceiling. A client that sends a
  gigabyte with no newline makes the daemon allocate a gigabyte. The control socket is local, so
  this is a robustness rather than an exposure issue — but a cap (say 1 MiB) with an error return
  costs three lines.
- `lean_mk_string(buf)` (line 108) is handed arbitrary bytes off the socket. Lean strings are assumed
  to be valid UTF-8; `lean_mk_string` does not validate. Invalid input therefore produces a `String`
  whose invariants do not hold. Use `lean_mk_string_from_bytes` and validate, or reject non-UTF-8.
- `strncpy(addr.sun_path, path, sizeof(addr.sun_path) - 1)` silently truncates a path longer than
  107 bytes, so a long `XDG_DATA_HOME` would bind or connect to a *different* socket than intended.
  Check the length and return an error.

### S8 — The MCP server accepts any local connection, unauthenticated
**Severity: medium.** `Orchestra/Server.lean:903-950`.

The per-task MCP server binds `127.0.0.1:0` and answers JSON-RPC with no authentication of any kind.
Its tool surface includes `refresh_token`, which mints a fresh GitHub App installation token and
returns it to the caller:

```lean
| .refreshToken =>
  let jwt ← GitHub.createJWT state.appId state.privateKeyPath
  let token ← GitHub.createInstallationToken jwt state.installationId
  return toolContent token
```

Any local process that finds the port (a 16-bit scan of loopback, or `ss -ltn`) can mint tokens,
and — subject to `allowedTools` — open pull requests as the PAT owner. The landrun sandbox limits
the *agent* to its own port, so this is not a cross-task escape between sandboxed agents; it is
exposure to anything else running on the host, including a compromised hook script from S1.

**Fix.** Generate a per-task bearer token alongside the port, pass it to the backend in the same
`setupMcp` step that already passes the port, and require it on every request. That is a handful of
lines and closes the gap without changing the transport.

### S9 — `handleClient` can panic on a UTF-8 sequence split across a receive boundary
**Severity: medium (availability/correctness).** `Orchestra/Server.lean:910`.

```lean
let data? ← awaitTcp (← client.recv? 65536)
…
buf.modify (· ++ String.fromUTF8! bytes)
```

`recv?` returns whatever bytes arrived, which for a message larger than 64 KiB is an arbitrary cut —
possibly mid-code-point. `String.fromUTF8!` is the partial version. A PR body or an issue thread with
any non-ASCII character crossing a 64 KiB boundary hits it. This is reachable from ordinary use
(`create_pr` with a long body, `comment` with an emoji in a long thread), not only from an attacker.

**Fix.** Accumulate a `ByteArray` and decode only up to the last complete line, or use
`String.fromUTF8?` and keep the undecodable tail in the buffer.

### S10 — Secret substitution is textual and pre-JSON
**Severity: medium.** `Orchestra/Config.lean:671-683`, `Orchestra/Listener.lean:497`.

```lean
def applySecrets (secrets : List (String × String)) (text : String) : String :=
  secrets.foldl (fun acc (k, v) => acc.replace ("{{" ++ k ++ "}}") v) text
```

`{{key}}` is replaced in the raw file text *before* `Json.parse`. A secret containing `"` or `\`
therefore either breaks parsing or injects structure into the surrounding JSON document. Since the
substituted text is a config file that decides which repositories a task targets and which tools it
is granted, a quote in a token is a config-injection primitive rather than a cosmetic bug.

There is a second, subtler edge: `validateListenerConfig` substitutes secrets into the body and then
reports parse failures back to the API caller —

```lean
let j ← match Json.parse (applySecrets secrets raw) with
  | .error e => return .error s!"the body is not valid JSON: {e}"
```

If Lean's JSON parse error quotes any of the offending input, an authenticated client could learn
secret values by `PUT`ing a body that becomes invalid JSON only after substitution. Worth confirming
what `Json.parse` includes in its message; if it quotes input, the error must be sanitised.

**Fix.** Substitute into the parsed JSON tree (walk `Json.str` leaves and replace there), or
JSON-escape each secret value before splicing it into text.

### S11 — No rate limiting on `POST /api/login`
**Severity: low.** `Orchestra/Dashboard.lean:1449-1461`.

The comparison is constant-time and a generated password is 24 random bytes, so an online attack on
the default configuration is not feasible. But `--password` and `ORCHESTRA_DASHBOARD_PASSWORD` let an
operator choose a weak one, and there is no throttle, lockout or delay — the endpoint will answer as
fast as the server can parse JSON.

**Fix.** A per-IP token bucket, or a fixed 250 ms delay on failure, or refusing to start with a
password under some entropy threshold. Any of the three.

### S12 — No Content-Security-Policy
**Severity: low.** `Orchestra/Dashboard.lean:183-186`.

`secured` sets `X-Content-Type-Options`, `X-Frame-Options` and `Referrer-Policy`, which is more than
most projects do — but there is no CSP. The dashboard renders text produced by language models
(prompts, task logs, validation output). React escapes by default and the front-end contains no
`dangerouslySetInnerHTML` (verified across `web/src/`), so there is no live XSS today; a CSP is the
cheap insurance that keeps it that way through future changes.

**Fix.** `default-src 'self'; script-src 'self'; style-src 'self' 'unsafe-inline'; img-src 'self'
data:; connect-src 'self'; frame-ancestors 'none'; base-uri 'none'` — adjust for the
`@fontsource` bundling, which is local, so `font-src 'self'` suffices.

### S13 — Config files holding secrets are written with default permissions
**Severity: low.** `Orchestra/Utils/Files.lean:13-22`, `Orchestra/Usage.lean:416-419`,
`Orchestra/Config.lean:652-669`.

`Orchestra/Secret.lean:64-70` carefully creates `dashboard.secret` empty, `chmod 600`s it, and only
then writes the value — with a comment explaining why. Nothing else does. `config.json` (PAT,
`claude_token`, `anthropic_api_key`), `secrets.json`, and the usage state files inherit whatever the
umask gives, typically `0644`. In the documented container deployment this is contained by the
volume's ownership; on a shared host it is not.

**Fix.** Apply the `Secret.lean` pattern in `writeFileAtomically` — create the temp file `600`
before writing — or at least in the paths that write credential-bearing files.

### S14 — `backend` is not sanitised when used as a path component
**Severity: low.** `Orchestra/Usage.lean:401-405`.

```lean
private def safeLabel (label : String) : String :=
  label.map fun c => if c.isAlphanum || c == '-' || c == '_' then c else '_'

private def statePath (backend label : String) : IO System.FilePath := do
  return (← usageDir) / backend / s!"{safeLabel label}.json"
```

`label` is flattened; `backend` — which comes from `agent_auth[].name` in the same config — is not.
An inconsistency rather than an exposure (config is operator-controlled), but the fix is to wrap
`backend` in `safeLabel` too, and it costs nothing.

### S15 — Prompt injection from issue and comment bodies is unmitigated
**Severity: informational (inherent to the design; worth stating).**

Listener prompt templates expand `{{issue_body}}`, `{{issue_comments}}` and similar into the prompt
of an agent that holds `create_pr`, `comment`, `label_issue` and repository write access. Anyone who
can comment on a watched issue is therefore writing part of the instruction stream of a
credentialed agent.

`Listener.isAuthorized` (`Orchestra/Listener.lean:66`) gates *who can trigger* a task, which is the
right first control and materially narrows this. But it does not gate whose text ends up *inside*
the prompt: an authorised trigger on an issue whose body was written by someone else carries that
body in.

This is inherent to what orchestra is, and the landrun sandbox plus the per-task `allowedTools` set
are the real mitigations. The point worth acting on is documentation: the README should say plainly
that a listener watching a public repository grants partial prompt control to the public, so
operators size `tools` accordingly.

---

## 2. Performance

### P1 — The whole state store is re-read from disk on every loop iteration
**Severity: high.** `Orchestra/Queue.lean:303-312`, `Orchestra/Daemon.lean:308`,
`Orchestra/Dashboard.lean:697-700,725,734,1034,1294`.

`Queue.loadAllEntries` opens, reads, JSON-parses and sorts *every file* in the queue directory:

```lean
def loadAllEntries : IO (Array QueueEntry) := do
  let entries ← System.FilePath.readDir dir
  for entry in entries do
    if let some id := stripJsonExt entry.fileName then
      if let some e ← loadEntry id then result := result.push e
  return result.qsort (fun a b => a.id > b.id)
```

It is called from:

- **`claimNextEntry`, once per worker per second** (`Daemon.lean:308`, inside the loop at
  `Daemon.lean:765-776`), under the global `claimMutex`. With `--parallel 8`, that is eight full
  directory scans per second, serialised.
- **Every SSE tick, per connected browser tab** — `sseLoop` re-runs `renderApi` every 2 s
  (`Dashboard.lean:1641-1666`), and `overviewApi` calls `loadAllEntries`, `loadAllTasks`,
  `loadAllListenerConfigs` and `loadAllConcertRuns` in sequence.

With a few thousand accumulated entries and two dashboard tabs open, that is tens of thousands of
`open`/`read`/`close` plus JSON parses per second, most of them re-reading files that have not
changed. On the daemon side it is worse than throughput: it is time spent holding the mutex that
every worker needs to claim work.

**Fix.** Keep the queue in memory in the daemon (the file store stays as the durable record) and
maintain it incrementally; on the dashboard side, cache per file keyed by `(path, mtime, size)` and
re-parse only what changed. An in-memory index also removes P4.

### P2 — Task logs are read in full, every 2 seconds, to display the tail
**Severity: high.** `Orchestra/Dashboard.lean:999-1021`.

```lean
let readLines (path : System.FilePath) : IO (List String) := do
  let raw ← IO.FS.readFile path
  return (raw.splitOn "\n").filter (!·.trimAscii.isEmpty)
…
let lines := attempts.toList.flatten
let total := lines.length
let kept := if total ≤ limit then lines else lines.drop (total - limit)
```

Watching a running task is the dashboard's most-used view, and it is the most expensive one. For each
2-second tick this reads the entire `<id>.log` (plus up to 100 `<id>.retryN.log` files) into a
`String`, splits it into a `List String` — one cons cell and one string per line — then walks that
list twice (`length`, `drop`) to keep the last 500. A long agent run produces megabytes; the code
allocates and discards all of it 30 times a minute, per viewer.

The docstring shows the author was already thinking about this cost ("this runs on every SSE tick,
and `++` would recopy the whole run's log for each retry it has been through") — the fix went to the
concatenation, but the dominant term is the full read.

**Fix.** Tail the file: `seek` to `size - N` and read forward, or track `(offset, lineCount)` per
task across ticks and read only what was appended. `Array String` instead of `List String` removes
the second walk.

### P3 — Nothing is ever pruned
**Severity: high (operational).** `Orchestra/Queue.lean`, `Orchestra/TaskStore.lean`,
`Orchestra/TaskRunner.lean:589-591`.

Queue entries, task records, concert runs and per-task JSONL logs are written and never deleted.
`orchestra cleanup` (`Main.lean:184-186`) removes *repository clones*, not history. `Usage.lean` is
the exception and does it right — `pruneWindows`, `maxWindowsPerSeries := 240`,
`historyRetentionSecs := 180 * 86400` (`Usage.lean:563-583`).

Left running, the daemon therefore gets monotonically slower at exactly the rate it does work, since
P1 and P2 are both linear in the accumulated history. A long-lived instance degrades without any
single change causing it.

**Fix.** Extend the `Usage.lean` retention model to the queue, the task store and the logs, with a
configurable window and a sweep on daemon start.

### P4 — `loadAllEntries` where `loadEntry` would do
**Severity: medium.** `Orchestra/Dashboard.lean:1034`, and similar at `1294`.

```lean
let entries ← Queue.loadAllEntries
let qEntry  := entries.find? (·.id == id)
```

`Queue.loadEntry id` reads exactly one file and already exists two lines away in the same module.
This is inside `taskDetailApi`, i.e. on the 2-second SSE path for whichever task someone is watching.
Same pattern in `cancelEntry` (`Dashboard.lean:1294`), where the entry is then looked up by
`e.id == id || e.taskId == some id` — that one genuinely needs a scan for the second disjunct, but
could try `loadEntry` first and fall back.

### P5 — `handleClient`'s buffer is quadratic in message size
**Severity: medium.** `Orchestra/Server.lean:908-916`.

```lean
buf.modify (· ++ String.fromUTF8! bytes)
let lines := (← buf.get).splitOn "\n"
buf.set (lines.getLast?.getD "")
```

For a message arriving in k chunks of 64 KiB, the `++` copies the accumulated buffer k times
(O(n²) bytes moved) and `splitOn` re-splits the whole buffer on each chunk. Tool arguments carrying
a full PR body or an issue thread reach this size routinely.

**Fix.** Accumulate into a `ByteArray` and scan only the newly-arrived region for `\n`.

### P6 — Four subprocesses per GitHub App JWT
**Severity: medium.** `Orchestra/GitHub.lean:176-193`.

`createJWT` spawns `date`, then three `sh -c` pipelines each of which spawns `sh`, `openssl` twice and
two `tr`s — roughly a dozen processes per token. A token is minted per task, again by `create_pr
target=fork`, and again on every `refresh_token` tool call.

`date +%s` is the most gratuitous: `Usage.nowEpoch` (`Orchestra/Usage.lean:156`) already exists in
this codebase and returns the same number without a fork.

**Fix.** As in S5 — sign via `runCmd "openssl" #["dgst", …] (input := unsigned)` and base64url-encode
in Lean. That collapses a dozen processes to one and removes the injection.

### P7 — `truncatePrompt` materialises the whole prompt as a `List Char`
**Severity: low.** `Orchestra/Sandbox.lean:71-81`.

```lean
for c in s.toList do
```

`String.toList` on a >120 KB prompt allocates one cons cell and one boxed `Char` per code point.
This only fires on the over-long path — which is exactly the path where the string is largest.

**Fix.** Use `String.Iterator`, or binary-search the byte offset and adjust to a code-point boundary.

### P8 — Per-line `flush` on three log handles
**Severity: low.** `Orchestra/Sandbox.lean:302-306,333-336`.

The debug handle, the structured JSONL handle and stdout/stderr are each flushed after every parsed
line. For a verbose agent that is three `write(2)` calls per event. Flushing the structured log
matters (the dashboard tails it); flushing the debug log on every line does not.

### P9 — Linear scans inside loops in the dispatcher
**Severity: low.** `Orchestra/Listener.lean:1423-1432`.

```lean
let labelled : Array Taxis.IssueId := issues.map (·.id) ++ reviewable.map (·.id)
let allEntries ← Queue.loadAllEntries
for e in activeEntries do
  if !labelled.contains eIid then continue
```

`Array.contains` is O(n) inside a loop over entries — O(n·m) per dispatcher tick. A `HashSet` makes
it O(n+m). Small today; it compounds with P3.

### P10 — `sseLoop` recurses per tick
**Severity: low.** `Orchestra/Dashboard.lean:1641-1666`.

`sseLoop` is `partial` and calls itself in tail position within `Async`. Whether that is a true tail
call depends on how the `Async` bind compiles; a long-lived stream (hours, one iteration every 2 s)
is exactly where it would matter if it is not. Worth converting to an explicit loop, or confirming
the frames do not accumulate.

---

## 3. Maintainability and readability

### M1 — Three functions carry most of the system
**Severity: high.**

| Function | Lines | File |
|---|---|---|
| `Daemon.run` | ~634 | `Orchestra/Daemon.lean` |
| `Listener.pollSource` | ~498 | `Orchestra/Listener.lean` |
| `TaskRunner.runIOTask` | ~330 | `Orchestra/TaskRunner.lean` |
| `Server.evalToolCall` | ~239 | `Orchestra/Server.lean` |
| `Main.enqueueHandler` | ~136 | `Main.lean` |

`Daemon.run` is the whole daemon in one body: the socket server, the usage poller, the listener
supervisor, the claim logic, the worker loop, the shutdown path, and a dozen closures
(`claimNextEntry`, `releaseEntry`, `runEntryBody`, `finish`, `announce`, `spawnListener`, …) defined
inline over shared mutable refs. It cannot be unit-tested at all — which is visible in the test
suite, where `ParallelQueueTest` tests `claimDecision` (correctly extracted as a pure function) and
nothing tests the loop that calls it.

The `claimDecision` extraction is the model to follow: pure core, injected effects
(`slotOccupant`, `parallelSafe`, `resolveAuth`), tested without a network. Applying the same
treatment to `runEntryBody` and the listener poll would make the two most failure-prone paths in the
system reachable from a test.

`Listener.pollSource` is a single `match` over eight source constructors with the full GitHub polling
logic inline in each arm. Each arm is independently comprehensible and none of them shares state —
one function per source type, dispatched by a table, would be a mechanical change with no behavioural
risk.

### M2 — Two copies of the curl wrapper
**Severity: medium.** `Orchestra/GitHub.lean:69-108` vs `Orchestra/Utils/Http.lean:12-125`.

`Orchestra/Utils/Http.lean` opens with:

> `Orchestra.GitHub` grew the original version of this against api.github.com; it is lifted here so
> the usage-limit poller can reuse it instead of keeping a second copy of the same careful error
> handling.

The lift happened but the original was not removed: `GitHub.lean` still defines its own
`httpStatusMarker`, `splitHttpStatus` and `curlWithStatus`, byte-identical in logic to
`Utils.Http.statusMarker`, `splitStatus` and `curlWithStatus` except for the timeouts (60 s vs 30 s).
Both are tested — `GitHubErrorTest` covers one, and the marker string is duplicated in both — so a
fix to one silently misses the other.

**Fix.** Delete the `GitHub.lean` copies and call `Utils.Http`, passing `maxTime := 60`.

### M3 — Durable state is written non-atomically, though the helper exists
**Severity: medium.** `Orchestra/Queue.lean:283,576`, `Orchestra/TaskStore.lean:161,207`,
`Orchestra/Usage.lean:419`.

`Utils.writeFileAtomically` exists precisely because "a plain `IO.FS.writeFile` truncates first, so a
reader landing in that window gets an empty or partial file". Listener, role and skill writes use it.
Queue entries, task records, concert runs, series pointers and usage state do not — they use
`IO.FS.writeFile` directly.

These are exactly the files read concurrently by another process: the dashboard container reads
`/data` while the daemon writes it. And the read path swallows the failure —

```lean
match Json.parse contents with
| .error _ => return none
```

— so a torn read does not raise, it makes the entry *disappear* from `loadAllEntries`. For the
dashboard that is a flicker. For `claimNextEntry`, it is an entry that is briefly invisible to the
scheduler.

**Fix.** Route every one of these through `writeFileAtomically`. Two further hardening notes on that
function: its temp file is named by PID, so two threads *in the same process* writing the same path
still collide (add a counter or thread id); and it does not `fsync` the temp file before `rename`, so
a crash can leave a zero-length file where the rename appeared to succeed.

### M4 — Read-modify-write on usage state has no lock
**Severity: medium.** `Orchestra/Usage.lean:421-422`.

```lean
private def modifyState (backend label : String) (f : SourceState → SourceState) : IO Unit := do
  saveState (f (← loadState backend label))
```

`markUsed`, `markLimited` and `markOk` all go through this. `markUsed` is called under `claimMutex`
(the daemon's comment at `Daemon.lean:283-289` explains why that serialisation matters), but
`markLimited` and `markOk` are called from task-completion paths that run concurrently across
workers. Two workers finishing at once lose one of the two updates — and what is lost is the record
that an account hit its limit, which is the input to the routing decision the whole module exists to
make.

**Fix.** A per-(backend,label) mutex around `modifyState`, or route all state mutation through a
single owning task.

### M5 — 33 silent `catch _ => pure ()` sites
**Severity: medium.** Throughout; e.g. `Orchestra/Server.lean:947`, `Orchestra/Sandbox.lean:284`,
`Orchestra/Queue.lean:488`, `Orchestra/Dashboard.lean:1296-1302`.

Some are correct and documented (best-effort unlink on shutdown, the cancel-token kill path). Others
discard information that would be the only clue to a failure — `Server.start`'s per-client
`try handleClient state client catch _ => pure ()` turns every MCP handler bug into a silently
disconnected agent. Similarly, `loadEntry`, `loadTask`, `loadState` and `loadRepoConfig` all map a
JSON parse error to "absent", so a corrupted file is indistinguishable from a missing one.

**Fix.** Log at the catch site. `catch e => log s!"…: {e}"` costs one line and turns an invisible
failure into a greppable one.

### M6 — No timeouts on agent runs or hooks
**Severity: medium.** `Orchestra/Sandbox.lean`, `Orchestra/RepoConfig.lean`,
`Orchestra/TaskRunner.lean`.

There is no wall-clock bound anywhere on a task. `child.wait` blocks until the agent exits;
`runHook` and `runValidation` likewise. An agent that hangs — waiting on a prompt inside a
`--rwx`-less directory is the documented failure mode at `Sandbox.lean:154-158` — holds its clone
slot and its parallelism budget indefinitely. The cancel token exists, but only a human clicking
cancel or `POST /api/v1/queue/{id}/cancel` ever fires it.

The budget mechanism bounds *spend*, not *time*, and does not fire for a process that is stuck rather
than working.

**Fix.** A per-task deadline (config default, per-task override) that cancels the token, and a
shorter one on hooks.

### M7 — `kill -9` by PID leaks the agent's children
**Severity: medium.** `Orchestra/Sandbox.lean:281-291,318-328`.

Cancellation spawns `/bin/kill -9 <pid>` against the `landrun` process. SIGKILL cannot be forwarded,
so landrun dies and the agent CLI it launched — and whatever the agent itself spawned — is
reparented and keeps running: still writing to the clone, still holding the `GH_TOKEN` it was given,
outliving the task that was cancelled.

**Fix.** Put the child in its own process group (or use landrun's own signal handling if it forwards
SIGTERM) and `kill(-pgid, SIGTERM)` then `SIGKILL` after a grace period. Spawning `/bin/kill` at all
is also avoidable — `IO.Process.Child` exposes `kill` — and the identical ~20-line cancel-token block
is duplicated verbatim between the interactive and headless branches; it should be one helper.

### M8 — Handles are not closed deterministically
**Severity: low.** `Orchestra/Sandbox.lean:294-301`.

`debugHandle` and `logHandle` are opened with `IO.FS.Handle.mk` and never closed — they are released
whenever the GC runs. In a daemon that runs thousands of tasks, that is thousands of file
descriptors held for an unbounded time. Wrap in `IO.FS.withFile` or close explicitly in a `finally`.

### M9 — `shellEscape` is a display helper with a security-sounding name
**Severity: low.** `Orchestra/Sandbox.lean:36-41`.

It is used only to render the `--debug` line. It does not escape `*`, `?`, `<`, `>`, `#`, `~` or
newlines-in-context, so the string it produces is not reliably safe to paste into a shell — which is
exactly what its name invites. Rename to `quoteForDisplay` and say in a docstring that it is not a
security boundary.

### M10 — Unpinned dependencies
**Severity: low.** `lakefile.lean:8-9`, `docker/Dockerfile:73,84`.

```lean
require Cli  from git "https://github.com/leanprover/lean4-cli.git" @ "main"
require Yaml from git "https://github.com/chrisflav/lean-yaml"      @ "master"
```

Two of the three Lean dependencies track a moving branch; only `Taxis` is pinned to a SHA.
`lake-manifest.json` pins the resolved revisions, which protects a checkout that has one — but any
`lake update` silently adopts whatever is on `main`, from a repository outside this project's
control.

The Dockerfile has the same shape: `npm install -g @anthropic-ai/claude-code` with no version, and
two `curl … | sh` installer pipes (elan, NodeSource). `LANDRUN_VERSION` is pinned to a tag, which a
maintainer can move.

**Fix.** Pin the Lean deps to SHAs like `Taxis` already is; pin the npm package to a version; pin the
apt/npm installers where practical.

### M11 — Frontend dependency hygiene
**Severity: low.** `web/package-lock.json`.

`vite@5.4.21` and `react-router@6.30.4` are current enough. `esbuild@0.21.5` (transitive, via Vite)
predates the fix for the dev-server request-forwarding advisory (GHSA-67mh-4wv8-2f99, fixed in
0.25.0). It is a development-server issue only — the production path here is a static bundle served
by the Lean binary — so the exposure is limited to developers running `npm run dev` on a shared
network. Worth an `npm audit` pass in CI regardless.

### M12 — Test coverage stops at the process boundary
**Severity: medium.**

The suite is genuinely good where it exists: 24 files, ~4k lines, covering JSON round-trips,
`claimDecision`, dispatcher selection, permission gates in `evalToolCall`, path traversal in
`staticCandidate`/`safeSegment`, CSRF content-type checks, usage-limit parsing, and — nicely — the
OpenAPI spec against the routes.

What is untested is everything that spawns a process or touches the network: `Sandbox`,
`RepoConfig` (the hook runner from S1/S2 — zero coverage), `Repo`, the daemon loop, the socket
shims, `ConcertManager`. That is where the two critical findings live, and it is not a coincidence:
the pattern that makes `claimDecision` testable (pure core, injected effects) has not been applied
to the effectful modules.

### M13 — CI has no linting or dependency audit
**Severity: low.** `.github/workflows/`.

`lean_action_ci.yml` runs `lake build`/`lake test`; `docker.yml` builds the image (which does run
`tsc --noEmit && vite build`, so the front-end is type-checked on PRs). Missing: `shellcheck` on
`docker/entrypoint.sh` and the example scripts, `npm audit`, and any check that
`container/configuration.nix` still matches the Dockerfile — a synchronisation the Dockerfile's own
comment asks for and nothing enforces.

---

## 4. What is done well

Worth recording, because a review that lists only problems misrepresents the codebase.

- **The docstrings explain reasoning, not mechanics.** `Orchestra/Repo.lean:19-38` on why the
  credential helper is scoped to `credential.https://github.com.helper` rather than the bare key;
  `Orchestra/Dashboard.lean:119-157` on why `SameSite=Strict` plus a JSON content-type requirement
  makes a synchroniser token redundant; `ffi/Signal.c:7-20` on why PID 1 needs an installed handler
  before SIGTERM is deliverable at all. These are the notes that stop a future maintainer from
  "simplifying" a load-bearing detail.
- **Fixed bugs are documented at the site.** `Orchestra/Utils/Files.lean:55-60` records that the
  skill store once let `{"name": "../../…"}` through, which is why the check now lives in the store
  rather than at the HTTP boundary. That is the right place for that sentence.
- **Ordering-sensitive code says so.** The comment at `Dashboard.lean:1166-1174` explaining why
  validation must precede the existence probe (or 409-vs-404 becomes a filesystem oracle) is a
  subtlety most codebases get wrong silently.
- **Credential threading is deliberate.** Per-invocation `GH_TOKEN` instead of global
  `gh auth login`, with the concurrency reason written down; `refresh_token` returning the token to
  the caller rather than writing `~/.config/gh/hosts.yml`, for the same reason.
- **`Usage.lean` is a complete, self-consistent subsystem** — window accounting, pruning, retention,
  backoff, pool selection — with the largest test file in the suite behind it.
- **The Docker packaging is careful**: privilege drop via `setpriv` after an O(1) ownership check,
  no seccomp relaxation with an explanation of why none is needed, a landrun probe that actually
  builds a Landlock ruleset rather than running `--help`, and a `stop_grace_period` matched to how
  long a real task takes to drain.

---

## 5. Suggested order of work

**Now (security-critical):**
1. S1 — snapshot `.orchestra/` before the agent runs, or sandbox the hooks.
2. S2 — stop executing checked-out PR code in the merger.
3. S3 / S4 — get tokens out of `argv` and out of the debug line.

**Next (short, high-value):**
4. S5 / P6 — rewrite `createJWT` without `sh -c`: removes an injection and ~11 processes per token.
5. S7 — move `lean_dec` past the last use in `lean_uds_send_line`.
6. S8 — a per-task bearer token on the MCP server.
7. M3 — route queue/task/usage writes through `writeFileAtomically`.

**Then (performance, in this order):**
8. P2 — tail task logs instead of reading them whole.
9. P1 / P4 — in-memory queue index in the daemon; mtime-keyed cache in the dashboard.
10. P3 — retention for queue, tasks and logs, modelled on `Usage.pruneWindows`.

**Ongoing (maintainability):**
11. M1 — decompose `Daemon.run` and `Listener.pollSource`, following the `claimDecision` pattern.
12. M2 — delete the duplicated curl wrapper in `GitHub.lean`.
13. M6 / M7 — task deadlines and process-group kill.
14. M12 — tests for `RepoConfig` and the sandbox launch path, which is where S1 and S2 live.
