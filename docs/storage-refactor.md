# Moving orchestra's state from files to a database

A design for replacing the one-JSON-file-per-record state store with SQLite, written against
`8fbad9d`. It follows the storage review in [`code-review.md`](./code-review.md), whose findings
P1–P4 and M3–M4 all have the same root cause.

**Verdict: worth doing, in phases, and cheaper than it looks — but not first.** The security
findings (S1–S4) are smaller, more urgent, and touch none of this code. Do those, then start here
with the pilot in Phase 1.

---

## 1. Why this is cheaper than it looks

Three things make the cost much lower than a storage migration usually is.

**SQLite is already linked into both binaries.** `lake-manifest.json` carries
`leanprover/leansqlite` at `v4.31.0` — pinned to the same version as `lean-toolchain` — pulled in
transitively by `Taxis`, which requires it directly:

```lean
-- taxis/lakefile.toml:19
require leansqlite from git "https://github.com/leanprover/leansqlite" @ "v4.31.0"
```

The Docker image already compiles its amalgamation; the Dockerfile comment at line 31 mentions it
by name. So this adds **no new dependency, no new build tooling, and no new image layer**. It
promotes an inherited dependency to a direct one.

**There is a proven pattern to copy, by the same author.** `Taxis/Db/` is a complete storage layer
in this exact stack: `Connection.lean` (WAL, busy timeout, foreign keys, `withTransaction` /
`withReadTransaction`), `Schema.lean` (one idempotent DDL string plus a `schema_version` row and
incremental `ALTER`s), and one module per entity. Mirroring it is not a speculative design —
it's a shape already running in production against the same toolchain.

**The store is already abstracted at the function level.** Every access goes through a named
accessor — `loadAllEntries`, `saveEntry`, `loadTask`, `loadListenerState`, `saveState` — and there
are about 126 call sites across them:

| Accessor | Call sites | | Accessor | Call sites |
|---|---:|---|---|---:|
| `loadAllEntries` | 19 | | `loadListenerState` | 16 |
| `saveEntry` | 16 | | `saveListenerState` | 11 |
| `loadTask` | 11 | | `loadState` (usage) | 10 |
| `loadAllTasks` | 8 | | `saveConcertRun` | 7 |
| `saveTask` | 7 | | `loadAllConcertRuns` | 6 |

Nothing outside these modules opens a state file by hand. That means **Phase 1 changes no call
sites at all**: `saveEntry : QueueEntry → IO Unit` keeps its signature and swaps its body. Only the
places that want to stop scanning and start *querying* need to change — and those are chosen, not
forced.

---

## 2. What moves, and what does not

The filesystem layout already encodes the right seam. `configBase` holds documents a human writes;
`dataBase` holds records the machine writes. **Only the second moves.**

### Moves to the database

| Today | Rows | Why |
|---|---|---|
| `data/queue/<id>.json` | `queue_entries` | The 1s claim loop and the 2s SSE tick both scan it whole (P1) |
| `data/tasks/<id>.json` | `tasks` | Same scan, plus it never shrinks (P3) |
| `data/concerts/<id>.json` | `concert_runs` | Joined to queue entries by `concert_id` |
| `data/series/<name>.json` | `series` | A one-field pointer per file |
| `data/usage/<backend>/<label>.json` | `usage_state`, `usage_windows` | Read-modify-write with no lock (M4) |
| `data/listeners/state/<name>.json` | `listener_state`, `listener_seen_events` | Unbounded array, O(n·m) scan — see §3 |

### Stays on disk

| Path | Why |
|---|---|
| `config/config.json`, `secrets.json`, `prompts/`, `listeners/`, `roles/`, `skills/` | Hand-edited, git-committable, bind-mounted, seeded by the entrypoint. A blob you cannot `vim` is a regression. |
| `data/projects/<pid>/roles/*.json` | Same: project-scoped role documents, not state. |
| `data/logs/<fork>/<id>.log` | Append-only JSONL, potentially megabytes, streamed while being written. Gets an **index row**, not a blob column. |
| `data/repos/`, `data/memory/` | Git clones and agent memory dirs, mounted into the sandbox. |
| `data/dashboard.secret` | Bootstrap credential — must be readable before the database is opened. |

Projects, issues and claims are **already remote**, in taxis (`Project/Claim.lean` stores a claim as
a `session`-kind artifact). A local database does not touch them, and — worth stating plainly — does
**not** fix the cross-process claim race that `ClaimManager`'s docstring describes. That race is
against a remote tracker and needs a compare-and-swap primitive there.

---

## 3. A finding the code review missed

`ListenerState.processedIds : Array String` accumulates one event id per event a listener has ever
seen, **and is never pruned**. `Listener.lean:1141` then does:

```lean
if state.processedIds.contains eventId then continue
```

`Array.contains` is a linear scan, inside a loop over incoming items — O(n·m) per poll — and the
whole array is re-parsed and re-serialised on every tick of every listener. A listener watching a
busy repository for a few months holds tens of thousands of ids and pays for all of them, every
`interval_seconds`, forever.

This is the single clearest argument for the refactor, because no amount of in-memory caching fixes
it: the data structure is wrong. It wants to be a table with a unique index:

```sql
CREATE TABLE IF NOT EXISTS listener_seen_events (
  listener    TEXT NOT NULL,
  event_id    TEXT NOT NULL,
  seen_epoch  INTEGER NOT NULL,
  PRIMARY KEY (listener, event_id)
) WITHOUT ROWID;
```

Membership becomes an index probe, insertion becomes `INSERT OR IGNORE`, and retention becomes
`DELETE FROM listener_seen_events WHERE seen_epoch < ?` — which is the pruning that does not exist
today at all.

---

## 4. Schema: hot columns plus a payload

`QueueEntry` has **38 fields** and is visibly still growing (`listenerName`, `goal`, `authMode` and
`concertStepKey` are all recent). A fully normalised 38-column table would mean a schema migration
every time a field is added, and `ALTER TABLE` churn of the kind `Taxis/Db/Schema.lean` has already
accumulated fifteen lines of.

So: **columns for what is queried, one JSON payload for the rest.** Reading the actual predicates in
`pendingCandidates`, `claimDecision`, `overviewApi`, `concertDetailApi`, `cancelEntry` and the two
dispatchers, exactly these fields are ever filtered, ordered or grouped on:

```sql
CREATE TABLE IF NOT EXISTS queue_entries (
  id             TEXT PRIMARY KEY,
  created_at     TEXT    NOT NULL,          -- ISO 8601, sorts lexically
  status         TEXT    NOT NULL,          -- pending|running|done|failed|unfinished|cancelled
  priority       INTEGER NOT NULL DEFAULT 10,
  fork           TEXT    NOT NULL,          -- "owner/name", the per-repo slot key
  backend        TEXT,                      -- parallel-safety check
  slot           INTEGER,
  task_id        TEXT,                      -- the run this entry became
  continues_from TEXT,                      -- predecessor's task_id
  concert_id     TEXT,
  project_id     TEXT,
  issue_id       TEXT,
  role           TEXT,
  listener_name  TEXT,
  payload        TEXT    NOT NULL           -- the whole entry as JSON
);

CREATE INDEX IF NOT EXISTS idx_queue_claim   ON queue_entries(status, priority DESC, id);
CREATE INDEX IF NOT EXISTS idx_queue_fork    ON queue_entries(fork, status);
CREATE INDEX IF NOT EXISTS idx_queue_task    ON queue_entries(task_id);
CREATE INDEX IF NOT EXISTS idx_queue_concert ON queue_entries(concert_id);
CREATE INDEX IF NOT EXISTS idx_queue_role    ON queue_entries(project_id, role, status);
```

Everything else — `prompt`, `systemPrompt`, `tools`, `inputJson`, `prLabels`, `authSources` — lives
in `payload` and is read only when an entry is actually claimed and run.

Two things fall out of this that matter more than the disk layout:

- **The existing `ToJson`/`FromJson` instances become the payload codec, unchanged.** They are
  already written, already exercised by the test suite, and already handle every `Option`, `List`
  and custom enum in the record. The refactor does not have to re-encode 38 fields; it has to
  extract 14 of them into columns.
- **Adding a field to `QueueEntry` needs no migration**, unless it is a field you want to query on.

The same treatment applies to `tasks` (21 fields, queried on `id`, `created_at`, `status`, `fork`,
`series`, `project_id`, `issue_id`) and `concert_runs` (6 fields, all hot — that one is fully
normalised).

Usage is the exception and should be **fully normalised**, because its whole problem is
read-modify-write on a nested array:

```sql
CREATE TABLE IF NOT EXISTS usage_state (
  backend       TEXT NOT NULL,
  label         TEXT NOT NULL,
  fetched_epoch INTEGER,
  last_used_tick INTEGER,
  last_error    TEXT,
  poll_after    INTEGER,
  block_json    TEXT,
  PRIMARY KEY (backend, label)
);

CREATE TABLE IF NOT EXISTS usage_windows (
  backend      TEXT NOT NULL,
  label        TEXT NOT NULL,
  kind         TEXT NOT NULL,
  scope        TEXT,
  start_epoch  INTEGER NOT NULL,
  last_epoch   INTEGER NOT NULL,
  reset_epoch  INTEGER,
  peak_percent INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY (backend, label, kind, scope, start_epoch)
);
CREATE INDEX IF NOT EXISTS idx_usage_windows_age ON usage_windows(last_epoch);
```

`markUsed`, `markLimited` and `markOk` become single `UPDATE`s inside `withTransaction` — which is
what closes M4. `pruneWindows` and `maxWindowsPerSeries` become a `DELETE` with a subquery, so the
retention logic that `Usage.lean` currently implements by hand in Lean is deleted, not ported.

And the log index, which is what makes P3 possible for logs:

```sql
CREATE TABLE IF NOT EXISTS task_logs (
  task_id    TEXT NOT NULL,
  attempt    INTEGER NOT NULL,     -- 0 = <id>.log, n = <id>.retryN.log
  path       TEXT NOT NULL,
  bytes      INTEGER NOT NULL,
  lines      INTEGER NOT NULL,
  PRIMARY KEY (task_id, attempt)
);
```

The file itself stays on disk. This row is what lets `loadTaskLog` stop probing 100 candidate paths
and stop reading a file whole to count its lines (P2) — and what lets a retention sweep find the
log files belonging to deleted tasks.

---

## 5. Module layout

Mirroring `Taxis/Db/`, because matching the author's existing pattern is worth more than any
improvement on it:

```
Orchestra/Db.lean              -- re-exports
Orchestra/Db/Connection.lean   -- connect, connectReadOnly, withTransaction, withReadTransaction
Orchestra/Db/Schema.lean       -- schemaSql, schemaVersion, migrate
Orchestra/Db/Queue.lean        -- queue_entries + concert_runs
Orchestra/Db/Tasks.lean        -- tasks + series + task_logs
Orchestra/Db/Usage.lean        -- usage_state + usage_windows
Orchestra/Db/Listener.lean     -- listener_state + listener_seen_events
```

`Connection.lean` can be taken almost verbatim from taxis:

```lean
def connect (path : System.FilePath) : IO Conn := do
  let db ← SQLite.openWith path .readWriteCreate (busyTimeoutMs := 5000)
  db.exec "PRAGMA foreign_keys = ON"
  db.exec "PRAGMA journal_mode = WAL"
  pure db
```

The existing modules keep their public names and become thin wrappers, so `Queue.loadEntry` still
exists and still means the same thing. New query functions are *added* next to them —
`Queue.pendingFor`, `Queue.activeCountByFork`, `Queue.byConcert` — and call sites move over one at
a time.

---

## 6. The two-container question

This is the design's one genuinely load-bearing risk, and it resolves well.

`docker-compose.yaml` runs `orchestrad queue` and `orchestrad dashboard` as **separate containers**
sharing `./data` by bind mount. Two processes on one SQLite file is exactly what WAL plus a busy
timeout is for, and taxis has already written the reasoning down in `connectReadOnly`'s docstring:

> WAL journalling — set once, by `connect`, since it is a property of the file rather than of a
> connection — is what makes these worth having: readers on separate connections proceed at the same
> time as each other and as a writer, where readers sharing one connection cannot.

So the split becomes:

- **daemon** → `connect` (read-write, WAL, `busy_timeout = 5000`)
- **dashboard** → `connectReadOnly`

That is a real improvement on today's arrangement, not just a port. The dashboard currently writes
`/config` — which is how the review's finding S6 turns the dashboard password into code execution in
the credentialed container. Making the dashboard a **provably read-only consumer of state** removes
one direction of that coupling. It does not close S6 by itself (listener *configs* are still files
the dashboard writes and the daemon executes), but it stops the exposed container from being able to
corrupt or forge queue and task state at all.

Two constraints to document:

- **WAL needs a real local filesystem.** Bind-mounting a host directory is fine; NFS, SMB or a
  network volume is not. The compose file already uses host directories and says why, so this costs
  a sentence in `docker/README.md`, not a design change.
- **`orchestrad serve`** runs both halves in one process. That one opens a single read-write
  connection and shares it; `withReadTransaction` is what keeps a multi-statement read
  (`overviewApi` touches four tables) from seeing a commit land midway.

---

## 7. Migration

`Orchestra/Migrate.lean` already exists and already has the shape for this — it copies the legacy
`~/.agent` layout to XDG with a `skip / copy` log per item. Extend it rather than adding a second
mechanism.

```
orchestra migrate --to-db
  read  data/queue/*.json        →  INSERT INTO queue_entries
  read  data/tasks/*.json        →  INSERT INTO tasks
  read  data/concerts/*.json     →  INSERT INTO concert_runs
  read  data/series/*.json       →  INSERT INTO series
  read  data/usage/*/*.json      →  INSERT INTO usage_state, usage_windows
  read  data/listeners/state/*   →  INSERT INTO listener_state, listener_seen_events
  scan  data/logs/**/*.log       →  INSERT INTO task_logs (index only; files untouched)
```

Properties it needs, all of which the existing code's idiom already supports:

- **Non-destructive.** The JSON files are left in place. Rollback is `rm data/orchestra.db` plus
  reverting the binary — no data is at risk during the trial period.
- **Idempotent.** `INSERT OR REPLACE` throughout, so a half-finished run is fixed by running it
  again. This matters because a file that fails to parse should not abort the migration; it should
  be reported and skipped, the same way `loadEntry` already tolerates one.
- **Guarded by a version row**, following `Taxis.Db.migrate`: `schema_version` is read, the DDL is
  applied idempotently, incremental `ALTER`s follow, the version is written back.
- **Runs automatically on daemon start** when the database is absent but the directories are not,
  so an existing install upgrades without a manual step — with the same one-time deprecation notice
  `Dirs.configBase` already prints for the legacy config path.

---

## 8. Phasing

Ordered so each phase is independently shippable and independently revertible, smallest first.

**Phase 0 — foundation.** `Db/Connection.lean`, `Db/Schema.lean`, `orchestra migrate --to-db`
skeleton, and `sqlite3` added to the runtime image for inspection. Nothing reads from the database
yet. *Diff: small. Risk: none — nothing depends on it.*

**Phase 1 — pilot: listener state.** The right pilot: fewest call sites, the clearest win (§3), and
a blast radius of one subsystem. `loadListenerState`/`saveListenerState` keep their signatures;
`processedIds` membership becomes an index probe; a `seen_epoch` retention sweep is added.
*Diff: ~250 lines. Risk: low — a listener that misbehaves re-fires an event, it does not lose work.*

**Phase 2 — usage.** Self-contained, and it closes a real race (M4). `modifyState`'s
read-modify-write becomes `withTransaction`; `pruneWindows` becomes a `DELETE`. *Diff: ~350 lines,
much of it deletion.*

**Phase 3 — queue, tasks, concerts, series.** The big one, and the one that pays for P1, P3 and P4.
Done in two steps: **3a** swaps the bodies behind the existing signatures (no call sites change,
behaviour identical, immediately revertible); **3b** replaces the hot scans with queries —
`claimNextEntry` stops calling `loadAllEntries` and starts calling `Queue.claimCandidates`, and
`overviewApi` becomes five `COUNT(*)`s instead of four full directory reads. *Diff: ~900 lines.*

**Phase 4 — log index and retention.** `task_logs` rows, `loadTaskLog` reads a bounded tail using
the indexed byte offset (P2), and a retention sweep on daemon start deletes rows and files past a
configurable window (P3). *Diff: ~300 lines.*

**Phase 5 — read-only dashboard.** Switch `orchestrad dashboard` to `connectReadOnly`. Cheap, and
it is the phase that makes the §6 property real.

---

## 9. What this fixes, and what it does not

Being precise about this matters, because a refactor this size attracts more credit than it earns.

**Fixed outright**

- **P1** — the claim loop and the SSE tick become indexed queries. `pendingCandidates` is
  `SELECT … WHERE status='pending' ORDER BY priority DESC, id` against `idx_queue_claim`.
- **P3** — retention becomes `DELETE … WHERE created_at < ?`, one statement per table, instead of a
  feature nobody has written.
- **P4** — `loadAllEntries`-to-find-one becomes `SELECT … WHERE id = ?`.
- **M3** — atomicity stops being a per-file concern. `writeFileAtomically`'s temp-file-and-rename
  dance, its PID-collision caveat and its missing `fsync` all become `withTransaction`.
- **M4** — the usage read-modify-write race, via `BEGIN IMMEDIATE`.
- **§3** — the unbounded `processedIds` scan.

**Partly fixed**

- **P2** — the log index removes the 100-path probe and the full read *to count lines*. Actually
  tailing the file efficiently is still a separate change (seek from the end); the database makes it
  easy, it does not do it.
- **M5** — the parse-error-means-absent conflation goes away for records in the database (a row is
  present or it is not), but the JSON payload column can still fail to decode, so the logging fix
  is still wanted.

**Not fixed — do not let this refactor absorb them**

- **S1, S2** — the sandbox escapes. Entirely unrelated code, and far more urgent.
- **S3, S4** — credentials in `argv` and in the debug line.
- **The taxis claim race** — remote, not local (§2).
- **M1** — `Daemon.run` is still 634 lines afterwards. Phase 3b is a good moment to extract
  `claimNextEntry`, but that is a separate decision.

---

## 10. Costs and risks, honestly

**The `cat`/`jq` property is lost, and the compose file explicitly values it:**

> Plain host directories rather than named volumes, so config and state are inspectable and editable
> without `docker cp`.

That sentence is about `./config` and `./data` together. Config stays inspectable — it does not
move. State does not: `cat data/queue/abc.json` becomes
`sqlite3 data/orchestra.db 'select * from queue_entries'`. Mitigations: `orchestra queue list` and
`orchestra task show` already exist and already print this state, `sqlite3` goes into the image in
Phase 0, and the payload column keeps every record readable as JSON. But it is a real ergonomic
regression for anyone used to poking at the files, and it should be called out in the changelog
rather than discovered.

**Hand-editing state to unstick something stops working.** Today, an entry wedged in `running`
after a daemon crash can be fixed with a text editor. Afterwards it needs a CLI command — which
suggests `orchestra queue set-status <id> <status>` should land in Phase 3, not later.

**leansqlite is young and uses `experimental.module`.** It is a Lean FRO library pinned to the
toolchain version, and taxis depends on it in production — but it is version 0.1.0, and the pin is
already effectively controlled by taxis's own pin. Promoting it to a direct `require` in
`lakefile.lean` means orchestra chooses its version rather than inheriting one, which is the safer
arrangement and should happen in Phase 0. (This is also the moment to fix **M10** and pin `Cli` and
`Yaml` to SHAs.)

**Tests get better, not worse.** `Project.setProjectsDirOverride` and
`Listener.listenerConfigDirOverride` already exist so tests can redirect to a temp directory. The
database equivalent is `SQLite.open ":memory:"`, which is faster, needs no cleanup, and makes the
storage layer testable in a way the filesystem version is not. `ParallelQueueTest` in particular
gets to test real concurrent claims against a real database instead of a pure function.

**I could not typecheck any of this here.** There is no Lean toolchain in this environment and no
`.lake` — a cold build is what the Docker CI job budgets 90 minutes for. Everything above is derived
from reading the source, the leansqlite API at `v4.31.0`, and taxis's use of it. The API signatures
quoted (`SQLite.openWith`, `db query!"…" as Row`, `deriving SQLite.Row`, `lastInsertRowId`) are
copied from leansqlite's own sources and tests, not recalled. But no line of the schema or the
migration has been compiled, and the first implementation phase should expect to spend time on
`SQLite.Row` deriving for types like `Option Taxis.IssueId` that will need a `QueryParam` instance
written by hand.

**The honest alternative.** An in-memory queue index in the daemon plus an mtime-keyed cache in the
dashboard — the fix proposed for P1 in the review — recovers most of the *performance* for perhaps
10% of this diff, with no format change and no migration. What it cannot give you is the rest:
transactional atomicity (M3), cross-process consistency between the two containers (M4), retention
as a statement rather than a feature (P3), and set membership for `processedIds` (§3). Those four
are the case for the database. If only the performance mattered, the cache would be the better
trade.

---

## 11. Recommendation

Do it, in this order, and not before the security work:

1. **S1–S4 first.** Smaller, more urgent, and they touch none of this code.
2. **Phase 0 + Phase 1** as one pull request — the foundation and the listener pilot. It is small
   enough to review by eye, it fixes a real unbounded-growth bug, and it proves the pattern against
   the real build before anything larger commits to it.
3. **Reassess after Phase 1.** If the pilot lands cleanly, Phases 2–5 follow mechanically. If
   `SQLite.Row` deriving turns out to fight orchestra's record types, the cost estimate above is
   wrong and the in-memory-index alternative in §10 is the fallback — with Phase 1 still worth
   keeping on its own merits.
