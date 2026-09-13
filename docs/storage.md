# Storage: moving orchestra's record stores to SQLite

Design for migrating orchestra's file-based record stores to a SQLite database through
[chrisflav/db](https://github.com/chrisflav/db). This is the document the implementation follows;
once the port has landed it is also the description of where things live.

## Why

Every record store today is a directory of one JSON file per record, and every listing reads the
whole directory: `TaskStore.loadAllTasks`, `Queue.loadAllEntries`, `Queue.loadAllConcertRuns` and
`Interactive.loadAllSessions` open, read and parse every file they own, and then sort in memory.
Those listings run on every dashboard request, on every daemon claim (`Daemon.lean`, the claim
loop), in the task reaper every 30 seconds, in the dispatcher on every listener tick, and in the
spawn-policy check of every `queue_task` call. `Interactive.readEvents` reads the whole
transcript backwards to answer "anything after seq N", three times a second per attached client.
With a few thousand tasks this is thousands of `open`/`read`/`parse` per tick.

A database turns each of those into one query, gives indexed lookups by id and by status, and gives
paging and counting without materialising the collection.

## Scope

**Migrated** (records orchestra itself writes and reads back):

| store | today | table(s) |
| --- | --- | --- |
| task records | `<data>/tasks/<id>.json` | `task` |
| series pointers | `<data>/series/<name>.json` | `series` |
| queue entries | `<data>/queue/<id>.json` | `queue_entry` |
| concert runs | `<data>/concerts/<id>.json` | `concert_run` |
| interactive sessions | `<data>/interactive/<id>/session.json` | `interactive_session` |
| interactive transcripts | `<data>/interactive/<id>/events.jsonl` | `interactive_event` |
| usage source state | `<data>/usage/<backend>/<label>.json` | `usage_source` |
| usage history | `<data>/usage/<backend>/<label>.history.json` | `usage_window` |
| listener state | `<data>/listeners/state/<name>.json` | `listener_state` |

**Not migrated** (not records, or not orchestra's to reshape): configuration under
`<config>/` (config.json, listeners, roles, prompts, skills, identities, secrets), task logs
(`<data>/logs`, streamed text), clones and workspaces (`<data>/repos`, `<data>/workspaces`),
agent memory directories (`<data>/memory`, `<data>/identities/<name>/memory`), per-project role
overrides (`<data>/projects/<pid>/roles`, explicitly config), `dashboard.secret`, and the
daemon's pid, socket and log files under `<data>/queue/`.

## The database

One file, `<data>/orchestra.db`, under `Dirs.dataBase` — so `Dirs.setDataBaseOverride` (what
`withTempData` sets in tests) redirects it along with everything else.

**Processes.** Three processes touch it: `orchestrad` (daemon and/or dashboard; in the compose
deployment those are two containers sharing `/data`), and the `orchestra` CLI, which enqueues
and reads history directly. SQLite handles that with WAL mode and a busy timeout; nothing else is
needed.

**Connections.** `Orchestra.Store.run (x : Sqlite.M α) : IO α` opens a fresh connection per
call, sets `busy_timeout` (10 s), runs `x`, and lets the connection close. No shared connection:
the daemon runs workers, the reaper, listeners and the HTTP server on separate threads, and one
shared SQLite connection would let their transactions interleave. Opening a connection costs
microseconds against a file the OS has cached; it is not what was slow. `Orchestra.Store.transaction`
is `run (withTransaction x)`.

**Schema initialisation.** The first `run` in a process against a given path applies the
declared migrations (`Db.Migration.migrate`) and sets `journal_mode=WAL` and `synchronous=NORMAL`
(both persist in the file). Guarded by a mutex and remembered per path, so a test that switches
`Dirs.dataBase` gets a fresh schema and production pays once. Two processes migrating at once is
safe by the library's design (the migration record is written first, under the primary key).

**Migrations, not `autoUpdate`.** The schema is a list of declared migrations in
`Orchestra/Store/Schema.lean`, applied at startup; the first is `0001_initial`. A test asserts
that `Db.Migration.planSteps migrations target` is empty, so a model change without a migration
fails the suite.

## Library additions (chrisflav/db)

Done on a branch `orchestra-support` in a worktree at `~/db-orchestra`, off `master`.

1. **Merge the `text-literal` branch** (one commit, `704232a`, adds `DBExpr.text` so a `String`
   constant compares with a `text` column in `query%`). Every id, name and status column here is
   `text`, and every lookup is `guard row.id = id`, so this is a hard requirement.
2. **A `float` column type.** `DBType.float`, `Value := Float`; `REAL` on SQLite, `double
   precision` on PostgreSQL; `HasDBType Float`; a `DBExpr.float` literal and the DSL embedding a
   `Float` constant as one; introspection reading the type back on both backends; `Generate`
   rendering it. Literals must be rendered so they round-trip: Lean's `Float.toString` prints six
   decimals (`1e-7` prints as `0.000000`), so render 17 significant digits from the exact value
   (`Float.frExp` gives `m ∈ [0.5, 1)` and `e` with `x = m·2^e`; `m·2^53` is an exact integer).
   Reading parses what the backends print (`0.1`, `1e-07`, `1.0e+20`, `-3`) via
   `Float.ofScientific`. NaN and infinities have no SQL literal; refuse to render them.
   Orchestra needs this for `budget`, `costUsd`.
3. **A declared primary key on `@[model]`**, e.g. `@[model (dbName := "task")
   (primaryKey := ["id"]) orchestra]`, composite allowed (`["session_id", "seq"]`). Validated
   against the fields; incompatible with an `AutoKey` field. Today a model without `AutoKey` has
   no key at all, and `upsert` then has nothing to conflict on.
4. **`HasModel.save (x : α)`**: insert-or-replace on the table's primary key, setting every
   non-key column — the "write this record" every store here does. Fails with a clear message
   for a model without a primary key.
5. **Pin `leansqlite` to `v4.31.0`** (`0be4df908d1a8e75b58961041e2b4973692623df`), the
   revision orchestra already gets through taxis; the two commits between it and the current pin
   are CI/toolchain chores. One revision, so a dependent does not see two.

Everything else orchestra needs already exists: `query%` with `guard`/`order_by`/`limit`/
`offset` (dynamic limits via `QuerySet.limit`/`.offset` on the elaborated set), `HasModel.count`,
`HasModel.update`/`delete` on a condition, `withTransaction`, declared migrations with
`createIndex`, `insertIfAbsent`.

## Orchestra: the `Orchestra.Store` module

Named `Store` rather than `Db` so that `Db.Migration` and friends resolve to the library inside it.

- `Orchestra/Store/Schema.lean` — `initialize_database orchestra`; one `@[model]` row structure
  per table; the index recipe; `migrations : List Db.Migration.Migration`.
- `Orchestra/Store/Connection.lean` — `path`, `run`, `transaction`, schema initialisation.
- `Orchestra/Store.lean` — re-exports both.
- `Orchestra/Store/Import.lean` — the one-time import of the JSON files (imports the stores, so
  it sits above them; see below).

### Row structures

Each store keeps its domain record (`TaskRecord`, `QueueEntry`, `SessionRecord`, …) exactly as it
is — those are the API's shapes and carry `ToJson`/`FromJson` that the HTTP API and the legacy
import both use. Beside it the store gets a flat **row structure** whose fields are only what
`@[model]` maps: `String`, `Int`, `Bool`, `Float`, and `Option` of those. Two total functions
convert: `toRow : Record → Row` and `ofRow? : Row → Except String Record`. A row that does not
convert (an unknown status name, say) is reported to stderr and skipped, which is what a file that
did not parse does today.

Conventions for the mapping:

- Field and column names are `snake_case`, matching the JSON keys the records already use
  (`created_at`, `continues_from`, `last_event_seq`).
- Enumerations (`TaskStatus`, `QueueStatus`, `ConcertStatus`, `SessionStatus`, `TaskMode`,
  `MemoryMode`, `AuthMode`) are `String` columns holding the lower-case name the JSON instances
  already use; convert through those instances (`toJson`/`fromJson?` on a `Json.str`).
- `Nat` fields are `Int` columns (`.toNat` on the way out; `priority`, `slot`, `turn_count`,
  `seq`, `samples`, percentages, epochs).
- `Repository` is `"owner/name"` (`Repository.toString`/`parse`); an `Option RepoPair` is two
  nullable columns `upstream`, `fork`.
- `Taxis.IssueId` is its string form.
- Anything structured with no scalar form — `List String` (`tools`, `auth_sources`, `pr_labels`,
  …), `ResultType`, `SpawnPolicy`, `inputJson`/`outputJson`, the usage `limits` and `blocks`
  arrays, listener `processed_ids` and `dispatches` — is a `text` column holding
  `Json.compress (toJson x)`, read back with `Json.parse` + `fromJson?`. Empty lists are stored
  as `[]`, not as NULL; `Option` distinguishes absent.
- Timestamps stay the RFC 3339 UTC strings the records carry (`YYYY-MM-DDTHH:MM:SSZ`, all from
  `TaskStore.currentIso8601`). That format orders lexicographically, so `ORDER BY created_at
  DESC, id DESC` is the newest-first order `Time.sortNewestFirst` computes today, and a `since`
  filter compares against `Time.secsToIso8601 since`.

### Tables

Primary keys in **bold**; `?` marks a nullable column; `json` is a `text` column as above.

- `task` — **id**, created_at, upstream?, fork?, mode, prompt, goal?, session_id?, status,
  continues_from?, series?, backend?, model?, agent?, system_prompt?, prepend_prompt?,
  budget float?, priority int, project_id?, issue_id?, role?, identity?.
  Indexes: (created_at, id), (status), (series), (issue_id).
- `series` — **name**, latest_task_id.
- `queue_entry` — **id**, created_at, status, upstream?, fork?, mode, prompt, goal?, agent?,
  system_prompt?, prepend_prompt?, backend?, model?, continues_from?, series?, task_id?,
  slot int?, config_path?, budget float?, memory, identity?, auth_source?, auth_sources json,
  auth_mode?, tools json?, read_only bool, priority int, concert_step_key?, concert_id?,
  input_type json, output_type json, input_json json?, output_json json?, issue_number int?,
  project_id?, issue_id?, role?, pr_labels json, triage_add_labels json,
  triage_remove_labels json, listener_name?, spawn_policy json?, spawned_by?, scope_root?.
  Indexes: (created_at, id), (status), (task_id), (concert_id), (spawned_by),
  (listener_name), (project_id), (issue_id).
- `concert_run` — **id**, started_at, status, name?, workflow_file?, finished_at?.
  Index: (started_at, id).
- `interactive_session` — **id**, status, created_at, last_activity_at, ended_at?, upstream,
  fork, backend, model?, budget float, slot int, agent_session_id?, agent_started bool,
  resumed_from?, tools json?, system_prompt?, identity?, turn_count int, cost_usd float,
  last_event_seq int, title?, error?. Indexes: (created_at, id), (status).
- `interactive_event` — **session_id, seq**, occurred_at, doc json. `doc` is the whole
  `TranscriptEvent` JSON exactly as a transcript line is written today, so `readEvents` keeps
  handing back parsed JSON a newer orchestra may have written fields into.
- `usage_source` — **backend, label**, fetched_epoch int?, limits json, blocks json,
  last_used_tick int?, last_error?, poll_after int?.
- `usage_window` — **id** (`AutoKey`), backend, label, kind, scope?, reset_epoch int?,
  start_epoch int, last_epoch int, peak_percent int, last_percent int, samples int.
  Index: (backend, label, id). `id` order is insertion order, which is the oldest-first order
  the history functions work over.
- `listener_state` — **name**, last_checked, enabled bool, processed_ids json, dispatches json.
- `legacy_import` — **store**, imported_at, records int. One row per store the import has
  carried over; see below.

### Store APIs

Every existing store function keeps its name and signature and is reimplemented over the
database, so callers compile unchanged:

- `TaskStore.saveTask` (`HasModel.save`), `loadTask`, `loadAllTasks`, `latestInSeries`,
  `updateSeriesPointer`.
- `Queue.saveEntry`, `loadEntry`, `loadAllEntries`, `saveConcertRun`, `loadConcertRun`,
  `loadAllConcertRuns`.
- `Interactive.saveSession`, `loadSession`, `loadAllSessions`, `appendEvent`, `readEvents`.
  `readEvents id after limit` becomes `WHERE session_id = id AND seq > after ORDER BY seq LIMIT
  limit` plus a count; `appendEvent` is one insert. The write-and-rename and torn-line
  machinery goes away with the files.
- `Usage.loadState`, `saveState`, `loadHistory`, `saveHistory`, `recordPoll`. The pure
  `recordWindows`/`pruneWindows` are untouched; `saveHistory` replaces a source's rows
  (delete + insert) in one transaction.
- `Listener.loadListenerState`, `saveListenerState`.

The per-store directory overrides (`Interactive.setSessionsDirOverride`,
`Listener.setListenerStateDirOverride`) are removed: `Dirs.setDataBaseOverride` covers the
database, and tests use `withTempData`. The directory functions (`tasksDir`, `queueDir`,
`concertsDir`, `usageDir`, …) survive only where a non-record file still lives there (pid, socket,
log) or renamed `legacy…Dir` for the import.

`Listener.migrateListenerStateNames` (the carry-over from in-file names to file names) is
retired: the import keys state by the file's stem, which is what it established.

### Query-shaped access

Once the stores are on the database, the callers that load everything to keep a few rows switch
to queries. The functions to add and the sites to move:

- `Queue.pendingEntries : IO (Array QueueEntry)` — status `pending`, in `claimOrder`. Used by
  `claimDecision` (`Daemon.lean` claim loop). `claimDecision` also looks up the predecessor of a
  continuation among all entries by `taskId`; give it `Queue.entryForTask (taskId : String) : IO
  (Option QueueEntry)` instead of the array.
- `Queue.activeEntries` (`pending` or `running`): `hasActiveEntryForListener`, both dispatcher
  sites in `Listener.lean`, `orchestra status` in `Main.lean`, `Project/Cli.lean`
  `findOrphanedIssues`.
- `Queue.runningEntries`: the task reaper (`Daemon.lean`), `cancelStaleRunningConcerts`-style
  startup sweeps.
- `Queue.countSpawnedBy (taskId) : IO Nat`: `TaskRunner.enqueueTaskImpl`.
- `Queue.entriesOfConcert (concertId)`: `Dashboard.concertDetailApi`.
- `Queue.findEntry (id)` matching `id` or `task_id`: `Dashboard.taskDetailApi`,
  `Dashboard.cancelEntry`.
- `Queue.countByStatus : IO (QueueStatus → Nat)` and `TaskStore.count : IO Nat`,
  `TaskStore.recent (n : Nat)`: `Dashboard.overviewApi`.
- `TaskStore.page` / `Queue.entriesPage` / `Queue.concertRunsPage` /
  `Interactive.sessionsPage` taking `(since? : Option Int) (offset limit : Nat)` and returning
  `(items, total)`: the four `pageOver` call sites in `Dashboard.lean`, and the `--limit`
  listings in `Main.lean`.
- `TaskStore.tasksForIssue (issueId)`, `TaskStore.tasksInSeries (series)`: `Project/Cli.lean`.

`loadAllTasks`/`loadAllEntries` remain for the callers that genuinely want everything
(`queue retry`, startup reconciliation), now one query each.

## Legacy import

`Orchestra.Store.Import.run : IO Unit`, called once at startup by `orchestrad` (before the daemon
or dashboard starts) and by the `orchestra` CLI's `main`. For each store whose legacy directory
exists and has no row in `legacy_import`: inside one transaction, insert the marker row with
`insertIfAbsent` (if it was already there, another process got here first — skip), read every
legacy file with the existing `FromJson` instances, and insert the rows. Files that do not parse
are reported and skipped, as their loaders did. Transcripts are imported line by line, the same
tolerance as `readEvents` has today. Prints one line per store imported, saying how many records
and that the directory can be deleted; the files are left in place. Silent when there is nothing
to do.

`orchestra migrate` (the `~/.agent` → XDG copy) is unchanged: it copies the legacy directories,
which the import then picks up.

## Build and deployment

The library is Lean plus `leansqlite`, which orchestra already builds through taxis, so
`docker/Dockerfile` and `container/configuration.nix` need no new packages. The PostgreSQL
option stays off. While the library branch is unpushed, orchestra's `lakefile.lean` requires it
by path (`/home/christian/db-orchestra`); once it is on GitHub that line becomes
`require db from git "https://github.com/chrisflav/db" @ "<rev>"`.

## Out of scope, worth doing after

- `TaskStore.currentIso8601` spawns `date` for every timestamp; `Std.Time` can format one.
- `processed_ids` is a JSON array in one row; a listener that never prunes it could get its own
  table with a membership index.
- The library could grow a partial-decoding `HasDBType` (enumerations as typed columns rather
  than strings) and `AVG` over the new float type.
