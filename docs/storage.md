# where the records live

Orchestra's record stores are rows of one SQLite database, `<data>/orchestra.db`, reached through
[chrisflav/db](https://github.com/chrisflav/db). This is the description of what is there: the
schema, the connection, the store APIs, and the one-time import of the JSON directories the
records used to be.

## why

Every record store used to be a directory of one JSON file per record, and every listing read the
whole directory: `TaskStore.loadAllTasks`, `Queue.loadAllEntries`, `Queue.loadAllConcertRuns` and
`Interactive.loadAllSessions` opened, read and parsed every file they owned and then sorted in
memory. Those listings ran on every dashboard request, on every daemon claim, in the task reaper
every thirty seconds, in the dispatcher on every listener tick, and in the spawn-policy check of
every `queue_task` call. `Interactive.readEvents` read the whole transcript backwards to answer
"anything after seq N", three times a second per attached client. With a few thousand tasks that
was thousands of `open`/`read`/`parse` per tick.

Each of those is one query now, with indexed lookups by id and by status, and paging and counting
that do not materialise the collection. On a queue of five thousand entries, reading all of them
takes about 350 ms; asking for the fifty that are pending takes about 5 ms, and one page of
twenty task records out of five thousand takes about 3 ms.

## scope

**In the database** (the records orchestra itself writes and reads back):

| store | before | table(s) |
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

**Still files** (not records, or not orchestra's to reshape): configuration under `<config>/`
(config.json, listeners, roles, prompts, skills, identities, secrets), task logs (`<data>/logs`,
streamed text), clones and workspaces (`<data>/repos`, `<data>/workspaces`), agent memory
directories (`<data>/memory`, `<data>/identities/<name>/memory`), per-project role overrides
(`<data>/projects/<pid>/roles`, explicitly config), `dashboard.secret`, and the daemon's pid,
socket and log files under `<data>/queue/`.

## the database

One file, `<data>/orchestra.db`, under `Dirs.dataBase` — so `Dirs.setDataBaseOverride`, which is
what `withTempData` sets in tests, redirects it along with everything else.

**Processes.** Three touch it: `orchestrad` as the daemon, `orchestrad` as the dashboard (in the
compose deployment those are two containers sharing `/data`), and the `orchestra` CLI, which
enqueues and reads history directly. WAL mode and a busy timeout are the whole of what SQLite
needs for that.

**Connections.** `Orchestra.Store.run (x : Sqlite.M α) : IO α` opens a connection, sets
`busy_timeout` to ten seconds, runs `x`, and lets the connection close.
`Orchestra.Store.transaction` is `run (withTransaction x)`, used by the handful of writes that
are more than one statement — the legacy import, replacing a source's usage history.

There is no shared connection. The daemon runs its workers, the reaper, the listeners and the
HTTP server on separate threads, and one connection between them would let their transactions
interleave: a `withTransaction` on one thread would wrap whatever another thread happened to be
doing. Opening a connection costs microseconds against a file the operating system has cached;
reading a directory of JSON files is what was slow, not this.

**Schema initialisation.** The first `run` in a process against a given path applies the declared
migrations (`Db.Migration.migrate`) and sets `journal_mode=WAL` and `synchronous=NORMAL`, both of
which persist in the file. It is guarded by a mutex and remembered per path rather than globally,
so that a test which switches `Dirs.dataBase` gets a fresh schema and production pays for it
once. Two processes migrating at the same moment is safe by the library's design: the migration's
record is written first, under the primary key of the tracking table, so the second one fails on
the duplicate before it has applied a step.

**Migrations, not `autoUpdate`.** The schema is a list of declared migrations, applied at
startup. The first and so far only one is `0001_initial`, in
`Orchestra/Store/Migrations/Initial.lean` — generated with `Db.Migration.planSteps`/`render` and
committed as source, so the schema a deployment gets is a function of the code rather than of the
database the code happens to find. `OrchestraTest.StoreTest` asserts that
`Db.Migration.planSteps migrations target` is empty, so a model change without a migration fails
the suite rather than the deployment.

## the modules

- `Orchestra/Store/Schema.lean` — `initialize_database orchestra`, one `@[model]` row structure
  per table, the indexes those tables carry (`target`), and `migrations`.
- `Orchestra/Store/Migrations/Initial.lean` — the generated `0001_initial`.
- `Orchestra/Store/Convert.lean` — the conversions between a record's types and a column's:
  enumerations, repository pairs, taxis ids, JSON columns, the `since` bound, and
  `keepConvertible`, which is how a listing reports and skips a row it cannot read.
- `Orchestra/Store/Connection.lean` — `path`, `run`, `transaction`, schema initialisation.
- `Orchestra/Store.lean` — re-exports the three.
- `Orchestra/Store/Import.lean` — the one-time import of the JSON files. It imports every store,
  so it sits above them.

## rows

Each store keeps its domain record (`TaskRecord`, `QueueEntry`, `SessionRecord`, …) exactly as it
was — those are the API's shapes and carry the `ToJson`/`FromJson` the HTTP API and the legacy
import both use. Beside it the store has a flat **row structure** whose fields are only what
`@[model]` maps: `String`, `Int`, `Bool`, `Float`, and `Option` of those. Two total functions
convert: `toRow : Record → Row` and `ofRow? : Row → Except String Record`. A row that does not
convert — an unknown status name, say — is reported on stderr and left out of the listing, which
is what a file that did not parse did.

The mapping:

- Field and column names are `snake_case`, matching the JSON keys the records already use
  (`created_at`, `continues_from`, `last_event_seq`).
- Enumerations (`TaskStatus`, `QueueStatus`, `ConcertStatus`, `SessionStatus`, `TaskMode`,
  `MemoryMode`, `AuthMode`) are `text` columns holding the lower-case name their JSON instances
  write, converted through those instances (`Store.enumColumn`, `Store.enumOfColumn?`).
- `Nat` fields are `int` columns (`priority`, `slot`, `turn_count`, `seq`, `samples`,
  percentages, epochs), `.toNat` on the way out.
- `Repository` is `"owner/name"`; an `Option RepoPair` is two nullable columns, `upstream` and
  `fork`. Both or neither — a row holding half a pair is one `repoOfColumns?` refuses.
- `Taxis.IssueId` is its decimal string.
- Anything structured with no scalar form — `List String` (`tools`, `auth_sources`, `pr_labels`,
  …), `ResultType`, `SpawnPolicy`, `inputJson`/`outputJson`, the usage `limits` and `blocks`
  arrays, listener `processed_ids` and `dispatches` — is a `text` column holding
  `Json.compress (toJson x)`, read back with `Json.parse` and `fromJson?`. An empty list is `[]`
  and not NULL; `Option` is what distinguishes absent.
- `Float` columns (`budget`, `cost_usd`) are the library's `float` type, declared as `double
  precision` on both backends: PostgreSQL's own name for an IEEE 754 double, and a name SQLite
  gives the same `REAL` affinity it gives `REAL`.
- Timestamps stay the RFC 3339 UTC strings the records carry (`YYYY-MM-DDTHH:MM:SSZ`, all from
  `TaskStore.currentIso8601`). That format orders lexicographically, so `ORDER BY created_at
  DESC, id DESC` is the newest-first order `Time.sortNewestFirst` computed in memory, and a
  `since` filter is a string comparison against `Time.secsToIso8601 since`.

Ordering is by the timestamp and not by the id. Ids come from a monotone clock that restarts at
boot, so across a reboot the smaller id is the *newer* record; the id is only there to break
ties.

## tables

Primary keys in **bold**; `?` marks a nullable column; `json` is a `text` column as above.

- `task` — **id**, created_at, upstream?, fork?, mode, prompt, goal?, session_id?, status,
  continues_from?, series?, backend?, model?, agent?, system_prompt?, prepend_prompt?,
  budget float?, priority int, project_id?, issue_id?, role?, identity?.
  Indexes: (created_at desc, id desc), (status), (series), (issue_id).
- `series` — **name**, latest_task_id.
- `queue_entry` — **id**, created_at, status, upstream?, fork?, mode, prompt, goal?, agent?,
  system_prompt?, prepend_prompt?, backend?, model?, continues_from?, series?, task_id?,
  slot int?, config_path?, budget float?, memory, identity?, auth_source?, auth_sources json,
  auth_mode?, tools json?, read_only bool, priority int, concert_step_key?, concert_id?,
  input_type json, output_type json, input_json json?, output_json json?, issue_number int?,
  project_id?, issue_id?, role?, pr_labels json, triage_add_labels json,
  triage_remove_labels json, listener_name?, spawn_policy json?, spawned_by?, scope_root?.
  Indexes: (created_at desc, id desc), (status), (task_id), (concert_id), (spawned_by),
  (listener_name), (project_id), (issue_id).
- `concert_run` — **id**, started_at, status, name?, workflow_file?, finished_at?.
  Index: (started_at desc, id desc).
- `interactive_session` — **id**, status, created_at, last_activity_at, ended_at?, upstream,
  fork, backend, model?, budget float, slot int, agent_session_id?, agent_started bool,
  resumed_from?, tools json?, system_prompt?, identity?, turn_count int, cost_usd float,
  last_event_seq int, title?, error?. Indexes: (created_at desc, id desc), (status).
- `interactive_event` — **session_id, seq**, occurred_at, doc. `doc` is the whole
  `TranscriptEvent` JSON exactly as a transcript line was written, so `readEvents` keeps handing
  back parsed JSON a newer orchestra may have put fields into.
- `usage_source` — **backend, label**, fetched_epoch int?, limits json, blocks json,
  last_used_tick int?, last_error?, poll_after int?.
- `usage_window` — **id** (`AutoKey`), backend, label, kind, scope?, reset_epoch int?,
  start_epoch int, last_epoch int, peak_percent int, last_percent int, samples int.
  Index: (backend, label, id). `id` order is insertion order, which is the oldest-first order the
  history functions fold over.
- `listener_state` — **name**, last_checked, enabled bool, processed_ids json, dispatches json.
- `legacy_import` — **store**, imported_at, records int. One row per store the import has carried
  over; see below.

Every index is declared on `Store.target` rather than by `@[model]`, which of a structure's
fields deserve one not being something the structure says. Index names are unique across the
whole database on both backends, hence the `idx_<table>_<columns>` spelling. Each one serves a
query the stores actually run.

## the store APIs

Every function the file stores had kept its name and signature, so the callers compiled
unchanged:

- `TaskStore.saveTask` (one `HasModel.save`), `loadTask`, `loadAllTasks`, `latestInSeries`,
  `updateSeriesPointer`, and `allSeries`, which is new — `orchestra series` used to answer by
  listing the directory the pointers were files in, and there is no directory to list.
- `Queue.saveEntry`, `loadEntry`, `loadAllEntries`, `saveConcertRun`, `loadConcertRun`,
  `loadAllConcertRuns`.
- `Interactive.saveSession`, `loadSession`, `loadAllSessions`, `appendEvent`, `readEvents`.
  `readEvents id after atMost` is `WHERE session_id = id AND seq > after ORDER BY seq LIMIT
  atMost` plus a count; `appendEvent` is one insert. The write-and-rename and the torn-line
  tolerance went with the files — the tolerance survives only in the import, which is the one
  place a torn line can still turn up.
- `Usage.loadState`, `saveState`, `loadHistory`, `saveHistory`, `recordPoll`. The pure
  `recordWindows`/`pruneWindows` are untouched; `saveHistory` replaces a source's rows, delete
  and insert, in one transaction.
- `Listener.loadListenerState`, `saveListenerState`.

The per-store directory overrides (`Interactive.setSessionsDirOverride`,
`Listener.setListenerStateDirOverride`) are gone: `Dirs.setDataBaseOverride` covers the database
and tests use `withTempData`. The directory functions survive only where a non-record file still
lives there — `Queue.queueDir` for the pid, the socket and the log; `TaskStore.tasksDir` for the
`--debug` transcript — or renamed `legacy…Dir` for the import.
`Listener.migrateListenerStateNames`, which carried state written under a config's old in-file
name over to its file name at every daemon startup, is retired: the import keys state by the
file's stem, which is what that established.

### query-shaped access

Beside them are the functions that exist because the database can answer a question the array
code had to read everything to answer. Each is one statement inside one `Store.run`.

| function | answers |
| --- | --- |
| `Queue.pendingEntries` | the pending entries, in `claimOrder` |
| `Queue.activeEntries` | pending or running, newest first |
| `Queue.runningEntries` | running, newest first |
| `Queue.entryForTask (taskId)` | the entry whose run became that task |
| `Queue.findEntry (id)` | the entry under either of the two ids it answers to |
| `Queue.countSpawnedBy (taskId)` | how many entries a task has queued |
| `Queue.entriesOfConcert (concertId)` | one concert's steps |
| `Queue.countByStatus` | `QueueStatus → Nat`, one count per status |
| `Queue.entriesPage (since?) (skip take)` | one page of the queue, and the total |
| `Queue.concertRunsPage (since?) (skip take)` | one page of the concert history, and the total |
| `TaskStore.count` | how many task records there are |
| `TaskStore.recent (n)` | the newest `n` records |
| `TaskStore.page (since?) (skip take)` | one page of the history, and the total |
| `TaskStore.tasksForIssue (issueId)` | every task recorded against an issue |
| `TaskStore.tasksInSeries (series)` | every task in a named series |
| `Interactive.sessionsPage (since?) (skip take)` | one page of the session list, and the total |

The paging functions take `since?` as epoch seconds and return `(items, total)`, where the total
counts everything `since?` matched *before* the window — the arithmetic `Dashboard.collection`
reports, and the only thing that makes an offset usable. No `since` at all is compared as the
empty string, which every timestamp is above, so a listing stays one query of one shape rather
than two spellings picked at runtime.

`Queue.claimDecision` takes the pending entries and a lookup `String → IO (Option QueueEntry)`
for a continuation's predecessor: the daemon passes `Queue.entryForTask`, one indexed query for
the one continuation being considered, where reading every entry to answer it was most of what a
claim cost. `pendingCandidates` and `claimOrder` stay pure and are tested as such.

`loadAllTasks`, `loadAllEntries`, `loadAllConcertRuns` and `loadAllSessions` remain for the
callers that genuinely want everything — `queue retry`, the startup reconciliation sweeps, the
concert listing on the overview — and are one query each.

## the legacy import

`Orchestra.Store.Import.run : IO Unit`, called once at startup by `orchestrad` before the daemon
or the dashboard starts, and by the `orchestra` CLI's `main`. For each store whose legacy
directory exists and has no row in `legacy_import`: inside one transaction, write the marker row
with `insertIfAbsent` — if it was already there another process got here first, so there is
nothing to do — read every legacy file with the record's existing `FromJson`, and insert the
rows. Files that do not parse are reported and skipped, as their loaders did. Transcripts are
imported line by line with the same tolerance `readEvents` used to have, including a tail torn
mid-character.

It prints one line per store, saying how many records it carried and that the directory can be
deleted, and leaves the files exactly where they are: a deployment that wants the disk back
deletes them when it is ready rather than having the decision made for it. On a fresh
installation, and on every start after the first, it says nothing.

`orchestra migrate` (the `~/.agent` → XDG copy) is unchanged: it copies the legacy directories,
which the import then picks up.

## build and deployment

The library is Lean plus `leansqlite`, which orchestra already builds through taxis, so
`docker/Dockerfile` and `container/configuration.nix` need no new packages. The PostgreSQL option
stays off.

`/data` must be one real directory shared by the daemon and the dashboard containers — they are
two processes on one SQLite file, and WAL mode needs a filesystem that supports it rather than a
network mount.

While the library's `orchestra-support` branch is unpushed, orchestra's `lakefile.lean` requires
it by path (`/home/christian/db-orchestra`); the comment above that line gives the `require db
from git` form it becomes once the branch is on GitHub.

## what the library gained

Orchestra needed five things that [chrisflav/db](https://github.com/chrisflav/db) did not have,
and they are on its `orchestra-support` branch:

1. **`DBExpr.text`**, a `String` constant at type `text` (merged from the `text-literal`
   branch). Every id, name and status column here is `text` and every lookup is
   `guard row.id = id`, so this was a hard requirement.
2. **A `float` column type.** `DBType.float` with `Value := Float`, declared `double precision`
   on both backends, a `DBExpr.float` literal, `HasDBType Float`, introspection reading the type
   back, and `Generate` rendering it. Literals are rendered at seventeen significant digits of
   the exact binary value, so that what is read back is what was written — `Float.toString`
   prints six decimals and would turn `1e-7` into `0`. NaN and the infinities have no SQL
   literal and are refused rather than approximated. `budget` and `costUsd` need this.
3. **A declared primary key on `@[model]`**, e.g. `@[model (dbName := "task")
   (primaryKey := ["id"]) orchestra]`, composite allowed (`["session_id", "seq"]`), validated
   against the fields and incompatible with an `AutoKey`. Before it, a model without an
   `AutoKey` had no key at all and `upsert` had nothing to conflict on.
4. **`HasModel.save (x : α)`** — insert-or-replace on the table's primary key, setting every
   non-key column. That is the "write this record" every store here does, and it fails with a
   clear message for a model that declares no key.
5. **`leansqlite` pinned to `v4.31.0`** (`0be4df9`), the revision orchestra already gets through
   taxis, so a dependent does not see two.

Two further commits on that branch stop the query DSL reserving the words it borrows: `guard`,
`select`, `order_by`, `limit`, `offset`, `v` and `to` are recognised only inside a `query% do`
block and are ordinary identifiers everywhere else.

Everything else orchestra needs was already there: `query%` with `guard`/`order_by`/`limit`/
`offset` (dynamic limits through `QuerySet.limit`/`.offset` on the elaborated set),
`HasModel.count`, `HasModel.update`/`delete` on a condition, `withTransaction`, declared
migrations with `createIndex`, and `insertIfAbsent`.

## out of scope, worth doing after

- `TaskStore.currentIso8601` spawns `date` for every timestamp; `Std.Time` can format one.
- `processed_ids` is a JSON array in one row; a listener that never prunes it could get its own
  table with a membership index.
- The library could grow a partial-decoding `HasDBType` (enumerations as typed columns rather
  than strings) and `AVG` over the new float type.
- `Dashboard.overviewApi` still reads every concert run to count the running ones, for want of a
  `countByStatus` on that table. Concert runs are far fewer than queue entries, so it has not
  been worth a second counting function yet.
- `Queue.reconcileStaleTaskRecords` and `cancelStaleConcertEntries` read the whole queue at
  startup. Both are startup sweeps that run once, but the first would be a join if the library
  had one in the DSL.
- `Queue.markTaskUnfinished` reads a record, decides, and writes it back; with
  `HasModel.update` on a condition it could be one statement and would not need the read to be
  a snapshot anyone trusts.
- Every `Store.run` opens its own connection. That is right for the daemon's threads, but a CLI
  listing that looks a record up per row — `orchestra tasks` labelling each run with its concert
  — pays for one connection per lookup. A per-call handle passed down would let a listing hold
  one.
