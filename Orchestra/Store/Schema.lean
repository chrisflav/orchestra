import Db
import Orchestra.Store.Migrations.Initial

/-!
# The shape of orchestra's database

One `@[model]` structure per table of `<data>/orchestra.db`, the indexes those tables carry, and
the list of migrations that builds them.

Every table is declared here, including the ones whose stores still write files: the schema is a
single object — index names are unique across the whole database, and `planSteps` diffs the whole
of it — so declaring it store by store as the stores are ported would mean a migration per stage
that renames nothing and a schema that is never the one the code describes. The stores that have
not moved yet simply have no reader or writer for their table.

The row structures are flat by necessity: a column holds a `String`, `Int`, `Bool`, `Float` or an
`Option` of one, so everything structured — a list, a `ResultType`, a `SpawnPolicy`, a `Json`
payload — is a `text` column holding the compressed JSON the record's own `ToJson` produces. The
conversions live with the stores, next to the records they convert, since they are where the
knowledge of what those strings mean is.

The migrations are in `Orchestra/Store/Migrations/`, one module per migration, generated with
`Db.Migration.planSteps`/`render` and committed as source. `Store.target` below is the schema the
code declares; `OrchestraTest.StoreTest` asserts that the two agree, so a model change without a
migration fails the suite rather than the deployment.
-/

namespace Orchestra.Store

initialize_database orchestra

/-- One agent run, from the moment the daemon starts it. The record a continuation inherits from
    and the row every listing of history is built out of. See `Orchestra.TaskStore.TaskRecord`. -/
@[model (dbName := "task") (primaryKey := ["id"]) orchestra]
structure TaskRow where
  id : String
  created_at : String
  /-- The repository pair, split in two: both columns or neither, a repository-independent run
      having no repository at all rather than half a pair. -/
  upstream : Option String
  fork : Option String
  mode : String
  prompt : String
  goal : Option String
  session_id : Option String
  status : String
  continues_from : Option String
  series : Option String
  backend : Option String
  model : Option String
  agent : Option String
  system_prompt : Option String
  prepend_prompt : Option String
  budget : Option Float
  priority : Int
  project_id : Option String
  issue_id : Option String
  role : Option String
  identity : Option String
  deriving Repr

/-- Where a named series has got to: the id of its most recent task, which is what a `--series`
    run continues from. -/
@[model (dbName := "series") (primaryKey := ["name"]) orchestra]
structure SeriesRow where
  name : String
  latest_task_id : String
  deriving Repr

/-- One entry on the queue: everything the daemon needs to start a run, plus what it wrote back
    when it claimed one. See `Orchestra.Queue.QueueEntry`. -/
@[model (dbName := "queue_entry") (primaryKey := ["id"]) orchestra]
structure QueueEntryRow where
  id : String
  created_at : String
  status : String
  upstream : Option String
  fork : Option String
  mode : String
  prompt : String
  goal : Option String
  agent : Option String
  system_prompt : Option String
  prepend_prompt : Option String
  backend : Option String
  model : Option String
  continues_from : Option String
  series : Option String
  task_id : Option String
  slot : Option Int
  config_path : Option String
  budget : Option Float
  memory : String
  identity : Option String
  auth_source : Option String
  /-- A JSON array. Empty is `[]` rather than NULL: the entry that names no candidate source and
      the entry whose list happens to be empty are the same entry. -/
  auth_sources : String
  auth_mode : Option String
  /-- A JSON array, and nullable — absent means "derive the tools from `mode`", which an empty
      list does not. -/
  tools : Option String
  read_only : Bool
  priority : Int
  concert_step_key : Option String
  concert_id : Option String
  input_type : String
  output_type : String
  input_json : Option String
  output_json : Option String
  issue_number : Option Int
  project_id : Option String
  issue_id : Option String
  role : Option String
  pr_labels : String
  triage_add_labels : String
  triage_remove_labels : String
  listener_name : Option String
  spawn_policy : Option String
  spawned_by : Option String
  scope_root : Option String
  deriving Repr

/-- One run of a concert workflow. See `Orchestra.Queue.ConcertRun`. -/
@[model (dbName := "concert_run") (primaryKey := ["id"]) orchestra]
structure ConcertRunRow where
  id : String
  started_at : String
  status : String
  name : Option String
  workflow_file : Option String
  finished_at : Option String
  deriving Repr

/-- One interactive session. See `Orchestra.Interactive.SessionRecord`. Written by a later stage
    of the port; the table is declared here so that stage adds no migration of its own. -/
@[model (dbName := "interactive_session") (primaryKey := ["id"]) orchestra]
structure InteractiveSessionRow where
  id : String
  status : String
  created_at : String
  last_activity_at : String
  ended_at : Option String
  upstream : String
  fork : String
  backend : String
  model : Option String
  budget : Float
  slot : Int
  agent_session_id : Option String
  agent_started : Bool
  resumed_from : Option String
  tools : Option String
  system_prompt : Option String
  identity : Option String
  turn_count : Int
  cost_usd : Float
  last_event_seq : Int
  title : Option String
  error : Option String
  deriving Repr

/-- One line of a session's transcript, keyed as the transcript addresses it.

    `doc` is the whole event as JSON rather than a column per field, because a reader hands the
    document straight back to the client: an orchestra that writes a field this one has never
    heard of should not lose it on the way through. -/
@[model (dbName := "interactive_event") (primaryKey := ["session_id", "seq"]) orchestra]
structure InteractiveEventRow where
  session_id : String
  seq : Int
  occurred_at : String
  doc : String
  deriving Repr

/-- The last poll of one backend's one authentication source: what the provider said, and when to
    ask again. See `Orchestra.Usage`. -/
@[model (dbName := "usage_source") (primaryKey := ["backend", "label"]) orchestra]
structure UsageSourceRow where
  backend : String
  label : String
  fetched_epoch : Option Int
  limits : String
  blocks : String
  last_used_tick : Option Int
  last_error : Option String
  poll_after : Option Int
  deriving Repr

/-- One window of usage history. Keyed by a generated id because the rows are a sequence rather
    than a set: `id` order is insertion order, which is the oldest-first order the history
    functions fold over. -/
@[model (dbName := "usage_window") orchestra]
structure UsageWindowRow where
  id : AutoKey
  backend : String
  label : String
  kind : String
  scope : Option String
  reset_epoch : Option Int
  start_epoch : Int
  last_epoch : Int
  peak_percent : Int
  last_percent : Int
  samples : Int
  deriving Repr

/-- What a listener has already seen, so a restart does not dispatch it all again. See
    `Orchestra.Listener`. -/
@[model (dbName := "listener_state") (primaryKey := ["name"]) orchestra]
structure ListenerStateRow where
  name : String
  last_checked : String
  enabled : Bool
  processed_ids : String
  dispatches : String
  deriving Repr

/-- One store the legacy JSON import has carried over. The row is the marker that says so: it is
    written under its primary key before anything is read, so two processes starting at once
    cannot import the same directory twice. See `Orchestra.Store.Import`. -/
@[model (dbName := "legacy_import") (primaryKey := ["store"]) orchestra]
structure LegacyImportRow where
  store : String
  imported_at : String
  records : Int
  deriving Repr

/-- The schema the code declares: the model tables with their indexes attached.

    `@[model]` generates a table without indexes — which of a structure's fields are worth one is
    not something the structure says — so every index this database has is named here. Index names
    are unique across the whole database on both backends, hence the `idx_<table>_<columns>`
    spelling.

    Each index serves a query the stores actually run: `(created_at, id)` is the newest-first
    order every listing is in, `(status)` the filter the daemon and the dashboard count by, and
    the rest are the foreign-key-shaped lookups (a task's entry, a concert's steps, a listener's
    work) that used to be a scan of the whole directory. -/
def target : DatabaseRecipe :=
  (%database orchestra).recipe
    |>.withIndexes "task" (tableIndexes TaskRowIndex
        [{ name := "idx_task_created_at_id",
           keys := [{ column := .created_at, direction := .desc },
                    { column := .id, direction := .desc }] },
         { name := "idx_task_status", keys := [{ column := .status }] },
         { name := "idx_task_series", keys := [{ column := .series }] },
         { name := "idx_task_issue_id", keys := [{ column := .issue_id }] }])
    |>.withIndexes "queue_entry" (tableIndexes QueueEntryRowIndex
        [{ name := "idx_queue_entry_created_at_id",
           keys := [{ column := .created_at, direction := .desc },
                    { column := .id, direction := .desc }] },
         { name := "idx_queue_entry_status", keys := [{ column := .status }] },
         { name := "idx_queue_entry_task_id", keys := [{ column := .task_id }] },
         { name := "idx_queue_entry_concert_id", keys := [{ column := .concert_id }] },
         { name := "idx_queue_entry_spawned_by", keys := [{ column := .spawned_by }] },
         { name := "idx_queue_entry_listener_name", keys := [{ column := .listener_name }] },
         { name := "idx_queue_entry_project_id", keys := [{ column := .project_id }] },
         { name := "idx_queue_entry_issue_id", keys := [{ column := .issue_id }] }])
    |>.withIndexes "concert_run" (tableIndexes ConcertRunRowIndex
        [{ name := "idx_concert_run_started_at_id",
           keys := [{ column := .started_at, direction := .desc },
                    { column := .id, direction := .desc }] }])
    |>.withIndexes "interactive_session" (tableIndexes InteractiveSessionRowIndex
        [{ name := "idx_interactive_session_created_at_id",
           keys := [{ column := .created_at, direction := .desc },
                    { column := .id, direction := .desc }] },
         { name := "idx_interactive_session_status", keys := [{ column := .status }] }])
    |>.withIndexes "usage_window" (tableIndexes UsageWindowRowIndex
        [{ name := "idx_usage_window_backend_label_id",
           keys := [{ column := .backend }, { column := .label }, { column := .id }] }])

/-- The migrations, in the order they are applied. Appended to, never edited: a deployment that
    has recorded one is a deployment the next one has to run on top of. -/
def migrations : List Db.Migration.Migration :=
  [Orchestra.Store.Migrations.migration_0001_initial]

end Orchestra.Store
