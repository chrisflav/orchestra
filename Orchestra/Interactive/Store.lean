import Orchestra.Config
import Orchestra.Utils.Time
import Orchestra.Utils.Files
import Orchestra.Dirs
import Orchestra.Store
import Orchestra.StreamFormat
import Orchestra.TaskStore
import Lean.Data.Json

open Lean (Json ToJson FromJson)

/-!
# Where a session is kept

Two tables of `<data>/orchestra.db` (`Orchestra.Store`):

  * `interactive_session` — one row per session, the record rewritten on every state change.
  * `interactive_event` — one row per transcript line, keyed by `(session_id, seq)`.

Two tables rather than one document because they are written at different rates and read for
different reasons: the record changes on every state change and is read whole, while the
transcript only grows and is read a window at a time from wherever a client's cursor is. The
daemon writes both and the API reads both, and in the compose deployment those are two
containers with nothing between them but this database.

The transcript's `seq` is still monotone, and still handed out by whoever holds the session's
lock, and that is what makes tailing cheap and a dropped stream lossless to resume: a reader
keeps a cursor, "what did I miss" is the range after it, and the answer is a range of a key
rather than a file read to the end. It is also what keeps an event in one place — `(session_id,
seq)` is the row's primary key, so a seq handed out twice replaces its row instead of leaving
the conversation with two events claiming the same position in it. The torn-line and
torn-character recovery the file transcript needed went away with the file; what is left of it
lives in `Orchestra.Store.Import`, which reads the old `events.jsonl` once.
-/

namespace Orchestra.Interactive

/-- Where a session is in its life.

    `ended` and `failed` are both terminal and deliberately distinct: a session that was closed,
    reaped, or ran out of budget ended, and one whose agent died did not. Telling a reader which
    is the difference between "that conversation is over" and "something went wrong". -/
inductive SessionStatus where
  /-- Acquiring the clone, the token, the MCP server and the agent process. -/
  | starting
  /-- Up and waiting for a turn. -/
  | idle
  /-- Working on a turn. -/
  | running
  /-- Nothing is running, and the conversation is intact.

      What an idle session becomes when its process is put down, and what every session becomes
      when the daemon restarts. A session holds a clone slot, an MCP server and an agent process,
      and none of those should be held by a conversation nobody is having — but the conversation
      itself costs a directory, and a person who closed a laptop on Friday has not finished
      talking. The next turn posted to a dormant session starts an agent again and resumes the
      agent-side history, so it picks up where it left off. Deliberately **not** terminal: this
      is a session that is waiting, not one that is over. -/
  | dormant
  /-- Closed by request, or out of budget. -/
  | ended
  /-- The agent process died, or the session could not be started at all. -/
  | failed
deriving Repr, BEq

instance : ToJson SessionStatus where
  toJson
    | .starting => "starting"
    | .idle     => "idle"
    | .running  => "running"
    | .dormant  => "dormant"
    | .ended    => "ended"
    | .failed   => "failed"

instance : FromJson SessionStatus where
  fromJson?
    | .str "starting" => .ok .starting
    | .str "idle"     => .ok .idle
    | .str "running"  => .ok .running
    | .str "dormant"  => .ok .dormant
    | .str "ended"    => .ok .ended
    | .str "failed"   => .ok .failed
    | j => .error s!"expected session status string, got {j}"

/-- Whether this status is one a session never leaves. -/
def SessionStatus.isTerminal : SessionStatus → Bool
  | .ended | .failed => true
  | _                => false

/-- Everything known about one session, and the whole of what survives a restart.

    Field names are the API's own: this record is what `GET /api/v1/interactive/{id}` answers,
    so instants are RFC 3339 in a `...At` field and an absent value is `null`, as everywhere
    else in that API. -/
structure SessionRecord where
  id : String
  status : SessionStatus := .starting
  createdAt : String
  /-- When the session last did anything — a turn posted, an event received. What the idle
      reaper measures against. -/
  lastActivityAt : String
  endedAt : Option String := none
  upstream : Repository
  fork : Repository
  backend : String := "claude"
  model : Option String := none
  /-- Maximum spend for the whole session, in USD. -/
  budget : Float := 20.0
  /-- The clone slot held for this session's lifetime. -/
  slot : Nat := 0
  /-- The agent CLI's own session id — what `--resume` takes.

      Assigned before the process starts rather than read back out of its stream, so a session
      whose agent died before saying anything can still be picked up where it left off. -/
  agentSessionId : Option String := none
  /-- Whether an agent has ever announced itself under `agentSessionId`.

      What `--resume` actually needs to know, and `turnCount` is not a proxy for it in either
      direction. A turn is counted before it reaches the CLI, so a daemon killed in that window
      leaves turns recorded against a conversation the CLI never wrote — and `--resume` on one of
      those errors, which for a dormant session means every wake fails the same way, forever.
      The other direction is the common one: a session started and never spoken to still has an
      `init` from the CLI, so it *is* resumable at zero turns. Set from the `init` event, which
      is the only thing that knows. -/
  agentStarted : Bool := false
  /-- The session this one resumed, when it was started to revive a dead one. -/
  resumedFrom : Option String := none
  /-- Optional tools this session's MCP server grants, as the request asked for them; `none`
      means all of them.

      On the record rather than only in the request because a dormant session is woken from what
      is on disk, and a conversation that comes back with a different set of tools than it went
      to sleep with is not the same session. -/
  tools : Option (List String) := none
  /-- The system prompt appended at launch, for the same reason. -/
  systemPrompt : Option String := none
  /-- The identity this session is held under, by name (`Orchestra.Identity`). `none` is the
      instance itself.

      On the record for the same reason `tools` is: a dormant session is woken from what is on
      disk, and a conversation that came back as somebody else — writing to the tracker under a
      different actor, against a different memory — would not be the same session. -/
  identity : Option String := none
  turnCount : Nat := 0
  /-- Spend so far, as the agent reported it. -/
  costUsd : Float := 0.0
  /-- The last seq written to the transcript. A client that has read this far is current. -/
  lastEventSeq : Nat := 0
  /-- The first thing the person said, truncated — so a list of sessions reads as a list of
      conversations rather than of ids. -/
  title : Option String := none
  /-- Why it failed, when it did. -/
  error : Option String := none

private def optStr : Option String → Json
  | some s => Json.str s
  | none   => Json.null

instance : ToJson SessionRecord where
  toJson r := Json.mkObj [
    ("id",             .str r.id),
    ("status",         ToJson.toJson r.status),
    ("createdAt",      .str r.createdAt),
    ("lastActivityAt", .str r.lastActivityAt),
    ("endedAt",        optStr r.endedAt),
    ("upstream",       ToJson.toJson r.upstream),
    ("fork",           ToJson.toJson r.fork),
    ("backend",        .str r.backend),
    ("model",          optStr r.model),
    ("budget",         ToJson.toJson r.budget),
    ("slot",           ToJson.toJson r.slot),
    ("agentSessionId", optStr r.agentSessionId),
    ("agentStarted",   ToJson.toJson r.agentStarted),
    ("resumedFrom",    optStr r.resumedFrom),
    ("turnCount",      ToJson.toJson r.turnCount),
    ("costUsd",        ToJson.toJson r.costUsd),
    ("lastEventSeq",   ToJson.toJson r.lastEventSeq),
    ("title",          optStr r.title),
    ("error",          optStr r.error),
    -- Not part of the API payload — `Orchestra.Dashboard` builds that itself. These are here
    -- because the record is what a dormant session is woken from.
    ("tools",          match r.tools with
                       | some ts => Json.arr (ts.map Json.str).toArray
                       | none    => Json.null),
    ("systemPrompt",   optStr r.systemPrompt),
    ("identity",       optStr r.identity)
  ]

instance : FromJson SessionRecord where
  fromJson? j := do
    let id             ← j.getObjValAs? String "id"
    let status         ← j.getObjValAs? SessionStatus "status"
    let createdAt      ← j.getObjValAs? String "createdAt"
    let lastActivityAt ← j.getObjValAs? String "lastActivityAt"
    let upstream       ← j.getObjValAs? Repository "upstream"
    let fork           ← j.getObjValAs? Repository "fork"
    return {
      id, status, createdAt, lastActivityAt, upstream, fork
      endedAt        := j.getObjValAs? String "endedAt"        |>.toOption
      backend        := j.getObjValAs? String "backend"        |>.toOption |>.getD "claude"
      model          := j.getObjValAs? String "model"          |>.toOption
      budget         := j.getObjValAs? Float  "budget"         |>.toOption |>.getD 20.0
      slot           := j.getObjValAs? Nat    "slot"           |>.toOption |>.getD 0
      agentSessionId := j.getObjValAs? String "agentSessionId" |>.toOption
      agentStarted   := j.getObjValAs? Bool   "agentStarted"   |>.toOption |>.getD false
      resumedFrom    := j.getObjValAs? String "resumedFrom"    |>.toOption
      turnCount      := j.getObjValAs? Nat    "turnCount"      |>.toOption |>.getD 0
      costUsd        := j.getObjValAs? Float  "costUsd"        |>.toOption |>.getD 0.0
      lastEventSeq   := j.getObjValAs? Nat    "lastEventSeq"   |>.toOption |>.getD 0
      title          := j.getObjValAs? String "title"          |>.toOption
      error          := j.getObjValAs? String "error"          |>.toOption
      tools          := j.getObjValAs? (List String) "tools"   |>.toOption
      systemPrompt   := j.getObjValAs? String "systemPrompt"   |>.toOption
      identity       := j.getObjValAs? String "identity"       |>.toOption
    }

/-- What a caller asks for when it starts a session.

    Here rather than beside the manager so that the control-socket protocol can name it without
    importing the sandbox, the MCP server and the task runner along with it. -/
structure SessionSpec where
  upstream : Repository
  fork : Repository
  backend : Option String := none
  model : Option String := none
  budget : Option Float := none
  /-- Optional tools to grant the agent through the MCP server; `none` means all of them, as
      `orchestra interactive` does. -/
  tools : Option (List String) := none
  systemPrompt : Option String := none
  /-- Start this session by resuming the conversation another one was having. Used to pick up a
      session whose agent died; the old session is not revived, this is a new one that inherits
      its transcript's agent-side history. -/
  resumeFrom : Option String := none
  /-- The identity to hold the session under (`Orchestra.Identity`). `none` is the instance. -/
  identity : Option String := none

/-! ## The transcript -/

/-- What one line of the transcript says.

    The agent's own stream cannot carry three of these five: it never says what the person typed,
    it does not mark where a turn began, and it has nothing to say about the daemon killing it.
    Wrapping its events rather than replacing them means a client that can already render a task
    log renders a transcript, and only the three new kinds are new work. -/
inductive TranscriptKind where
  /-- What the person said. -/
  | user (text : String)
  /-- What the agent said, exactly as the log format already carries it. -/
  | agent (event : StreamFormat.Event)
  | turnStarted (turn : Nat)
  | turnEnded (turn : Nat) (subtype : String) (costUsd : Option Float)
              (durationSeconds : Option Nat)
  /-- Something the daemon did or found: a crash, a usage limit, an interrupt, an idle reap. -/
  | notice (level : String) (message : String)

/-- One transcript line: what happened, when, and where it falls in the order. -/
structure TranscriptEvent where
  seq : Nat
  occurredAt : String
  kind : TranscriptKind

instance : ToJson TranscriptEvent where
  toJson e :=
    let base : List (String × Json) :=
      [("seq", ToJson.toJson e.seq), ("occurredAt", .str e.occurredAt)]
    let rest : List (String × Json) := match e.kind with
      | .user text     => [("kind", .str "user"), ("text", .str text)]
      | .agent ev      => [("kind", .str "agent"), ("event", ToJson.toJson ev)]
      | .turnStarted t => [("kind", .str "turnStarted"), ("turn", ToJson.toJson t)]
      | .turnEnded t sub cost dur =>
        [("kind", .str "turnEnded"), ("turn", ToJson.toJson t), ("subtype", .str sub),
         ("costUsd", match cost with | some c => ToJson.toJson c | none => Json.null),
         ("durationSeconds", match dur with | some d => ToJson.toJson d | none => Json.null)]
      | .notice level msg =>
        [("kind", .str "notice"), ("level", .str level), ("message", .str msg)]
    Json.mkObj (base ++ rest)

/-! ## Where a session lives -/

/-- `<data>/interactive`, where a session used to be a directory of two files. Read once, by
    `Orchestra.Store.Import`, and then left alone: the import copies, it does not delete. -/
def legacySessionsDir : IO System.FilePath :=
  return (← Dirs.dataBase) / "interactive"

/-- Refuse an id that is not a name.

    The store used to hold this because every path went through it and `Utils.ensureConfigName`
    exists precisely because a store that trusted its callers wrote a file outside its root and
    answered `201`. A row key cannot escape anything, but the property is worth keeping for a
    different reason: an id is a name every other surface can also name — a directory of logs, a
    URL path segment, a line of `orchestra interactive list` — and the check is what says so. Not
    every id reaching this module comes from a path segment the HTTP layer has already looked at;
    `resumeFrom` arrives in a request *body*. -/
private def checkId (id : String) : IO Unit :=
  Utils.ensureConfigName "session" id

/-! ## The record as a row -/

/-- The record as a row of the `interactive_session` table.

    Total, and spelled the way the record's own JSON is: the status name, `"owner/repo"` for each
    half of the repository pair, the tools as the compressed JSON array they are on the wire. The
    legacy import builds a row straight out of a `session.json` it has parsed, so anything that
    does not round-trip here is a conversation that changed shape on its way into the database. -/
def SessionRecord.toRow (r : SessionRecord) : Store.InteractiveSessionRow :=
  { id               := r.id
    status           := Store.enumColumn r.status
    created_at       := r.createdAt
    last_activity_at := r.lastActivityAt
    ended_at         := r.endedAt
    upstream         := r.upstream.toString
    fork             := r.fork.toString
    backend          := r.backend
    model            := r.model
    budget           := r.budget
    slot             := Store.natColumn r.slot
    agent_session_id := r.agentSessionId
    agent_started    := r.agentStarted
    resumed_from     := r.resumedFrom
    tools            := r.tools.map Store.jsonColumn
    system_prompt    := r.systemPrompt
    identity         := r.identity
    turn_count       := Store.natColumn r.turnCount
    cost_usd         := r.costUsd
    last_event_seq   := Store.natColumn r.lastEventSeq
    title            := r.title
    error            := r.error }

/-- The record a row holds, or why this build cannot read it.

    Fails rather than guessing. A status name this build does not know belongs to a session a
    newer orchestra is running, and calling it `ended` would have the reaper give away the clone
    slot of a conversation that is still going. The callers report and skip, which is what a
    `session.json` that would not parse cost. -/
def SessionRecord.ofRow? (row : Store.InteractiveSessionRow) : Except String SessionRecord := do
  let status   ← Store.enumOfColumn? "status" row.status
  let upstream ← Repository.parse row.upstream
  let fork     ← Repository.parse row.fork
  let tools ← match row.tools with
    | none   => pure none
    | some s => some <$> Store.jsonOfColumn? (α := List String) "tools" s
  return { id             := row.id
           status         := status
           createdAt      := row.created_at
           lastActivityAt := row.last_activity_at
           endedAt        := row.ended_at
           upstream       := upstream
           fork           := fork
           backend        := row.backend
           model          := row.model
           budget         := row.budget
           slot           := row.slot.toNat
           agentSessionId := row.agent_session_id
           agentStarted   := row.agent_started
           resumedFrom    := row.resumed_from
           tools          := tools
           systemPrompt   := row.system_prompt
           identity       := row.identity
           turnCount      := row.turn_count.toNat
           costUsd        := row.cost_usd
           lastEventSeq   := row.last_event_seq.toNat
           title          := row.title
           error          := row.error }

/-! ## Reading and writing the record -/

/-- Write the record, replacing whatever is under its id.

    One statement, so a reader in the other container never sees half a record — which is what
    the write-and-rename this replaces was for. -/
def saveSession (r : SessionRecord) : IO Unit := do
  checkId r.id
  Store.run <| HasModel.save r.toRow

open Db.Query.DSL in
/-- The record, or `none` when there is none.

    A row that exists but cannot be read is reported before it is dropped. Silence there is
    expensive in a specific way: `loadAllSessions` feeds the startup reconciliation, so a record
    an older binary cannot read — a status added by a newer one, say — would be a session that
    never gets closed and a clone slot pinned for the life of the daemon, with nothing said. -/
def loadSession (id : String) : IO (Option SessionRecord) := do
  checkId id
  let rows ← Store.run <| HasModel.fetch <| query% do
    let s ← from Store.InteractiveSessionRow
    guard s.id = id
    select s
  return (← Store.keepConvertible "session" (·.id) SessionRecord.ofRow? rows)[0]?

open Db.Query.DSL in
/-- Every session, newest first.

    By `created_at`, not by id — a monotone clock restarts at boot, so ids only order sessions
    within one, and the id is here only to break ties. The same thing `TaskStore.loadAllTasks`
    does, and for the same reason. -/
def loadAllSessions : IO (Array SessionRecord) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let s ← from Store.InteractiveSessionRow
    select s
    order_by_desc s.created_at
    order_by_desc s.id
  Store.keepConvertible "session" (·.id) SessionRecord.ofRow? rows

/-! ## Reading and writing the transcript -/

/-- Append one event.

    The seq comes from the record's `lastEventSeq`, so the caller holding the session is the only
    thing that hands them out and they cannot collide; `(session_id, seq)` is the row's key, and a
    seq handed out twice replaces its row rather than leaving the transcript with two events
    claiming the same place in it.

    `doc` is the whole event as JSON — exactly the text a transcript line held — rather than a
    column per field, so a reader hands the document straight back to the client and a field a
    newer orchestra writes is not lost on the way through. -/
def appendEvent (id : String) (seq : Nat) (occurredAt : String) (kind : TranscriptKind)
    : IO Unit := do
  checkId id
  Store.run <| HasModel.save
    ({ session_id  := id
       seq         := Store.natColumn seq
       occurred_at := occurredAt
       doc         := Json.compress (ToJson.toJson ({ seq, occurredAt, kind } : TranscriptEvent)) } :
      Store.InteractiveEventRow)

open Db.Query.DSL in
/-- The transcript events after `after`, at most `atMost` of them, and how many there are in
    total after `after`.

    The total counts what matches before the window, so a client knows whether it is caught up
    without asking a second time — the same envelope arithmetic every collection in the API uses.

    One query and one count, where this used to read the whole file backwards on every poll,
    three times a second per attached client. The seq is monotone and indexed by the row's key,
    so "anything after N" is a range the database walks rather than a conversation this has to
    parse to the cursor.

    Documents are handed back as parsed JSON rather than re-serialised from a typed value, so a
    field written by a newer orchestra survives being read by an older one. One that does not
    parse is skipped rather than refused, as a torn line was. -/
def readEvents (id : String) (after : Nat := 0) (atMost : Nat := 500)
    : IO (Array Json × Nat) := do
  checkId id
  let afterSeq : Int := Store.natColumn after
  let (rows, total) ← Store.run do
    let matching := query% do
      let e ← from Store.InteractiveEventRow
      guard e.session_id = id
      guard e.seq > afterSeq
      select e
      order_by e.seq
    let rows ← HasModel.fetch (matching.limit atMost)
    let total ← HasModel.count matching
    return (rows, total)
  let mut docs : Array Json := #[]
  for row in rows do
    if let .ok j := Json.parse row.doc then
      docs := docs.push j
  return (docs, total.toNat)

end Orchestra.Interactive
