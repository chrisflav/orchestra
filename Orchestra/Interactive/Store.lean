import Orchestra.Config
import Orchestra.Dirs
import Orchestra.StreamFormat
import Orchestra.TaskStore
import Lean.Data.Json

open Lean (Json ToJson FromJson)

/-!
# Where a session lives on disk

`<data>/interactive/<id>/` holds two files:

  * `session.json` — the record, rewritten on every state change.
  * `events.jsonl` — the transcript, appended to and flushed per event.

Split that way because the daemon writes both and the API reads both, and in the compose
deployment those are two containers with nothing between them but this directory. A socket
round-trip for every read would make the transcript unreadable whenever the daemon is busy;
files are readable whether the daemon is busy, restarting, or gone.

The transcript is append-only and carries a monotone `seq`. That is what makes it cheap to tail
from another process — a reader keeps a cursor and asks for what follows it — and what makes a
dropped stream lossless to resume, since "what did I miss" has an exact answer.
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
  /-- Closed, reaped, out of budget, or the daemon restarted under it. -/
  | ended
  /-- The agent process died, or the session could not be started at all. -/
  | failed
deriving Repr, BEq

instance : ToJson SessionStatus where
  toJson
    | .starting => "starting"
    | .idle     => "idle"
    | .running  => "running"
    | .ended    => "ended"
    | .failed   => "failed"

instance : FromJson SessionStatus where
  fromJson?
    | .str "starting" => .ok .starting
    | .str "idle"     => .ok .idle
    | .str "running"  => .ok .running
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
  /-- The session this one resumed, when it was started to revive a dead one. -/
  resumedFrom : Option String := none
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
    ("resumedFrom",    optStr r.resumedFrom),
    ("turnCount",      ToJson.toJson r.turnCount),
    ("costUsd",        ToJson.toJson r.costUsd),
    ("lastEventSeq",   ToJson.toJson r.lastEventSeq),
    ("title",          optStr r.title),
    ("error",          optStr r.error)
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
      resumedFrom    := j.getObjValAs? String "resumedFrom"    |>.toOption
      turnCount      := j.getObjValAs? Nat    "turnCount"      |>.toOption |>.getD 0
      costUsd        := j.getObjValAs? Float  "costUsd"        |>.toOption |>.getD 0.0
      lastEventSeq   := j.getObjValAs? Nat    "lastEventSeq"   |>.toOption |>.getD 0
      title          := j.getObjValAs? String "title"          |>.toOption
      error          := j.getObjValAs? String "error"          |>.toOption
    }

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

/-! ## Paths -/

/-- Redirects the session root, so a test writing sessions cannot reach the developer's own.
    The same device `Skill.skillsDirOverride` and `Project.globalRolesDirOverride` use. -/
initialize sessionsDirOverride : IO.Ref (Option System.FilePath) ← IO.mkRef none

def setSessionsDirOverride (p : Option System.FilePath) : IO Unit :=
  sessionsDirOverride.set p

def sessionsDir : IO System.FilePath := do
  match ← sessionsDirOverride.get with
  | some p => return p
  | none   => return (← Dirs.dataBase) / "interactive"

def sessionDir (id : String) : IO System.FilePath :=
  return (← sessionsDir) / id

def recordPath (id : String) : IO System.FilePath :=
  return (← sessionDir id) / "session.json"

def transcriptPath (id : String) : IO System.FilePath :=
  return (← sessionDir id) / "events.jsonl"

/-! ## Reading and writing the record -/

/-- Write the record, creating the session's directory if this is the first time.

    Write-and-rename, so a reader in the other container never sees half a record. The transcript
    beside it is appended to instead, which has the same property for a different reason: a line
    is either fully written and flushed or not there. -/
def saveSession (r : SessionRecord) : IO Unit := do
  let dir ← sessionDir r.id
  IO.FS.createDirAll dir
  let path ← recordPath r.id
  let tmp := dir / "session.json.tmp"
  IO.FS.writeFile tmp (Json.compress (ToJson.toJson r))
  IO.FS.rename tmp path

def loadSession (id : String) : IO (Option SessionRecord) := do
  let path ← recordPath id
  if !(← path.pathExists) then return none
  match Json.parse (← IO.FS.readFile path) with
  | .error _ => return none
  | .ok j    => return (FromJson.fromJson? j : Except String SessionRecord).toOption

/-- Every session on disk, newest first.

    Ids are minted from a monotone clock, so sorting by id descending is sorting by age — the
    same thing `TaskStore.loadAllTasks` relies on, and for the same reason. -/
def loadAllSessions : IO (Array SessionRecord) := do
  let dir ← sessionsDir
  if !(← dir.pathExists) then return #[]
  let mut out : Array SessionRecord := #[]
  for entry in ← System.FilePath.readDir dir do
    if let some r ← loadSession entry.fileName then
      out := out.push r
  return out.qsort (fun a b => a.id > b.id)

/-! ## Reading and writing the transcript -/

/-- Append one event and answer the seq it was given.

    The seq comes from the record's `lastEventSeq`, so the caller holding the session is the only
    thing that hands them out and they cannot collide. Flushed per line, because the reader is
    another process and an unflushed line is a line that did not happen. -/
def appendEvent (id : String) (seq : Nat) (occurredAt : String) (kind : TranscriptKind)
    : IO Unit := do
  let dir ← sessionDir id
  IO.FS.createDirAll dir
  let h ← IO.FS.Handle.mk (← transcriptPath id) .append
  h.putStrLn (Json.compress (ToJson.toJson ({ seq, occurredAt, kind } : TranscriptEvent)))
  h.flush

/-- The transcript events after `after`, at most `limit` of them, and how many there are in
    total after `after`.

    The total counts what matches before the window, so a client knows whether it is caught up
    without asking a second time — the same envelope arithmetic every collection in the API
    uses.

    Lines are handed back as parsed JSON rather than re-serialised from a typed value, so a field
    written by a newer orchestra survives being read by an older one. A line that does not parse
    is skipped rather than fatal: it can only be a torn last line, and the next read gets it
    whole. -/
def readEvents (id : String) (after : Nat := 0) (limit : Nat := 500)
    : IO (Array Json × Nat) := do
  let path ← transcriptPath id
  if !(← path.pathExists) then return (#[], 0)
  let contents ← IO.FS.readFile path
  let mut matching : Array Json := #[]
  for line in contents.splitOn "\n" do
    let line := line.trimAscii.toString
    if line.isEmpty then continue
    let some j := (Json.parse line).toOption | continue
    let seq := j.getObjValAs? Nat "seq" |>.toOption |>.getD 0
    if seq > after then
      matching := matching.push j
  return (matching.take limit, matching.size)

end Orchestra.Interactive
