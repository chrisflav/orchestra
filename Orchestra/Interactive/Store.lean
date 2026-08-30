import Orchestra.Config
import Orchestra.Utils.Time
import Orchestra.Utils.Files
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
    ("systemPrompt",   optStr r.systemPrompt)
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

/-- The directory a session's files live in.

    Every path in this module goes through here, and here is where the id is checked. That
    placement is deliberate and the codebase has been bitten before: `Utils.ensureConfigName`
    exists precisely because a store that trusted its callers wrote a file outside its root and
    answered `201`. Not every id reaching this module comes from a path segment the HTTP layer
    already checked — `resumeFrom` arrives in a request *body* — so the property is held here
    rather than assumed. -/
def sessionDir (id : String) : IO System.FilePath := do
  Utils.ensureConfigName "session" id
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
  -- Named uniquely rather than `session.json.tmp`: two writers sharing one temp file interleave
  -- their writes and the later rename publishes the splice.
  let tmp := dir / s!"session.json.{← uniqueToken}.tmp"
  IO.FS.writeFile tmp (Json.compress (ToJson.toJson r))
  IO.FS.rename tmp path

/-- The record, or `none` when there is none.

    A record that exists but cannot be read is reported before it is dropped. Silence there is
    expensive in a specific way: `loadAllSessions` feeds the startup reconciliation, so a record
    an older binary cannot parse — a status added by a newer one, say — would be a session that
    never gets closed and a clone slot pinned for the life of the daemon, with nothing said. -/
def loadSession (id : String) : IO (Option SessionRecord) := do
  let path ← recordPath id
  if !(← path.pathExists) then return none
  let complain (why : String) : IO (Option SessionRecord) := do
    IO.eprintln s!"  Warning: the session record at {path} could not be read ({why}); it is \
being skipped, and any resource it still holds will not be reclaimed."
    return none
  match Json.parse (← IO.FS.readFile path) with
  | .error e => complain e
  | .ok j    =>
    match (FromJson.fromJson? j : Except String SessionRecord) with
    | .ok r    => return some r
    | .error e => complain e

/-- Every session on disk, newest first.

    By `createdAt`, not by id — a monotone clock restarts at boot, so ids only order sessions
    within one. The same thing `TaskStore.loadAllTasks` does, and for the same reason. -/
def loadAllSessions : IO (Array SessionRecord) := do
  let dir ← sessionsDir
  if !(← dir.pathExists) then return #[]
  let mut out : Array SessionRecord := #[]
  for entry in ← System.FilePath.readDir dir do
    if let some r ← loadSession entry.fileName then
      out := out.push r
  return Time.sortNewestFirst (·.createdAt) (·.id) out

/-! ## Reading and writing the transcript -/

/-- Append one event.

    The seq comes from the record's `lastEventSeq`, so the caller holding the session is the
    only thing that hands them out and they cannot collide. Flushed per line, because the reader
    is another process and an unflushed line is a line that did not happen.

    The newline goes *before* the record, not after it, and that is the whole of what makes a
    torn write cost one event instead of two. A daemon killed mid-write leaves a fragment at the
    end of the file; with a trailing-newline format the next append lands straight onto that
    fragment and splices the two into a line that parses as neither, so the crash takes the next
    event with it. A leading newline bounds the fragment instead: it is skipped, and everything
    appended afterwards is read normally. -/
def appendEvent (id : String) (seq : Nat) (occurredAt : String) (kind : TranscriptKind)
    : IO Unit := do
  let dir ← sessionDir id
  IO.FS.createDirAll dir
  let h ← IO.FS.Handle.mk (← transcriptPath id) .append
  h.putStr ("\n" ++ Json.compress (ToJson.toJson ({ seq, occurredAt, kind } : TranscriptEvent)))
  h.flush

/-- The transcript as text, tolerating a tail torn mid-character.

    `IO.FS.readFile` throws outright on invalid UTF-8 — one level below the per-line recovery in
    `readEvents`, so it never gets the chance. A daemon killed in the middle of writing a
    multi-byte character (this repo's own tool output is full of `→` and `✓`) would leave a
    transcript that throws on *every* subsequent read: not one lost event, the whole
    conversation unreadable, permanently. A truncated code point is at most three bytes short,
    so trimming back to the last valid boundary recovers everything written before the tear.

    Damage anywhere but the tail is not something this writer can produce, and is reported
    rather than papered over. -/
private def readTranscriptText (path : System.FilePath) : IO String := do
  let bytes ← IO.FS.readBinFile path
  if let some s := String.fromUTF8? bytes then return s
  for back in [1, 2, 3] do
    if bytes.size ≥ back then
      if let some s := String.fromUTF8? (bytes.extract 0 (bytes.size - back)) then
        return s
  throw (.userError s!"the transcript at {path} is not valid UTF-8, and not merely torn at the \
end; it needs looking at by hand")

/-- The transcript events after `after`, at most `limit` of them, and how many there are in
    total after `after`.

    The total counts what matches before the window, so a client knows whether it is caught up
    without asking a second time — the same envelope arithmetic every collection in the API
    uses.

    Walked backwards, and stopped at the first event the caller already has. Seqs only increase,
    so everything past that point is behind the cursor by construction; scanning forward instead
    meant parsing the entire conversation on every poll — three times a second, per attached
    client — to answer "nothing new". A cursor that costs the whole file is not a cursor.

    Lines are handed back as parsed JSON rather than re-serialised from a typed value, so a
    field written by a newer orchestra survives being read by an older one. A line that does not
    parse, or that carries no seq, is skipped rather than stopping the scan: it can only be a
    torn write, and treating it as a boundary would hide every event before it. -/
def readEvents (id : String) (after : Nat := 0) (limit : Nat := 500)
    : IO (Array Json × Nat) := do
  let path ← transcriptPath id
  if !(← path.pathExists) then return (#[], 0)
  let mut newer : List Json := []
  for line in (← readTranscriptText path).splitOn "\n" |>.reverse do
    let line := line.trimAscii.toString
    if line.isEmpty then continue
    let some j := (Json.parse line).toOption | continue
    let some seq := j.getObjValAs? Nat "seq" |>.toOption | continue
    if seq ≤ after then break
    newer := j :: newer
  return ((newer.take limit).toArray, newer.length)

end Orchestra.Interactive
