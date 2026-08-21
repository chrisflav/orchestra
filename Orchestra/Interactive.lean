import Orchestra.Interactive.Store
import Orchestra.Interactive.Wire
import Orchestra.Sandbox
import Orchestra.Server
import Orchestra.Repo
import Orchestra.GitHub
import Orchestra.Secret
import Orchestra.TaskRunner
import Orchestra.Usage
import Std.Sync

open Lean (Json)

/-!
# The session manager

The daemon's half of an interactive session: it holds the agent process, the clone slot, the MCP
server and the credentials for as long as the conversation lasts, and writes everything that
happens to `<data>/interactive/<id>/` where the API can read it.

The prologue — clone, mint a token, start the MCP server, resolve an authentication source — is
the one `orchestra interactive` already runs, and the one every queued task runs. What changes is
what happens after it. A task launches an agent with a prompt and waits for it to exit; a session
launches one in streaming mode and keeps it, so that the second turn costs a line on a pipe
rather than another clone, another token and another process start.

## What a session holds, and why the slot matters most

The clone slot is reserved for the session's whole life, in the *same* table the queue claims
from. Nothing else would do. Slots are recycled between tasks, and a queued task that took this
one would reset the working tree — leaving the agent mid-conversation with a wiped tree and a
context full of references to edits that are no longer there. The queue's own continuation
mechanism has the same hazard and settles for detecting it; a conversation cannot, so the slot is
held rather than re-acquired.

## What happens when it goes wrong

Every failure ends with the slot, the MCP server and the process released, and with the session
in a terminal state on disk saying which failure it was. The one that needs saying out loud is a
daemon restart: the processes are gone, but the records are not, and a session left reading
`running` after a restart is a conversation that will never answer. `reconcile` runs at startup
and closes them, so what is on disk is only ever what is true.
-/

namespace Orchestra.Interactive

open Orchestra.StreamFormat

/-- How the interactive half of the daemon is configured.

    Capacity, not access: a session pins a clone slot and an agent process for as long as it
    lives, and an abandoned browser tab should not hold either forever. Spelled like
    `parallel` and `parallel_per_repo`, which bound the same resource for the queue. -/
structure Config where
  /-- Sessions that may be up at once, across all repositories. -/
  maxSessions : Nat := 2
  /-- How long a session may sit without a turn before it is closed. -/
  idleTimeoutSeconds : Nat := 1800

/-- What a caller asks for when it starts a session. -/
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

/-- A session the daemon is holding right now. Its on-disk record is the durable half; this is
    the half that cannot be written down. -/
structure LiveSession where
  id : String
  fork : Repository
  slot : Nat
  /-- The record as it currently stands. Written through to disk under `lock`. -/
  record : IO.Ref SessionRecord
  /-- Serialises appends to the transcript and writes of the record.

      Both the pump thread and whichever thread posted a turn append, and a seq handed to two
      events is a transcript a cursor cannot read. -/
  lock : Std.BaseMutex
  stream : Sandbox.StreamingSession
  /-- Shuts down this session's MCP server. -/
  shutdownMcp : IO Unit
  /-- The authentication source this session is spending against, if one was resolved. -/
  authLabel : Option String
  backend : String

/-- The daemon's table of live sessions, plus the two things it needs from the queue's own
    bookkeeping.

    `reserveSlot` and `releaseSlot` are passed in rather than reached for, because the table they
    touch is the queue worker's local state: taking it as an interface is what lets a session
    occupy a slot the queue then knows better than to hand out, without this module reaching into
    `Daemon.run`. -/
structure Manager where
  private sessions : Std.Mutex (Array LiveSession)
  cfg : Config
  /-- Take a clone slot for `fork`, or answer `none` when the repository is at its limit. -/
  private reserveSlot : Repository → IO (Option Nat)
  private releaseSlot : Repository → Nat → IO Unit

/-- A UUID the agent CLI will accept for `--session-id`. Version 4, from `/dev/urandom`. -/
private def newUuid : IO String := do
  let hex ← Secret.randomHex 16
  let part (start len : Nat) : String := ((hex.drop start).take len).toString
  -- The version and variant nibbles are fixed rather than random, because a CLI that validates
  -- the shape rejects a bare 32 hex digits.
  return s!"{part 0 8}-{part 8 4}-4{part 13 3}-a{part 17 3}-{part 20 12}"

/-- The first line of what someone said, short enough to be a name for the conversation. -/
private def titleOf (text : String) : String :=
  let firstLine := (text.splitOn "\n").headD text |>.trimAscii.toString
  if firstLine.length ≤ 80 then firstLine
  else (firstLine.take 77).toString ++ "..."

/-! ## Writing to a session

Everything that changes a session goes through one of these two, so that the record on disk and
the transcript beside it can never disagree about how far the conversation has got. -/

/-- Append one event to the transcript and record that it happened, under the session's lock. -/
private def append (s : LiveSession) (kind : TranscriptKind) : IO Nat := do
  s.lock.lock
  try
    let r ← s.record.get
    let seq := r.lastEventSeq + 1
    let now ← TaskStore.currentIso8601
    appendEvent s.id seq now kind
    let r := { r with lastEventSeq := seq, lastActivityAt := now }
    s.record.set r
    saveSession r
    return seq
  finally s.lock.unlock

/-- Apply `f` to the record and write it through. -/
private def update (s : LiveSession) (f : SessionRecord → SessionRecord) : IO Unit := do
  s.lock.lock
  try
    let r := f (← s.record.get)
    s.record.set r
    saveSession r
  finally s.lock.unlock

/-! ## The pump

What the agent says, as it says it. Called from `launchStreaming`'s stdout thread, one event at a
time, in the order the agent emitted them. -/

/-- Record one event from the agent.

    Three of them mean something to the session beyond being written down: `init` names the
    conversation the CLI is actually in, which is what a later `--resume` needs; `result` ends
    the turn; and a rate-limit event carries when the limit lifts. The rest are transcript. -/
private def onAgentEvent (s : LiveSession) (event : Event) : IO Unit := do
  if Wire.isNoise event then return
  if let .init sid _ := event then
    -- Belt and braces: the id was assigned before launch, but a CLI that decided on a different
    -- one would otherwise leave the record naming a session nobody can resume.
    update s fun r => { r with agentSessionId := some sid }
  let _ ← append s (.agent event)
  if let .result sub _ durationMs cost _ := event then
    let costFloat := cost.bind (·.getNum?.toOption) |>.map (·.toFloat)
    let r ← s.record.get
    let _ ← append s (.turnEnded r.turnCount (subtypeName sub) costFloat
                                 (durationMs.map (· / 1000)))
    update s fun r => { r with
      status  := if r.status.isTerminal then r.status else .idle
      costUsd := r.costUsd + costFloat.getD 0.0 }
    -- A budget the session has spent is the session over, not a turn that went badly: the CLI
    -- will refuse every turn after it, and a session that answers nothing while still reading
    -- `idle` is worse than one that says it is finished.
    if sub == .errorMaxBudgetUsd then
      let _ ← append s (.notice "warning"
        "the session reached its budget; no further turns will be answered")
      let now ← TaskStore.currentIso8601
      update s fun r => { r with status := .ended, endedAt := some now }
where
  subtypeName : ResultSubtype → String
    | .success           => "success"
    | .errorMaxBudgetUsd => "error_max_budget_usd"
    | .error _           => "error"
    | .unknown raw       => raw

/-! ## Starting one -/

/-- Every optional tool an interactive session may be granted. The same set
    `orchestra interactive --tools all` grants, and for the same reason: a person sitting in
    front of the agent is expected to be able to ask it for anything a task could do. -/
def allOptionalTools : List String :=
  ["create_pr", "merge_pr", "label_issue", "comment", "manage_issues", "work_issues",
   "review_issues"]

def Manager.new (cfg : Config)
    (reserveSlot : Repository → IO (Option Nat))
    (releaseSlot : Repository → Nat → IO Unit) : IO Manager := do
  return { sessions := ← Std.Mutex.new #[], cfg, reserveSlot, releaseSlot }

/-- Close a live session: stop the agent, shut down its MCP server, release its slot, and stamp
    the record terminal.

    Everything here is best-effort and ordered so that the resources go back even if an earlier
    step throws — a leaked clone slot is never recovered, and the repository it belongs to runs
    one narrower for the life of the daemon. -/
private def teardown (mgr : Manager) (s : LiveSession) (status : SessionStatus)
    (why : Option String) : IO Unit := do
  try s.stream.shutdown catch _ => pure ()
  try s.shutdownMcp catch _ => pure ()
  try mgr.releaseSlot s.fork s.slot catch _ => pure ()
  let now ← TaskStore.currentIso8601
  try
    update s fun r =>
      if r.status.isTerminal then r
      else { r with status, endedAt := some now, error := why }
  catch _ => pure ()
  mgr.sessions.atomically <| modify fun ss => ss.filter (·.id != s.id)

private def find (mgr : Manager) (id : String) : IO (Option LiveSession) := do
  let ss ← mgr.sessions.atomically get
  return ss.find? (·.id == id)

/-- Start a session: acquire everything it will hold, launch the agent, and register it.

    The prologue is `Main.interactiveHandler`'s, moved where a later HTTP request can reach what
    it built. Every failure past the slot reservation goes through `teardown`, so a session that
    could not start leaves nothing behind but a record saying why. -/
def Manager.start (mgr : Manager) (appConfig : AppConfig) (spec : SessionSpec)
    (debug : Bool := false) : IO (Except String SessionRecord) := do
  let backendName := spec.backend.getD "claude"
  let agentDef := TaskRunner.agentDefOfBackend (some backendName)
  -- Checked before anything is acquired, and said plainly. The alternative — falling back to the
  -- one-shot invocation — hands back a process that answers the first turn and exits, which to
  -- everyone watching looks like a session that ended on its own.
  if (agentDef.buildStreamArgs { mcpContext := "" }).isNone then
    return .error s!"backend '{backendName}' cannot host an interactive session: its CLI has no \
streaming input mode. Backends that can: claude."
  let live ← mgr.sessions.atomically get
  if live.size ≥ mgr.cfg.maxSessions then
    return .error s!"the daemon is already holding {live.size} interactive \
session{if live.size == 1 then "" else "s"}, which is its limit; end one first"
  let some slot ← mgr.reserveSlot spec.fork
    | return .error s!"no free clone slot for {spec.fork}: every one is in use by a running task \
or another session"
  let id ← TaskStore.generateId
  let now ← TaskStore.currentIso8601
  -- Resuming inherits the agent-side history of the session named, not the session itself.
  let resumeAgentSession : Option String ← match spec.resumeFrom with
    | none     => pure none
    | some old => pure ((← loadSession old).bind (·.agentSessionId))
  let record : SessionRecord := {
    id, createdAt := now, lastActivityAt := now
    upstream := spec.upstream, fork := spec.fork
    backend := backendName, model := spec.model
    budget := spec.budget.getD 20.0
    slot
    agentSessionId := ← newUuid
    resumedFrom := spec.resumeFrom
  }
  saveSession record
  -- From here on the slot is held, so every exit releases it.
  let fail (msg : String) : IO (Except String SessionRecord) := do
    try mgr.releaseSlot spec.fork slot catch _ => pure ()
    let now ← TaskStore.currentIso8601
    let r := { record with status := .failed, endedAt := some now, error := some msg }
    saveSession r
    appendEvent id 1 now (.notice "error" msg)
    saveSession { r with lastEventSeq := 1 }
    return .error msg
  try
    let jwt ← GitHub.createJWT appConfig.appId appConfig.privateKeyPath
    let installationId ← match appConfig.installationId with
      | some i => pure i
      | none   => GitHub.getInstallationId jwt spec.fork.owner
    let token ← GitHub.createInstallationToken jwt installationId
    GitHub.setupGhAuth token
    let repoPath ← Repo.ensureSlot spec.fork spec.upstream
      { slot, occupant := some id } (token := some token)
    let (port, shutdownMcp) ← Server.start {
      upstream := spec.upstream, fork := spec.fork
      allowedTools := spec.tools.getD allOptionalTools
      appId := appConfig.appId, privateKeyPath := appConfig.privateKeyPath
      installationId, pat := appConfig.pat
      agentBackend := backendName
    }
    -- A session goes through the same resolver as a queued run, so an account the daemon has
    -- already found to be out of quota is not handed to a person either.
    let authLabel ← match ← Usage.resolveLabel appConfig backendName [] none none spec.model with
      | .ok label => pure label
      | .error e  => do
        shutdownMcp
        return ← fail s!"no usable authentication source for '{backendName}': {e}"
    if let some l := authLabel then Usage.markUsed backendName l
    let apiKeyEnv ← TaskRunner.resolveAuthEnv appConfig agentDef backendName authLabel
    let extraPorts := appConfig.agentAuthConfigs.find? (fun c => c.name == backendName)
      |>.map (·.extraPorts) |>.getD #[]
    let recordRef ← IO.mkRef record
    let lock ← Std.BaseMutex.new
    -- The pump needs the session to deliver events to, and the session needs the pump. Broken
    -- with a ref the callback reads: nothing arrives before `launchStreaming` returns, because
    -- the agent has not been spawned yet.
    let liveRef ← IO.mkRef (none : Option LiveSession)
    let onEvent (e : Event) : IO Unit := do
      if let some s ← liveRef.get then onAgentEvent s e
    let some stream ← Sandbox.launchStreaming agentDef repoPath port token
        { mcpContext := "", model := spec.model, systemPrompt := spec.systemPrompt,
          resume := resumeAgentSession, sessionId := record.agentSessionId,
          budget := record.budget }
        onEvent (debug := debug)
        (extraEnv := apiKeyEnv) (pluginDirs := appConfig.pluginDirs)
        (extraPorts := extraPorts) (additionalPaths := appConfig.additionalSandboxPaths)
      | do
        shutdownMcp
        return ← fail s!"backend '{backendName}' cannot host an interactive session"
    let session : LiveSession := {
      id, fork := spec.fork, slot, record := recordRef, lock, stream, shutdownMcp
      authLabel, backend := backendName
    }
    liveRef.set (some session)
    mgr.sessions.atomically <| modify (·.push session)
    update session fun r => { r with status := .idle }
    return .ok (← recordRef.get)
  catch e =>
    fail s!"could not start the session: {e}"

/-! ## Talking to one -/

/-- Post a turn. Answers the seq the turn was written at, so a caller can start reading from it.

    Refused rather than queued when a turn is already running: the agent would take the second
    line as soon as it finished the first, and a person who typed twice would get two answers in
    an order nobody chose. -/
def Manager.send (mgr : Manager) (id : String) (text : String) : IO (Except String Nat) := do
  let some s ← find mgr id | return .error "no such session"
  let r ← s.record.get
  if r.status.isTerminal then
    return .error s!"this session has {statusWord r.status}"
  if r.status == .running then
    return .error "this session is working on a turn; interrupt it or wait for it to finish"
  if (← s.stream.hasExited) then
    let out ← s.stream.stderrSoFar
    teardown mgr s .failed (some "the agent process exited")
    return .error s!"the agent process is gone{if out.isEmpty then "" else s!": {out}"}"
  let seq ← append s (.user text)
  update s fun r => { r with
    status := .running
    turnCount := r.turnCount + 1
    title := r.title.orElse fun _ => some (titleOf text) }
  let _ ← append s (.turnStarted (← s.record.get).turnCount)
  try
    s.stream.sendLine (Wire.userTurn text)
    return .ok seq
  catch e =>
    let _ ← append s (.notice "error" s!"the turn could not be delivered: {e}")
    teardown mgr s .failed (some s!"the turn could not be delivered: {e}")
    return .error s!"the turn could not be delivered: {e}"
where
  statusWord : SessionStatus → String
    | .failed => "failed"
    | _       => "ended"

/-- Abandon the turn in flight, keeping the process and the conversation.

    The turn ends the way every turn ends — with a result on the stream — so nothing here waits
    for the acknowledgement. -/
def Manager.interrupt (mgr : Manager) (id : String) : IO (Except String Unit) := do
  let some s ← find mgr id | return .error "no such session"
  let r ← s.record.get
  if r.status != .running then
    return .error "this session is not working on a turn"
  let requestId ← TaskStore.generateId
  try
    s.stream.sendLine (Wire.interrupt requestId)
    let _ ← append s (.notice "info" "the turn was interrupted")
    return .ok ()
  catch e =>
    return .error s!"the interrupt could not be delivered: {e}"

/-- End a session and release everything it holds. -/
def Manager.close (mgr : Manager) (id : String) : IO (Except String Unit) := do
  let some s ← find mgr id | do
    -- A session the daemon is not holding but that exists on disk is one a restart already
    -- closed. Ending it again is not a failure; it is a request that no longer applies.
    match ← loadSession id with
    | some r => return if r.status.isTerminal then .ok () else .error "no such session"
    | none   => return .error "no such session"
  let _ ← append s (.notice "info" "the session was closed")
  teardown mgr s .ended none
  return .ok ()

/-! ## Keeping the table honest -/

/-- Close sessions that have sat without a turn for longer than the timeout, and any whose agent
    has gone without saying so. Called on the daemon's tick. -/
def Manager.reap (mgr : Manager) : IO Unit := do
  let ss ← mgr.sessions.atomically get
  for s in ss do
    if ← s.stream.hasExited then
      let _ ← append s (.notice "error" "the agent process exited")
      teardown mgr s .failed (some "the agent process exited")
      continue
    let r ← s.record.get
    if r.status == .running then continue
    let idleFor ← secondsSince r.lastActivityAt
    if idleFor ≥ mgr.cfg.idleTimeoutSeconds then
      let _ ← append s (.notice "info"
        s!"the session was closed after {mgr.cfg.idleTimeoutSeconds}s without a turn")
      teardown mgr s .ended none
where
  /-- Seconds between an RFC 3339 instant and now, via `date`, which is how the rest of the
      daemon does arithmetic on the timestamps it writes. `0` for anything unparseable, so a
      malformed record is never reaped for being old. -/
  secondsSince (iso : String) : IO Nat := do
    try
      let out ← IO.Process.output { cmd := "date", args := #["-u", "+%s", "-d", iso] }
      let then_ := out.stdout.trimAscii.toString.toNat!
      let nowOut ← IO.Process.output { cmd := "date", args := #["-u", "+%s"] }
      let now := nowOut.stdout.trimAscii.toString.toNat!
      return if now > then_ then now - then_ else 0
    catch _ => return 0

/-- Close every live session. The daemon's shutdown path. -/
def Manager.closeAll (mgr : Manager) : IO Unit := do
  for s in ← mgr.sessions.atomically get do
    let _ ← append s (.notice "info" "the daemon is shutting down")
    teardown mgr s .ended (some "the daemon shut down")

/-- Close every session on disk that is not already terminal.

    Run at start-up, before anything else can read them. The processes those records describe
    died with the last daemon, and a session left reading `running` is a conversation that will
    never answer — worse than one that says plainly it is over, because a client will sit on its
    stream waiting. Resuming is offered as a new session; see `SessionSpec.resumeFrom`. -/
def reconcile : IO Unit := do
  for r in ← loadAllSessions do
    unless r.status.isTerminal do
      let now ← TaskStore.currentIso8601
      let seq := r.lastEventSeq + 1
      appendEvent r.id seq now (.notice "error"
        "the daemon restarted, so this session's agent is gone; start a new session resuming \
this one to pick the conversation up")
      saveSession { r with
        status := .ended, endedAt := some now, lastEventSeq := seq, lastActivityAt := now
        error := some "the daemon restarted" }

end Orchestra.Interactive
