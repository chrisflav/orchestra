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
  /-- Sessions past the cap check but not yet in `sessions`.

      A session takes seconds to start — a clone, a token, a sandbox — and counting only the
      table meant two requests arriving together both saw room and both took it. Counted under
      the same mutex as the table, so the cap is a decision rather than an observation. -/
  private starting : IO.Ref Nat
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
  -- Recorded against the source this session is spending, exactly as a queued run does. Without
  -- it the resolver keeps offering an account whose window is spent, and the next tasks
  -- dispatched to it each clone a slot, mint a token, start a sandbox and die on the limit —
  -- the waste `markLimited` exists to prevent.
  if let .rateLimit reset := event then
    if let some label := s.authLabel then
      Usage.markLimited s.backend label none "the agent reported a usage limit"
        (resetHint := reset)
  if let .init sid _ := event then
    -- Belt and braces: the id was assigned before launch, but a CLI that decided on a different
    -- one would otherwise leave the record naming a session nobody can resume.
    update s fun r => { r with agentSessionId := some sid }
  let _ ← append s (.agent event)
  if let .result sub _ durationMs cost _ := event then
    -- `total_cost_usd` is the CLI's own running total for the process, not this turn's bill —
    -- it returns a module-global. Adding it up across a session that keeps one process for
    -- every turn therefore counts turn 1 once, turn 2 twice, and reads far over budget while
    -- the CLI, enforcing `--max-budget-usd` against the true total, is nowhere near it. The
    -- record takes the total as given; the turn reports the difference it made.
    let r ← s.record.get
    let total := (cost.bind (·.getNum?.toOption) |>.map (·.toFloat)).getD r.costUsd
    let _ ← append s (.turnEnded r.turnCount (subtypeName sub) (some (total - r.costUsd))
                                 (durationMs.map (· / 1000)))
    update s fun r => { r with
      status  := if r.status.isTerminal then r.status else .idle
      costUsd := total }
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
  return { sessions := ← Std.Mutex.new #[], starting := ← IO.mkRef 0, cfg, reserveSlot,
           releaseSlot }

/-- Close a live session: stop the agent, shut down its MCP server, release its slot, and stamp
    the record terminal.

    Everything here is best-effort and ordered so that the resources go back even if an earlier
    step throws — a leaked clone slot is never recovered, and the repository it belongs to runs
    one narrower for the life of the daemon. -/
private def teardown (mgr : Manager) (s : LiveSession) (status : SessionStatus)
    (why : Option String) : IO Unit := do
  -- Claim the session out of the table first, and do nothing at all if someone else got there
  -- first. Two callers reach here for the same session routinely — the reaper finding a dead
  -- process while a close request is in flight, `send` noticing the same thing — and releasing
  -- a clone slot twice is not a harmless repeat. The second release removes whichever session
  -- or task has since been given that slot number, so a third claimant takes it too, and two
  -- agents end up in one working tree with `git reset --hard` running under both.
  let claimed ← mgr.sessions.atomically do
    let ss ← get
    if ss.any (·.id == s.id) then
      set (ss.filter (·.id != s.id))
      return true
    else
      return false
  unless claimed do return
  try s.stream.shutdown catch _ => pure ()
  try s.shutdownMcp catch _ => pure ()
  try mgr.releaseSlot s.fork s.slot catch _ => pure ()
  let now ← TaskStore.currentIso8601
  try
    update s fun r =>
      if r.status.isTerminal then r
      else { r with status, endedAt := some now, error := why }
  catch _ => pure ()

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
  let held ← mgr.sessions.atomically do
    let live ← get
    let starting ← mgr.starting.get
    let held := live.size + starting
    if held < mgr.cfg.maxSessions then mgr.starting.set (starting + 1)
    return held
  if held ≥ mgr.cfg.maxSessions then
    return .error s!"the daemon is already holding {held} interactive \
session{if held == 1 then "" else "s"}, which is its limit; end one first"
  let some slot ← mgr.reserveSlot spec.fork
    | return .error s!"no free clone slot for {spec.fork}: every one is in use by a running task \
or another session"
  -- The slot is held from here, so from here everything is inside the handler that gives it
  -- back. An earlier version opened its `try` several statements later, and every one of those
  -- statements could throw — minting an id, reading the clock, reading the session being
  -- resumed, opening `/dev/urandom`, writing the record. A throw there escaped `start`
  -- entirely, the socket handler logged it and closed the connection without a reply, and the
  -- slot stayed in `activeSlots` with nothing holding it: no `LiveSession`, so nothing the
  -- reaper could ever find. With `parallel_per_repo: 1` that repository was finished for the
  -- life of the daemon.
  --
  -- `shutdownRef` is how the MCP server joins the same guarantee. It is empty until
  -- `Server.start` succeeds and holds that server's own shutdown afterwards, so the handler
  -- below closes it on every failing path rather than only the two that were remembered by
  -- hand — a leaked MCP server is a loopback port still serving `create_pr`, `merge_pr` and
  -- `comment` against a live installation, with no session and no way to reach it.
  let shutdownRef ← IO.mkRef (none : Option (IO Unit))
  let recordRef ← IO.mkRef (none : Option SessionRecord)
  let dropStarting : IO Unit :=
    mgr.sessions.atomically <| mgr.starting.modify fun n => if n > 0 then n - 1 else 0
  let release : IO Unit := do
    if let some shut ← shutdownRef.get then try shut catch _ => pure ()
    try mgr.releaseSlot spec.fork slot catch _ => pure ()
  let fail (msg : String) : IO (Except String SessionRecord) := do
    release
    dropStarting
    -- Only stamp a record if one was written; before that there is nothing on disk to stamp.
    if let some record ← recordRef.get then
      let now ← TaskStore.currentIso8601
      let r := { record with status := .failed, endedAt := some now, error := some msg }
      -- The seq continues from wherever the transcript got to. The pump may have written the
      -- `init` event already, and reusing seq 1 would put two events at one cursor position.
      let seq := r.lastEventSeq + 1
      appendEvent r.id seq now (.notice "error" msg)
      saveSession { r with lastEventSeq := seq }
    return .error msg
  try
    let id ← TaskStore.generateId
    let now ← TaskStore.currentIso8601
    -- Resuming inherits the agent-side history of the session named, not the session itself.
    let resumed ← match spec.resumeFrom with
      | none     => pure none
      | some old => loadSession old
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
    recordRef.set (some record)
    let resumeAgentSession := resumed.bind (·.agentSessionId)
    let jwt ← GitHub.createJWT appConfig.appId appConfig.privateKeyPath
    let installationId ← match appConfig.installationId with
      | some i => pure i
      | none   => GitHub.getInstallationId jwt spec.fork.owner
    -- Deliberately no `GitHub.setupGhAuth`: that writes the token into
    -- `~/.config/gh/hosts.yml`, which every concurrently running task in this daemon shares.
    -- `TaskRunner` avoids it for the same reason, and it is not needed — the token reaches the
    -- clone through `ensureSlot` and the agent through `GH_TOKEN` in the sandbox. A session
    -- doing it would silently re-point every other `gh` call in the process at its own
    -- installation, and leave the token on disk afterwards.
    let token ← GitHub.createInstallationToken jwt installationId
    -- `resumeFrom` on the assignment is what keeps the predecessor's working tree instead of
    -- resetting it, and it is checked against the slot's recorded occupant, so asking for it is
    -- safe even when the tree has since been taken. Without it a resumed session restored the
    -- conversation onto a wiped tree: the agent opens with a context full of edits it made and
    -- a checkout where none of them exist — the exact hazard the slot-pinning design exists to
    -- avoid, reintroduced at the one moment it matters most.
    let resumeSlotOf := resumed.filter (·.slot == slot) |>.map (·.id)
    let repoPath ← Repo.ensureSlot spec.fork spec.upstream
      { slot, occupant := some id, resumeFrom := resumeSlotOf } (token := some token)
    let (port, shutdownMcp) ← Server.start {
      upstream := spec.upstream, fork := spec.fork
      allowedTools := spec.tools.getD allOptionalTools
      appId := appConfig.appId, privateKeyPath := appConfig.privateKeyPath
      installationId, pat := appConfig.pat
      agentBackend := backendName
    }
    shutdownRef.set (some shutdownMcp)
    -- A session goes through the same resolver as a queued run, so an account the daemon has
    -- already found to be out of quota is not handed to a person either.
    let authLabel ← match ← Usage.resolveLabel appConfig backendName [] none none spec.model with
      | .ok label => pure label
      | .error e  => return ← fail s!"no usable authentication source for '{backendName}': {e}"
    if let some l := authLabel then Usage.markUsed backendName l
    let apiKeyEnv ← TaskRunner.resolveAuthEnv appConfig agentDef backendName authLabel
    let extraPorts := appConfig.agentAuthConfigs.find? (fun c => c.name == backendName)
      |>.map (·.extraPorts) |>.getD #[]
    let recordRef ← IO.mkRef record
    let lock ← Std.BaseMutex.new
    -- The pump needs the session to deliver events to, and the session needs the pump. Broken
    -- with a ref the callback reads. `launchStreaming` does spawn the child and start the pump
    -- before it returns, so there is a window here in which an event would be dropped — it is
    -- the width of the rest of this function against a `landrun` plus a node start, so in
    -- practice even the `init` event lands after the set. Worth knowing rather than believing
    -- it cannot happen.
    let liveRef ← IO.mkRef (none : Option LiveSession)
    let onEvent (e : Event) : IO Unit := do
      if let some s ← liveRef.get then onAgentEvent s e
    let some stream ← Sandbox.launchStreaming agentDef repoPath port token
        { mcpContext := "", model := spec.model, systemPrompt := spec.systemPrompt,
          resume := resumeAgentSession, sessionId := record.agentSessionId,
          budget := record.budget }
        onEvent (debug := debug)
        (extraEnv := apiKeyEnv) (pluginDirs := ← TaskRunner.defaultPluginDirs appConfig)
        (extraPorts := extraPorts) (additionalPaths := appConfig.additionalSandboxPaths)
      | return ← fail s!"backend '{backendName}' cannot host an interactive session"
    let session : LiveSession := {
      id, fork := spec.fork, slot, record := recordRef, lock, stream, shutdownMcp
      authLabel, backend := backendName
    }
    liveRef.set (some session)
    mgr.sessions.atomically do
      modify (·.push session)
      mgr.starting.modify fun n => if n > 0 then n - 1 else 0
    -- Registered. From here the session owns the slot and the MCP server, and `fail` must not
    -- take either back — `teardown` is the only thing that may, and only via the table. A throw
    -- past this point would otherwise hand a running agent's clone slot to the queue.
    shutdownRef.set none
    try update session fun r => { r with status := .idle }
    catch _ => pure ()
    return .ok (← session.record.get)
  catch e =>
    fail s!"could not start the session: {e}"

/-! ## Talking to one -/

/-- Take the session from `idle` to `running`, and answer the turn number that claim owns.

    The check and the transition are one critical section on purpose. Read-then-act, which is
    what this was, let two turns posted at once both see `idle` and both proceed: the agent took
    the second off the pipe the moment it finished the first, and the transcript recorded two
    `turnStarted 2` and no turn 1 — "two answers in an order nobody chose", which is the exact
    thing refusing a concurrent turn is for. -/
private def claimTurn (s : LiveSession) (text : String) : IO (Except String Nat) := do
  s.lock.lock
  try
    let r ← s.record.get
    if r.status.isTerminal then
      return .error s!"this session has {if r.status == .failed then "failed" else "ended"}"
    if r.status != .idle then
      return .error "this session is working on a turn; interrupt it or wait for it to finish"
    let r := { r with
      status := .running
      turnCount := r.turnCount + 1
      title := r.title.orElse fun _ => some (titleOf text) }
    s.record.set r
    saveSession r
    return .ok r.turnCount
  finally s.lock.unlock

/-- Post a turn. Answers the seq the turn was written at, so a caller can start reading from it.

    Refused rather than queued when a turn is already running: the agent would take the second
    line as soon as it finished the first, and a person who typed twice would get two answers in
    an order nobody chose. -/
def Manager.send (mgr : Manager) (id : String) (text : String) : IO (Except String Nat) := do
  let some s ← find mgr id | return .error "no such session"
  if (← s.stream.hasExited) then
    -- Not the agent's stderr: it is unbounded output from a process holding a GitHub token, and
    -- this string is on its way to an HTTP client. The transcript carries the detail.
    teardown mgr s .failed (some "the agent process exited")
    return .error "the agent process is gone"
  let turn ← match ← claimTurn s text with
    | .error e   => return .error e
    | .ok   turn => pure turn
  let seq ← append s (.user text)
  let _ ← append s (.turnStarted turn)
  try
    s.stream.sendLine (Wire.userTurn text)
    return .ok seq
  catch e =>
    let _ ← append s (.notice "error" s!"the turn could not be delivered: {e}")
    teardown mgr s .failed (some s!"the turn could not be delivered: {e}")
    return .error s!"the turn could not be delivered: {e}"

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
  /-- Seconds between an RFC 3339 instant and now.

      Through `Usage.parseIso8601`, which is pure, unit-tested and what the rest of the daemon
      uses. An earlier version shelled out to `date -u +%s -d <iso>` and read the result with
      `toNat!`. Three things were wrong with that and they compounded: `IO.Process.output` does
      not throw on a non-zero exit, so a failure arrived as empty stdout; `"".toNat!` is a
      *panic*, not an `IO.Error`, so the `catch` could not see it; and the panic's default value
      is `0`, which made the elapsed time the whole Unix epoch. So the one input that was
      supposed to be safe — an unparseable timestamp — reaped every live session on the next
      tick instead of none. `date -d` is also GNU-only, which would have done the same thing on
      BusyBox or macOS unconditionally.

      `0` for anything unparseable, so a malformed record is never reaped for being old. -/
  secondsSince (iso : String) : IO Nat := do
    let some then_ := Usage.parseIso8601 iso | return 0
    let now ← Usage.nowEpoch
    return if now > then_ then (now - then_).toNat else 0

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
