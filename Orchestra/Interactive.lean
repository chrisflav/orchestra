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
  /-- The environment the agent lives in, held for as long as the conversation is.

      One session, not one per turn, for the same reason the process is one: a conversation is a
      thing that continues, and on a backend that runs the agent elsewhere the workspace was
      carried there once. Released by `teardown`, which is also what brings it back. -/
  exec : Exec.Session
  /-- What the session had spent before this process started. See `onAgentEvent`. -/
  costBase : Float
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
    -- one would otherwise leave the record naming a session nobody can resume. `agentStarted`
    -- is set from the same event because this is the only moment anything knows the CLI has a
    -- conversation under that id — which is what a later `--resume` needs and what nothing else
    -- on the record can stand in for.
    update s fun r => { r with agentSessionId := some sid, agentStarted := true }
  let _ ← append s (.agent event)
  if let .result sub _ durationMs cost _ := event then
    -- `total_cost_usd` is the CLI's own running total for the process, not this turn's bill —
    -- it returns a module-global. Adding it up across a session that keeps one process for
    -- every turn therefore counts turn 1 once, turn 2 twice, and reads far over budget while
    -- the CLI, enforcing `--max-budget-usd` against the true total, is nowhere near it. The
    -- record takes the total as given; the turn reports the difference it made.
    let r ← s.record.get
    -- `costBase` is what the session had spent before *this* process started, which is zero for
    -- a session that has only ever had one. A woken session has had more than one, and the
    -- CLI's total counts only its own: without the base, the first turn after a wake would
    -- overwrite the accumulated spend with a few cents and report a negative cost for itself.
    let processTotal := (cost.bind (·.getNum?.toOption) |>.map (·.toFloat)).getD
      (r.costUsd - s.costBase)
    let total := s.costBase + processTotal
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

/-- What to say about an id the table does not hold.

    "No such session" is true of an id that was never real and false of one that ended a moment
    ago — and the second is the common case, because a session leaves the table the instant it
    is torn down while its record stays on disk forever. The record is right there; reading it
    is what makes the answer to "why won't it take my turn" the reason rather than a denial that
    the conversation existed. `close` has answered this way from the start. -/
private def gone (id : String) : IO String := do
  match ← loadSession id with
  | some r =>
    if r.status.isTerminal then
      return s!"this session has {if r.status == .failed then "failed" else "ended"}"
    -- Dormant reaches here from `interrupt`, which cannot wake anything: there is no turn in
    -- flight to abandon, and starting an agent in order to stop it is not what was asked for.
    else if r.status == .dormant then
      return "this session is asleep; say something to wake it"
    -- A wake takes as long as a start does, and for that minute the record reads `starting`
    -- while nothing holds the session. Saying "no such session" there denies a conversation
    -- that is in the middle of coming back.
    else if r.status == .starting then
      return "this session is starting up; try again in a moment"
    else return "no such session"
  | none => return "no such session"

/-! ## Starting one -/

/-- Characters that can appear inside a filesystem path as an error message renders it. -/
private def pathChar (c : Char) : Bool :=
  c.isAlphanum || c == '/' || c == '.' || c == '_' || c == '-' || c == '+'

private partial def redactAux (rest : List Char) (prev : Char) (acc : String) : String :=
  match rest with
  | [] => acc
  | c :: tl =>
    -- `prev != ':'` is what keeps a URL intact: `https://…` is not a path, and cutting it to
    -- its last component would take the host out of the one message where it is the answer.
    if c == '/' && !pathChar prev && prev != ':' then
      let run  := rest.takeWhile pathChar
      let tail := rest.dropWhile pathChar
      let base := ((String.ofList run).splitOn "/").getLastD ""
      redactAux tail (run.getLastD '/')
        (acc ++ (if base.isEmpty then "…" else "…/" ++ base))
    else
      redactAux tl c (acc.push c)

/-- Cut every absolute path in a message down to its last component.

    A failure to start is reported to whoever asked for the session, over an API that is not the
    daemon's log. The exceptions that reach it name files: `createJWT` on an unreadable key
    fails with the path to the GitHub App private key, `ensureSlot` with the layout of the data
    directory. None of that is the caller's business, and the part that is — *which* file, and
    what went wrong with it — survives the trim. The daemon still writes the untouched text to
    its own stderr, where an operator can read it. -/
def redactPaths (msg : String) : String :=
  redactAux msg.toList ' ' ""

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
  -- After the agent, before the slot: closing the environment is what copies a remote workspace
  -- back onto the slot this is about to release, and it has nothing to copy while the agent is
  -- still writing.
  try s.exec.close catch _ => pure ()
  try s.shutdownMcp catch _ => pure ()
  try mgr.releaseSlot s.fork s.slot catch _ => pure ()
  let now ← TaskStore.currentIso8601
  try
    update s fun r =>
      if r.status.isTerminal then r
      -- Dormant is not an ending, so it does not get stamped with one. The record keeps its
      -- `endedAt` empty because the conversation has not ended; only the process has.
      else if status == .dormant then { r with status := .dormant, error := why }
      else { r with status, endedAt := some now, error := why }
  catch _ => pure ()

private def find (mgr : Manager) (id : String) : IO (Option LiveSession) := do
  let ss ← mgr.sessions.atomically get
  return ss.find? (·.id == id)

/-- Acquire everything a session needs and put it in the table, for a record `prepare` has just
    written to disk.

    Shared by starting a new session and waking a dormant one, which differ only in where the
    record comes from and which agent-side conversation is resumed. The caller has already
    checked the backend, counted the session against the cap and reserved `record.slot`; from
    here on the slot is held, so from here on everything is inside the handler that gives it
    back — including `prepare`, which reads a clock, `/dev/urandom` and the session store, any
    of which can throw. An earlier version opened its `try` after the record was written, and a
    throw there escaped `start` entirely: the socket handler logged it and closed the connection
    without a reply, and the slot stayed in `activeSlots` with nothing holding it — no
    `LiveSession`, so nothing the reaper could ever find. With `parallel_per_repo: 1` that
    repository was finished for the life of the daemon.

    `prepare` answers the record it saved, the agent-side session to resume (`none` to start a
    fresh one), and the slot occupant whose working tree may be kept. -/
private def Manager.acquire (mgr : Manager) (appConfig : AppConfig) (fork : Repository)
    (slot : Nat) (agentDef : AgentDef) (debug : Bool) (onFailure : SessionStatus)
    (prepare : IO (SessionRecord × Option String × Option String))
    : IO (Except String SessionRecord) := do
  -- `shutdownRef` is how the MCP server joins the slot's guarantee. It is empty until
  -- `Server.start` succeeds and holds that server's own shutdown afterwards, so the handler
  -- below closes it on every failing path rather than only the two that were remembered by
  -- hand — a leaked MCP server is a loopback port still serving `create_pr`, `merge_pr` and
  -- `comment` against a live installation, with no session and no way to reach it.
  let shutdownRef ← IO.mkRef (none : Option (IO Unit))
  -- The execution environment joins the same guarantee, and for a sharper reason than the MCP
  -- server: on a backend that runs the agent elsewhere this is a pod holding a copy of the
  -- workspace, and a failing acquire that left it behind would keep both until its deadline.
  let execRef ← IO.mkRef (none : Option Exec.Session)
  let recordRef ← IO.mkRef (none : Option SessionRecord)
  -- `fail` is called from inside the `try` as well as from the `catch`, and its own writes can
  -- throw on a full or torn disk — which would land in the `catch` and call it a second time.
  -- Releasing a clone slot twice is not a harmless repeat: the second release takes it from
  -- whichever session or task has since been given it. See `teardown`.
  let failed ← IO.mkRef false
  let dropStarting : IO Unit :=
    mgr.sessions.atomically <| mgr.starting.modify fun n => if n > 0 then n - 1 else 0
  let release : IO Unit := do
    if let some ex ← execRef.get then try ex.close catch _ => pure ()
    if let some shut ← shutdownRef.get then try shut catch _ => pure ()
    try mgr.releaseSlot fork slot catch _ => pure ()
  let fail (raw : String) : IO (Except String SessionRecord) := do
    if ← failed.get then return .error (redactPaths raw)
    failed.set true
    release
    dropStarting
    -- The operator's copy is the whole truth; the caller's is the same sentence with the
    -- daemon's filesystem taken out of it. See `redactPaths`.
    try IO.eprintln s!"interactive: {raw}" catch _ => pure ()
    let msg := redactPaths raw
    -- Only stamp a record if one was written; before that there is nothing on disk to stamp.
    if let some captured ← recordRef.get then
      let now ← TaskStore.currentIso8601
      -- Re-read rather than write back what was captured before any of this started. A `close`
      -- can land while a wake is failing, and an end is not something a failed acquire may
      -- undo: the caller was told the session was over and it has to stay over. The captured
      -- copy is the fallback for a record that has since become unreadable.
      let current := (← loadSession captured.id).getD captured
      -- Where a session lands when it could not be brought up. A *start* that fails has failed:
      -- there is no conversation yet, and the record exists only to say what went wrong. A
      -- *wake* that fails has not — the transcript is intact and the daemon's GitHub App being
      -- misconfigured for a minute is no reason to throw away what was said — so it goes back
      -- to sleep and the next turn tries again. Either way `slot` is left as it was: this
      -- acquire never got a working tree into the slot it reserved, and a record naming a slot
      -- the session has never occupied loses the pin to the tree it actually left behind.
      let r :=
        if current.status.isTerminal then current
        else if onFailure == .dormant then { current with status := .dormant, error := some msg }
        else { current with status := .failed, endedAt := some now, error := some msg }
      -- The seq continues from wherever the transcript got to. The pump may have written the
      -- `init` event already, and reusing seq 1 would put two events at one cursor position.
      let seq := r.lastEventSeq + 1
      appendEvent r.id seq now (.notice "error" msg)
      saveSession { r with lastEventSeq := seq }
    return .error msg
  try
    let (record, resumeAgentSession, resumeSlotOf) ← prepare
    recordRef.set (some record)
    let backendName := record.backend
    let jwt ← GitHub.createJWT appConfig.appId appConfig.privateKeyPath
    let installationId ← match appConfig.installationId with
      | some i => pure i
      | none   => GitHub.getInstallationId jwt fork.owner
    -- Deliberately no `GitHub.setupGhAuth`: that writes the token into
    -- `~/.config/gh/hosts.yml`, which every concurrently running task in this daemon shares.
    -- `TaskRunner` avoids it for the same reason, and it is not needed — the token reaches the
    -- clone through `ensureSlot` and the agent through `GH_TOKEN` in the sandbox. A session
    -- doing it would silently re-point every other `gh` call in the process at its own
    -- installation, and leave the token on disk afterwards.
    let token ← GitHub.createInstallationToken jwt installationId
    -- `resumeFrom` on the assignment is what keeps a working tree instead of resetting it, and
    -- it is checked against the slot's recorded occupant, so asking for it is safe even when the
    -- tree has since been taken. Without it a resumed conversation was restored onto a wiped
    -- tree: the agent opens with a context full of edits it made and a checkout where none of
    -- them exist — the exact hazard the slot-pinning design exists to avoid, reintroduced at the
    -- one moment it matters most.
    let repoPath ← Repo.ensureSlot fork record.upstream
      { slot, occupant := some record.id, resumeFrom := resumeSlotOf } (token := some token)
    -- The environment this conversation happens in, opened before the MCP server because it is
    -- what decides where that server has to listen and whether it needs a token.
    let execBackend ← match ← Exec.resolve appConfig.execution with
      | .ok b     => pure b
      | .error e  => return ← fail s!"cannot run the agent: {e}"
    let (mcpBind, mcpPorts, mcpToken) ← Exec.mcpBinding execBackend
    let pluginDirs ← TaskRunner.defaultPluginDirs appConfig
    let repoConfig ← RepoConfig.loadRepoConfig repoPath
    let execSession ← execBackend.openSession {
      workdir := repoPath
      grants  := Sandbox.grantsFor agentDef.sandboxPaths appConfig.additionalSandboxPaths
                   repoPath false pluginDirs #[]
      label   := record.id
      repo    := some fork.toString
      image   := repoConfig.image }
    execRef.set (some execSession)
    -- Reviving a dormant conversation asks the agent to resume a session it wrote under its own
    -- home. An environment that is new every time does not have it, and a revived conversation
    -- that silently starts over is worse than one that says it cannot: the person sees their
    -- history in the transcript and the agent does not.
    if resumeAgentSession.isSome && !execSession.carriesAgentState then
      return ← fail s!"this conversation was left in an environment that no longer exists ({execSession.id} is new every time). Configure a persistent agent home for this execution backend — execution.options.home_claim for kubernetes — to wake sessions on it."
    let (port, shutdownMcp) ← Server.start {
      repo := some { upstream := record.upstream, fork }
      allowedTools := record.tools.getD allOptionalTools
      appId := appConfig.appId, privateKeyPath := appConfig.privateKeyPath
      installationId := some installationId, pat := appConfig.pat
      agentBackend := backendName
      authToken := mcpToken
    } (bindHost := mcpBind) (portRange := mcpPorts)
    shutdownRef.set (some shutdownMcp)
    -- A session goes through the same resolver as a queued run, so an account the daemon has
    -- already found to be out of quota is not handed to a person either.
    let authLabel ← match ← Usage.resolveLabel appConfig backendName [] none none record.model with
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
    -- What is *left* of the budget, not all of it. `--max-budget-usd` bounds the process, and a
    -- woken session is a second process: handing it the whole figure again would give a session
    -- that had spent $19 of $20 a fresh $20, once per wake, and the budget would bound nothing.
    -- Floored at a cent because zero is not a budget the CLI will take.
    let remaining := max 0.01 (record.budget - record.costUsd)
    let some stream ← Sandbox.launchStreaming agentDef repoPath port token
        { mcpContext := "", model := record.model, systemPrompt := record.systemPrompt,
          resume := resumeAgentSession, sessionId := record.agentSessionId,
          budget := remaining }
        onEvent (debug := debug)
        (extraEnv := apiKeyEnv) (pluginDirs := pluginDirs)
        (extraPorts := extraPorts) (additionalPaths := appConfig.additionalSandboxPaths)
        (session := execSession) (mcpToken := mcpToken)
      | return ← fail s!"backend '{backendName}' cannot host an interactive session"
    let session : LiveSession := {
      id := record.id, fork, slot, record := recordRef, lock, stream, shutdownMcp
      exec := execSession
      authLabel, backend := backendName, costBase := record.costUsd
    }
    liveRef.set (some session)
    mgr.sessions.atomically do
      modify (·.push session)
      mgr.starting.modify fun n => if n > 0 then n - 1 else 0
    -- Registered. From here the session owns the slot and the MCP server, and `fail` must not
    -- take either back — `teardown` is the only thing that may, and only via the table. A throw
    -- past this point would otherwise hand a running agent's clone slot to the queue.
    shutdownRef.set none
    execRef.set none
    -- `slot` is written here and nowhere earlier: it is only true once `ensureSlot` has put a
    -- working tree in it, and a record naming a slot a failed acquire merely reserved would send
    -- the next wake looking for its tree in the wrong place — and, finding none, reset the right
    -- one out from under the conversation.
    try update session fun r => { r with status := .idle, slot }
    catch _ => pure ()
    return .ok (← session.record.get)
  catch e =>
    fail s!"could not bring the session up: {e}"

/-- Move a stored session from one status to another, under the table's mutex, or answer `none`
    because it was not in the status the caller expected.

    The lock a session has on disk. A `LiveSession` is serialised by its own `lock`, but a
    dormant session has no `LiveSession` — and two things now want to write its record without
    one: a turn that wakes it, and a request to end it. Left unguarded both are blind
    read-modify-writes of the same file, and the two races are not theoretical:

    * six turns posted at once each passed the "is it dormant" check, and six wakes ran on one
      id — six clone slots, six MCP servers, six agent processes and six writers on one record,
      with `teardown` able to release only one of them because it claims by id and finds them
      all. Twelve transcript events landed at two cursor positions, which is the one thing the
      seq is supposed to make impossible;
    * an end issued while a wake was failing was acknowledged, and then undone when the wake
      wrote the session back to `dormant` — a `204` for a conversation that stayed alive.

    Both close on the same observation: the status *is* the claim, if the check and the write
    happen together. A session already in the table is never claimable this way, because
    something is holding it and that something is the writer. -/
private def Manager.claimRecord (mgr : Manager) (id : String) (expected next : SessionStatus)
    : IO (Option SessionRecord) :=
  mgr.sessions.atomically do
    let live ← get
    if live.any (·.id == id) then return none
    match ← loadSession id with
    | none => return none
    | some r =>
      if r.status != expected then return none
      let moved := { r with status := next }
      saveSession moved
      return some moved

/-- Take a place under the session cap, or say why there is none. -/
private def Manager.claimPlace (mgr : Manager) : IO (Option String) := do
  let held ← mgr.sessions.atomically do
    let live ← get
    let starting ← mgr.starting.get
    let held := live.size + starting
    if held < mgr.cfg.maxSessions then mgr.starting.set (starting + 1)
    return held
  if held ≥ mgr.cfg.maxSessions then
    return some s!"the daemon is already holding {held} interactive \
session{if held == 1 then "" else "s"}, which is its limit; end one first"
  return none

/-- Start a session: acquire everything it will hold, launch the agent, and register it.

    The prologue is `Main.interactiveHandler`'s, moved where a later HTTP request can reach what
    it built. -/
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
  if let some why ← mgr.claimPlace then return .error why
  let some slot ← mgr.reserveSlot spec.fork
    | do
        mgr.sessions.atomically <| mgr.starting.modify fun n => if n > 0 then n - 1 else 0
        return .error s!"no free clone slot for {spec.fork}: every one is in use by a running \
task or another session"
  mgr.acquire appConfig spec.fork slot agentDef debug .failed do
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
      tools := spec.tools, systemPrompt := spec.systemPrompt
    }
    saveSession record
    return (record, resumed.bind (·.agentSessionId),
            resumed.filter (·.slot == slot) |>.map (·.id))

/-- Bring a dormant session back up, resuming the conversation it was having.

    Same id, same transcript, same record — only the process, the clone slot and the MCP server
    are new, which is the whole point: they are what a session should not hold while nobody is
    talking to it, and the conversation is not one of them. -/
def Manager.wake (mgr : Manager) (appConfig : AppConfig) (id : String) (debug : Bool := false)
    : IO (Except String SessionRecord) := do
  let some peek ← loadSession id | return .error "no such session"
  -- Cheap rejection before anything is reserved. The claim below is what actually decides — this
  -- only keeps the common "it is over" and "someone else is already waking it" cases from
  -- taking a place under the cap and a clone slot on the way to being refused.
  unless peek.status == .dormant do return .error (← gone id)
  let agentDef := TaskRunner.agentDefOfBackend (some peek.backend)
  if (agentDef.buildStreamArgs { mcpContext := "" }).isNone then
    return .error s!"backend '{peek.backend}' cannot host an interactive session"
  if let some why ← mgr.claimPlace then return .error why
  -- The claim, and the point of no return for every other caller: from here the record reads
  -- `starting`, so a second turn arriving behind this one is told the session is coming up
  -- rather than starting a second one, and an end is refused rather than silently reversed.
  let some record ← mgr.claimRecord id .dormant .starting
    | do
        mgr.sessions.atomically <| mgr.starting.modify fun n => if n > 0 then n - 1 else 0
        return .error "this session is already waking up"
  let some slot ← mgr.reserveSlot record.fork
    | do
        mgr.sessions.atomically <| mgr.starting.modify fun n => if n > 0 then n - 1 else 0
        return .error s!"no free clone slot for {record.fork}: every one is in use by a running \
task or another session"
  mgr.acquire appConfig record.fork slot agentDef debug .dormant do
    let now ← TaskStore.currentIso8601
    let seq := record.lastEventSeq + 1
    appendEvent record.id seq now (.notice "info" "waking the session up")
    -- `slot` is deliberately not written here; `acquire` sets it once there is a tree in it.
    let woken := { record with lastActivityAt := now, lastEventSeq := seq, error := none }
    saveSession woken
    -- Resumed only if an agent ever announced itself under that id. See `agentStarted`.
    let resumeAgentSession := if record.agentStarted then record.agentSessionId else none
    -- The tree this session left behind, if the slot it left it in is the one just handed back.
    let resumeSlotOf := if slot == record.slot then some record.id else none
    return (woken, resumeAgentSession, resumeSlotOf)

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
def Manager.send (mgr : Manager) (appConfig : AppConfig) (id : String) (text : String)
    (debug : Bool := false) : IO (Except String Nat) := do
  -- A turn posted to a session the daemon is not holding is not necessarily a mistake. A session
  -- whose process was put down for being idle, or whose daemon restarted under it, is dormant:
  -- the conversation is on disk and nothing is running. Posting to one is how you pick it up —
  -- the wake happens here rather than as a thing the caller has to know to ask for, because
  -- "say something to this session" is the same request whether or not an agent happens to be
  -- up, and every client would otherwise have to implement the same two-step by hand.
  let s ← match ← find mgr id with
    | some s => pure s
    | none =>
      match (← loadSession id).map (fun r => r.status) with
      | some SessionStatus.dormant =>
        match ← mgr.wake appConfig id debug with
        -- Unprefixed: every sentence `wake` answers with already says what happened, and "the
        -- session could not be woken: this session is starting up" says it twice and worse.
        | .error e => return .error e
        | .ok _ =>
          match ← find mgr id with
          | some s => pure s
          | none   => return .error "the session was woken but is not holding"
      | _ => return .error (← gone id)
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
  let some s ← find mgr id | return .error (← gone id)
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
    match ← loadSession id with
    -- Already over: ending it again is not a failure, it is a request that no longer applies.
    | some r =>
      if r.status.isTerminal then return .ok ()
      -- Dormant. Nothing is running, so there is nothing to shut down — but the caller is
      -- asking for the conversation to be over, and only an explicit end says that. This is
      -- what makes "closed" and "put down for being idle" different states rather than the
      -- same one reached two ways.
      else if r.status == .dormant then
        -- Through the claim, so an end cannot land on a session a turn is already waking — and
        -- cannot be undone by one. If the claim is lost the session is coming up, and ending it
        -- is a request for the state it is leaving.
        match ← mgr.claimRecord id .dormant .ended with
        | none => return .error (← gone id)
        | some ended =>
          let now ← TaskStore.currentIso8601
          let seq := ended.lastEventSeq + 1
          appendEvent id seq now (.notice "info" "the session was closed")
          saveSession { ended with
            endedAt := some now, lastEventSeq := seq, lastActivityAt := now }
          return .ok ()
      else return .error (← gone id)
    | none => return .error "no such session"
  let _ ← append s (.notice "info" "the session was closed")
  teardown mgr s .ended none
  return .ok ()

/-! ## Keeping the table honest -/

/-- Close sessions that have sat without a turn for longer than the timeout, and any whose agent
    has gone without saying so. Called on the daemon's tick. -/
def Manager.reap (mgr : Manager) : IO Unit := do
  let ss ← mgr.sessions.atomically get
  for s in ss do
    let r ← s.record.get
    -- Asked *before* the process is, because the two are not mutually exclusive and only one of
    -- them is news. A session can reach a terminal state without anything having torn it down —
    -- spending the budget is the case that does it: the pump sees `error_max_budget_usd`, writes
    -- the record as `ended` because every further turn would be refused, and stops there,
    -- leaving the entry in the table holding the clone slot, the MCP server and the process.
    -- Whether the agent then exits or stays up refusing turns is the vendor CLI's business, and
    -- checking the process first meant a session that ended cleanly on its budget was announced
    -- to the reader as a crash — an `error` notice saying the agent exited, under a record that
    -- already said why it ended. A session that knows how it finished is not told it crashed.
    --
    -- `teardown` leaves an already-terminal record exactly as it is, so this releases what is
    -- held without rewriting what happened.
    if r.status.isTerminal then
      teardown mgr s r.status r.error
      continue
    if ← s.stream.hasExited then
      let _ ← append s (.notice "error" "the agent process exited")
      teardown mgr s .failed (some "the agent process exited")
      continue
    if r.status == .running then continue
    let idleFor ← secondsSince r.lastActivityAt
    if idleFor ≥ mgr.cfg.idleTimeoutSeconds then
      -- Put down, not ended. What an idle session is wasting is a clone slot, an MCP server and
      -- an agent process — and a conversation is none of those. The timeout takes the three that
      -- cost something and leaves the one that does not, so the next turn picks up where this
      -- one left off instead of finding a session that expired while nobody was looking.
      let _ ← append s (.notice "info"
        s!"no turn for {mgr.cfg.idleTimeoutSeconds}s, so the agent was stopped; say something \
to pick the conversation up")
      teardown mgr s .dormant none
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
    let _ ← append s (.notice "info"
      "the daemon is shutting down, so the agent was stopped; say something to pick the \
conversation up")
    teardown mgr s .dormant none

/-- Put every session on disk that is not already terminal to sleep.

    Run at start-up, before anything else can read them. The processes those records describe
    died with the last daemon, and a session left reading `running` is a conversation that will
    never answer — worse than one that says what state it is in, because a client will sit on its
    stream waiting for it. What died is the process, though, not the conversation: the record and
    the transcript are exactly as they were, so these come back as `dormant` and the next turn
    posted to one starts an agent and resumes it. A session already dormant is left alone, or
    every restart would append the same notice to a transcript nobody was adding to. -/
def reconcile : IO Unit := do
  for r in ← loadAllSessions do
    unless r.status.isTerminal || r.status == .dormant do
      let now ← TaskStore.currentIso8601
      let seq := r.lastEventSeq + 1
      appendEvent r.id seq now (.notice "info"
        "the daemon restarted, so this session's agent is gone; say something to pick the \
conversation up")
      saveSession { r with
        status := .dormant, lastEventSeq := seq, lastActivityAt := now, error := none }

end Orchestra.Interactive
