import Orchestra.Interactive.Store
import Orchestra.Interactive.Wire
import Orchestra.Sandbox
import Orchestra.Server
import Orchestra.Repo
import Orchestra.GitHub
import Orchestra.Identity
import Orchestra.Secret
import Orchestra.TaskRunner
import Orchestra.Usage
import Orchestra.Resident.Inbox
import Orchestra.Resident.Zulip
import Orchestra.Resident.EnvFile
import Std.Sync
import Std.Data.HashMap

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
  /-- Defaults and limits for resident sessions (`docs/design/resident-sessions.md`). -/
  residents : ResidentsConfig := {}

/-- The kleis tokens a session holds (`docs/design/resident-sessions.md` §7).

    Every agent process is started on a token of its own — the first on the one derived when the
    environment was opened, each later one (a warm wake, a cycle) on a new one — and the tokens it
    replaces are revoked once it is up. `refresh_kleis_token` adds one more, for the commands of the
    running process (through its env file), and sets `refreshed`, which has the process cycled
    before its next turn. `held` is every token not yet revoked; all of them are revoked when the
    session's environment goes. -/
structure Creds where
  held : Array Kleis.Minted := #[]
  /-- The token the running agent process holds in its environment. -/
  process : Option Kleis.Minted := none
  /-- `refresh_kleis_token` was called since the process started. -/
  refreshed : Bool := false

/-- When the running process's token expires, epoch seconds, if it has one and kleis said. -/
def Creds.processExpires (c : Creds) : Option Int :=
  c.process.bind fun m => if m.expires == 0 then none else some (Int.ofNat m.expires)

/-- A session the daemon is holding right now. Its on-disk record is the durable half; this is
    the half that cannot be written down. -/
structure LiveSession where
  id : String
  /-- The fork whose clone slot this session holds; `none` for a session without a repository,
      which holds none. -/
  fork : Option Repository
  slot : Nat
  /-- The record as it currently stands. Written through to disk under `lock`. -/
  record : IO.Ref SessionRecord
  /-- Serialises appends to the transcript and writes of the record.

      Both the pump thread and whichever thread posted a turn append, and a seq handed to two
      events is a transcript a cursor cannot read. -/
  lock : Std.BaseMutex
  /-- The agent process. `none` while the session is `warm`: a resident's process stopped with
      its environment kept, to be started again in it (`launch`). -/
  stream : IO.Ref (Option Sandbox.StreamingSession)
  /-- The environment the agent lives in, held for as long as the conversation is.

      One session, not one per turn, for the same reason the process is one: a conversation is a
      thing that continues, and on a backend that runs the agent elsewhere the workspace was
      carried there once. Released by `teardown`, which is also what brings it back. -/
  exec : Exec.Session
  /-- What the session had spent before this process started. See `onAgentEvent`. -/
  costBase : IO.Ref Float
  /-- Shuts down this session's MCP server. -/
  shutdownMcp : IO Unit
  /-- The authentication source this session is spending against, if one was resolved. -/
  authLabel : IO.Ref (Option String)
  backend : String
  /-- Start an agent process in this environment: the conversation to resume, if any, and the
      most it may spend. What `acquire` launches the first process with, and what waking a warm
      resident launches the next one with — the environment, the MCP server and the credentials
      are the ones already held. -/
  launch : Option String → Float → IO (Except String Sandbox.StreamingSession)
  /-- The resident this session is, with its settings, when it is one. -/
  resident : Option (String × Identity.ResidentSettings) := none
  /-- The budget day the running process's allowance (`--max-budget-usd`) was computed for. A
      process whose day is over is stopped between turns, so the next one gets the new day's. -/
  budgetDay : IO.Ref String
  /-- Said at the head of the next turn: a conversation that was lost, for one. -/
  notes : IO.Ref (List String)
  /-- The kleis tokens this session holds, and which of them its agent process was started with.
      See `Creds`. -/
  creds : IO.Ref Creds
  /-- Revoke every kleis token the session holds and empty its env file: for a resident put to
      sleep warm, whose next process is started on a token of its own (`launch`). -/
  releaseCreds : IO Unit := pure ()
  /-- Set once `teardown` has claimed the session out of the table. Something that started a
      process for it meanwhile — a warm wake that raced the teardown — reads this when the
      process is up and stops it again, revoking the token it was started on, rather than leave a
      process nobody holds on a token nobody will revoke. -/
  torn : IO.Ref Bool
  /-- When the turn in flight was handed over, epoch seconds; `0` when none is. For
      `residents.turn_timeout_seconds`. -/
  turnStartedAt : IO.Ref Int
  /-- The turn in flight was abandoned on purpose — interrupted by a person, or by the turn
      timeout — so the error result it ends with is not counted as a failure of its own. -/
  abandoned : IO.Ref Bool

/-- The delivery loop's memory of one resident. `lock` serialises deliveries to it — the tick and a
    person's message can both want to hand it a turn, and two at once would wake it twice — and
    lives only here; the rest is `Resident.Runtime`, which is also on the database
    (`resident_runtime`) so that a restart does not forget it: `failures` counts wakes (and
    processes) that failed in a row, for the backoff and for pausing it after
    `Resident.maxFailures`. -/
structure ResidentRuntime where
  lock : Std.BaseMutex
  state : Resident.Runtime := {}

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
  /-- What the delivery loop knows about each resident that is not on its record. -/
  private residentState : Std.Mutex (Std.HashMap String ResidentRuntime)
  /-- Residents being woken from cold right now, counted against `max_awake` with the ones that
      have a process, under `sessions`' mutex. -/
  private residentWaking : IO.Ref Nat
  /-- The residents configuration names, with their settings. Refreshed by the daemon
      (`Manager.setConfigured`). -/
  private configured : IO.Ref (Array (String × Identity.ResidentSettings))

/-- Resident `name`'s runtime: from memory, else from the database (`Resident.loadRuntime`) — so a
    restarted daemon picks up the backoff and failure count where the last one left them. -/
private def Manager.runtime (mgr : Manager) (name : String) : IO ResidentRuntime := do
  if let some rt := (← mgr.residentState.atomically get).get? name then return rt
  let stored ← try Resident.loadRuntime name catch e => do
    IO.eprintln s!"resident {name}: could not read its runtime state: {e}"
    pure {}
  let fresh ← Std.BaseMutex.new
  mgr.residentState.atomically do
    let m ← get
    match m.get? name with
    | some rt => return rt
    | none =>
      let rt : ResidentRuntime := { lock := fresh, state := stored }
      set (m.insert name rt)
      return rt

/-- Change resident `name`'s runtime, and write it through to the database when it changed. -/
private def Manager.modifyRuntime (mgr : Manager) (name : String)
    (f : Resident.Runtime → Resident.Runtime) : IO Resident.Runtime := do
  let current ← mgr.runtime name
  let (before, after) ← mgr.residentState.atomically do
    let m ← get
    let rt := (m.get? name).getD current
    let next := f rt.state
    set (m.insert name { rt with state := next })
    return (rt.state, next)
  if before != after then
    try Resident.saveRuntime name after (← Usage.nowEpoch)
    catch e => IO.eprintln s!"resident {name}: could not record its runtime state: {e}"
  return after

/-- One more failure in a row for resident `name`, and the backoff it earns. What pausing it after
    `Resident.maxFailures` takes is the delivery loop's to do (`Manager.noteFailure`, and
    `deliverLocked`, which pauses a resident found over the limit), under the resident's lock. -/
private def Manager.countFailure (mgr : Manager) (name why : String) : IO Resident.Runtime := do
  let now ← Usage.nowEpoch
  let rt ← mgr.modifyRuntime name fun rt =>
    let failures := rt.failures + 1
    { rt with failures, retryAfter := now + Int.ofNat (Resident.backoffSeconds failures) }
  IO.eprintln s!"resident {name}: failure {rt.failures} in a row: {why}"
  return rt

/-- Whether a turn's result means the turn was done: the CLI said `success`, and not as the
    wrapper of an error. Only such a turn confirms the items it carried. Everything else — an
    `error_during_execution` (an interrupted turn ends that way), `is_error` (an API error, a
    refused credential), a subtype this build does not know — leaves them ready, to be carried
    again under the note that they may have been seen; a turn that failed has not handled them. -/
def turnSucceeded (sub : ResultSubtype) (result : String) : Bool :=
  sub == .success && !refusal result.trimAscii.toString
where
  /-- What the CLI writes as a "successful" turn's whole answer when the API never answered it. -/
  refusal (r : String) : Bool :=
    ["API Error", "Invalid API key", "OAuth token", "Failed to authenticate",
     "Credit balance is too low"].any fun (p : String) => r.startsWith p

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
private def onAgentEvent (mgr : Manager) (s : LiveSession) (event : Event) : IO Unit := do
  if Wire.isNoise event then return
  -- Recorded against the source this session is spending, exactly as a queued run does. Without
  -- it the resolver keeps offering an account whose window is spent, and the next tasks
  -- dispatched to it each clone a slot, mint a token, start a sandbox and die on the limit —
  -- the waste `markLimited` exists to prevent.
  if let .rateLimit reset := event then
    if let some label ← s.authLabel.get then
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
  if let .result sub _ durationMs cost resultText := event then
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
    let costBase ← s.costBase.get
    let processTotal := (cost.bind (·.getNum?.toOption) |>.map (·.toFloat)).getD
      (r.costUsd - costBase)
    let total := costBase + processTotal
    let _ ← append s (.turnEnded r.turnCount (subtypeName sub) (some (total - r.costUsd))
                                 (durationMs.map (· / 1000)))
    if let some (name, _) := s.resident then
      -- The turn's cost goes against the budget day it ended on, and the items it carried are
      -- handed over — unless it ended on the day's cap, in which case they may well not have been
      -- read to the end and are carried again tomorrow (`docs/design/resident-sessions.md` §8).
      --
      -- All of it *before* the record reads `idle`: `idle` is what lets the delivery loop hand
      -- over the next turn, and one that looked at the inbox between the two would find this
      -- turn's items still unconfirmed and carry them again as if the turn had been interrupted.
      let day ← Resident.today mgr.cfg.residents.timezone
      try Resident.addSpend name day (total - r.costUsd)
      catch e => IO.eprintln s!"resident {name}: could not record a turn's spend: {e}"
      if sub == .errorMaxBudgetUsd then
        let _ ← append s (.notice "warning"
          s!"the day's budget is spent; the resident is paused until the next budget day ({day} \
is over) and its inbox keeps filling meanwhile")
        update s fun r => { r with
          costUsd := total
          status := if r.status.isTerminal then r.status else .paused
          pausedReason := some s!"budget:{day}" }
      else if turnSucceeded sub resultText then
        try discard <| Resident.confirm name r.turnCount
        catch e => IO.eprintln s!"resident {name}: could not confirm turn {r.turnCount}: {e}"
        let _ ← mgr.modifyRuntime name fun rt =>
          { rt with failures := 0, retryAfter := 0, capacityWaits := 0 }
        s.turnStartedAt.set 0
        update s fun r => { r with
          status  := settled r.status
          costUsd := total }
      else
        -- A turn that failed has not handled what it carried: the items stay ready, and go again
        -- with the next turn under the note that they may have been seen. It counts as a failure
        -- — backed off, and paused after `Resident.maxFailures` by the delivery loop — unless it
        -- was abandoned on purpose, which says so itself.
        let abandoned ← s.abandoned.modifyGet fun a => (a, false)
        s.turnStartedAt.set 0
        let why := s!"turn {r.turnCount} ended in an error ({subtypeName sub}\
{if resultText.isEmpty then "" else s!": {(resultText.take 200).toString}"})"
        let _ ← append s (.notice "warning" s!"{why}; the items it carried stay in the inbox and \
are handed over again with the next turn")
        unless abandoned do
          let _ ← mgr.countFailure name why
        update s fun r => { r with
          status  := settled r.status
          costUsd := total }
      return
    update s fun r => { r with
      status  := if r.status.isTerminal || r.status == .paused then r.status else .idle
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
  /-- Where a resident's record goes when a turn ends: `idle`, unless it is over, paused, or warm
      — its process stopped under the turn (the turn timeout), whose result arrived after. -/
  settled (st : SessionStatus) : SessionStatus :=
    if st.isTerminal || st == .paused || st == .warm then st else .idle
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
    else if r.status == .paused then
      return s!"this resident is paused ({(r.pausedReason.getD "no reason recorded")}); \
`orchestra resident resume` brings it back"
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
           releaseSlot, residentState := ← Std.Mutex.new {}, residentWaking := ← IO.mkRef 0
           configured := ← IO.mkRef #[] }

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
  s.torn.set true
  if let some st ← s.stream.modifyGet (fun st => (st, none)) then
    try st.shutdown catch _ => pure ()
  -- After the agent, before the slot: closing the environment is what copies a remote workspace
  -- back onto the slot this is about to release, and it has nothing to copy while the agent is
  -- still writing.
  try s.exec.close catch _ => pure ()
  try s.shutdownMcp catch _ => pure ()
  if let some fork := s.fork then
    try mgr.releaseSlot fork s.slot catch _ => pure ()
  if let some (name, _) := s.resident then
    let _ ← mgr.modifyRuntime name fun rt => { rt with tokenExpires := none }
  let now ← TaskStore.currentIso8601
  try
    update s fun r =>
      if r.status.isTerminal then r
      -- A paused resident stays paused, for the reason it was paused for: putting it down is
      -- not what lifts that.
      else if r.status == .paused then { r with error := why }
      -- Dormant is not an ending, so it does not get stamped with one. The record keeps its
      -- `endedAt` empty because the conversation has not ended; only the process has.
      else if status == .dormant || status == .paused then { r with status, error := why }
      else { r with status, endedAt := some now, error := why }
  catch _ => pure ()

/-- Stop a resident's agent process and keep everything else: its environment, its MCP server,
    its credentials. The session is then `warm`, and the next delivery starts a process on the
    same environment (`Manager.relaunch`) — seconds, where a cold wake on Kubernetes is a pod.

    Asleep, the session holds no kleis token: they are revoked here, and the next process gets one
    of its own. And what the agent wrote to its memory directory comes back now rather than when
    the environment goes, which for a resident's pod may be hours later. A `cycle` — stopped to be
    started again at once — keeps both: the tokens are revoked once the new process is up, and a
    cycle is meant to take seconds. -/
private def stopProcess (s : LiveSession) (cycle : Bool := false) : IO Unit := do
  if let some st ← s.stream.modifyGet (fun st => (st, none)) then
    try st.shutdown catch _ => pure ()
  unless cycle do
    try s.releaseCreds catch e => IO.eprintln s!"session {s.id}: could not release its tokens: {e}"
    try s.exec.syncBack catch e =>
      IO.eprintln s!"session {s.id}: could not bring its memory directory back: {e}"
  update s fun r =>
    if r.status.isTerminal || r.status == .paused then r else { r with status := .warm }

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
private def Manager.acquire (mgr : Manager) (appConfig : AppConfig) (fork : Option Repository)
    (slot : Nat) (agentDef : AgentDef) (debug : Bool) (onFailure : SessionStatus)
    (prepare : IO (SessionRecord × Option String × Option String × Option (String × Bool)))
    (resident : Option (String × Identity.ResidentSettings) := none)
    (budget? : Option Float := none)
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
  -- A resident is counted against `max_awake` while it wakes, every other session against the
  -- session cap (`claimPlace`); whichever was taken is given back once it is in the table or
  -- has failed.
  let dropStarting : IO Unit :=
    mgr.sessions.atomically do
      if resident.isSome then mgr.residentWaking.modify fun n => if n > 0 then n - 1 else 0
      else mgr.starting.modify fun n => if n > 0 then n - 1 else 0
  let release : IO Unit := do
    if let some ex ← execRef.get then try ex.close catch _ => pure ()
    if let some shut ← shutdownRef.get then try shut catch _ => pure ()
    if let some f := fork then
      try mgr.releaseSlot f slot catch _ => pure ()
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
        if current.status.isTerminal || current.status == .paused then current
        else if onFailure == .dormant then { current with status := .dormant, error := some msg }
        else { current with status := .failed, endedAt := some now, error := some msg }
      -- The seq continues from wherever the transcript got to. The pump may have written the
      -- `init` event already, and reusing seq 1 would put two events at one cursor position.
      let seq := r.lastEventSeq + 1
      appendEvent r.id seq now (.notice "error" msg)
      saveSession { r with lastEventSeq := seq }
    return .error msg
  try
    let (record, resumeAgentSession, resumeSlotOf, workspaceOf) ← prepare
    recordRef.set (some record)
    let backendName := record.backend
    -- Both halves of the pair or neither; a session without one is repository-less throughout.
    let repoPair : Option RepoPair := match record.upstream, fork with
      | some upstream, some f => some { upstream, fork := f }
      | _, _                  => none
    -- A session without a repository is installed where a repository-less task is: the configured
    -- installation, else the default organisation's, else none — and then it runs with no GitHub
    -- token at all rather than refusing to start (`TaskRunner`, the same choice).
    --
    -- And it needs no GitHub App to start. With kleis it reaches GitHub through the proxy on its
    -- own token and no installation token is minted for it at all, so no JWT is signed — a daemon
    -- whose App key is not readable (or not there: an orchestra that runs only residents) can still
    -- wake one. Without kleis the installation token is a convenience it may lack: one that cannot
    -- be had is said in the daemon's log and the session starts without it.
    let installationId : Option Nat ← match repoPair with
      | some p => match appConfig.installationId with
        | some i => pure (some i)
        | none   => some <$> GitHub.getInstallationId
                      (← GitHub.createJWT appConfig.appId appConfig.privateKeyPath) p.fork.owner
      | none =>
        if appConfig.kleis.isSome then pure none
        else try
            match appConfig.installationId, appConfig.defaultOrganization with
            | some i, _ => pure (some i)
            | none, some org =>
              GitHub.getInstallationId? (← GitHub.createJWT appConfig.appId
                appConfig.privateKeyPath) org
            | none, none => pure none
          catch e =>
            IO.eprintln s!"interactive: {record.id}: no GitHub installation token for a session \
without a repository ({e}); starting it without one"
            pure none
    let jwt ← match installationId with
      | some _ => some <$> GitHub.createJWT appConfig.appId appConfig.privateKeyPath
      | none   => pure none
    -- Deliberately no `GitHub.setupGhAuth`: that writes the token into
    -- `~/.config/gh/hosts.yml`, which every concurrently running task in this daemon shares.
    -- `TaskRunner` avoids it for the same reason, and it is not needed — the token reaches the
    -- clone through `ensureSlot` and the agent through `GH_TOKEN` in the sandbox. A session
    -- doing it would silently re-point every other `gh` call in the process at its own
    -- installation, and leave the token on disk afterwards.
    let mintToken : IO String := match installationId with
      | some i => do GitHub.createInstallationToken (← GitHub.createJWT appConfig.appId
                       appConfig.privateKeyPath) i
      | none   => pure ""
    let token ← match installationId, jwt with
      | some i, some jwt => GitHub.createInstallationToken jwt i
      | _, _ => pure ""
    -- `resumeFrom` on the assignment is what keeps a working tree instead of resetting it, and
    -- it is checked against the slot's recorded occupant, so asking for it is safe even when the
    -- tree has since been taken. Without it a resumed conversation was restored onto a wiped
    -- tree: the agent opens with a context full of edits it made and a checkout where none of
    -- them exist — the exact hazard the slot-pinning design exists to avoid, reintroduced at the
    -- one moment it matters most.
    --
    -- A backend that keeps workspaces itself does that job instead: the session's workspace — tree,
    -- build, and the agent's home with its conversation — is a volume keyed by the session, kept
    -- while it is dormant and handed back when it wakes. The checkout made here is then only what a
    -- first start fills that volume from, and goes when the session's environment closes.
    --
    -- A session without a repository has a scratch workspace instead: on such a backend an empty
    -- one to fill a new volume from, and otherwise one of its own that is kept across wakes.
    let keepsWorkspaces := Exec.keepsWorkspaces appConfig.execution
    let repoPath ← match repoPair with
      | some p =>
        if keepsWorkspaces then
          Repo.ensureTaskCheckout p.fork p.upstream record.id (token := some token)
        else
          Repo.ensureSlot p.fork p.upstream
            { slot, occupant := some record.id, resumeFrom := resumeSlotOf } (token := some token)
      | none =>
        if keepsWorkspaces then Repo.ensureTaskWorkspace record.id
        else Repo.ensureSessionWorkspace record.id
    -- Read from the record rather than from the spec, so a wake brings the session back as the
    -- same identity a start gave it. A record naming one that has since been deleted fails the
    -- acquire, which for a wake leaves the session dormant with the reason on its transcript —
    -- the right answer, since coming back as the instance would quietly re-author the rest of
    -- the conversation on the tracker under orchestra's own actor.
    let identity ← match record.identity with
      | none      => pure none
      | some name => some <$> Identity.requireIdentity name
    let identityMemory : Option String ← match identity with
      | none     => pure none
      | some idn => pure (some (← Identity.ensureMemoryDir idn.name).toString)
    -- The environment this conversation happens in, opened before the MCP server because it is
    -- what decides where that server has to listen and whether it needs a token — and after the
    -- identity, because the identity's memory is one of the directories the session has to carry
    -- to wherever the agent runs.
    let execBackend ← match ← Exec.resolve appConfig.execution with
      | .ok b     => pure b
      | .error e  => return ← fail s!"cannot run the agent: {e}"
    let (mcpBind, mcpPorts, mcpToken) ← Exec.mcpBinding execBackend
    let pluginDirs ← TaskRunner.defaultPluginDirs appConfig
    -- The default branch's, not the checkout's: a session that came back to a tree its agent wrote
    -- must not have that agent choose what it runs in (`RepoConfig.Scripts`).
    let repoConfig ← if fork.isSome then RepoConfig.loadDefaultBranchConfig repoPath else pure {}
    let seedDir ← match keepsWorkspaces, fork with
      | true, some f => some <$> Repo.seedPath f
      | _, _         => pure none
    -- The tools a session without a repository can use: the repository-scoped ones need one.
    let allowedTools ← Server.withoutRepoScopedTools repoPair (record.tools.getD allOptionalTools)
      (warn := false)
    -- A session's kleis token says what a queued task's does about it, and comes from the same
    -- sources, less the ones a session has no field for: its identity's, then `default_spec`.
    -- Derived before the environment is opened, as for a queued task, because its setup names the
    -- CA bundle the session opens with; and revoked by whatever brings the session down. A
    -- session without a repository states the repository-less form of the facts, as a task does.
    let kleisFacts : Kleis.TaskFacts := {
      taskId := record.id, repo := repoPair
      tools := allowedTools
      identity := identity.map (·.name), pushPrefix := appConfig.kleis.bind (·.pushPrefix)
      org := appConfig.defaultOrganization }
    let prepareKleis : IO (Option Kleis.Prepared) :=
      Kleis.prepareFor appConfig.kleis
        { identitySpec := identity.bind (·.kleisSpec)
          identityToken := (identity.bind (·.kleisToken)).isSome }
        (identity.bind (·.kleisToken)) kleisFacts ((identity.map (·.kleisNarrow)).getD [])
    let kleisPrepared ← prepareKleis
    -- Every token this session derives or mints is in `credsRef` until it is revoked, and
    -- `revokeHeld` is what revokes them — all of them when the session goes, all but the running
    -- process's once a new process is up (`Creds`).
    let credsRef ← IO.mkRef ({ held := (kleisPrepared.map (·.minted)).toArray } : Creds)
    -- Each token held is also on record (`session_token`) until it is revoked, so a daemon that
    -- dies holding it has it revoked by the next (`revokeLeftoverSessionTokens`).
    let noteHeld (m : Kleis.Minted) : IO Unit := do
      try recordSessionToken record.id m.revocationId
      catch e => IO.eprintln s!"interactive: {record.id}: could not record a kleis token: {e}"
    if let some p := kleisPrepared then noteHeld p.minted
    let hold (m : Kleis.Minted) : IO Unit := do
      credsRef.modify fun c => { c with held := c.held.push m }
      noteHeld m
    let revokeHeld (keep : Option String) : IO Unit := do
      let some kc := appConfig.kleis | return
      let gone ← credsRef.modifyGet fun c =>
        let gone := c.held.filter (fun m => some m.revocationId != keep)
        (gone, { c with held := c.held.filter (fun m => some m.revocationId == keep)
                        process := c.process.filter (fun m => some m.revocationId == keep) })
      for m in gone do
        let revoked ← try
            Kleis.revoke kc m
            pure true
          catch e =>
            IO.eprintln s!"interactive: could not revoke one of the session's kleis tokens: {e}"
            pure false
        -- Kept on record when it could not be revoked, so the next start tries again.
        if revoked then
          try forgetSessionToken m.revocationId catch _ => pure ()
    shutdownRef.set (some (revokeHeld none))
    -- Granted at open, as for a queued task: see `Kleis.bundleGrant`.
    let kleisBundle ← match appConfig.kleis, kleisPrepared with
      | some kc, some p => some <$> Kleis.setupBundle kc p.setup
      | some kc, none   => some <$> Kleis.caBundle kc
      | none, _         => pure none
    -- A resident's environment is keyed by its session, which never changes, and always asks for
    -- its own volume back — a fresh one will do, since losing it is survivable for a resident
    -- (its conversation starts over, said so in the first turn) where it is not for a chat.
    let isResident := resident.isSome
    let execSession ← execBackend.openSession {
      workdir := repoPath
      -- Keyed by the session, so a wake finds the volume its first start made, and a session
      -- started from another one finds that one's (see `workspaceOf`).
      taskId  := if keepsWorkspaces then some record.id else none
      continuesFrom := if keepsWorkspaces then
          (if isResident then some record.id else workspaceOf.map (·.1)) else none
      continuationOptional := isResident || (workspaceOf.map (·.2) |>.getD false)
      seedDir
      -- Someone is waiting on the other end: a full cluster is said at once, not after minutes.
      roomWaitSeconds := 0
      grants  := Sandbox.grantsFor agentDef.sandboxPaths appConfig.additionalSandboxPaths
                   repoPath false pluginDirs identityMemory.toArray
                   ++ (kleisBundle.map Kleis.bundleGrant).toArray
      label   := record.id
      -- The upstream, not the fork: an operator pinning an image writes the name the project is
      -- known by, and the fork is per-bot. Same key as the queue path and the merger.
      repo    := record.upstream.map (·.toString)
      image   := repoConfig.image
      resident := resident.map (·.1)
      volumeSize := if isResident then mgr.cfg.residents.volumeSize else none }
    -- Its checkout goes with it: closing the environment is the one thing every way out of a
    -- session does.
    let execSession := if keepsWorkspaces then
        { execSession with close := do
            try execSession.close finally Repo.removeTaskDir repoPath }
      else execSession
    -- And its env file (`Resident.EnvFile`), where this machine keeps it.
    let execSession := if isResident then
        { execSession with close := do
            try execSession.close finally Resident.EnvFile.removeHost record.id }
      else execSession
    execRef.set (some execSession)
    -- A resident whose conversation did not survive — its volume was lost, or this backend keeps
    -- none — starts a new one in the same session rather than failing every wake: it says so in
    -- the transcript and at the head of its next turn, and its memory directory, which lives
    -- apart from the conversation, is intact (`docs/design/resident-sessions.md` §9).
    let lostConversation := isResident && resumeAgentSession.isSome &&
      (!execSession.carriesAgentState || !execSession.workspaceReused)
    let (record, resumeAgentSession) ← if lostConversation then do
        let fresh := { record with agentSessionId := some (← newUuid), agentStarted := false }
        let now ← TaskStore.currentIso8601
        let seq := fresh.lastEventSeq + 1
        appendEvent fresh.id seq now (.notice "warning"
          "the previous conversation was lost with the environment that held it; starting a new \
one in this session (the resident's memory directory is intact)")
        let fresh := { fresh with lastEventSeq := seq }
        saveSession fresh
        recordRef.set (some fresh)
        pure (fresh, (none : Option String))
      else pure (record, resumeAgentSession)
    -- Reviving a dormant conversation asks the agent to resume a session it wrote under its own
    -- home. An environment that is new every time does not have it, and a revived conversation
    -- that silently starts over is worse than one that says it cannot: the person sees their
    -- history in the transcript and the agent does not.
    if resumeAgentSession.isSome && !execSession.carriesAgentState then
      return ← fail s!"this conversation was left in an environment that no longer exists ({execSession.id} is new every time). Enable per-task workspaces for this execution backend — execution.options.task_volumes for kubernetes — to wake sessions on it."
    -- A session reaches GitHub through kleis the same way a queued task does, on a token of its
    -- own: derived when it has a source, and otherwise minted, per agent process
    -- (`docs/design/resident-sessions.md` §7) — the first on the token derived above, which its
    -- setup and card were fetched for.
    let installLaunch (p : Kleis.Prepared) : IO Kleis.Launch := do
      let some kc := appConfig.kleis | throw (IO.userError "no kleis is configured")
      let bundle ← Kleis.setupBundle kc p.setup
      let fileGrants ← Kleis.installFiles execSession p.setup.files
      return Kleis.launchDerived kc p bundle fileGrants
    let mintLaunch : IO (Kleis.Launch × Kleis.Minted) := do
      let some kc := appConfig.kleis | throw (IO.userError "no kleis is configured")
      let minted ← Kleis.mint kc kleisFacts
      hold minted
      let bundle ← match kleisBundle with
        | some b => pure b
        | none   => Kleis.caBundle kc
      return (Kleis.launch kc minted bundle, minted)
    -- A new token for this session — derived from its source as the first was, or minted — with
    -- what its sandbox needs to use it, and its card when there is one.
    let newCredentials : IO (Option (Kleis.Launch × Kleis.Minted × Option String)) := do
      if appConfig.kleis.isNone then return none
      match kleisPrepared with
      | some _ =>
        let some p ← prepareKleis | return none
        hold p.minted
        return some (← installLaunch p, p.minted, some p.card)
      | none =>
        let (l, m) ← mintLaunch
        return some (l, m, none)
    -- What the first process starts with: the token derived above, or one minted now.
    let firstCredentials ← IO.mkRef (none : Option (Kleis.Launch × Kleis.Minted))
    match appConfig.kleis, kleisPrepared with
    | some _, some p => firstCredentials.set (some (← installLaunch p, p.minted))
    | some _, none   => firstCredentials.set (some (← mintLaunch))
    | none, _        => pure ()
    -- A resident's commands can be handed a new token mid-turn (`refresh_kleis_token`), through
    -- an env file of its own that `CLAUDE_ENV_FILE` points the agent's command shell at.
    let envFile ← if isResident && appConfig.kleis.isSome then
        some <$> Resident.EnvFile.locate execSession record.id
      else pure none
    -- When each refresh was asked for, `(turn, epoch seconds)`: a bound on an agent that calls it
    -- in a loop (`Resident.refreshRefusal`).
    let refreshLog ← IO.mkRef (#[] : Array (Nat × Int))
    let refreshKleisToken : IO (Except String String) := do
      let some loc := envFile
        | return .error "this session reaches nothing through kleis, so it has no token to refresh"
      let now ← Usage.nowEpoch
      let turn := ((← loadSession record.id).map (·.turnCount)).getD 0
      if let some why := Resident.refreshRefusal (← refreshLog.get) turn now then
        return .error why
      refreshLog.modify fun l => (l.filter (·.2 > now - 3600)).push (turn, now)
      let some (l, m, card) ← newCredentials
        | return .error "no kleis is configured"
      loc.writeEnv (Resident.EnvFile.contents l.env)
      credsRef.modify fun c => { c with refreshed := true }
      let expiry := if m.expires == 0 then "kleis did not say when it expires"
        else s!"it expires at {Usage.secsToIso8601 (Int.ofNat m.expires)} \
(in {(Int.ofNat m.expires - now) / 60} minutes)"
      return .ok (s!"A new kleis token is in place for the shell commands you start from now on; \
{expiry}. Commands already running keep the old one. After this turn your session is restarted on \
a fresh token and the old ones are revoked; nothing for you to do." ++
        ((card.map fun c => s!"\n\nWhat it allows:\n\n{c.trimAscii}").getD ""))
    let (port, shutdownMcp) ← Server.start {
      repo := repoPair
      allowedTools
      appId := appConfig.appId, privateKeyPath := appConfig.privateKeyPath
      installationId, pat := (record.upstream.map appConfig.patFor).getD appConfig.pat
      agentBackend := backendName
      identity
      authToken := mcpToken
      kleis := appConfig.kleis
      -- A resident's own tools: its watches (`docs/design/resident-sessions.md` §6), and a new
      -- kleis token for its commands (§7).
      residentTools := resident.map fun (name, _) => (name, fun tool args =>
        if tool == "refresh_kleis_token" then refreshKleisToken
        else Resident.Zulip.residentTool name tool args)
      -- A resident may queue tasks when its block has a policy (`Identity.ResidentSpec`). What
      -- it queues is not the resident: no identity — its memory, tracker token and kleis spec
      -- stay its own — and no repository to inherit, so each task names one the policy lists
      -- and gets a task's token for it (`kleis.default_spec`). Counted per budget day: a
      -- resident never ends, and a lifetime allowance would run out once and stay out.
      spawnPolicy := resident.bind (·.2.spawnPolicy)
      spawnContext := { backend := some backendName, model := record.model }
      enqueueTask := resident.map fun _ => fun resolved _ => do
        let day ← Resident.today mgr.cfg.residents.timezone
        TaskRunner.enqueueTaskImpl appConfig resolved s!"{record.id}@{day}"
      spawnerId := resident.map fun _ => record.id
    } (bindHost := mcpBind) (portRange := mcpPorts)
    -- Every token the session holds is revoked with the MCP server: the one closure every way out
    -- of a session runs, so no token can outlive the session that held it.
    let shutdownMcp : IO Unit := do
      try shutdownMcp catch _ => pure ()
      revokeHeld none
    shutdownRef.set (some shutdownMcp)
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
      if let some s ← liveRef.get then onAgentEvent mgr s e
    -- The same sections a task's system prompt is built from, minus the shared memories: a
    -- session is held open by a person and has no `memory` field to choose them with, so the
    -- only directory here is the identity's own — which is what makes it worth naming.
    --
    -- The card is the one of the token the process starts on: each later process — a warm wake, a
    -- cycle — is started on a token derived anew, whose card says what it allows *now*, and a
    -- prompt built once at the first start would go on describing the first token's.
    let cardRef ← IO.mkRef (kleisPrepared.map (·.card))
    let systemPromptNow : IO (Option String) := do
      let card ← cardRef.get
      let promptSections : List String :=
        [ record.systemPrompt
        , identity.map (TaskRunner.identitySystemPrompt · identityMemory)
        , identity.bind TaskRunner.identityInstructions
        , TaskRunner.memorySystemPrompt identityMemory.toArray
        , match appConfig.kleis, card with
          | _, some c    => some (Kleis.cardPrompt c)
          | some _, none => some (Kleis.systemPrompt kleisFacts)
          | none, none   => none ].filterMap id
      return if promptSections.isEmpty then none else some (String.intercalate "\n\n" promptSections)
    let authRef ← IO.mkRef (none : Option String)
    -- The installation token the agent holds lives an hour. The first process is given the one
    -- minted above; a later one in the same environment — a warm resident woken hours on — gets a
    -- fresh one rather than a token that has long expired.
    let firstLaunch ← IO.mkRef true
    -- The card for the first turn of a session that resumes its conversation as it is acquired:
    -- the session's notes do not exist until it is built below.
    let pendingCardNote ← IO.mkRef (none : Option String)
    -- One agent process in this environment. Each goes through the resolver again, as a queued
    -- run does, so an account found out of quota since the last process is not handed the next.
    let launch (resume : Option String) (budget : Float) :
        IO (Except String Sandbox.StreamingSession) := do
      let ghToken ← if ← firstLaunch.modifyGet (fun b => (b, false)) then pure token
        else try mintToken catch _ => pure token
      -- With the queue's running agents counted, so a person is not handed the account the daemon
      -- has just loaded up; an unreadable queue only makes every account look idle.
      let running ← try Queue.runningPerAuthSource catch _ => pure fun _ => 0
      let authLabel ← match ← Usage.resolveLabel appConfig backendName [] none none record.model
          (running := running) with
        | .ok label => pure label
        | .error e  => return .error s!"no usable authentication source for '{backendName}': {e}"
      if let some l := authLabel then Usage.markUsed backendName l
      authRef.set authLabel
      let apiKeyEnv ← TaskRunner.resolveAuthEnv appConfig agentDef backendName authLabel
      -- A token of its own: the first process the one derived with the environment, every later
      -- one — a warm wake, a cycle — a new one, so the card in its prompt and the token it holds
      -- are current however long the session has been asleep (§7.1).
      let creds ← match ← firstCredentials.modifyGet (fun c => (c, none)) with
        | some c => pure (some c)
        | none   =>
          match ← newCredentials with
          | some (l, m, card) =>
            if let some c := card then cardRef.set (some c)
            pure (some (l, m))
          | none => pure none
      let systemPrompt ← systemPromptNow
      -- A resumed conversation keeps the system prompt it began with, whatever this process is
      -- started with, so the card goes to it as a note on the next turn too — replacing a card
      -- note that has not been delivered yet, which describes a token about to be revoked.
      if resume.isSome then
        if let some c ← cardRef.get then
          let note := Kleis.cardNote c
          match ← liveRef.get with
          | some s => s.notes.modify fun ns =>
              ns.filter (fun n => !n.startsWith Kleis.cardNoteHeading) ++ [note]
          | none => pendingCardNote.set (some note)
      -- The env file is emptied before the process starts: its own token is the current one, and
      -- whatever a refresh wrote there is about to be revoked.
      let (kleisLaunch, envFileVar) ← match creds, envFile with
        | some (l, _), some loc => do
          loc.writeStub
          loc.writeEnv Resident.EnvFile.emptyContents
          pure (some { l with files := l.files ++ loc.grant.toArray },
                #[("CLAUDE_ENV_FILE", some loc.stubPath)])
        | some (l, _), none => pure (some l, #[])
        | none, _ => pure (none, #[])
      let some stream ← Sandbox.launchStreaming agentDef repoPath port ghToken
          { mcpContext := "", model := record.model, systemPrompt,
            resume, sessionId := (← recordRef.get).agentSessionId,
            budget := max 0.01 budget }
          onEvent (debug := debug)
          (extraEnv := apiKeyEnv ++ envFileVar) (pluginDirs := pluginDirs)
          (memoryDirs := identityMemory.toArray)
          (extraPorts := extraPorts) (additionalPaths := appConfig.additionalSandboxPaths)
          (session := execSession) (mcpToken := mcpToken) (kleis := kleisLaunch)
        | return .error s!"backend '{backendName}' cannot host an interactive session"
      -- Up: this process's token is the one to keep, and every token it replaces — the last
      -- process's, any a refresh handed its commands — is revoked now (§7.2).
      if let some (_, m) := creds then
        credsRef.modify fun c => { c with process := some m, refreshed := false }
        revokeHeld (some m.revocationId)
      return .ok stream
    -- What is *left* of the budget, not all of it. `--max-budget-usd` bounds the process, and a
    -- woken session is a second process: handing it the whole figure again would give a session
    -- that had spent $19 of $20 a fresh $20, once per wake, and the budget would bound nothing.
    -- A resident's is what is left of today's (`budget?`).
    let remaining := budget?.getD (record.budget - record.costUsd)
    let stream ← match ← launch resumeAgentSession remaining with
      | .ok st    => pure st
      | .error e  => return ← fail e
    let day ← Resident.today mgr.cfg.residents.timezone
    let session : LiveSession := {
      id := record.id, fork, slot, record := recordRef, lock, stream := ← IO.mkRef (some stream)
      shutdownMcp, exec := execSession
      authLabel := authRef, backend := backendName, costBase := ← IO.mkRef record.costUsd
      launch, resident, budgetDay := ← IO.mkRef day
      notes := ← IO.mkRef ((if lostConversation then
          ["Your previous conversation was lost; your memory directory is intact."]
        -- A new conversation in a session that has had turns — after a reset, mostly. What it set
        -- up outside the last conversation is still there, and it should know.
        else if isResident && resumeAgentSession.isNone && record.turnCount > 0 then
          ["This is a new conversation; the earlier one (an operator reset it) is not available \
to you. Your memory directory and your watches (list_watches) are as they were."]
        else []) ++ (← pendingCardNote.get).toList)
      creds := credsRef
      torn := ← IO.mkRef false
      turnStartedAt := ← IO.mkRef 0
      abandoned := ← IO.mkRef false
      -- Emptied before the revocation, so the file never names a revoked token.
      releaseCreds := do
        if let some loc := envFile then
          try loc.writeEnv Resident.EnvFile.emptyContents catch _ => pure ()
        revokeHeld none
    }
    liveRef.set (some session)
    mgr.sessions.atomically do
      modify (·.push session)
      if resident.isSome then mgr.residentWaking.modify fun n => if n > 0 then n - 1 else 0
      else mgr.starting.modify fun n => if n > 0 then n - 1 else 0
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
  -- Before a place or a clone slot is taken: an identity that is not configured is a mistake in
  -- the request, and finding that out after reserving a slot would hold one for the length of a
  -- clone before saying so.
  if let some name := spec.identity then
    try let _ ← Identity.requireIdentity name
    catch e => return .error (toString e)
  if spec.upstream.isSome != spec.fork.isSome then
    return .error "a session names both an upstream and a fork, or neither"
  if let some why ← mgr.claimPlace then return .error why
  -- A session without a repository has no clone to hold, so it takes no slot.
  let slot ← match spec.fork with
    | none   => pure (some 0)
    | some f => mgr.reserveSlot f
  let some slot := slot
    | do
        mgr.sessions.atomically <| mgr.starting.modify fun n => if n > 0 then n - 1 else 0
        return .error s!"no free clone slot for {(spec.fork.map toString).getD "?"}: every one \
is in use by a running task or another session"
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
      identity := spec.identity
    }
    saveSession record
    -- On a backend that keeps workspaces, a session started from another one is handed *that*
    -- session's volume — required only if it has a conversation to resume.
    return (record, resumed.bind (·.agentSessionId),
            resumed.filter (·.slot == slot) |>.map (·.id),
            resumed.map fun r => (r.id, !r.agentStarted))

/-- Bring a dormant session back up, resuming the conversation it was having.

    Same id, same transcript, same record — only the process, the clone slot and the MCP server
    are new, which is the whole point: they are what a session should not hold while nobody is
    talking to it, and the conversation is not one of them. -/
def Manager.wake (mgr : Manager) (appConfig : AppConfig) (id : String) (debug : Bool := false)
    (resident : Option (String × Identity.ResidentSettings) := none)
    (budget? : Option Float := none)
    : IO (Except String SessionRecord) := do
  -- A resident is counted against `max_awake` by whoever woke it (`Manager.deliver`), never
  -- against the session cap: a person opening a chat is not refused because a resident is up.
  -- Its place is given back on every way out before `acquire`, which gives it back after.
  --
  -- `placed` says whether this call holds such a place right now — a resident's from the start, a
  -- person's once `claimPlace` gave one — so that it is given back exactly once on every way out,
  -- a throw included (an unreadable record, a database that went away): a place leaked here is one
  -- fewer resident awake for the life of the daemon.
  let placed ← IO.mkRef resident.isSome
  let giveBack : IO Unit := do
    if ← placed.modifyGet (fun p => (p, false)) then
      mgr.sessions.atomically do
        if resident.isSome then mgr.residentWaking.modify fun n => if n > 0 then n - 1 else 0
        else mgr.starting.modify fun n => if n > 0 then n - 1 else 0
  let early (why : String) : IO (Except String SessionRecord) := do
    giveBack
    return .error why
  try
    let some peek ← loadSession id | early "no such session"
    -- Cheap rejection before anything is reserved. The claim below is what actually decides — this
    -- only keeps the common "it is over" and "someone else is already waking it" cases from
    -- taking a place under the cap and a clone slot on the way to being refused.
    unless peek.status == .dormant do return ← early (← gone id)
    let agentDef := TaskRunner.agentDefOfBackend (some peek.backend)
    if (agentDef.buildStreamArgs { mcpContext := "" }).isNone then
      return ← early s!"backend '{peek.backend}' cannot host an interactive session"
    if resident.isNone then
      if let some why ← mgr.claimPlace then return .error why
      placed.set true
    -- The claim, and the point of no return for every other caller: from here the record reads
    -- `starting`, so a second turn arriving behind this one is told the session is coming up
    -- rather than starting a second one, and an end is refused rather than silently reversed.
    let some record ← mgr.claimRecord id .dormant .starting
      | do
          giveBack
          return .error "this session is already waking up"
    let slot ← match record.fork with
      | none   => pure (some 0)
      | some f => mgr.reserveSlot f
    let some slot := slot
      | do
          giveBack
          -- Back to sleep: the claim above left it `starting`, which nothing would ever undo.
          saveSession { record with status := .dormant }
          return .error s!"no free clone slot for {(record.fork.map toString).getD "?"}: every one \
  is in use by a running task or another session"
    -- `acquire` gives the place back from here, once the session is up or has failed.
    placed.set false
    mgr.acquire appConfig record.fork slot agentDef debug .dormant (resident := resident)
        (budget? := budget?) do
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
      -- A wake always asks for its own volume back; a fresh one will do only if the agent never
      -- started, so there is no conversation (and no tree worth keeping) to lose.
      return (woken, resumeAgentSession, resumeSlotOf, some (record.id, !record.agentStarted))
  catch e =>
    giveBack
    throw e

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
  -- A resident's turns go through its inbox (`Manager.residentMessage`), never straight onto the
  -- pipe: a turn written past the inbox would be one the delivery loop does not know is running.
  if let some r ← loadSession id then
    if let some name := r.resident then
      return .error s!"this is resident {name}'s session; its turns go through its inbox"
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
  let some stream ← s.stream.get
    | return .error "this session has no agent process right now; say something to it through \
its resident's inbox"
  if (← stream.hasExited) then
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
    stream.sendLine (Wire.userTurn text)
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
  let some stream ← s.stream.get | return .error "this session is not working on a turn"
  -- A resident's turn interrupted on purpose ends in an error that is not a failure of its own;
  -- its items stay ready and go with the next turn.
  if s.resident.isSome then s.abandoned.set true
  try
    stream.sendLine (Wire.interrupt requestId)
    let _ ← append s (.notice "info" "the turn was interrupted")
    return .ok ()
  catch e =>
    return .error s!"the interrupt could not be delivered: {e}"

/-- End a session and release everything it holds. -/
def Manager.close (mgr : Manager) (id : String) : IO (Except String Unit) := do
  -- A resident's session does not end (`docs/design/resident-sessions.md` §3.2): it is paused or
  -- reset, by name, and an end asked of it by id is refused rather than quietly obeyed.
  if let some r ← loadSession id then
    if let some name := r.resident then
      return .error s!"this is resident {name}'s session, which does not end; pause it with \
`orchestra resident pause {name}`, or start its conversation over with `orchestra resident reset \
{name}`"
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

/-! ## Residents

A resident is an identity with one session that never ends (`docs/design/resident-sessions.md`).
Its session is created asleep at start-up (`ensureResidentSession`), and from then on what drives it
is its **inbox** (`Orchestra.Resident`): whenever the inbox has something ready and the session is
not mid-turn, `Manager.deliver` hands it over as one turn — waking it first, from warm (a process
started in the environment it kept) or from cold (`Manager.wake`, which opens a new one). A person's
message goes through the inbox too, so writing to a busy resident queues rather than being refused.

The paths that end or fail a session put a resident to sleep instead: a process found dead, a turn
that could not be written, a wake that failed. The day's budget pauses it. Only an operator ends its
conversation, and only by starting a new one (`Manager.resetResident`). -/

/-- The id of resident `name`'s session: stable, so its environment — a task volume, keyed by
    session — is the same one every wake, and a reset keeps it. -/
def residentSessionId (name : String) : String := s!"resident-{name}"

/-- Whether a paused resident's reason is gone: a budget pause once its day is over. A pause for
    failures is lifted after `Resident.failedPauseSeconds` (`Resident.failedPauseOver`, which needs
    the runtime); one by an operator only by `resumeResident`. -/
def pauseLifted (reason : Option String) (today : String) : Bool :=
  match reason with
  | none   => true
  | some r =>
    if r.startsWith "budget:" then (r.drop "budget:".length).toString != today else false

/-- What one delivery did. -/
inductive Delivery where
  /-- A turn was written, at transcript seq `seq`, carrying these inbox items. -/
  | sent (seq : Nat) (items : List Nat)
  /-- Nothing was ready. -/
  | nothing
  /-- Not now — mid-turn, batching, backing off, no room under `max_awake` — and why. The items
      stay ready. -/
  | waiting (why : String)
  /-- Paused, and why. -/
  | paused (why : String)
  /-- Tried, and failed; the items stay ready and the failure counts toward the backoff. -/
  | failed (why : String)
deriving Repr

/-- Tell the manager which residents configuration names, and their settings. -/
def Manager.setConfigured (mgr : Manager) (rs : Array (String × Identity.ResidentSettings)) :
    IO Unit :=
  mgr.configured.set rs

/-- The settings of resident `name`, if configuration names it. -/
def Manager.settingsOf (mgr : Manager) (name : String) : IO (Option Identity.ResidentSettings) :=
  return (← mgr.configured.get).find? (·.1 == name) |>.map (·.2)

/-- The residents configuration names. -/
def Manager.configuredResidents (mgr : Manager) : IO (Array String) :=
  return (← mgr.configured.get).map (·.1)

/-- What the manager knows of a resident beyond its record: failures in a row, and when it may be
    tried again. For `orchestra resident show`. -/
def Manager.residentFailures (mgr : Manager) (name : String) : IO (Nat × Int) := do
  let rt ← mgr.runtime name
  return (rt.state.failures, rt.state.retryAfter)

/-- Append a notice to a session that is not in the table. -/
private def noticeOnRecord (r : SessionRecord) (level msg : String) : IO SessionRecord := do
  let now ← TaskStore.currentIso8601
  let seq := r.lastEventSeq + 1
  appendEvent r.id seq now (.notice level msg)
  let r := { r with lastEventSeq := seq, lastActivityAt := now }
  saveSession r
  return r

/-- Make sure resident `name` has its session — a record, asleep, with no process — and keep what
    the record says about it (backend, model, tools, budget) as configuration says, while it is
    asleep: a wake is made from the record. -/
def ensureResidentSession (name : String) (settings : Identity.ResidentSettings) :
    IO SessionRecord := do
  match ← loadResidentSession name with
  | some r =>
    if r.status == .dormant || r.status == .paused then
      let r' := { r with backend := settings.agent, model := settings.model,
                         tools := settings.tools, budget := settings.budgetPerDay }
      unless r'.backend == r.backend && r'.model == r.model && r'.tools == r.tools &&
          r'.budget == r.budget do
        saveSession r'
      return r'
    return r
  | none =>
    let id := residentSessionId name
    if let some other ← loadSession id then
      throw (IO.userError s!"resident {name}: its session id {id} is already taken by a session \
that is not its own ({other.resident.getD "not a resident"})")
    let now ← TaskStore.currentIso8601
    let r : SessionRecord := {
      id, status := .dormant, createdAt := now, lastActivityAt := now
      backend := settings.agent, model := settings.model, budget := settings.budgetPerDay
      agentSessionId := some (← newUuid), tools := settings.tools
      identity := some name, resident := some name, title := some s!"{name} (resident)" }
    saveSession r
    noticeOnRecord r "info" s!"resident {name}'s session was created; it is asleep until \
something arrives in its inbox"

/-- Run `act` holding resident `name`'s delivery lock — what a delivery holds while it wakes the
    resident and hands it a turn. Everything else that changes a resident's process takes it too:
    the reaper putting it to sleep, a pause, a reset, the turn timeout, `max_awake` taking its
    process for another. None of them may run under a delivery that is starting a process or
    writing a turn: one that did left a process nobody held on a token nobody revoked, a session
    stuck `running` with no process, or `warm` written over a turn that had just started. -/
private def Manager.withResidentLock (mgr : Manager) (name : String) (act : IO α) : IO α := do
  let rt ← mgr.runtime name
  rt.lock.lock
  try act finally rt.lock.unlock

/-- `withResidentLock`, or `none` at once when something else holds the lock: for the reaper and
    `max_awake`, which can come back to a resident later rather than wait for it. -/
private def Manager.tryResidentLock (mgr : Manager) (name : String) (act : IO α) :
    IO (Option α) := do
  let rt ← mgr.runtime name
  unless ← rt.lock.tryLock do return none
  try some <$> act finally rt.lock.unlock

/-- Put a session to `paused` for `reason`, saying `message` on its transcript: torn down if it is
    live (a turn in flight is abandoned; its items stay ready), claimed on its record otherwise.
    Answers whether it is now paused. For a resident, called holding its lock. -/
private def Manager.pauseSession (mgr : Manager) (id reason message : String) : IO Bool := do
  match ← find mgr id with
  | some s =>
    update s fun r => { r with status := .paused, pausedReason := some reason }
    let _ ← append s (.notice "warning" message)
    teardown mgr s .paused none
    return true
  | none =>
    let some r ← loadSession id | return false
    unless r.status == .dormant || r.status == .paused do return false
    let some p ← mgr.claimRecord id r.status .paused | return false
    let _ ← noticeOnRecord { p with pausedReason := some reason } "warning" message
    return true

/-- Pause resident `name` for failing `failures` times in a row: `paused: failed`, for
    `Resident.failedPauseSeconds`, after which the delivery loop lifts the pause and tries once
    more (`unpauseIfDue`). Called holding its lock. -/
private def Manager.pauseForFailures (mgr : Manager) (name id : String) (failures : Nat)
    (why : String) : IO Unit := do
  let now ← Usage.nowEpoch
  let _ ← mgr.modifyRuntime name fun rt =>
    { rt with retryAfter := now + Int.ofNat Resident.failedPauseSeconds }
  let _ ← mgr.pauseSession id "failed"
    s!"{failures} failures in a row (the last: {redactPaths why}); the resident is paused for \
{Resident.failedPauseSeconds / 60} minutes and then tried again, or until an operator resumes it \
(`orchestra resident resume {name}`)"

/-- One more failure in a row for resident `name`: back off, and after `Resident.maxFailures`,
    pause it (`docs/design/resident-sessions.md` §10). Called holding its lock. -/
private def Manager.noteFailure (mgr : Manager) (name id why : String) : IO Unit := do
  let rt ← mgr.countFailure name why
  if rt.failures ≥ Resident.maxFailures then
    mgr.pauseForFailures name id rt.failures why

/-- Whether a wake failed for want of room rather than for anything wrong with the resident: the
    cluster is full or at its quota (`Exec.noRoom`), or every account is out of quota. Such a wake
    is backed off and tried again, and never counts toward `Resident.maxFailures` — a resident
    paused because the cluster was busy for a quarter of an hour would stay deaf long after it was
    not. -/
def isCapacityFailure (why : String) : Bool :=
  let has (n : String) := (why.splitOn n).length > 1
  has Exec.noRoomPrefix || has "no usable authentication source" || has "at its quota"

/-- A wake of resident `name` that found no room: backed off, 30s doubling to five minutes, and not
    counted as a failure. -/
private def Manager.noteNoRoom (mgr : Manager) (name why : String) : IO Unit := do
  let now ← Usage.nowEpoch
  let rt ← mgr.modifyRuntime name fun rt =>
    let waits := rt.capacityWaits + 1
    { rt with capacityWaits := waits
              retryAfter := now + Int.ofNat (Resident.backoffSeconds waits) }
  IO.eprintln s!"resident {name}: no room to wake it ({rt.capacityWaits} time(s) in a row); \
trying again in {Resident.backoffSeconds rt.capacityWaits}s: {why}"

/-- A wake of resident `name` that did not come up: a failure, or no room (`isCapacityFailure`). -/
private def Manager.noteWakeFailure (mgr : Manager) (name id why : String) : IO Unit :=
  if isCapacityFailure why then mgr.noteNoRoom name why else mgr.noteFailure name id why

/-- Take a place under `max_awake` for a resident about to have a process, or say there is none.
    The resident idle longest is put to sleep (warm) to make one; a resident mid-turn never is, nor
    one whose delivery lock is held — something is about to hand it a turn. The place is counted in
    `residentWaking` until the process is up or has failed. -/
private def Manager.claimAwake (mgr : Manager) (forId : String) : IO Bool := do
  for _ in [0:2] do
    let (ok, victim) ← mgr.sessions.atomically do
      let live ← get
      let mut awake : Array LiveSession := #[]
      for s in live do
        if s.resident.isSome && s.id != forId && (← s.stream.get).isSome then
          awake := awake.push s
      let waking ← mgr.residentWaking.get
      if awake.size + waking < mgr.cfg.residents.maxAwake then
        mgr.residentWaking.set (waking + 1)
        return (true, none)
      -- The idle one asleep longest-awake: the least recent activity.
      let mut best : Option (LiveSession × String) := none
      for s in awake do
        let r ← s.record.get
        if r.status != .idle then continue
        match best with
        | some (_, at_) => if r.lastActivityAt < at_ then best := some (s, r.lastActivityAt)
        | none => best := some (s, r.lastActivityAt)
      return (false, best.map (·.1))
    if ok then return true
    let some v := victim | return false
    let some (vname, _) := v.resident | return false
    -- Under the victim's own lock, taken only if free, and looked at again once it is held: what
    -- was idle a moment ago may have been handed a turn since.
    let stopped ← mgr.tryResidentLock vname do
      let live ← mgr.sessions.atomically do return (← get).any (·.id == v.id)
      let r ← v.record.get
      if !live || r.status != .idle || (← v.stream.get).isNone then return false
      let _ ← append v (.notice "info"
        s!"put to sleep to make room for another resident (residents.max_awake is \
{mgr.cfg.residents.maxAwake}); its environment is kept")
      stopProcess v
      let _ ← mgr.modifyRuntime vname fun rt => { rt with tokenExpires := none }
      return true
    unless stopped == some true do return false
  return false

private def Manager.dropAwake (mgr : Manager) : IO Unit :=
  mgr.sessions.atomically <| mgr.residentWaking.modify fun n => if n > 0 then n - 1 else 0

/-- Record when the token resident `name`'s process holds expires, for the dashboard. -/
private def Manager.noteToken (mgr : Manager) (name : String) (s : LiveSession) : IO Unit := do
  let exp := (← s.creds.get).processExpires
  let _ ← mgr.modifyRuntime name fun rt => { rt with tokenExpires := exp }

/-- Start a process for a warm resident in the environment it kept. Its place under `max_awake` is
    given back however this ends, a throw included. -/
private def Manager.relaunch (mgr : Manager) (s : LiveSession) (budget : Float) (day : String) :
    IO (Except String Unit) := do
  let res ← tryFinally (do
      let r ← s.record.get
      let resume := if r.agentStarted then r.agentSessionId else none
      s.costBase.set r.costUsd
      let _ ← append s (.notice "info" "waking the resident (its environment was kept)")
      try s.launch resume budget catch e => pure (.error (toString e)))
    mgr.dropAwake
  -- Torn down while the process was starting (the daemon stopping): nothing holds the session any
  -- more, so the process is stopped again and every token — the one it was started on included,
  -- which the teardown could not have seen — revoked.
  if ← s.torn.get then
    if let .ok st := res then try st.shutdown catch _ => pure ()
    try s.releaseCreds catch _ => pure ()
    return .error "the session was put down while its agent process was starting"
  match res with
  | .ok st =>
    s.stream.set (some st)
    s.budgetDay.set day
    update s fun r => if r.status.isTerminal then r else { r with status := .idle, error := none }
    return .ok ()
  | .error e =>
    let _ ← append s (.notice "error" s!"the agent process could not be started: {redactPaths e}")
    teardown mgr s .dormant (some (redactPaths e))
    return .error e

/-- Write `items` into one turn of a resident's live, idle session. Called holding its lock. -/
private def Manager.handTurn (mgr : Manager) (name : String) (s : LiveSession)
    (items : Array Resident.InboxItem) : IO Delivery := do
  let some stream ← s.stream.get | return .waiting "the resident has no process"
  let turn ← match ← claimTurn s (Resident.renderTurn items) with
    | .error e => return .waiting e
    | .ok t    => pure t
  -- Read again now that this turn is claimed: what was ready a moment ago may have been
  -- confirmed since by the turn that was still finishing, and carrying it again would hand it
  -- over twice under a note saying the last turn was interrupted when it was not.
  let items ← Resident.ready name
  if items.isEmpty then
    update s fun r => { r with
      status := if r.status == .running then .idle else r.status
      turnCount := r.turnCount - 1 }
    return .nothing
  let notes ← s.notes.get
  let text := Resident.renderTurn items notes
  s.notes.set []
  let ids := items.toList.map (·.id)
  let seq ← append s (.user text)
  let _ ← append s (.notice "info"
    s!"this turn carries inbox item{if ids.length == 1 then "" else "s"} \
{", ".intercalate (ids.map toString)}")
  let _ ← append s (.turnStarted turn)
  Resident.markSent ids turn seq
  s.abandoned.set false
  s.turnStartedAt.set (← Usage.nowEpoch)
  try
    stream.sendLine (Wire.userTurn text)
    let now ← Usage.nowEpoch
    let _ ← mgr.modifyRuntime name fun rt =>
      { rt with turnStamps := (rt.turnStamps.filter (· > now - 3600)).push now }
    return .sent seq ids
  catch e =>
    -- Asleep, not failed: the items are marked sent and not confirmed, so the next turn carries
    -- them again under the note that they may have been seen.
    let why := s!"the turn could not be delivered: {e}"
    s.turnStartedAt.set 0
    let _ ← append s (.notice "error" s!"{why}; the resident is asleep and the turn will be \
handed over again")
    teardown mgr s .dormant (some why)
    mgr.noteFailure name s.id why
    return .failed why

/-- Lift a paused resident's pause if its reason is gone — a budget day that is over, a pause for
    failures that has lasted `Resident.failedPauseSeconds` — and answer the record as it now is. A
    pause for failures lifted this way leaves one failure short of the limit: the next try is one
    try, not three more. -/
private def Manager.unpauseIfDue (mgr : Manager) (r : SessionRecord) (today : String) :
    IO SessionRecord := do
  unless r.status == .paused do return r
  let now ← Usage.nowEpoch
  let failedOver ← match r.resident with
    | some name =>
      pure (Resident.failedPauseOver r.pausedReason (← mgr.runtime name).state.retryAfter now)
    | none => pure false
  unless failedOver || pauseLifted r.pausedReason today do return r
  match ← mgr.claimRecord r.id .paused .dormant with
  | none   => return r
  | some d =>
    if failedOver then
      if let some name := r.resident then
        let _ ← mgr.modifyRuntime name fun rt =>
          { rt with failures := Resident.maxFailures - 1, retryAfter := 0 }
    noticeOnRecord { d with pausedReason := none, error := none } "info"
      (if failedOver then s!"paused for failures {Resident.failedPauseSeconds / 60} minutes ago; \
the resident is tried again (one more failure pauses it again)"
       else "a new budget day; the resident is no longer paused")

private def Manager.deliverLocked (mgr : Manager) (appConfig : AppConfig) (name : String)
    (settings : Identity.ResidentSettings) (urgent debug : Bool) : IO Delivery := do
  let some rec ← loadResidentSession name | return .failed s!"resident {name} has no session"
  let items ← Resident.ready name
  if items.isEmpty then return .nothing
  let now ← Usage.nowEpoch
  let day ← Resident.today mgr.cfg.residents.timezone
  let rec ← mgr.unpauseIfDue rec day
  if rec.status == .paused then
    return .paused (rec.pausedReason.getD "paused")
  let rt := (← mgr.runtime name).state
  -- A turn that failed is counted by the pump, which cannot pause the resident itself — that
  -- tears the session down from under the stream it is reading. One found over the limit is
  -- paused here, under the lock.
  if rt.failures ≥ Resident.maxFailures then
    mgr.pauseForFailures name rec.id rt.failures "its turns ended in errors"
    return .paused "failed"
  if !urgent && rt.retryAfter > now then
    return .waiting s!"backing off after {rt.failures + rt.capacityWaits} failure(s) in a row"
  if !urgent then
    if let some due := Resident.batchDueAt items settings.batchWindowSeconds then
      if due > now then return .waiting "waiting for the batch window"
  if Resident.turnsInLastHour rt.turnStamps now ≥ mgr.cfg.residents.maxTurnsPerHour then
    return .waiting s!"{mgr.cfg.residents.maxTurnsPerHour} turns in the last hour, which is \
residents.max_turns_per_hour"
  -- The day's allowance, which is also the process's cap: what is left of it.
  let remaining := settings.budgetPerDay - (← Resident.spentOn name day)
  if remaining < 0.01 then
    let _ ← mgr.pauseSession rec.id s!"budget:{day}"
      s!"the day's budget (${settings.budgetPerDay}) is spent; the resident is paused until the \
next budget day and its inbox keeps filling meanwhile"
    return .paused s!"budget:{day}"
  -- Woken from cold: a new environment, through `wake`.
  let coldWake : IO (Except Delivery LiveSession) := do
    let some r ← loadSession rec.id | return .error (.failed "the session is gone")
    unless r.status == .dormant do
      return .error (.waiting s!"the session is {Lean.Json.compress (Lean.ToJson.toJson r.status)}")
    unless ← mgr.claimAwake rec.id do
      return .error (.waiting s!"{mgr.cfg.residents.maxAwake} residents are awake, which is \
residents.max_awake, and none is idle")
    match ← mgr.wake appConfig rec.id debug (resident := some (name, settings))
        (budget? := some remaining) with
    | .error e =>
      mgr.noteWakeFailure name rec.id e
      return .error (if isCapacityFailure e then .waiting e else .failed e)
    | .ok _ =>
      match ← find mgr rec.id with
      | some s =>
        mgr.noteToken name s
        return .ok s
      | none   => return .error (.failed "the resident was woken but is not holding")
  -- Woken from warm: a process in the environment it kept, on a token of its own (`launch`).
  let warmWake (s : LiveSession) : IO (Except Delivery LiveSession) := do
    unless ← mgr.claimAwake s.id do
      return .error (.waiting s!"{mgr.cfg.residents.maxAwake} residents are awake, which is \
residents.max_awake, and none is idle")
    match ← mgr.relaunch s remaining day with
    | .ok () =>
      mgr.noteToken name s
      return .ok s
    | .error e =>
      mgr.noteWakeFailure name s.id e
      return .error (if isCapacityFailure e then .waiting e else .failed e)
  let got ← match ← find mgr rec.id with
    | none => coldWake
    | some s =>
      let r ← s.record.get
      if r.status == .running || r.status == .starting then
        pure (.error (.waiting "working on a turn"))
      else if r.status.isTerminal || r.status == .paused then
        pure (.error (.waiting "being put to sleep"))
      else
        match ← s.stream.get with
        | none => warmWake s
        | some st =>
          if ← st.hasExited then
            let _ ← append s (.notice "error"
              "the agent process exited; the resident is asleep and will be woken again")
            teardown mgr s .dormant (some "the agent process exited")
            mgr.noteFailure name s.id "the agent process exited"
            pure (.error (.failed "the agent process exited"))
          else
            -- A cycle (§7, §8): for the new day's allowance — its cap was yesterday's remainder —
            -- or for a new kleis token, when its own is close to expiring or a refresh replaced
            -- its commands' one. Stopped and started again at once in the environment it has; the
            -- tokens it held are revoked once the new process is up.
            let creds ← s.creds.get
            match Resident.cycleReason now creds.processExpires
                mgr.cfg.residents.tokenRefreshMarginSeconds creds.refreshed
                (← s.budgetDay.get) day with
            | some why =>
              let _ ← append s (.notice "info" why)
              stopProcess s (cycle := true)
              warmWake s
            | none => pure (.ok s)
  match got with
  | .error d => return d
  | .ok s    => mgr.handTurn name s items

/-- Hand resident `name` what its inbox holds, if anything is ready and it is not mid-turn —
    waking it first. `urgent` is a person waiting on the answer: the batch window and the backoff
    are skipped, and a delivery in progress is waited for rather than skipped. -/
def Manager.deliver (mgr : Manager) (appConfig : AppConfig) (name : String)
    (urgent : Bool := false) (debug : Bool := false) : IO Delivery := do
  unless mgr.cfg.residents.enabled do
    return .waiting "residents are disabled (residents.enabled is false)"
  let some settings ← mgr.settingsOf name
    | return .failed s!"no resident named {name} is configured"
  let rt ← mgr.runtime name
  if urgent then rt.lock.lock
  else
    unless ← rt.lock.tryLock do return .waiting "a delivery is in progress"
  try
    mgr.deliverLocked appConfig name settings urgent debug
  catch e =>
    return .failed (toString e)
  finally
    rt.lock.unlock

/-- Read which identities are residents, make sure each has its session, and tell the manager.
    Called at start-up and then now and again, so a resident added to configuration is picked up
    without a restart. One removed is left as it is — asleep, with its inbox — and no longer
    delivered to. -/
def Manager.refreshResidents (mgr : Manager) : IO Unit := do
  unless mgr.cfg.residents.enabled do
    mgr.setConfigured #[]
    return
  let mut found : Array (String × Identity.ResidentSettings) := #[]
  for idn in ← Identity.loadAllIdentities do
    let some spec := idn.resident | continue
    let settings := spec.settings mgr.cfg.residents
    try
      let _ ← ensureResidentSession idn.name settings
      found := found.push (idn.name, settings)
    catch e =>
      IO.eprintln s!"resident {idn.name}: could not prepare its session: {e}"
  let before ← mgr.configuredResidents
  if before != found.map (·.1) then
    IO.println s!"Residents: {if found.isEmpty then "none" else ", ".intercalate (found.map (·.1)).toList}"
  mgr.setConfigured found
  -- A pause whose reason is over is lifted here too, not only when something arrives: the
  -- dashboard should not go on saying "paused: budget" all of the next day.
  let today ← Resident.today mgr.cfg.residents.timezone
  for (name, _) in found do
    if let some r ← loadResidentSession name then
      let _ ← mgr.unpauseIfDue r today

/-- Whether the delivery loop has anything to try for resident `name` this tick: something is
    ready, and the resident can take it or its pause is due to lift — not mid-turn, not waking, not
    paused for good reason, not backing off, not at its hourly limit, and no delivery already
    holding it. A guess, made without the lock, that only decides whether a delivery is started:
    `deliver` decides again, properly. -/
private def Manager.deliverable (mgr : Manager) (name : String) : IO Bool := do
  if (← Resident.ready name).isEmpty then return false
  let rt ← mgr.runtime name
  unless ← rt.lock.tryLock do return false
  rt.lock.unlock
  let some r ← loadResidentSession name | return false
  if let some s ← find mgr r.id then
    let st := (← s.record.get).status
    if st == .running || st == .starting then return false
  let now ← Usage.nowEpoch
  let state := (← mgr.runtime name).state
  if r.status == .starting then return false
  if r.status == .paused then
    return pauseLifted r.pausedReason (← Resident.today mgr.cfg.residents.timezone) ||
      Resident.failedPauseOver r.pausedReason state.retryAfter now
  if state.failures ≥ Resident.maxFailures then return true
  if state.retryAfter > now then return false
  return Resident.turnsInLastHour state.turnStamps now < mgr.cfg.residents.maxTurnsPerHour

/-- One pass of the delivery loop over every configured resident. Called on the daemon's tick. -/
def Manager.tickResidents (mgr : Manager) (appConfig : AppConfig) (debug : Bool := false) :
    IO Unit := do
  unless mgr.cfg.residents.enabled do return
  for name in ← mgr.configuredResidents do
    -- Each on a thread of its own, and only when there is something it could hand over: a cold
    -- wake takes as long as a pod does, and the next resident's message should not wait for it —
    -- but a thread a second for a resident that is mid-turn, paused or backing off is a thread a
    -- second for nothing. Two ticks reaching one resident at once cannot both deliver: `deliver`
    -- skips a resident whose delivery is in progress.
    unless ← (try mgr.deliverable name catch _ => pure false) do continue
    let _ ← IO.asTask (prio := .dedicated) do
      try
        match ← mgr.deliver appConfig name (debug := debug) with
        | .sent seq items =>
          IO.println s!"  Resident {name}: delivered item(s) {items} (transcript seq {seq})"
        | .failed why => IO.eprintln s!"  Resident {name}: delivery failed: {redactPaths why}"
        | _ => pure ()
      catch e => IO.eprintln s!"  Resident {name}: delivery failed: {e}"

/-- What became of a person's message to a resident. -/
inductive HumanOutcome where
  /-- Written into a turn at this transcript seq: stream from it. -/
  | sent (seq : Nat)
  /-- In the inbox as this item, to be handed over when the resident can take it, and why it
      could not yet. -/
  | queued (item : Nat) (why : String)

/-- A person's message to resident `name`: into its inbox, and handed over at once if it can be,
    with whatever else is ready. -/
def Manager.residentMessage (mgr : Manager) (appConfig : AppConfig) (name text : String)
    (debug : Bool := false) : IO (Except String HumanOutcome) := do
  let some _ ← mgr.settingsOf name | return .error s!"no resident named {name} is configured"
  let eventId ← TaskStore.generateId
  let some item ← Resident.insert name "human" eventId true (Json.mkObj [("text", .str text)])
    | return .error "the message could not be put in the inbox"
  let outcome ← mgr.deliver appConfig name (urgent := true) (debug := debug)
  -- Whatever the delivery did, the item knows whether it went: a turn the tick handed over a
  -- moment ago may have carried it.
  let why := match outcome with
    | .waiting w | .paused w | .failed w => w
    | _ => "it is waiting for the resident"
  match ← Resident.find? item with
  | some i => match i.sentSeq with
    | some seq => return .ok (.sent seq)
    | none     => return .ok (.queued item (redactPaths why))
  | none => return .ok (.queued item (redactPaths why))

/-- Pause resident `name` until an operator resumes it. -/
def Manager.pauseResident (mgr : Manager) (name : String) : IO (Except String Unit) := do
  let some r ← loadResidentSession name | return .error s!"resident {name} has no session"
  mgr.withResidentLock name do
    if r.status == .paused then
      -- A pause for some other reason becomes the operator's, which nothing lifts on its own.
      if r.pausedReason != some "operator" then
        if let some p ← mgr.claimRecord r.id .paused .paused then
          saveSession { p with pausedReason := some "operator" }
      return .ok ()
    if ← mgr.pauseSession r.id "operator" "paused by an operator; its inbox keeps filling" then
      return .ok ()
    return .error s!"resident {name} cannot be paused right now ({(← gone r.id)})"

/-- Lift resident `name`'s pause, whatever its reason, and forget its failures. -/
def Manager.resumeResident (mgr : Manager) (name : String) : IO (Except String Unit) := do
  let some r ← loadResidentSession name | return .error s!"resident {name} has no session"
  mgr.withResidentLock name do
    let _ ← mgr.modifyRuntime name fun rt =>
      { rt with failures := 0, retryAfter := 0, capacityWaits := 0 }
    unless r.status == .paused do return .ok ()
    match ← mgr.claimRecord r.id .paused .dormant with
    | none => return .error s!"resident {name} is being put to sleep; try again in a moment"
    | some d =>
      let _ ← noticeOnRecord { d with pausedReason := none, error := none } "info"
        "resumed by an operator"
      return .ok ()

/-- End resident `name`'s conversation and start a new one, asleep: the only way a resident's
    conversation ends. The identity's memory directory carries over; the conversation does not.
    The inbox is kept — what is waiting in it is handed to the new conversation. -/
def Manager.resetResident (mgr : Manager) (name : String) : IO (Except String Unit) := do
  let some r ← loadResidentSession name | return .error s!"resident {name} has no session"
  mgr.withResidentLock name do
    if let some s ← find mgr r.id then
      let _ ← append s (.notice "info" "reset by an operator")
      teardown mgr s .dormant none
    let some r ← loadSession r.id | return .error s!"resident {name} has no session"
    unless r.status == .dormant || r.status == .paused do
      return .error s!"resident {name} is {Lean.Json.compress (Lean.ToJson.toJson r.status)}; \
try again in a moment"
    let some d ← mgr.claimRecord r.id r.status .dormant
      | return .error s!"resident {name} is waking up; try again in a moment"
    let _ ← mgr.modifyRuntime name fun rt =>
      { rt with failures := 0, retryAfter := 0, capacityWaits := 0 }
    let fresh ← newUuid
    let renewed : SessionRecord :=
      { d with agentSessionId := some fresh, agentStarted := false, pausedReason := none,
               error := none }
    let _ ← noticeOnRecord renewed "info"
      "the conversation was reset; the next turn starts a new one (the resident's memory directory \
and its watches carry over)"
    return .ok ()

/-! ## Keeping the table honest -/

/-- Abandon the turn resident `name` is running: interrupt it — the turn ends the way an
    interrupted one does, with an error result that leaves its items ready — then stop the process
    (warm), and count it a failure. Called holding its lock, by the turn timeout. -/
private def Manager.abandonTurn (mgr : Manager) (name : String) (s : LiveSession) (why : String) :
    IO Unit := do
  s.abandoned.set true
  if let some st ← s.stream.get then
    try st.sendLine (Wire.interrupt (← TaskStore.generateId)) catch _ => pure ()
  let _ ← append s (.notice "warning" s!"{why}: the turn was interrupted and the resident put to \
sleep; what it carried is handed over again with the next turn")
  -- A moment for the interrupt to end the turn the ordinary way, with a result on the stream.
  for _ in [0:40] do
    if (← s.record.get).status != .running then break
    IO.sleep 250
  s.turnStartedAt.set 0
  stopProcess s
  let _ ← mgr.modifyRuntime name fun rt => { rt with tokenExpires := none }
  mgr.noteFailure name s.id why

/-- What the reaper does about one resident's live session, holding its lock: release one that is
    over or paused, put one whose process died to sleep, abandon a turn past
    `residents.turn_timeout_seconds`, stop an idle process (warm) and release an idle environment
    (cold). -/
private def Manager.reapResident (mgr : Manager) (s : LiveSession) (name : String)
    (settings : Identity.ResidentSettings) (secondsSince : String → IO Nat) : IO Unit := do
  -- Let go of while the lock was being taken: whoever did has it.
  if ← s.torn.get then return
  let r ← s.record.get
  if r.status.isTerminal || r.status == .paused then
    teardown mgr s r.status r.error
    return
  match ← s.stream.get with
  | some st =>
    if ← st.hasExited then
      let _ ← append s (.notice "error"
        "the agent process exited; the resident is asleep and the next delivery wakes it")
      teardown mgr s .dormant (some "the agent process exited")
      mgr.noteFailure name s.id "the agent process exited"
      return
    if r.status == .running then
      let started ← s.turnStartedAt.get
      let now ← Usage.nowEpoch
      let limit := mgr.cfg.residents.turnTimeoutSeconds
      if started > 0 && now - started ≥ Int.ofNat limit then
        mgr.abandonTurn name s s!"the turn had run for {(now - started) / 60} minutes, past \
residents.turn_timeout_seconds ({limit}s)"
      return
    if r.status != .idle then return
    if (← secondsSince r.lastActivityAt) ≥ settings.processIdleSeconds then
      let _ ← append s (.notice "info"
        s!"nothing to do for {settings.processIdleSeconds}s, so the agent process was \
stopped; its environment is kept (warm)")
      stopProcess s
      let _ ← mgr.modifyRuntime name fun rt => { rt with tokenExpires := none }
  | none =>
    if r.status != .warm then return
    if (← secondsSince r.lastActivityAt) ≥ settings.podIdleSeconds then
      let _ ← append s (.notice "info"
        s!"nothing to do for {settings.podIdleSeconds}s, so the environment was released too \
(cold)")
      teardown mgr s .dormant none

/-- Close sessions that have sat without a turn for longer than the timeout, and any whose agent
    has gone without saying so. Called on the daemon's tick.

    A resident is never closed: idle, its process is stopped after `process_idle_seconds` (warm)
    and its environment after `pod_idle_seconds` (cold); a dead process puts it to sleep, to be
    woken by the next delivery; a turn past `turn_timeout_seconds` is abandoned. All of it under
    the resident's lock, and skipped for this round when a delivery holds it. -/
def Manager.reap (mgr : Manager) : IO Unit := do
  let ss ← mgr.sessions.atomically get
  for s in ss do
    if let some (name, settings) := s.resident then
      let _ ← mgr.tryResidentLock name (mgr.reapResident s name settings secondsSince)
      continue
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
    if r.status.isTerminal || r.status == .paused then
      teardown mgr s r.status r.error
      continue
    let stream ← s.stream.get
    let some st := stream | continue
    if ← st.hasExited then
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

/-- Revoke the kleis tokens a session was recorded holding (`session_token`) that no session of
    this daemon holds: a daemon that died — killed, crashed — before revoking them left them live
    until they expire. Run at start-up, when no session is live, so every one recorded is one. -/
def revokeLeftoverSessionTokens (kleis : Option KleisConfig) : IO Unit := do
  let some kc := kleis | return
  for (session, rid) in ← recordedSessionTokens do
    try
      Kleis.revokeId kc rid
      forgetSessionToken rid
      IO.println s!"  Session {session}: revoked a kleis token the previous daemon left"
    catch e => IO.eprintln s!"  Session {session}: could not revoke a leftover kleis token: {e}"

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
    every restart would append the same notice to a transcript nobody was adding to — and so is a
    paused resident, whose pause a restart does not lift. -/
def reconcile : IO Unit := do
  for r in ← loadAllSessions do
    unless r.status.isTerminal || r.status == .dormant || r.status == .paused do
      let now ← TaskStore.currentIso8601
      let seq := r.lastEventSeq + 1
      appendEvent r.id seq now (.notice "info"
        "the daemon restarted, so this session's agent is gone; say something to pick the \
conversation up")
      saveSession { r with
        status := .dormant, lastEventSeq := seq, lastActivityAt := now, error := none }

end Orchestra.Interactive
