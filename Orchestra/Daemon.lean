import Std.Sync
import Orchestra.Concert
import Orchestra.ConcertManager
import Orchestra.Config
import Orchestra.DaemonRequest
import Orchestra.GitHub
import Orchestra.Listener
import Orchestra.Project
import Orchestra.Queue
import Orchestra.Repo
import Orchestra.RepoConfig
import Orchestra.TaskRunner
import Orchestra.TaskStore
import Orchestra.Usage
import Orchestra.Utils.Signals
import Orchestra.Utils.UnixSocket
import Orchestra.Workflow
import Orchestra.WorkflowParser

/-!
# The queue daemon

The long-running half of orchestra: the process that claims queued entries, launches agents in
sandboxes, polls listeners, and answers the control socket. It lives in the library rather than
in a `Main` because it is the *server*, and since taxis #433 the server and the CLI are separate
binaries — `orchestrad` runs this, `orchestra` talks to it. Leaving it in the CLI's root module
would have compiled a task runner, a sandbox launcher and a socket server into a binary that only
ever needs to make HTTP requests.

Nothing here is new behaviour; it is `orchestra queue start` as it stood, with the flags it used
to read out of a `Cli.Parsed` lifted into `Config` so that a caller other than a command line can
supply them, and with one change: the set of listeners is now rescanned while the daemon runs
(see `run`), because configuration became writable at run time and a listener that only existed
after a restart would not be.

Its sibling is `Orchestra.Dashboard`, which serves the HTTP API over the same state. The two are
independent: either can run without the other, and `orchestrad serve` runs both in one process.
-/

open Lean (Json)
open Orchestra.DaemonRequest (DaemonRequest DaemonResponse)

namespace Orchestra.Daemon

/-- How a daemon run is configured. Every field mirrors a flag `orchestrad queue` accepts; the
    two `Option`s fall back to the `queue` block in `config.json` rather than to a constant, so
    an unset flag is genuinely "whatever the config says" rather than a second default that could
    disagree with it. -/
structure Config where
  /-- `config.json` to run against; `none` uses the XDG default. -/
  configPath : Option String := none
  /-- Print each agent's sandbox command before executing it. -/
  debug : Bool := false
  /-- Maximum tasks running at once across all repositories. -/
  parallel : Option Nat := none
  /-- Maximum tasks running at once on any one repository. -/
  parallelPerRepo : Option Nat := none

def handleSocketRequest
    (conn             : Utils.UnixSocket.Connection)
    (appConfig        : Orchestra.AppConfig)
    (concertMgr       : ConcertManager.ConcertManager)
    (debug            : Bool)
    (shutdownToken    : Std.CancellationToken)
    (activeTaskTokens : Std.Mutex (Array (Nat × Std.CancellationToken)))
    : IO Unit := do
  try
    let line ← conn.recvLine
    let response : DaemonRequest.DaemonResponse ← match Lean.Json.parse line with
      | .error e => pure (.error s!"invalid JSON: {e}")
      | .ok j    =>
        match (Lean.FromJson.fromJson? j : Except String DaemonRequest.DaemonRequest) with
        | .error e => pure (.error s!"invalid request: {e}")
        | .ok msg  => match msg with
        | .addTask entry =>
          Queue.saveEntry entry
          pure (.withId entry.id)
        | .addConcert wfPath vars cfgPath =>
          let result : Except String String ← try
            let yaml ← IO.FS.readFile wfPath
            match Workflow.WorkflowProgram.parseYaml yaml with
            | .error e => pure (Except.error s!"workflow parse failed: {e}")
            | .ok prog =>
              let concertId ← TaskStore.generateId
              let varsList := vars
                |>.bind (·.getObj?.toOption)
                |>.map (·.toList)
                |>.getD []
              let cfg ← match cfgPath with
                | none    => pure appConfig
                | some cp => loadAppConfig (some (System.FilePath.mk cp))
              let concert := Workflow.WorkflowProgram.toConcert prog varsList
              let run : Queue.ConcertRun := {
                id           := concertId
                startedAt    := ← TaskStore.currentIso8601
                name         := if prog.name.isEmpty then none else some prog.name
                workflowFile := some wfPath
              }
              Queue.saveConcertRun run
              IO.println s!"  Concert {concertId}: starting from {wfPath}"
              let _concertTask ← IO.asTask (prio := .dedicated) do
                try
                  Concert.evalQueued concertMgr cfg debug none (some concertId) concert
                  let t ← TaskStore.currentIso8601
                  Queue.saveConcertRun { run with status := .done, finishedAt := some t }
                catch e =>
                  IO.eprintln s!"  Concert {concertId} failed: {e}"
                  let t ← TaskStore.currentIso8601
                  Queue.saveConcertRun { run with status := .failed, finishedAt := some t }
              pure (Except.ok concertId)
          catch e => pure (Except.error s!"failed to start concert: {e}")
          match result with
          | .ok id   => pure (.withId id)
          | .error e => pure (.error e)
        | .cancel =>
          let pairs ← activeTaskTokens.atomically (·.get)
          for (_, token) in pairs do
            token.cancel .cancel
          pure DaemonRequest.DaemonResponse.ok
        | .shutdown force =>
          if force then
            let pairs ← activeTaskTokens.atomically (·.get)
            for (_, token) in pairs do
              token.cancel .cancel
          shutdownToken.cancel .shutdown
          pure DaemonRequest.DaemonResponse.ok
        | .claimIssue pid iid taskId agent series =>
          let now ← TaskStore.currentIso8601
          match ← Project.tryClaim TaskRunner.globalClaimManager pid iid taskId agent now series with
          | .acquired _            => pure (.withId taskId)
          | .alreadyClaimed exist  => pure (.error s!"already_claimed by task {exist.taskId}")
          | .invalid reason        => pure (.error reason)
    conn.sendLine (Lean.ToJson.toJson response).compress
  catch e =>
    IO.eprintln s!"Socket request error: {e}"
  try conn.close catch _ => pure ()

/-- Run the queue daemon in the foreground until it is asked to stop, and exit the process.

    Does not return: a graceful shutdown ends in `IO.Process.exit 0` once the last in-flight task
    has landed, which is what the drain on `SIGTERM` depends on. Returns `1` without starting
    anything if a daemon is already running. -/
def run (cfg : Config) : IO UInt32 := do
  -- Foreground mode
  if ← Queue.daemonRunning then
    IO.eprintln "Queue daemon is already running."
    return 1
  let pid ← Queue.ownPid
  Queue.writePid pid
  IO.println s!"Queue daemon started (PID {pid})"
  -- Startup cleanup
  Queue.markStaleRunningAsUnfinished
  Queue.cancelStaleConcertEntries
  Queue.cancelStaleRunningConcerts
  let appConfig ← loadAppConfig (cfg.configPath.map System.FilePath.mk)
  -- Concurrency limits: the `queue` block in config.json, overridden by the flags for a single
  -- run. Resolved here rather than at the top of the handler because it needs the config, and
  -- `max 1` because zero workers would be a daemon that silently never runs anything.
  let parallelLimit := max 1 <|
    cfg.parallel.getD appConfig.queue.parallel
  let parallelLimitPerRepo := max 1 <|
    cfg.parallelPerRepo.getD appConfig.queue.parallelPerRepo
  -- Shared concurrency primitives
  let shutdownToken  ← Std.CancellationToken.new
  -- Map of (id → cancel token) for all currently running tasks (one per worker).
  let activeTaskTokens ← Std.Mutex.new (Array.empty : Array (Nat × Std.CancellationToken))
  let nextTokenId ← IO.mkRef (0 : Nat)
  -- Mutex serialising the "find next pending + mark running" claim operation.
  let claimMutex ← Std.BaseMutex.new
  -- Tracks which task slots are currently occupied per repo (fork key), and the total
  -- number of running tasks. Both are protected by claimMutex.
  let activeSlots ← IO.mkRef ({} : Std.HashMap String (Array Nat))
  let totalActive ← IO.mkRef (0 : Nat)
  -- Set while a task on a backend that is not parallel-safe holds the daemon exclusively.
  -- Also protected by claimMutex.
  let exclusiveActive ← IO.mkRef false
  -- Concert manager: handles suspended concert fibers waiting for task results.
  let concertMgr ← ConcertManager.new
  -- Socket server: receives control requests (add_task, add_concert, cancel, shutdown).
  let socketPath ← Queue.socketFile
  try Utils.UnixSocket.Server.unlink socketPath catch _ => pure ()
  let socketServerRef ← IO.mkRef (none : Option Utils.UnixSocket.Server)
  let _socketTask ← IO.asTask (prio := .dedicated) do
    try
      let server ← Utils.UnixSocket.Server.listen socketPath
      socketServerRef.set (some server)
      repeat do
        let conn ← server.accept
        let _h ← IO.asTask (prio := .dedicated) do
          handleSocketRequest conn appConfig concertMgr cfg.debug shutdownToken activeTaskTokens
    catch _ => pure ()
  -- Signal watcher: turns SIGTERM/SIGINT into the same graceful drain as `queue shutdown`.
  -- Needed because the daemon is PID 1 in the container image, where `docker stop` is the way in
  -- and `queue shutdown` is not: draining works, but the daemon's exit stops the container and
  -- the restart policy immediately brings up a new one. Handling the signal is also what makes
  -- it deliverable at all — PID 1 ignores signals left at their default disposition, so before
  -- this, `docker stop` fell through to SIGKILL and left in-flight entries stuck in `running`.
  --
  -- Polled rather than acted on inside the handler itself: a signal handler may not touch the
  -- Lean heap, so it only bumps a counter (ffi/Signal.c) and the real work happens here. The
  -- 200ms tick is well inside any sane `docker stop` grace period and costs nothing when idle.
  Utils.Signals.install
  let _signalTask ← IO.asTask (prio := .dedicated) do
    let mut announced := false
    repeat
      let n ← Utils.Signals.count
      if n > 0 && !announced then
        announced := true
        IO.println "Received termination signal; finishing in-flight tasks before shutting down."
        IO.println "Send it again to cancel them instead."
        shutdownToken.cancel .shutdown
      -- A second signal escalates to `queue shutdown --force`: whoever is stopping us has said
      -- once that they are willing to wait, and then changed their mind.
      if n > 1 then
        IO.println "Second termination signal; cancelling in-flight tasks."
        let pairs ← activeTaskTokens.atomically (·.get)
        for (_, token) in pairs do
          token.cancel .cancel
        break
      IO.sleep 200
  -- Helper: atomically claim the next pending entry, marking it as running.
  -- Serialised by claimMutex so that multiple workers cannot claim the same entry.
  -- Returns (entry, slot, tokenId, reuseTree):
  --   slot       index of the per-repo task slot reserved for this entry. Slots are
  --              independent clones, so the entry's agent can create any branch name it
  --              likes without colliding with another task running on the same repository.
  --   tokenId    cancellation-token id, allocated here rather than in `runEntry` because
  --              `IO.Ref.modifyGet` is not atomic across workers: two of them racing on it
  --              are handed the same id, and `removeToken` then drops the other task's
  --              token, leaving `queue cancel` unable to reach a running task.
  --   resumeFrom set when this entry continues a previous task and got that task's slot back
  --              *with its tree intact*, naming the predecessor entry.
  -- The decision itself lives in `Queue.claimDecision`, which is a function of its inputs and
  -- therefore testable; this wrapper is only the locking and the bookkeeping around it.
  -- Both helpers unlock via `finally`: reading the queue directory and saving an entry
  -- are file operations that can throw, and leaving the mutex held would wedge every
  -- worker in the pool for the rest of the daemon's life.
  -- Which authentication source an entry may run on, asked once per claim attempt.
  --
  -- Entries whose sources are all usage-limited are reported as `wait`, which leaves them
  -- pending: unlike a cancellation, waiting costs nothing and the entry runs the moment the
  -- window resets, without anyone having to re-queue it. The reason is logged once per entry so
  -- a queue that looks stalled says why it is stalled.
  let authWaitNoted ← IO.mkRef ({} : Std.HashSet String)
  let resolveEntryAuth (e : Queue.QueueEntry) : IO Queue.AuthDecision := do
    let backend := e.backend.getD "claude"
    -- Per-entry config, so an entry pinned to a different config file is judged against the
    -- auth sources that file declares.
    let entryCfg ← match e.configPath with
      | none    => pure appConfig
      | some cp => try loadAppConfig (some (System.FilePath.mk cp)) catch _ => pure appConfig
    match ← Usage.resolveLabel entryCfg backend e.authSources e.authSource e.authMode e.model with
    | .ok label =>
      authWaitNoted.modify (·.erase e.id)
      -- Stamp the source as used *here*, while `claimMutex` is still held and before any other
      -- worker can resolve. `distribute` breaks ties on least-recently-used, and this is the
      -- only point at which that ordering is serialised: recording it once the task is actually
      -- launching would be several seconds later — a clone and a token mint later — by which
      -- time every other worker claiming in that window has read the same stale timestamp and
      -- picked the same account, which is precisely what `distribute` exists to avoid.
      if let some l := label then Usage.markUsed backend l
      return .use label
    | .error reason =>
      unless (← authWaitNoted.get).contains e.id do
        authWaitNoted.modify (·.insert e.id)
        IO.println s!"  Entry {e.id} waiting on {backend} usage limits: {reason}"
      return .wait reason
  let claimNextEntry : IO (Option (Queue.Claim × Nat)) := do
    claimMutex.lock
    try
      let slotMap ← activeSlots.get
      let ctx : Queue.ClaimContext := {
        occupiedSlots   := slotMap
        total           := ← totalActive.get
        exclusiveActive := ← exclusiveActive.get
        parallelLimit
        perRepoLimit    := parallelLimitPerRepo
        parallelSafe    := TaskRunner.backendIsParallelSafe
        resolveAuth     := resolveEntryAuth
      }
      let allEntries ← Queue.loadAllEntries
      let some claim ← Queue.claimDecision ctx allEntries Repo.slotOccupant | return none
      let e := claim.entry
      let occupied := slotMap.getD e.fork.toString #[]
      let tokenId ← nextTokenId.modifyGet (fun n => (n, n + 1))
      -- Record the resolved source on the entry, so `orchestra queue list` and any later
      -- continuation show which account actually ran it.
      Queue.saveEntry { e with
        status := .running, slot := some claim.slot
        authSource := claim.authSource.orElse fun _ => e.authSource }
      activeSlots.modify (fun m => m.insert e.fork.toString (occupied.push claim.slot))
      totalActive.modify (· + 1)
      if !TaskRunner.backendIsParallelSafe e.backend then exclusiveActive.set true
      if e.continuesFrom.isSome && claim.resumeFrom.isNone then
        IO.eprintln s!"  Note: queue entry {e.id} continues a previous task but no longer has \
its workspace; it will start from a clean checkout."
      return some (claim, tokenId)
    finally
      claimMutex.unlock
  -- Helper: release the slot held by a completed entry.
  let releaseEntry (entry : Queue.QueueEntry) (slot : Nat) : IO Unit := do
    claimMutex.lock
    try
      let occupied := (← activeSlots.get).getD entry.fork.toString #[]
      activeSlots.modify (fun m => m.insert entry.fork.toString (occupied.filter (· != slot)))
      totalActive.modify (fun t => if t > 0 then t - 1 else 0)
      if !TaskRunner.backendIsParallelSafe entry.backend then exclusiveActive.set false
    finally
      claimMutex.unlock
  -- Helper: run one queue entry to completion and update its status.
  -- Also signals the ConcertManager if the entry belongs to a concert.
  -- slot: index of the per-repo task slot reserved for this entry by `claimNextEntry`.
  -- The slot directory is prepared (created if absent, otherwise reset to a clean default
  -- branch) inside the try below. The slot is released by `runEntry`, which wraps this.
  let runEntryBody (entry : Queue.QueueEntry) (slot : Nat) (tokenId : Nat)
      (resumeFrom : Option String) (authSource : Option String) : IO Unit := do
    let taskToken ← Std.CancellationToken.new
    activeTaskTokens.atomically (·.modify (·.push (tokenId, taskToken)))
    let removeToken : IO Unit :=
      activeTaskTokens.atomically (·.modify (·.filter (·.1 != tokenId)))
    -- Terminal writes go through here rather than saving the claim-time snapshot: a socket
    -- `cancel`, a listener, or a cascade from another worker can rewrite this entry's file
    -- while the task runs, and writing the stale snapshot back would silently revert it.
    let finish (status : Queue.QueueStatus) (taskId : Option String)
        (outputJson : Option Lean.Json) : IO Unit := do
      let cur := (← Queue.loadEntry entry.id).getD entry
      Queue.saveEntry { cur with
        status
        taskId     := taskId.orElse (fun _ => cur.taskId)
        outputJson := outputJson.orElse (fun _ => cur.outputJson) }
    -- Record which task this entry became as soon as the task exists, rather than when it ends.
    -- The entry is the only handle the queue, the concert steps and the overview have on a run,
    -- and everything that wants the run itself — the dashboard's log above all — has to get
    -- there through this field. Writing it at `finish` meant a task was linkable only once it
    -- was over: for the whole time an agent was working, its trace was reachable by nothing on
    -- screen. Re-read-then-write like `finish`, for the same reason.
    -- Only on a successful re-read, and only this one field, which is where it parts company
    -- with `finish`. `entry` is the *pre-claim* snapshot: `claimNextEntry` writes `running` and
    -- the slot to disk but hands the worker the original, so falling back to it here would put
    -- a running entry back as pending and slotless — free for another worker to claim and run a
    -- second time. `finish` can afford the fallback because it always writes `status` itself.
    let announce (taskId : String) : IO Unit := do
      if let some cur ← Queue.loadEntry entry.id then
        Queue.saveEntry { cur with taskId := some taskId }
    let task : Task := {
      i := entry.inputType, o := entry.outputType
      ioTask := {
        upstream         := entry.upstream
        fork             := entry.fork
        mode             := entry.mode
        prompt           := entry.prompt
        goal             := entry.goal
        agent            := entry.agent
        systemPrompt     := entry.systemPrompt
        prependPrompt    := entry.prependPrompt
        backend          := entry.backend
        model            := entry.model
        budget           := entry.budget
        memory           := entry.memory
        authSource       := entry.authSource
        authSources      := entry.authSources
        authMode         := entry.authMode
        tools            := entry.tools
        readOnly         := entry.readOnly
        priority         := entry.priority
        issueNumber        := entry.issueNumber
        projectId          := entry.projectId
        issueId            := entry.issueId
        role               := entry.role
        prLabels           := entry.prLabels
        triageAddLabels    := entry.triageAddLabels
        triageRemoveLabels := entry.triageRemoveLabels
      }
    }
    -- If this entry holds a pre-claimed issue, release it back to open on any
    -- unhandled exception so the issue never gets permanently stuck.
    let releaseClaimOnError : IO Unit := do
      match entry.projectId, entry.issueId with
      | some pid, some iid =>
        let now ← TaskStore.currentIso8601
        let _ ← Project.release TaskRunner.globalClaimManager pid iid .open now
      | _, _ => pure ()
    try
      -- Inside the `try` so that a bad per-entry config path is reported as a failed
      -- entry rather than escaping and taking the worker down with it.
      let entryCfg ← match entry.configPath with
        | none    => pure appConfig
        | some cp => loadAppConfig (some (System.FilePath.mk cp))
      -- Preparing the slot sits inside the `try` so that a failure here — a clone that
      -- cannot be created, a fetch that cannot reach GitHub — is reported through the same
      -- path as any other task failure: the entry is marked failed, a pre-claimed issue is
      -- released, and a concert waiting on this step is signalled instead of hanging until
      -- the daemon shuts down.
      -- `resumeFrom` for a continuation that got its predecessor's slot back: the resumed
      -- agent's context describes a tree it expects to still be there. `ensureSlot` re-checks
      -- that the tree really is the predecessor's before keeping it.
      -- `occupant` stamps this entry as the owner of the tree it leaves behind, which is what
      -- a later continuation of *this* task will check.
      -- `runTask` prepares the slot itself, since doing so needs the installation token.
      let (taskId, taskStatus, outputJson) ← TaskRunner.runTask entryCfg task 0 cfg.debug
        (continuesFrom := entry.continuesFrom) (series := entry.series)
        (cancelToken := some taskToken) (interactive := false)
        (slotOverride := some { slot, occupant := some entry.id, resumeFrom })
        (preresolvedAuth := authSource)
        (onStart := announce)
      removeToken
      -- The sandbox always cancels taskToken with `.custom "done"` on normal exit, so
      -- `isCancelled` is true for both normal completion and watcher-triggered cancellation.
      -- Check the reason to distinguish the two cases.
      let explicitlyCancelled := (← taskToken.getCancellationReason) == some .cancel
      if explicitlyCancelled then
        finish .cancelled (some taskId) none
        IO.println s!"  Task cancelled."
        ConcertManager.signal concertMgr (entry.concertStepKey.getD "") outputJson
      else
        -- The queue entry now records exactly what the task record says. These used to be
        -- computed separately, and a run that exhausted its budget ended up `unfinished` in the
        -- task store and `done` in the queue — so `queue retry`, which reads queue status, could
        -- never pick it up.
        let queueStatus : Queue.QueueStatus := match taskStatus with
          | .completed  => .done
          | .unfinished => .unfinished
          | .failed     => .failed
          | .cancelled  => .cancelled
          | .running    => .done
        finish queueStatus (some taskId) outputJson
        match taskStatus with
        | .unfinished =>
          -- Nothing is cancelled here any more. The source that ran out is now marked in the
          -- usage store, so entries that would land on it are held back at claim time and every
          -- other entry — different account, different backend, different model family — keeps
          -- running. Dependents still go, because they were queued to continue *this* task's
          -- session and it did not finish.
          Queue.cancelDependents taskId
          IO.println s!"  Task unfinished; entry stays retryable and dependents were cancelled."
          ConcertManager.signal concertMgr (entry.concertStepKey.getD "") none
        | _ =>
          if let some key := entry.concertStepKey then
            ConcertManager.signal concertMgr key outputJson
    catch e =>
      removeToken
      let explicitlyCancelled := (← taskToken.getCancellationReason) == some .cancel
      if explicitlyCancelled then
        IO.eprintln s!"  Task cancelled (with error: {e})"
        try finish .cancelled none none catch _ => pure ()
        ConcertManager.signal concertMgr (entry.concertStepKey.getD "") none
      else
        IO.eprintln s!"Queue entry {entry.id} failed: {e}"
        try finish .failed none none catch _ => pure ()
        try releaseClaimOnError catch _ => pure ()
        ConcertManager.signal concertMgr (entry.concertStepKey.getD "") none
  -- The release wraps the *whole* body, not just the part that runs the task: allocating the
  -- cancellation token and registering it are themselves IO, and an exception there would
  -- otherwise escape past the release and leak the slot. A leaked slot is never recovered, so
  -- a handful of them permanently shrink the pool until the daemon is restarted.
  let runEntry (entry : Queue.QueueEntry) (slot : Nat) (tokenId : Nat)
      (resumeFrom : Option String) (authSource : Option String) : IO Unit := do
    try runEntryBody entry slot tokenId resumeFrom authSource
    finally releaseEntry entry slot
  -- Usage poller: refresh every configured OAuth source on a slow cadence.
  --
  -- Claim-time resolution already refreshes what it is about to use, but only for sources it is
  -- about to use. This is what notices that a *blocked* source has come back, and what keeps
  -- `orchestra usage` truthful while the daemon is otherwise idle. Errors are swallowed per
  -- source inside `refreshAll`; an unreachable endpoint must not take the fiber down.
  -- Only backends that opt into polling; a fully-disabled config spawns no poll fiber at all.
  let usageBackends := appConfig.agentAuthConfigs.toList.filterMap fun a =>
    if a.pollUsage then some a.name else none
  if !usageBackends.isEmpty then
    let _usageTask ← IO.asTask (prio := .dedicated) do
      while !(← shutdownToken.isCancelled) do
        for backend in usageBackends do
          try Usage.refreshAll appConfig backend
          catch e => IO.eprintln s!"[usage] poll failed for {backend}: {e}"
        -- Five minutes: fast enough that a reset is picked up promptly, slow enough that an
        -- idle daemon makes a handful of requests an hour. Shared with `ensureFresh`'s default
        -- TTL, which is what keeps this the only path that polls while the daemon is up.
        for _ in List.range Usage.pollIntervalSecs.toNat do
          if ← shutdownToken.isCancelled then break
          IO.sleep 1000
  -- Listeners: one fiber each, and the *set* of them is rescanned rather than fixed at
  -- start-up.
  --
  -- Before configuration became writable at run time, reading the directory once was equivalent
  -- to reading it repeatedly: nothing could add a listener to a running daemon. Now the API can,
  -- and a listener created through it that only started polling after the next restart would
  -- make "live configuration" true of every field except the one that decides whether the
  -- listener exists at all. So the supervisor below picks up new config files, and each fiber
  -- retires itself when its own config disappears — which is what makes a delete take effect
  -- too, rather than leaving a fiber polling a config nobody can see any more.
  --
  -- The scan is a directory read every `listenerScanSeconds`, not a filesystem watch: the
  -- listeners directory holds a handful of small files, an inotify watch would be a second
  -- mechanism to get wrong, and fifteen seconds is far inside the interval of any listener.
  let listenerScanSeconds : Nat := 15
  -- Before the first fiber polls anything. A listener used to be named by a `name` field inside
  -- its config and is named by its file now, so one whose two spellings disagreed is about to be
  -- known by a name with no state behind it — and would re-fire every event it has already
  -- handled unless its state comes with it.
  try Listener.migrateListenerStateNames
  catch e => IO.eprintln s!"Listener state migration failed: {e}"
  let listenerFibers ← IO.mkRef ({} : Std.HashSet String)
  let spawnListener (name : String) : IO Unit := do
    let _listenerTask ← IO.asTask (prio := .dedicated) do
      -- Fire immediately on first iteration, then respect the configured interval.
      let mut firstRun := true
      let mut interval := 60
      while !(← shutdownToken.isCancelled) do
        if !firstRun then
          IO.sleep (interval * 1000).toUInt32
        firstRun := false
        -- Re-read the config each tick so config changes take effect live; its absence means
        -- the listener was deleted, and this fiber is what is left of it.
        let some liveCfg ← Listener.loadListenerConfig name
          | IO.println s!"  Listener '{name}': config is gone, stopping"
            listenerFibers.modify (·.erase name)
            break
        interval := liveCfg.intervalSeconds
        try
          let state  ← Listener.loadListenerState name
          if !state.enabled then pure () else
          let (events, processedIdsReplacement) ← Listener.pollSource liveCfg.source state appConfig.pat
            appConfig.authorizedUsers
          for (_, vars) in (events : Array (String × List (String × String))) do
            -- github-label-count: skip if a task from this listener is already active.
            if let .githubLabelCount .. := liveCfg.source then
              if ← Queue.hasActiveEntryForListener name then
                IO.println s!"  Listener '{name}': skipping (active entry already in queue)"
                continue
            -- Project-dispatcher source: synthetic events carry only `role_name`
            -- and (optionally) `issue_id`. Build the queue entry directly from
            -- the named role template, pre-claiming through the in-process
            -- ClaimManager when the role wants it.
            match liveCfg.source with
            | .projectDispatcher pid _ =>
              let roleName := vars.find? (·.1 == "role_name") |>.map (·.2) |>.getD ""
              let issueId := vars.find? (·.1 == "issue_id") |>.bind (fun p => Taxis.IssueId.parse? p.2)
              let some project ← Project.loadProject pid | continue
              let some role ← Project.loadRole pid roleName | continue
              let issue? : Option Project.Issue ← match issueId with
                | none => pure none
                | some iid => Project.loadIssue pid iid
              let entryOpt ← Listener.buildRoleEntry appConfig project role issue?
              match entryOpt with
              | none =>
                IO.eprintln s!"  Listener '{name}': cannot dispatch {roleName}: no effective \
                  target, or its target is not writable and could not be forked (see [fork] logs)"
              | some entry =>
                -- Pre-claim if the role wants it and we have an issue.
                let needsClaim :=
                  match role.dispatch with
                  | some d => d.preClaim
                  | none   => false
                let claimed ← match needsClaim, issue? with
                  | true, some i =>
                    let now ← TaskStore.currentIso8601
                    let agent := role.backend.getD "claude"
                    match ← Project.tryClaim TaskRunner.globalClaimManager pid i.id
                                             entry.id agent now none with
                    | .acquired _ => pure true
                    | .alreadyClaimed e =>
                      IO.eprintln s!"  Listener '{name}': skipping {roleName}: \
                        issue {i.id.toString} already claimed by {e.taskId}"
                      pure false
                    | .invalid r =>
                      IO.eprintln s!"  Listener '{name}': skipping {roleName}: {r}"
                      pure false
                  | _, _ => pure true
                if claimed then
                  Queue.saveEntry entry
                  IO.println s!"  Listener '{name}': dispatched {roleName} → {entry.id}"
              continue
            | .labelDispatcher label _ =>
              -- Same shape as the branch above, but the project and target come from the event
              -- rather than the listener config: this source spans projects, and each issue's
              -- target was resolved from its taxis artifacts at poll time
              -- (Project.artifactTarget) rather than inherited from a project default.
              let getVar (k : String) := vars.find? (·.1 == k) |>.map (·.2)
              let roleName := getVar "role_name" |>.getD ""
              -- Absent for an unbound role (`always` trigger): it is scoped to the labelled root
              -- in `project_id` and picks its own issues from there, claiming each through the
              -- daemon. Only a malformed id is an error; no id at all is a valid event.
              let issueIdVar := getVar "issue_id"
              let issueId? := issueIdVar.bind Taxis.IssueId.parse?
              if issueIdVar.isSome && issueId?.isNone then
                IO.eprintln s!"  Listener '{name}': label-dispatcher event with an \
                  unparseable issue_id; skipping"
                continue
              let some projectId := getVar "project_id" |>.bind Taxis.IssueId.parse?
                | IO.eprintln s!"  Listener '{name}': label-dispatcher event without a \
                    usable project_id; skipping"
                  continue
              let target? : Option Project.RepoTarget :=
                match getVar "target_repo", getVar "target_branch" with
                | some r, some b => (Repository.parse r).toOption.map ({ repo := ·, branch := b })
                | _, _ => none
              let some target := target?
                | IO.eprintln s!"  Listener '{name}': label-dispatcher event for \
                    {issueId?.map (·.toString) |>.getD projectId.toString} carried no usable \
                    target; skipping"
                  continue
              let some project ← Project.loadProject projectId | continue
              -- Global roles only: these issues can span projects, so no single project's
              -- roles/ directory takes precedence (see Project.loadGlobalRoles).
              let some role := (← Project.loadGlobalRoles).find? (·.name == roleName)
                | IO.eprintln s!"  Listener '{name}': no global role '{roleName}'; skipping"
                  continue
              let issue? ← match issueId? with
                | none     => pure none
                | some iid => Project.loadIssue projectId iid
              -- A bound event naming an issue that cannot be loaded is a real failure; an
              -- unbound one legitimately has none.
              if issueId?.isSome && issue?.isNone then continue
              let scope := match issue? with
                | some i => s!"issue {i.id.toString}"
                | none   => s!"root {projectId.toString}"
              let entryOpt ← Listener.buildRoleEntry appConfig project role issue?
                (targetOverride := some target)
              match entryOpt with
              | none =>
                IO.eprintln s!"  Listener '{name}': cannot dispatch {roleName} for \
                  {scope}: no effective target, or its target is not writable and could not be \
                  forked (see [fork] logs)"
              | some entry =>
                -- Pre-claiming needs an issue to claim. An unbound role has none by construction;
                -- it claims for itself through the daemon once it picks one, which is the same
                -- mutex this would have taken.
                let needsClaim := match issue?, role.dispatch with
                  | some _, some d => d.preClaim
                  | _,      _      => false
                let claimed ← match issue?, needsClaim with
                  | some issue, true =>
                    let now ← TaskStore.currentIso8601
                    let agent := role.backend.getD "claude"
                    match ← Project.tryClaim TaskRunner.globalClaimManager projectId issue.id
                                             entry.id agent now none with
                    | .acquired _ => pure true
                    | .alreadyClaimed e =>
                      IO.eprintln s!"  Listener '{name}': skipping {roleName}: issue \
                        {issue.id.toString} already claimed by {e.taskId}"
                      pure false
                    | .invalid r =>
                      IO.eprintln s!"  Listener '{name}': skipping {roleName}: {r}"
                      pure false
                  | _, _ => pure true
                if claimed then
                  Queue.saveEntry entry
                  IO.println s!"  Listener '{name}': dispatched {roleName} for \
                    {scope} ({label}) → {entry.id}"
              continue
            | _ => pure ()
            if let some wfPath := liveCfg.action.workflowPath then
              -- Concert mode: parse the YAML, apply template vars, start a concert fiber.
              let resolvedPath := Listener.renderTemplate wfPath vars
              try
                let rawYaml ← IO.FS.readFile resolvedPath
                let yaml := Listener.renderTemplate rawYaml vars
                match Workflow.WorkflowProgram.parseYaml yaml with
                | .error e =>
                  IO.eprintln s!"  Listener '{name}': workflow parse error: {e}"
                | .ok prog =>
                  let upstreamStr :=
                    let r := Listener.renderTemplate liveCfg.action.upstream vars
                    if r.isEmpty then vars.find? (·.1 == "upstream") |>.map (·.2) |>.getD ""
                    else r
                  let forkStr :=
                    let r := Listener.renderTemplate liveCfg.action.fork vars
                    if r.isEmpty then vars.find? (·.1 == "fork") |>.map (·.2) |>.getD ""
                    else r
                  let upstream := Repository.parse upstreamStr |>.toOption
                  let fork     := Repository.parse forkStr     |>.toOption
                  let prog := { prog with upstream, fork }
                  let jsonVars := vars.map fun (k, v) => (k, Lean.Json.str v)
                  let concert := Workflow.WorkflowProgram.toConcert prog jsonVars
                  IO.println s!"  Listener '{name}': starting concert from {resolvedPath}"
                  let concertId ← TaskStore.generateId
                  let concertRun : Queue.ConcertRun := {
                    id           := concertId
                    startedAt    := ← TaskStore.currentIso8601
                    name         := if prog.name.isEmpty then none else some prog.name
                    workflowFile := some resolvedPath
                  }
                  Queue.saveConcertRun concertRun
                  let _concertTask ← IO.asTask (prio := .dedicated) do
                    try
                      Concert.evalQueued concertMgr appConfig cfg.debug none (some concertId) concert
                      let t ← TaskStore.currentIso8601
                      Queue.saveConcertRun { concertRun with status := .done, finishedAt := some t }
                    catch e =>
                      IO.eprintln s!"  Concert {concertId} failed: {e}"
                      let t ← TaskStore.currentIso8601
                      Queue.saveConcertRun { concertRun with status := .failed, finishedAt := some t }
                  pure ()
              catch e =>
                IO.eprintln s!"  Listener '{name}': failed to load workflow: {e}"
            else
              -- Single-task mode: enqueue a QueueEntry as before.
              let qentry ← Listener.buildQueueEntry liveCfg.action vars (some name)
              Queue.saveEntry qentry
              IO.println s!"  Listener '{name}': queued entry {qentry.id}"
          let newIds := events.filterMap (fun ev =>
            if (ev.1 : String).isEmpty then none else some ev.1)
          -- Re-read enabled so a disable issued mid-tick is not overwritten.
          let currentEnabled := (← Listener.loadListenerState name).enabled
          let newState : Listener.ListenerState := {
            lastChecked  := ← TaskStore.currentIso8601
            processedIds := processedIdsReplacement.getD (state.processedIds ++ newIds)
            enabled      := currentEnabled
          }
          Listener.saveListenerState name newState
        catch e =>
          IO.eprintln s!"  Listener '{name}' poll error: {e}"
  let _listenerSupervisor ← IO.asTask (prio := .dedicated) do
    let mut firstScan := true
    while !(← shutdownToken.isCancelled) do
      try
        let configs ← Listener.loadAllListenerConfigs
        let known ← listenerFibers.get
        let fresh := configs.filter (fun (name, _) => !known.contains name)
        if firstScan then
          if !configs.isEmpty then IO.println s!"Loaded {configs.size} listener(s)"
        else if !fresh.isEmpty then
          IO.println s!"Picked up {fresh.size} new listener(s): \
{String.intercalate ", " (fresh.toList.map (·.1))}"
        for (name, _) in fresh do
          listenerFibers.modify (·.insert name)
          spawnListener name
      catch e =>
        IO.eprintln s!"  Listener scan failed: {e}"
      firstScan := false
      -- Slept a second at a time so a shutdown is not held up by the rest of the interval.
      for _ in List.range listenerScanSeconds do
        if ← shutdownToken.isCancelled then break
        IO.sleep 1000
  -- Queue worker loop: claim and run one entry at a time.
  -- Spawning parallelLimit copies of this loop enables parallel execution.
  let workerLoop : IO Unit := do
    while !(← shutdownToken.isCancelled) do
      -- A worker must never die on an unhandled exception. `runEntry` already records
      -- per-entry failures, but anything escaping here (or out of `claimNextEntry`)
      -- would silently retire this worker and shrink the pool permanently — invisible
      -- with one worker on the main thread, and invisible *and* silent for the spawned
      -- ones, whose task results are discarded.
      try
        match ← claimNextEntry with
        | none => IO.sleep 1000
        | some (claim, tokenId) =>
          runEntry claim.entry claim.slot tokenId claim.resumeFrom claim.authSource
      catch e =>
        IO.eprintln s!"Queue worker error: {e}"
        IO.sleep 1000
  -- Spawn additional workers beyond the first (which runs on the main thread below).
  let mut workerTasks : Array (_root_.Task (Except IO.Error Unit)) := #[]
  for _ in List.range (parallelLimit - 1) do
    workerTasks := workerTasks.push (← IO.asTask (prio := .dedicated) workerLoop)
  if parallelLimit > 1 then
    IO.println s!"Queue daemon running with parallelLimit={parallelLimit}, parallelLimitPerRepo={parallelLimitPerRepo}"
  try
    workerLoop
    -- Let the other workers finish their in-flight tasks before the socket server and
    -- PID file are torn down; otherwise `IO.Process.exit 0` below kills them mid-task
    -- and their entries are left stuck in `running`.
    for t in workerTasks do
      let _ ← IO.wait t
  finally
    match ← socketServerRef.get with
    | some s => try s.close catch _ => pure ()
    | none   => pure ()
    try Utils.UnixSocket.Server.unlink socketPath catch _ => pure ()
    ConcertManager.cancelAll concertMgr
    Queue.deletePid
  IO.println "Queue daemon shut down gracefully."
  IO.Process.exit 0
end Orchestra.Daemon
