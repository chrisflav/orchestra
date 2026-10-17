import Std.Sync
import Std.Data.HashSet
import Orchestra.Exec.Spec
import Lean.Data.Json

/-!
# The execution backend interface

An `Exec.Backend` is everything orchestra needs from "the thing that runs an agent": it turns a
`RunSpec` into something running, and hands back a handle over which the run can be read, waited
on and killed. Two ship today — `landrun`, which is what orchestra has always done, and `local`,
which does the same without confinement — and the interface is deliberately the smallest one that
also fits a backend running the agent on another machine.

`Handle` is the whole of what supervision needs, and nothing about it says "process". A backend
that launches a pod satisfies it with the pod's log stream, a wait that watches the pod's phase,
and a kill that deletes it.
-/

namespace Orchestra.Exec

/-- Set once this process has been asked to stop (a termination signal, `queue shutdown`). A
    drain lets tasks that are running finish; a task still waiting for somewhere to run is not
    running, and reads this to stop waiting. -/
initialize stopRequestedRef : IO.Ref Bool ← IO.mkRef false

def requestStop : IO Unit := stopRequestedRef.set true
def stopRequested : IO Bool := stopRequestedRef.get

/-- Set when the stop that was asked for is one that may hand tasks over: a termination signal, or
    `orchestra queue shutdown --hand-off`. A plain `queue shutdown` drains, as it always did. -/
initialize handOffRequestedRef : IO.Ref Bool ← IO.mkRef false

def requestHandOff : IO Unit := handOffRequestedRef.set true
def handOffRequested : IO Bool := handOffRequestedRef.get

/-- How a backend says it has nowhere to run a task *yet*: the cluster is full, and nothing is
    wrong with the task. The daemon puts such an entry back to `pending` rather than failing it. -/
def noRoomPrefix : String := "no room for the task yet"

def noRoom (detail : String) : IO.Error := IO.userError s!"{noRoomPrefix}: {detail}"

def isNoRoom (e : IO.Error) : Bool := (toString e).startsWith noRoomPrefix

/-! ## Handing running tasks over to the next daemon

A daemon asked to stop used to *drain*: stop claiming, wait for every running task to finish, then
exit — for up to half an hour, after which it was killed anyway and the tasks with it. On a backend
whose runs outlive the daemon (the kubernetes backend's detached runs, `Exec.Runner`) there is
nothing to wait for: the agent, its build and its validation are processes in a pod that do not
care whether the daemon is there, and the next daemon picks each task up where it is
(`TaskRunner`, `Daemon.run`). So a stop *hands off* instead, as soon as every task still running is
one that can be picked up again — and drains, as it always did, while any is not.

"Can be picked up again" is a property of a task at a moment, not of a backend: a task opening its
pod, or one whose environment is being torn down, is in the middle of something no other daemon
could finish for it. Each task says so itself: `enterReattachable` once its state is on record and
everything it does from there is re-doable (`TaskRunner`), `leaveReattachable` before the stretch
where it is not. The daemon's decision and a task's leaving are serialised, and a task that tries to
leave after the decision was taken simply stops there — the process is about to exit, and the next
daemon will redo whatever it was going to do. That is what makes "every running task can be picked
up" stay true from the moment it is decided until the exit. -/

/-- The tasks, by task id, that are in a stretch the next daemon could pick up, and whether a
    hand-off has been decided. Pure, so the rules can be tested without deciding a hand-off in the
    test process; the daemon holds one behind a lock (`reattachableRef`). -/
structure HandOffState where
  reattachable : Std.HashSet String := {}
  decided : Bool := false
deriving Inhabited

namespace HandOffState

/-- `taskId` can be carried on by the next daemon from here. -/
def enter (s : HandOffState) (taskId : String) : HandOffState :=
  { s with reattachable := s.reattachable.insert taskId }

/-- `taskId` is leaving that stretch: the new state, and whether it must stop where it is instead
    because a hand-off was already decided (in which case nothing changes). -/
def leave (s : HandOffState) (taskId : String) : HandOffState × Bool :=
  if s.decided then (s, true) else ({ s with reattachable := s.reattachable.erase taskId }, false)

/-- Decide a hand-off when every task in `running` can be carried on: the new state, and whether
    it is decided. Never taken back once taken; never taken for an empty `running`, since a daemon
    with nothing running simply exits. -/
def decide (s : HandOffState) (running : Array String) : HandOffState × Bool :=
  if s.decided then (s, true)
  else if running.isEmpty || !running.all s.reattachable.contains then (s, false)
  else ({ s with decided := true }, true)

end HandOffState

/-- The daemon's `HandOffState`. One lock for the decision and every task's leaving: see
    `leaveReattachable`. -/
initialize reattachableRef : Std.Mutex HandOffState ← Std.Mutex.new {}

/-- `taskId` is now somewhere a daemon taking over could carry it on from. -/
def enterReattachable (taskId : String) : IO Unit :=
  reattachableRef.atomically (modify (·.enter taskId))

/-- `taskId` is about to do something the next daemon could not pick up. Returns at once unless a
    hand-off has already been decided, in which case it never returns: the process exits in a
    moment, leaving the task where it was decided to be, which is where the next daemon looks. -/
def leaveReattachable (taskId : String) : IO Unit := do
  let frozen ← reattachableRef.atomically do
    let (s, frozen) := (← get).leave taskId
    set s
    return frozen
  if frozen then
    repeat IO.sleep 1000

/-- Decide a hand-off if every task in `running` can be picked up by the next daemon, and say
    whether it was (`HandOffState.decide`). -/
def tryHandOff (running : Array String) : IO Bool :=
  reattachableRef.atomically do
    let (s, decided) := (← get).decide running
    set s
    return decided

/-- How a re-adopted task says it could not be picked up after all — its pod is gone, its server's
    port is taken — so that the daemon falls back to what it does for any task a restart
    interrupted (a continuation that resumes the conversation in a new pod) rather than failing it.
    A prefix, the way `noRoom` is one. -/
def reattachFailedPrefix : String := "could not pick the task up again"

def reattachFailed (detail : String) : IO.Error := IO.userError s!"{reattachFailedPrefix}: {detail}"

def isReattachFailed (e : IO.Error) : Bool := (toString e).startsWith reattachFailedPrefix

/-- A run in progress, whatever is running it.

    The fields are functions rather than data because a remote backend answers them with API
    calls, not by reading a struct it already has. -/
structure Handle where
  /-- The run's stdout, when the spec asked for `.piped` or `.stream`; `none` under `.inherit`,
      where it went straight to orchestra's own terminal and there is nothing to read. -/
  stdout : Option IO.FS.Handle
  /-- The run's stderr, on the same terms as `stdout`. -/
  stderr : Option IO.FS.Handle
  /-- The run's stdin, under `.stream` only, and taken off the child so that dropping it is what
      closes the pipe.

      That detail is the whole point of holding it here: a handle closes when its last reference
      goes, a spawned child keeps one of its own, and while it does, nothing can deliver EOF —
      which is how a CLI reading turns from a pipe is told there are no more. -/
  stdin : Option IO.FS.Handle := none
  /-- Block until the run finishes, and return its exit code. Called once. -/
  wait : IO UInt32
  /-- Whether the run has finished, without waiting for it. `none` while it is still going.

      Asked of the run itself, never of whatever is draining its output: a task reading stdout can
      end for reasons of its own, and reading that as "the process is gone" is how a healthy
      session gets reaped mid-conversation. -/
  tryWait : IO (Option UInt32) := return none
  /-- Ask the run to stop, the way a person would: `SIGTERM` and its equivalents. The polite half
      of `kill`, for a caller that will escalate if it is ignored. -/
  terminate : IO Unit := pure ()
  /-- Stop the run now, as a cancellation. Best-effort and safe to call on a run that has already
      finished: cancellation races completion by nature, and every caller here would otherwise
      have to guess which one won. -/
  kill : IO Unit
  /-- What this run is called where the backend runs it — a pid, a container id, a pod name. For
      logs and for a human trying to find it by hand. -/
  id : String
  /-- How many bytes at the start of `stdout` were already read once, by a daemon that has since
      gone away: the run was started before this handle existed and is being followed again from
      its beginning (`Exec.Runner`). Whoever reads the stream parses all of it — that is how the
      state the old daemon held is rebuilt — and keeps this much off the console, where it was
      printed the first time. Zero for every run this daemon started. -/
  replayBytes : Nat := 0

/-- Where the MCP server has to listen for this backend's agents to reach it.

    This is the backend's answer to a question only it can answer, and the reason it is asked
    before the server starts: `Server.start` binds what this says, and a task whose agent runs off
    this machine gets a per-run token minted along with it. -/
inductive Exposure where
  /-- The agent runs here, so loopback is reachable and no one else is. No token, and any free
      port will do — nothing outside this machine has to find it. -/
  | loopback
  /-- The agent runs elsewhere: bind this address, and mint a token the agent must present.

      `ports` is an inclusive range to pick the listening port from, for the case where something
      between the agent and the daemon has to be told about it in advance — a firewall rule, a
      port-forward, a tunnel. One server is started per task, so the range bounds how many tasks
      can run at once. `none` takes any free port, which is right when nothing between the two
      needs to know it: a daemon inside the same cluster as its pods, for instance. -/
  | network (bindHost : String) (ports : Option (UInt16 × UInt16))
deriving Repr, BEq, Inhabited

/-- One of the repository's own scripts — `init.sh`, `before.sh`, `validation.sh`, `after.sh` —
    as an execution environment is asked to run it.

    These are not the agent, but they belong exactly where the agent is: `validation.sh` decides
    whether the agent's work is finished, and a validation run on a different machine from the one
    the work happened on is answering a different question. -/
structure ScriptSpec where
  /-- The script, as a path in the checkout. -/
  path : String
  /-- Where to run it: the checkout. -/
  workdir : System.FilePath
  /-- `.inherit` for the hooks, whose output belongs in the daemon log as it happens; `.piped` for
      `validation.sh`, whose output is fed back to the agent in the retry prompt. -/
  stdio : Stdio := .inherit
  /-- What the backend calls this run, when it keeps runs past the daemon (`RunSpec.runId`): the
      same id every time the same step of the same task is reached — `before-0`, `validate-1`,
      `after` — so that a daemon coming back to the task attaches to the script the last one
      started rather than running it twice. `none` picks a fresh one. -/
  runId : Option String := none

/-- What a script did. -/
structure ScriptResult where
  exitCode : UInt32
  /-- Combined stdout and stderr, captured only under `.piped`; empty otherwise. -/
  output : String := ""

/-! ## Files written into a run's home

What kleis's `/setup` hands a sandbox (`Kleis.Setup`): configuration files a client expects in its
home — `~/.zuliprc`, `~/.config/khal/config` — holding placeholders rather than secrets. Each is a
path *relative to the run's home*, and that path is the one thing about it orchestra checks: it
comes from a server, and a `../` or an absolute path in it would be a write wherever the daemon
can write. -/

/-- The characters a setup file's path may hold: ASCII letters and digits, `/` between segments,
    and `.`, `_`, `-`, `@`, `+`, `=`, `,`, `:`, `%`. Allowed rather than refused, because the path
    reaches a shell on the Kubernetes backend, and a shell reads far more characters than a list
    of the dangerous ones would remember (`>` is a redirection, `*` a glob, `#` a comment). -/
def homePathCharOk (c : Char) : Bool :=
  (c.isAlphanum && c.toNat < 128) || c == '/' || c == '.' || c == '_' || c == '-' || c == '@'
    || c == '+' || c == '=' || c == ',' || c == ':' || c == '%'

/-- Why `rel` cannot be written under a run's home, or `none` when it can: it is relative, has no
    `..`, `.` or empty segment, and only characters `homePathCharOk` allows. -/
def homePathError? (rel : String) : Option String :=
  if rel.isEmpty then some "the path is empty"
  else if rel.startsWith "/" then some s!"'{rel}' is absolute; only paths under the home are written"
  else if !rel.all homePathCharOk then
    some s!"'{rel.quote}' holds a character no setup file's name may"
  else
    let segs := rel.splitOn "/"
    if segs.any (· == "..") then some s!"'{rel}' has a '..' segment"
    else if segs.any (fun s => s.isEmpty || s == ".") then
      some s!"'{rel}' has an empty or '.' segment"
    else none

/-- `mode` (`"0600"`, `"644"`) as permission bits, or `none` when it is not three or four octal
    digits without setuid, setgid or sticky bits. -/
def fileModeOf? (mode : String) : Option UInt32 :=
  let digits := if mode.length == 4 && mode.startsWith "0" then (mode.drop 1).toString else mode
  if digits.length != 3 || !digits.all (fun c => '0' ≤ c && c ≤ '7') then none
  else some (digits.foldl (fun acc c => acc * 8 + (c.toNat - '0'.toNat).toUInt32) 0)

/-- Write `content` to `rel` under `home` on this machine, refusing to follow a symbolic link at
    any point below `home`: every directory on the way is created if missing and refused if it is
    a link or not a directory, and the file itself is written beside its destination and renamed
    over it — a rename replaces a link rather than writing through it.

    `home` itself is trusted; everything below it may have been put there by an agent. -/
def writeUnderHome (home : System.FilePath) (rel : String) (mode : UInt32) (content : String)
    : IO Unit := do
  if let some e := homePathError? rel then throw (IO.userError s!"refusing to write {e}")
  let segs := rel.splitOn "/"
  let mut dir := home
  for seg in segs.dropLast do
    dir := dir / seg
    match ← (dir.symlinkMetadata.toBaseIO) with
    | .error _ => IO.FS.createDir dir
    | .ok md =>
      if md.type == .symlink then
        throw (IO.userError s!"refusing to write under {dir}: it is a symbolic link")
      else if md.type != .dir then
        throw (IO.userError s!"refusing to write under {dir}: it is not a directory")
  let dest := dir / (segs.getLast?.getD rel)
  if let .ok md ← dest.symlinkMetadata.toBaseIO then
    if md.type == .dir then
      throw (IO.userError s!"refusing to write {dest}: a directory is there")
  let tmp := dir / s!".{segs.getLast?.getD "file"}.orchestra-{← IO.monoNanosNow}"
  IO.FS.writeFile tmp content
  IO.Prim.setAccessRights tmp mode
  IO.FS.rename tmp dest

/-- `writeUnderHome` against this machine's `$HOME`, for the backends that run the agent here. -/
def hostPutHomeFile (rel mode content : String) : IO Unit := do
  let some bits := fileModeOf? mode
    | throw (IO.userError s!"refusing to write {rel}: '{mode}' is not a file mode")
  let home := (← IO.getEnv "HOME").getD ""
  if home.isEmpty then throw (IO.userError s!"cannot write {rel}: HOME is not set")
  writeUnderHome home rel bits content

/-- An execution environment, held for as long as one task.

    A task is not one command. It is `init.sh`, then `before.sh`, then the agent, then
    `validation.sh`, then the agent again if that failed, and `after.sh` at the end — and all of it
    has to happen in one place, on one copy of the checkout. For the backends that run on this
    machine that is true by construction. For one that runs the agent elsewhere it is the whole
    problem, and a session is the answer: it is opened once, everything runs through it, and it is
    closed when the task ends.

    It is also what makes the agent's own state outlive a single launch. A run relaunched after a
    failed validation resumes the session the first attempt started, and that session is a file the
    agent CLI wrote where it was running. -/
structure Session where
  /-- What to call this environment in a log line: a host, a pod name. -/
  id : String
  /-- Where the agent should reach the MCP server, given the endpoint orchestra would hand it —
      this machine's loopback, plus the token if one was minted. Copied from the backend when the
      session is opened; see `Backend.mcpEndpoint`. -/
  mcpEndpoint : McpEndpoint → IO McpEndpoint
  /-- The run as a human-readable command, for `--debug`. What it prints should be close enough to
      what happens that a person can paste it and see the same thing. -/
  describe : RunSpec → IO String
  /-- Start the agent. -/
  start : RunSpec → IO Handle
  /-- Run one of the repository's scripts, where the agent works. -/
  runScript : ScriptSpec → IO ScriptResult
  /-- Carry orchestra-supplied paths into the environment after it was opened.

      `openSession` stages everything the task is granted, but the agent's own MCP configuration
      does not exist yet at that point: it is written by `AgentDef.setupMcp`, which needs the
      endpoint, which needs the server, which is started after the session. It is nonetheless
      `.orchestra` content in the strictest sense — a file on the daemon's disk, in no image — and
      a backend that runs the agent elsewhere has to carry it there or the agent starts with a
      `--mcp-config` pointing at nothing and quietly does the task with no tools.

      A no-op for the backends that run the agent on this machine, where the file is already
      where the agent will look. -/
  provide : Array PathGrant → IO Unit := fun _ => pure ()
  /-- Write a file at a path relative to the run's home — `putHomeFile rel mode content` — without
      following a symbolic link below it (`writeUnderHome`). For what kleis's `/setup` hands a
      sandbox: configuration a client expects at a fixed place in its home.

      The default writes into this machine's `$HOME`, which is the run's home for every backend
      that runs the agent here — and so is shared by every task, and by the daemon. A backend that
      gives each run a home of its own writes it there instead. -/
  putHomeFile : String → String → String → IO Unit := hostPutHomeFile
  /-- The run's home as the agent sees it — an absolute path where the agent runs — for a backend
      that gives each run a home of its own, where `putHomeFile` writes. `none` for the backends
      whose runs share this machine's `$HOME`: a file one session must not share with another
      belongs somewhere else there (`Resident.EnvFile`). -/
  agentHome : Option String := none
  /-- Bring back what the agent wrote to a memory directory, without letting go of anything. For
      an environment held for a long time between agent processes — a resident's pod while it is
      warm (`docs/design/resident-sessions.md` §9) — whose memories would otherwise come back only
      at `close`. A no-op for the backends that run the agent here, where the memory directory is
      the daemon's own. -/
  syncBack : IO Unit := pure ()
  /-- Finish: bring back anything that has to come back, and release whatever was held. Called once
      per task, including when the task failed. -/
  close : IO Unit
  /-- Whether every task starts from a new environment, with nothing a previous one installed.

      This is what the repository's `init.sh` is measured against. It is meant to run once — a
      toolchain install, a warmed build cache — and it records that it has by writing a marker
      *into the checkout*, which is the right place when the environment is a machine that keeps
      what it was given. It is the wrong place when the environment is new each time and the
      checkout is the one thing carried into it: the marker arrives from the last task, and the
      hook that installs the toolchain is skipped in a container that has none.

      `true` says so, and `RepoConfig.runInitIfNeeded` then runs the hook every task. A repository's
      `init.sh` is expected to be idempotent for exactly this reason. -/
  freshEnvironment : Bool := false
  /-- Whether a conversation the agent started in an *earlier* task can be resumed in this one.

      An agent CLI's session is a file it wrote under its own `$HOME`, and `--resume <id>` is a
      request to read it. That is always true on a machine, where every task shares one home. It is
      not true of an environment that is new each task and takes its home with it, and a task that
      continues another one — `continues_from`, a series — has no conversation to continue there.

      Not a detail to leave to the agent CLI: the failure is a follow-up prompt ("now also handle
      the timeout case") answered by a model that has never seen what came before. -/
  carriesAgentState : Bool := true
  /-- Whether the workspace this environment opened on is one a predecessor left — the tree and
      agent home `continuesFrom` asked for — rather than a fresh one. `true` for a backend that
      keeps no workspaces of its own: there the agent's home is this machine's, and nothing of it
      is lost between runs. A resident reads it to tell a woken conversation from one whose volume
      was lost (`docs/design/resident-sessions.md` §9). -/
  workspaceReused : Bool := true
  /-- What another daemon needs to open this same environment again (`Backend.reattach`), or
      `none` when it cannot be: an environment that dies with this process, or one that holds
      nothing worth coming back to.

      Only the backend reads it. The task runner stores it with the task's progress
      (`Orchestra.RunState`) so that a daemon that comes back after a restart can find the pod, the
      claim and the run helper the task was using, without opening — and paying for — a new one. -/
  snapshot : Option Lean.Json := none

/-- What an execution environment has to be opened with.

    Everything here is known before the agent's own command line is built, which is what lets the
    session exist before the first thing that runs in it — `init.sh`, which runs before any agent
    has been launched. -/
structure SessionSpec where
  /-- The checkout: where scripts and the agent run. -/
  workdir : System.FilePath
  /-- Every path the task was granted. A backend running the agent elsewhere carries the ones
      marked `.orchestra` there (see `Provenance`); the rest describe an environment it is
      expected to already have. -/
  grants : Array PathGrant := #[]
  /-- What to call the environment where it is created — the task's id. -/
  label : String := "orchestra"
  /-- The repository being worked on, as `owner/name`. For labelling the environment, and for
      backends that let an operator pin one repository's environment separately from the rest. -/
  repo : Option String := none
  /-- The environment this repository asked for, from its own `.orchestra/config.json`.

      Which build and dev dependencies a task needs is a property of the repository, not of the
      daemon: one needs a Lean toolchain, the next a JDK, the one after that a browser and a
      database client. A backend that runs tasks in an image of some kind reads this to decide
      which; one that runs them on this machine has nothing to choose and ignores it. -/
  image : Option String := none
  /-- The task, interactive session or `orchestra interactive` run this environment is for, for a
      backend that keeps its workspace after it ends (`Backend.persistentWorkspaces`). `none` for
      what keeps nothing — the merger, `orchestra prepare` — which gets a throwaway one. -/
  taskId : Option String := none
  /-- The task this one continues, whose kept workspace — tree, build and agent home — it should be
      handed instead of `workdir`'s contents. A waking interactive session names itself. -/
  continuesFrom : Option String := none
  /-- Whether a fresh workspace will do when `continuesFrom` has none left: for a chat session that
      is woken although its agent never started, so there is no conversation to lose. -/
  continuationOptional : Bool := false
  /-- Whether the caller knows the run `continuesFrom` names is over — its task record is no
      longer `running`: it landed, or a startup sweep found it killed and its pods were removed
      (`Backend.reclaim`). Covers every continuation of such a run alike: the daemon's own restart
      resume (`Queue.resumeInterrupted`), a series follow-up or a continuation somebody queued.

      A backend that guards a kept workspace against a second agent may then judge the
      predecessor by what is actually running and nothing else. The kubernetes backend otherwise
      also counts a holder as alive for a while after it took the workspace, in case its pod is
      still being created (`holderStillWorking`) — a grace that would refuse a continuation of
      any task killed within its first few minutes. A run that is over will not create a pod, so
      the grace has nothing to wait for. Only ever relaxes that assumption about the predecessor
      (and `workspaceFallback`); a live pod still refuses. -/
  predecessorDead : Bool := false
  /-- An earlier run of the same chain whose workspace to take when `continuesFrom` never took one,
      and which is as over as `continuesFrom` is (set only with `predecessorDead`).

      For a restart resume whose predecessor was itself a resume killed before its agent started:
      the conversation then comes from further back (`Queue.sessionOwner`), and that killed resume
      may not have got as far as its workspace either, so nothing is labelled with its id. The
      workspace is then the one this run — the owner of the conversation — left. -/
  workspaceFallback : Option String := none
  /-- Where the daemon keeps build output carried from one task chain to the next, for this
      repository. Read when a fresh workspace is filled, written when a task ends. -/
  seedDir : Option System.FilePath := none
  /-- Whether the run has been called off. For a backend that may wait before the environment
      exists — for room on a cluster — so that a cancelled task stops waiting. -/
  cancelled : IO Bool := pure false
  /-- Called by `Session.close` immediately before the environment is let go of for good — the
      pod deleted — and after everything that comes back out of it is back.

      For the task runner, whose task can be handed over to the next daemon while its environment
      exists and not after: this is where it says so (`Exec.leaveReattachable`). A daemon that exits
      before this point leaves a pod the next one can finish closing; one that exits after it would
      leave a task with nowhere to be picked up from and its verdict not yet written. -/
  beforeRelease : IO Unit := pure ()
  /-- How long a backend may wait for room before answering `noRoom`. Short: credentials minted
      for the task are already ticking, and an entry put back to `pending` loses nothing. Zero
      for someone waiting on the other end of a chat. -/
  roomWaitSeconds : Nat := 600
  /-- The resident this environment is for, by name, when it is one. A backend that keeps
      workspaces keeps a resident's whatever its age — exempt from retention — and does not bound
      how long the environment may live, since a resident's warm sleep keeps it for hours
      (`docs/design/resident-sessions.md` §9). -/
  resident : Option String := none
  /-- The size of a new workspace volume, where the backend makes one; `none` is its own default. -/
  volumeSize : Option String := none

/-- What `Backend.probe` found of an environment a previous daemon left. -/
inductive ProbeResult where
  /-- There, and ready to be reopened. -/
  | alive
  /-- Positively not there to be picked up, and why. -/
  | gone (why : String)
  /-- Could not be asked, and why. Not the same as `gone`. -/
  | unknown (why : String)
deriving Repr, BEq, Inhabited

/-- One way of executing agents.

    Everything a backend needs to know about the task is in the specs it is handed; everything
    orchestra needs to know about the backend is here. -/
structure Backend where
  /-- The name this backend is selected by in `execution.backend`. -/
  name : String
  /-- Where the MCP server must listen for this backend's agents. -/
  exposure : Exposure := .loopback
  /-- Rewrite the endpoint orchestra would hand the agent — this machine's loopback, plus the
      token if one was minted — into the one the agent can actually reach.

      The default is that endpoint unchanged, which is right for any backend that runs the agent
      here. A backend that runs it elsewhere replaces the host: the agent's MCP client is
      configured from what this returns, and getting it wrong is silent — the agent starts, finds
      no tools, and does the task without them. See `docs/execution.md`. -/
  mcpEndpoint : McpEndpoint → IO McpEndpoint := pure
  /-- Check that this backend can actually run something here, before a task depends on it.

      Called once per task, ahead of the clone and the token, so that a machine without `landrun`
      says so in one line instead of failing every attempt with `could not execute external
      process` and burning the retry budget on it. -/
  preflight : IO (Except String Unit) := pure (.ok ())
  /-- Whether this backend keeps each task's workspace itself, after the task, for a continuation
      to be handed back (`SessionSpec.continuesFrom`).

      When it does, the daemon's clone slots hold nothing worth keeping: the checkout prepared on
      this machine is only what a *fresh* workspace is filled from, and is thrown away when the task
      ends. The queue then stops pinning a continuation to its predecessor's slot, and the task
      runner prepares a checkout per task rather than resetting a pooled one. -/
  persistentWorkspaces : Bool := false
  /-- Remove what a previous daemon left running, and say nothing about the work that was in it —
      that is the queue's business, and the startup sweep has already marked it `unfinished`.

      Called once, by the queue daemon at startup, after the stale-entry sweeps and before the
      first worker or interactive session exists. A backend whose environments die with the
      process that started them (landrun, local: they are child processes) has nothing to do,
      which is the default. One that runs them somewhere that outlives the daemon — pods — would
      otherwise leave them holding capacity, and, for the kubernetes backend, holding the
      workspace claim a continuation of the same task needs (`acquireWorkspaceClaim`).

      The argument decides, for the task or session id an environment was opened for
      (`SessionSpec.label`), whether it is a leftover: `true` only for an id this daemon's own
      database knows and does not have running. That, and not anything the backend can see, is
      the criterion — whatever else shares the place (another daemon, an `orchestra run` in the
      foreground, a previous version that labelled nothing) runs ids this database either does
      not know or still calls `running`, and is left alone.

      Must not throw for an ordinary failure to reach the place: the daemon logs and carries on,
      since leftovers that were not removed are a nuisance and a daemon that would not start is an
      outage. -/
  reclaim : (String → IO Bool) → IO Unit := fun _ => pure ()
  /-- Remove whatever was opened for task `taskId`, and say whether it is confirmed gone. For the
      daemon giving up on a task it was picking up again: a continuation of it may only take its
      workspace once nothing of the old run can still be writing there. `true` for a backend whose
      environments die with the process that opened them. -/
  removeTask : String → IO Bool := fun _ => pure true
  /-- Open an environment for one task. -/
  openSession : SessionSpec → IO Session
  /-- Open, again, an environment a previous daemon opened and left running: from its
      `Session.snapshot`, and the same `SessionSpec` the task was opened with. Nothing is created
      or copied — the environment is the one the task has been working in all along — and the
      session handed back closes it the way the original would have.

      `none` for a backend whose environments die with the process that opened them, which is
      every backend that runs the agent on this machine. Throws when the environment is gone. -/
  reattach : Option (Lean.Json → SessionSpec → IO Session) := none
  /-- Whether the environment a snapshot names is still there to be reopened, asked of every
      running task at startup before anything is decided about it (`Daemon.run`). `unknown` when
      the place cannot be asked: the task is then picked up anyway, and its reopening tries again
      (and falls back if it still cannot) — treating "could not ask" as "gone" would sweep up a
      task whose agent is still working. -/
  probe : Lean.Json → IO ProbeResult := fun _ => pure (.gone "this backend cannot reopen anything")

namespace Handle

/-- Kill a local process, and say nothing if it is already gone.

    Shelling out to `kill` rather than signalling directly: Lean's process API offers no kill, and
    this is the one place orchestra needs it. -/
def killPid (pid : UInt32) : IO Unit := do
  try
    let killer ← IO.Process.spawn {
      cmd := "kill"
      args := #["-9", toString pid]
      stdin := .null, stdout := .null, stderr := .null
    }
    let _ ← killer.wait
  catch _ => pure ()

/-- A handle over a local child process whose streams orchestra reads. -/
def ofPipedChild (child : IO.Process.Child { stdin := .null, stdout := .piped, stderr := .piped })
    : Handle :=
  { stdout    := some child.stdout
    stderr    := some child.stderr
    wait      := child.wait
    tryWait   := child.tryWait
    terminate := (try child.kill catch _ => pure ())
    kill      := killPid child.pid
    id        := s!"pid {child.pid}" }

/-- A handle over a local child process orchestra both writes to and reads from.

    `takeStdin` first, so the handle here is the only reference to that pipe and dropping it
    delivers EOF; a child that still held one would make the close unobservable. -/
def ofStreamChild
    (spawned : IO.Process.Child { stdin := .piped, stdout := .piped, stderr := .piped })
    : IO Handle := do
  let (stdinHandle, child) ← spawned.takeStdin
  return {
      stdout    := some child.stdout
      stderr    := some child.stderr
      stdin     := some stdinHandle
      wait      := child.wait
      tryWait   := child.tryWait
      terminate := (try child.kill catch _ => pure ())
      kill      := killPid child.pid
      id        := s!"pid {child.pid}" }


/-- A handle over a local child process that was given orchestra's own terminal. -/
def ofInheritChild
    (child : IO.Process.Child { stdin := .inherit, stdout := .inherit, stderr := .inherit })
    : Handle :=
  { stdout    := none
    stderr    := none
    wait      := child.wait
    tryWait   := child.tryWait
    terminate := (try child.kill catch _ => pure ())
    kill      := killPid child.pid
    id        := s!"pid {child.pid}" }

end Handle

/-- Run a repository script on this machine, as `bash <script>` in the checkout.

    Shared by the two backends that run the agent here, and unchanged from what the task runner
    did itself before sessions existed: the hooks inherit the daemon's streams, and
    `validation.sh` has its output captured for the retry prompt. -/
def hostRunScript (spec : ScriptSpec) : IO ScriptResult := do
  match spec.stdio with
  | .inherit =>
    let child ← IO.Process.spawn {
      cmd := "bash", args := #[spec.path], cwd := spec.workdir
      stdout := .inherit, stderr := .inherit }
    return { exitCode := ← child.wait }
  -- `.stream` never reaches here — a repository's script is run, not conversed with — and is
  -- captured like any other output rather than being a case nobody can hit.
  | .piped | .stream =>
    let child ← IO.Process.spawn {
      cmd := "bash", args := #[spec.path], cwd := spec.workdir
      stdout := .piped, stderr := .piped }
    -- Both pipes drained at once: a build writing more than a pipe buffer to stderr would
    -- otherwise block there while stdout is read to the end, and never finish.
    let stderrTask ← IO.asTask (prio := .dedicated) child.stderr.readToEnd
    let stdout ← child.stdout.readToEnd
    let stderr ← match ← IO.wait stderrTask with
      | .ok s => pure s
      | .error e => throw e
    let exitCode ← child.wait
    return { exitCode, output := (stdout ++ stderr).trimAscii.toString }

/-- How a backend is built from the `execution.options` object in `config.json`.

    A registry of these rather than of `Backend`s directly, because a backend that runs agents
    somewhere else needs to be told where: a namespace, an image, an address. Settings that belong
    to one backend are read by that backend, and a bad one is an `.error` naming the key rather
    than a launch that fails later for a reason nobody can trace to a config file. -/
structure BackendFactory where
  /-- The name this backend is selected by in `execution.backend`. -/
  name : String
  /-- One line for the error a person sees when they name a backend that does not exist. -/
  summary : String
  /-- Build the backend from `execution.options`. -/
  make : Lean.Json → Except String Backend

private def hexOf (bs : ByteArray) : String := Id.run do
  let digits := "0123456789abcdef".toList
  let mut out := ""
  for b in bs.toList do
    out := out.push digits[b.toNat / 16]! |>.push digits[b.toNat % 16]!
  return out

/-- Exactly `bytes` bytes of `/dev/urandom`, as lowercase hex. Throws rather than return anything
    less.

    Separate from `randomHex` because the two callers want opposite failure modes. A pod name only
    has to be unique, so degrading to a clock reading is better than refusing to start a task. A
    secret that degrades to a clock reading is not a secret, and nothing downstream can tell the
    difference — so the one caller that mints a credential gets a function that fails loudly.

    The read is repeated rather than trusted once: `Handle.read` is permitted to return fewer
    bytes than asked for, and a short read here would silently shorten the token. -/
def randomSecret (bytes : Nat) : IO String := do
  IO.FS.withFile "/dev/urandom" .read fun h => do
    let mut acc : ByteArray := .empty
    -- Bounded so a handle that yields nothing fails with this message instead of spinning.
    for _ in [0:16] do
      if acc.size ≥ bytes then break
      let chunk ← h.read (bytes - acc.size).toUSize
      if chunk.isEmpty then break
      acc := acc ++ chunk
    if acc.size < bytes then
      throw (IO.userError s!"could not read {bytes} bytes from /dev/urandom (got {acc.size})")
    return hexOf acc

/-- `bytes` random bytes as lowercase hex, for names that must not collide.

    Falls back to the monotonic clock if `/dev/urandom` cannot be read, which keeps a name unique
    but not unpredictable. Never use it for a secret — `randomSecret` is that function, and it
    throws instead of degrading. -/
def randomHex (bytes : Nat) : IO String := do
  try
    let h ← IO.FS.Handle.mk "/dev/urandom" .read
    let bs ← h.read bytes.toUSize
    let out := hexOf bs
    if out.isEmpty then throw (IO.userError "no entropy") else return out
  catch _ =>
    return toString (← IO.monoNanosNow)

/-- This machine's `$HOME`, or `""` when it is unset.

    `""` rather than an error: a home-scoped grant then resolves to a path that does not exist,
    which the host backends already drop with a warning, and that is a better failure than
    refusing to launch. -/
def hostHome : IO String :=
  return (← IO.getEnv "HOME").getD ""

end Orchestra.Exec
