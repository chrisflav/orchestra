/-!
# What a run asks for, said without saying how it is confined

`RunSpec` is the whole of what orchestra needs from whatever executes an agent: a command, a
working directory, the filesystem it may touch, the ports it may open, and the environment it
starts with. It names no mechanism. `landrun` renders it as Landlock flags; a Kubernetes backend
would render the same fields as a pod spec — mounts, a `NetworkPolicy`, `env` entries — without
orchestra learning anything about pods.

The split matters because the three things `Sandbox.launchAgent` used to do at once come apart
along it: *what the agent needs* (here), *how that is enforced* (`Orchestra.Exec.Backend` and its
implementations), and *how the run is supervised* — streams parsed, cancellation honoured, logs
written — which is the same work whatever runs the process, and stays in `Sandbox`.

See `docs/execution.md` for how a new backend is added.
-/

namespace Orchestra.Exec

/-- What a run may do with one path.

    The four cases are what agents actually need, which is why they are not a set of independent
    read/write/execute bits: read-without-execute is the right grant for configuration, and
    write-without-execute for scratch space, but write-without-read is not a thing any agent
    wants and would only be a way to spell a mistake. -/
inductive Access where
  /-- Read, no execute. Configuration, certificates, `/etc`. -/
  | ro
  /-- Read and execute. Binaries, libraries, toolchains that are only run, never installed. -/
  | rox
  /-- Read and write, no execute. Scratch space, caches, agent state directories. -/
  | rw
  /-- Read, write and execute. Toolchain managers, which install binaries and then run them. -/
  | rwx
deriving Repr, BEq, Inhabited, DecidableEq

/-- Where a grant's path is rooted. -/
inductive Scope where
  /-- An absolute path, valid as written on whatever filesystem the run sees. -/
  | absolute
  /-- Relative to the *run's* home directory, not orchestra's.

      They are the same directory for a backend that runs the agent on this machine, and they are
      not for one that runs it elsewhere: `.claude` means "wherever this agent keeps its state",
      and only the backend knows where that is. Resolving it here would bake this machine's
      `$HOME` into a path that will be interpreted somewhere else entirely. -/
  | home
deriving Repr, BEq, Inhabited, DecidableEq

/-- Who is expected to have put the content at a path there. -/
inductive Provenance where
  /-- Whatever the agent runs on supplies it: `/usr`, `/etc`, the toolchain, the agent's own
      `$HOME`. A backend that runs the agent elsewhere finds these in its image, or does without
      them, and either way has nothing to carry. -/
  | environment
  /-- Orchestra supplies the content: the checkout, plugin directories, memory directories. These
      exist on the daemon's disk and nowhere else, so a backend that runs the agent elsewhere has
      to carry them there — and carry the writable ones back, or the work is lost. -/
  | orchestra
deriving Repr, BEq, Inhabited, DecidableEq

/-- One path the run may touch, and how. -/
structure PathGrant where
  /-- The path, interpreted according to `scope`. -/
  path : String
  /-- What the run may do with it. -/
  access : Access
  /-- How to read `path`. -/
  scope : Scope := .absolute
  /-- Who supplies what is at the path. Backends that run the agent on this machine can ignore
      this; one that runs it elsewhere cannot. -/
  from_ : Provenance := .environment
  /-- Whether the run is expected to break without this path.

      A backend that can only grant paths that exist has to drop the rest, and the two kinds of
      missing path want opposite treatment: `/nix` on a non-Nix machine is absent because that
      machine has no Nix, and saying so every launch is noise, while a missing `~/.claude` is a
      broken image — the agent cannot create it either, since home itself is never granted, so it
      typically reports that it cannot write its configuration and then hangs. Backends warn for
      the second kind and stay quiet about the first. -/
  required : Bool := false
deriving Repr, BEq, Inhabited

/-- The ports a run may use. Everything not named here is denied. -/
structure Ports where
  /-- Ports the run may connect out to. -/
  connect : Array UInt16 := #[]
  /-- Ports the run may listen on. -/
  bind : Array UInt16 := #[]
deriving Repr, BEq, Inhabited

/-- Where the agent should reach the MCP server this task started, and what it must say to be
    let in.

    A port alone was enough while every backend ran the agent on the same machine and loopback was
    the answer by construction. It stops being enough the moment the agent runs anywhere else, and
    the failure is silent in the worst way: the agent starts, finds no tools, and does the task
    with its bare hands. The host travels with the port so that a backend which moves the agent
    has somewhere to say so — see `Backend.mcpEndpoint`. -/
structure McpEndpoint where
  /-- Host the agent connects to. `127.0.0.1` for anything running on this machine. -/
  host : String
  /-- Port the MCP server was started on. -/
  port : UInt16
  /-- A one-run secret the client sends as its first line, when the server is listening anywhere
      but loopback.

      Loopback needs no token: only a process on this machine can connect at all, which is the
      same thing the sandbox already grants. Off-loopback it is not optional — the server holds
      the PAT that opens pull requests and posts comments, the one credential the sandbox exists
      to keep out of the agent's hands, and a listening socket on a cluster network is reachable
      by everything else on it. `none` means loopback; see `Exposure`. -/
  token : Option String := none
deriving Repr, BEq, Inhabited

/-- How the run's standard streams are wired. -/
inductive Stdio where
  /-- Output captured, nothing written in. What every queued task uses. -/
  | piped
  /-- Handed to the terminal orchestra was started from, for `orchestra interactive`. -/
  | inherit
  /-- Output captured *and* input open: the process stays up and turns are written to it, which
      is what an interactive session holds. The difference from `.piped` is not cosmetic — the
      run's stdin has to be a pipe the daemon still holds, because closing it is how a CLI reading
      turns is told there are no more. -/
  | stream
deriving Repr, BEq, Inhabited, DecidableEq

/-- Everything one agent run needs, in terms no execution backend is privileged by.

    Built by `Sandbox.launchAgent` from the task's parameters and its agent backend's declared
    needs; consumed by an `Orchestra.Exec.Backend`. -/
structure RunSpec where
  /-- The executable to run, resolved on the run's `PATH`, not orchestra's. -/
  command : String
  /-- Its arguments, already built by the agent backend. -/
  args : Array String := #[]
  /-- The directory the command starts in: the repository checkout. -/
  workdir : System.FilePath
  /-- Every path the run may touch. Order is preserved so that a rendered command line is stable
      enough to diff between two runs. -/
  grants : Array PathGrant := #[]
  /-- The ports it may use. -/
  ports : Ports := {}
  /-- Environment variables set explicitly for the run. -/
  env : Array (String × String) := #[]
  /-- Names of environment variables to carry over from orchestra's own environment. Kept apart
      from `env` because the *value* is not orchestra's to state: `PATH` and `HOME` mean what they
      mean wherever the agent ends up, and a backend that runs it elsewhere resolves them there
      rather than copying this machine's. -/
  envPassthrough : Array String := #[]
  /-- How the standard streams are wired. -/
  stdio : Stdio := .piped
  /-- What to call this run in a log line. Not passed to the command; used by backends that have
      to name the thing they create (a container, a pod) and by `--debug` output. -/
  label : String := "orchestra-agent"
  /-- What to call this run where the backend keeps it, for a backend whose runs outlive the
      daemon (`Exec.Runner`): an id that is the same every time the same step of the same task is
      reached — `agent-0`, `agent-1` — so that a daemon coming back to the task finds the run the
      last one started instead of starting a second. `none` lets the backend pick a fresh one. -/
  runId : Option String := none
deriving Inhabited

namespace PathGrant

/-- `g` with a `.home` path resolved against `home`. Absolute grants are returned unchanged.

    Kept pure and separate from any filesystem check so that the two questions a backend asks —
    *where is this* and *is it there* — stay answerable one at a time. -/
def resolve (home : String) (g : PathGrant) : PathGrant :=
  match g.scope with
  | .absolute => g
  | .home     => { g with path := (System.FilePath.mk home / System.FilePath.mk g.path).toString
                        , scope := .absolute }

end PathGrant

/-- Quote `s` for a shell command line.

    Used both for display — `--debug` output a person can paste — and for the few places a backend
    genuinely needs a shell, such as a pipeline between two commands. Nothing orchestra spawns
    directly goes through a shell; `IO.Process.spawn` takes an argv. -/
def shellEscape (s : String) : String :=
  if s.any (fun c => c == ' ' || c == '"' || c == '\'' || c == '\\' || c == '$' || c == '`'
                   || c == '(' || c == ')' || c == '!' || c == '&' || c == '|'
                   || c == ';' || c == '\n' || c == '\t') then
    "'" ++ s.replace "'" "'\\''" ++ "'"
  else s

namespace McpEndpoint

/-- The `"error"` member the relay answers a request with when the connection it was sent on ended
    before its answer came back (`relayScript`). Plain text, no apostrophe and no backslash: it is
    spliced into a single-quoted shell word and then into TOML and JSON. -/
def relayLostReply : String :=
  "\"error\":{\"code\":-32001,\"message\":\"The connection to orchestra was interrupted before \
this call was answered (orchestra is probably restarting). The call was not retried: if it is \
still needed, make it again.\"}"

/-- The script `stdioCommand` runs under `bash` for an endpoint with a token: a relay between the
    agent's MCP client and the server that survives the server going away and coming back.

    It used to be `{ echo <token>; cat; } | nc <host> <port>`, which ends — and with it every tool
    the agent has — the moment the connection does. The connection ends whenever the daemon
    restarts, and a daemon that now hands its running tasks over to the next one
    (`docs/kubernetes.md`, restarts) has to give the agent its tools back afterwards, or the
    reconnect is a conversation with no hands. The next daemon brings the task's server back on
    the same port with the same token (`TaskRunner`); this is the half that dials it again.

    How, in one line of `bash` (one line, and no backslash anywhere: it is written into TOML as well
    as JSON, see `vibe`'s config):

    * The agent's requests go into a FIFO held open read-write by the relay itself, by one `cat`
      that lives as long as the agent's stdin does. Nothing the agent writes while the server is
      away is lost: it waits in the FIFO for the next connection.
    * Each connection is `bash`'s own `/dev/tcp`: the token goes first, then one loop copies whole
      lines from the FIFO to the socket and another whole lines from the socket to the agent
      (`read -r`, so a long request is never split across two connections). When the server's
      side ends (`wait -n`, so `bash` 4.3 or later) the writer is *asked* to stop rather than
      killed: it reads with a one-second timeout, keeps a partial line across timeouts, and stops
      only between lines — so a request half way into the FIFO is neither cut in two nor sent as
      garbage on the next connection.
    * **Every request is answered exactly once.** The id of each request sent is noted (one file
      per id; the id is the last `"id":` in the line, which is where the MCP client puts it), and
      crossed off when the server's answer to it passes back (the server writes `"id"` first,
      directly followed by `"jsonrpc"`: its keys are sorted). When a connection ends, every id not
      crossed off is answered by the relay itself with an error (`relayLostReply`), and nothing is
      sent again — a tool call that may have half happened (a comment posted, a pull request
      opened) is not the relay's to repeat. So a request the old server read and never answered,
      or one that went out on a connection about to close, gets an error the agent can act on
      instead of a wait that never ends. Notifications carry no id and need no answer.
    * The one exception is `initialize`, which has no effect to repeat and whose error would make
      the MCP client give the server up for the rest of the session: while unanswered it is kept,
      and sent again first on the next connection.
    * Reconnecting backs off from one second to fifteen, and gives up after an hour without a
      connection that lasted, which is longer than any restart and shorter than a pod's deadline.
    * The agent closing its stdin — it is done — ends the relay.

    The server is stateless per connection (every request is answered on its own, `initialize` or
    not), so a request on a new connection is served exactly as it would have been on the old one.
    As a last backstop for anything that still slips through, a remote agent runs with
    `MCP_TOOL_TIMEOUT` set (`Sandbox.remoteMcpEnv`), so no tool call waits forever. -/
def relayScript (e : McpEndpoint) (token : String) : String :=
  s!"t={shellEscape token}; h={shellEscape e.host}; p={e.port}; " ++
  "d=$(mktemp -d) || exit 1; mkdir \"$d/p\" && mkfifo \"$d/q\" || exit 1; exec 5<>\"$d/q\"; " ++
  "exec 4<&0; rq='.*\"id\":([0-9]+|\"[^\"]*\")[,}]'; rs='\"id\":([0-9]+|\"[^\"]*\"),\"jsonrpc\"'; " ++
  "( cat <&4 >&5; kill -TERM $$ 2>/dev/null ) & " ++
  "trap 'kill $(jobs -p) 2>/dev/null; rm -rf \"$d\"' EXIT; trap 'exit 0' HUP INT TERM; " ++
  "w=1; down=0; " ++
  "while :; do s=$SECONDS; rm -f \"$d/stop\"; " ++
  "if { exec 3<>\"/dev/tcp/$h/$p\"; } 2>/dev/null; then echo \"$t\" >&3; " ++
  "[ -f \"$d/init\" ] && cat \"$d/init\" >&3; " ++
  "( while IFS= read -r l; do echo \"$l\"; if [[ $l =~ $rs ]]; then " ++
  "k=${BASH_REMATCH[1]//[^A-Za-z0-9]/_}; rm -f \"$d/p/$k\"; " ++
  "[ \"$k\" = \"$(cat \"$d/initid\" 2>/dev/null)\" ] && rm -f \"$d/init\" \"$d/initid\"; " ++
  "fi; done <&3 ) & r=$!; " ++
  "( b=; while :; do if IFS= read -r -t 1 l; then l=$b$l; b=; if [[ $l =~ $rq ]]; then " ++
  "k=${BASH_REMATCH[1]//[^A-Za-z0-9]/_}; echo \"${BASH_REMATCH[1]}\" > \"$d/p/$k\"; " ++
  "if [[ $l == *'\"method\":\"initialize\"'* ]]; then echo \"$l\" > \"$d/init\"; " ++
  "echo \"$k\" > \"$d/initid\"; fi; fi; echo \"$l\" >&3 || exit 1; " ++
  "elif [ $? -gt 128 ]; then b=$b$l; [ -z \"$b\" ] && [ -f \"$d/stop\" ] && exit 0; " ++
  "else exit 0; fi; done <&5 ) & f=$!; " ++
  "wait -n; touch \"$d/stop\"; wait $f 2>/dev/null; kill $r 2>/dev/null; wait $r 2>/dev/null; " ++
  "exec 3>&-; fi; " ++
  "for x in \"$d\"/p/*; do [ -f \"$x\" ] || continue; " ++
  "[ \"${x##*/}\" = \"$(cat \"$d/initid\" 2>/dev/null)\" ] && continue; " ++
  "echo '{\"jsonrpc\":\"2.0\",\"id\":'\"$(cat \"$x\")\"'," ++ relayLostReply ++ "}'; " ++
  "rm -f \"$x\"; done; " ++
  "if [ $((SECONDS - s)) -ge 5 ]; then w=1; down=0; else down=$((down + w)); " ++
  "[ $down -ge 3600 ] && exit 1; w=$((w * 2)); [ $w -gt 15 ] && w=15; fi; " ++
  "sleep $w & wait $!; done"

/-- The command an agent's MCP client runs to reach this endpoint over a stdio transport, as
    `(command, args)`.

    Every agent backend asks for this rather than writing `nc <host> <port>` itself, because the
    answer stops being `nc` as soon as the endpoint carries a token: the token has to be the first
    line on the connection, and `nc` has no way to put it there. With a token it is `relayScript`
    under `bash`, which sends the token first on every connection and keeps the agent's side of
    the pipe open across the server restarting. Only an endpoint off this machine carries a token
    (`Exec.mcpBinding`), and only an agent off this machine outlives the daemon, so the loopback
    case stays plain `nc`.

    The token is emitted through `echo` rather than `printf` on purpose: it is written into TOML
    as well as JSON (`vibe`'s config), and a `\n` escape means one thing in one and another in the
    other.

    The host is escaped like the token even though `validHost?` has already refused anything that
    would need it. Two cheap guards rather than one, because they fail differently: validation
    rejects a bad `mcp_host` at startup with the key named, and escaping means a host that reached
    here some other way is still a host and not a second command. -/
def stdioCommand (e : McpEndpoint) : String × Array String :=
  match e.token with
  | none   => ("nc", #[e.host, toString e.port])
  | some t => ("bash", #["-c", relayScript e t])

/-- `host` if it is something that can be a host at all, otherwise why not.

    Deliberately narrower than the grammar of a hostname: this string is interpolated into a shell
    pipeline and rendered into the agent's configuration in two different quoting languages, and
    the set of characters that survive all three unambiguously is exactly the set a DNS name or an
    IP address is spelled with. An operator who needs something outside it wants a DNS name for
    it, not an escape. -/
def validHost? (host : String) : Except String String :=
  if host.isEmpty then
    .error "is empty"
  else if host.all (fun c => c.isAlphanum || c == '.' || c == '-' || c == ':' || c == '_') then
    .ok host
  else
    .error s!"'{host}' is not a hostname or an IP address (letters, digits, '.', '-', ':' and \
'_' only)"

end McpEndpoint

namespace RunSpec

/-- Every grant, with `.home` paths resolved against `home`. -/
def resolveGrants (home : String) (spec : RunSpec) : RunSpec :=
  { spec with grants := spec.grants.map (PathGrant.resolve home) }

/-- The paths orchestra itself supplies, which a backend running the agent elsewhere has to carry
    there. -/
def orchestraGrants (spec : RunSpec) : Array PathGrant :=
  spec.grants.filter (·.from_ == .orchestra)

end RunSpec

/-- Why `ref` cannot be the image a task runs in, or `none` when it can.

    Not a parse of the OCI grammar — the registry is the authority on that, and a reference this
    accepts and the registry rejects fails as an unstartable pod with the registry's own message.
    What it is for is the strings that end up in a pod manifest without an operator having typed
    them into this daemon's configuration: what a repository asks for in its
    `.orchestra/config.json`, and a task's own `execution_image`, which a task file, a role, a
    listener action, a workflow step, an agent's `queue_task` or an API client's request can set.
    A name with a space, a quote or a newline in it is refused where it is read rather than turned
    into a manifest field.

    Lives here, beside `RunSpec`, rather than with the one backend that reads images: the task
    configuration that carries an image is read long before any backend is chosen, and checking it
    there is what reports a typo when the task file is read instead of after the task has been
    dispatched. -/
def imageRefError? (ref : String) : Option String :=
  if ref.isEmpty then some "an image reference is empty"
  else if ref.length > 512 then
    some s!"'{(ref.take 40).toString}…' is longer than 512 characters, which no image reference is"
  else if ref.all (fun c => c.isAlphanum || c == '.' || c == '_' || c == '-' || c == '/' ||
      c == ':' || c == '@') then none
  else some s!"'{ref}' is not an image reference (letters, digits and . _ - / : @ only)"

end Orchestra.Exec
