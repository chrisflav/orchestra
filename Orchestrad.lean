import Orchestra.Daemon
import Orchestra.Dashboard
import Orchestra.Project
import Orchestra.Utils.Streams
import Cli

open Cli
open Orchestra

/-!
# `orchestrad` — the orchestra backend

The half of orchestra that runs continuously and holds the credentials: the queue daemon that
dispatches agents, and the HTTP API that everything else reads and writes orchestra's
configuration through.

It is a separate binary from `orchestra` because the two are separate things. The CLI is a client
— it makes requests and prints what comes back — and after taxis #433 it no longer touches the
daemon's state directly. Compiling the task runner, the sandbox launcher, the socket server and
the HTTP stack into it as well would have meant shipping a server inside a client and leaving it
inert: importable, unreachable, and impossible to tell from a bug.

The name follows the convention its job implies. `orchestra` is the thing you type; `orchestrad`
is the daemon it types at.

Three commands:

  * `orchestrad queue` — the queue daemon alone. What `orchestra queue start` used to be.
  * `orchestrad dashboard` — the HTTP API (and, with `--site`, the front-end) alone.
  * `orchestrad serve` — both in one process, which is what a single-host install wants.

They stay separable because the deployment in `docker/docker-compose.yaml` separates them: the
daemon drains in-flight tasks for up to half an hour on a stop, and a web console tied to that
would be unavailable exactly when someone is trying to find out why a task is stuck.
-/

/-- The queue daemon's flags, in the one place both `queue` and `serve` read them from. -/
private def daemonConfigOf (p : Parsed) : Daemon.Config where
  configPath      := p.flag? "config" |>.map (·.as! String)
  debug           := p.hasFlag "debug"
  parallel        := p.flag? "parallel" |>.map (·.as! Nat)
  parallelPerRepo := p.flag? "parallel-per-repo" |>.map (·.as! Nat)

/-- Start the HTTP API, printing where it landed and how to authenticate to it.

    Returns the bound port. Does not block: the caller decides what the process does next, which
    is how `serve` runs the daemon on top of it. -/
private def startDashboard (p : Parsed) : IO (Option UInt16) := do
  let port := p.flag? "port" |>.map (·.as! Nat) |>.getD 8080
  let host := p.flag? "host" |>.map (·.as! String) |>.getD "127.0.0.1"
  let (password, generated) ← Secret.resolvePassword (p.flag? "password" |>.map (·.as! String))
  let siteDir := p.flag? "site" |>.map (fun f => System.FilePath.mk (f.as! String))
  let configPath := p.flag? "config" |>.map (fun f => System.FilePath.mk (f.as! String))
  let sessionTtl := p.flag? "session-ttl" |>.map (·.as! Nat) |>.getD 43200
  if let some dir := siteDir then
    unless ← (dir / "index.html").pathExists do
      IO.eprintln s!"error: '{dir}' has no index.html, so it is not a built front-end."
      IO.eprintln "Build it first:  cd web && npm ci && npm run build"
      IO.eprintln s!"then point --site at web/dist (given here: {dir})."
      return none
  let (boundPort, _shutdown) ← Dashboard.serve
    { password, port := port.toUInt16, host, siteDir, configPath, sessionTtlSeconds := sessionTtl,
      secureCookie := p.hasFlag "secure-cookie" }
  match siteDir with
  | some _ => IO.println s!"Orchestra API and dashboard on http://{host}:{boundPort}"
  | none   => IO.println s!"Orchestra API (no front-end) on http://{host}:{boundPort}"
  -- A configured password is already known to whoever configured it, and echoing it would
  -- only copy it into the container logs; a generated one has to be shown once or it is lost.
  if generated then
    IO.println ""
    IO.println s!"  Dashboard password: {password}"
    IO.println ""
    IO.println "Generated on first run and saved to <data>/dashboard.secret. Set --password or"
    IO.println s!"${Secret.passwordEnvVar} to choose it yourself."
  else
    IO.println "Log in with the configured password (--password / \
$ORCHESTRA_DASHBOARD_PASSWORD / <data>/dashboard.secret)."
  IO.println "Scripts and the orchestra CLI authenticate with 'Authorization: Bearer <password>'."
  if siteDir.isNone then
    IO.println "Pass --site web/dist to serve the built front-end from here as well."
  return some boundPort

private def queueHandler (p : Parsed) : IO UInt32 :=
  Daemon.run (daemonConfigOf p)

private def dashboardHandler (p : Parsed) : IO UInt32 := do
  let some _ ← startDashboard p | return 1
  repeat do
    IO.sleep 60000
  return 0

/-- Both halves in one process.

    The API comes up first and the daemon runs in the foreground on top of it, so a shutdown
    signal reaches the part that has work to drain. `Daemon.run` exits the process when that
    drain completes, which takes the HTTP server with it — correct, because a backend that has
    stopped dispatching should not still be claiming it can be configured. -/
private def serveHandler (p : Parsed) : IO UInt32 := do
  let some _ ← startDashboard p | return 1
  IO.println ""
  Daemon.run (daemonConfigOf p)

private def queueCmd : Cmd := `[Cli|
  queue VIA queueHandler; ["0.1.0"]
  "Run the queue daemon in the foreground: claim queued entries, run listeners, answer the \
control socket."

  FLAGS:
    c, config : String; "Path to config file (default: ~/.config/orchestra/config.json)"
    d, debug; "Print the landrun command before executing each task"
    parallel : Nat; "Maximum number of tasks to run in parallel. Overrides queue.parallel in \
config.json (default: 1)"
    "parallel-per-repo" : Nat; "Maximum tasks on any one repository. Overrides \
queue.parallel_per_repo in config.json (default: 1). Run 'orchestra prepare --slots N' to match."
]

private def dashboardCmd : Cmd := `[Cli|
  dashboard VIA dashboardHandler; ["0.1.0"]
  "Serve the HTTP API, its SSE streams, and the built front-end."

  FLAGS:
    p, port : Nat; "Port to listen on (default: 8080)"
    host : String; "Address to bind (default: 127.0.0.1; use 0.0.0.0 in a container)"
    password : String; "Shared secret to require (default: $ORCHESTRA_DASHBOARD_PASSWORD or a \
generated, persisted one)"
    s, site : String; "Serve the front-end built into this directory (web/dist)"
    c, config : String; "Path to config file (read for the auth-sources page)"
    "session-ttl" : Nat; "Session cookie lifetime in seconds (default: 43200)"
    "secure-cookie"; "Mark the session cookie Secure (only behind a TLS-terminating proxy)"
]

private def serveCmd : Cmd := `[Cli|
  serve VIA serveHandler; ["0.1.0"]
  "Run the whole backend: the HTTP API and the queue daemon in one process."

  FLAGS:
    p, port : Nat; "Port to listen on (default: 8080)"
    host : String; "Address to bind (default: 127.0.0.1; use 0.0.0.0 in a container)"
    password : String; "Shared secret to require (default: $ORCHESTRA_DASHBOARD_PASSWORD or a \
generated, persisted one)"
    s, site : String; "Serve the front-end built into this directory (web/dist)"
    c, config : String; "Path to config file"
    "session-ttl" : Nat; "Session cookie lifetime in seconds (default: 43200)"
    "secure-cookie"; "Mark the session cookie Secure (only behind a TLS-terminating proxy)"
    d, debug; "Print the landrun command before executing each task"
    parallel : Nat; "Maximum number of tasks to run in parallel"
    "parallel-per-repo" : Nat; "Maximum tasks on any one repository"
]

private def defaultHandler (_ : Parsed) : IO UInt32 := do
  IO.eprintln "Use a subcommand: serve, queue, or dashboard. Try 'orchestrad --help'."
  return 1

def orchestradCmd : Cmd := `[Cli|
  orchestrad VIA defaultHandler; ["0.1.0"]
  "The orchestra backend: the queue daemon and the HTTP API."

  SUBCOMMANDS:
    serveCmd;
    queueCmd;
    dashboardCmd
]

def main (args : List String) : IO UInt32 := do
  Utils.unbufferIfPiped
  Project.ensureTaxisConfigured args
  orchestradCmd.validate args
