import Agent
import Cli

open Cli
open Agent

/-- Run a single task: clone repo, start MCP server, launch agent. -/
private def runTask (appConfig : AppConfig) (task : Task) (idx : Nat) (debug : Bool) : IO Unit := do
  IO.println s!"=== Task {idx}: {task.fork} ({repr task.mode}) ==="
  -- 1. Clone / update repo
  IO.println s!"Cloning/updating {task.fork}..."
  let repoPath ← Repo.ensureCloned task.fork task.upstream
  IO.println s!"  Repo at {repoPath}"
  -- 2. Create GitHub App token
  IO.println "  Creating GitHub App token..."
  let jwt ← GitHub.createJWT appConfig.appId appConfig.privateKeyPath
  let (forkOwner, _) ← Repo.splitRepo task.fork
  let installationId ← match appConfig.installationId with
    | some id => pure id
    | none => GitHub.getInstallationId jwt forkOwner
  let token ← GitHub.createInstallationToken jwt installationId
  GitHub.setupGhAuth token
  IO.println "  Token ready"
  -- 3. Start MCP/SSE server (runs in this process, outside the sandbox)
  let serverState : Server.State := {
    upstream := task.upstream
    fork := task.fork
    allowPR := match task.mode with | .pr => true | .fork => false
    appId := appConfig.appId
    privateKeyPath := appConfig.privateKeyPath
    installationId
    pat := appConfig.pat
  }
  let (port, shutdown) ← Server.start serverState
  IO.println s!"  MCP server on port {port}"
  -- 4. Launch agent in sandbox (connects to MCP server over localhost)
  IO.println "  Launching agent..."
  let exitCode ← Sandbox.launchAgent repoPath task.prompt port token (debug := debug)
  IO.println s!"  Agent exited with code {exitCode}"
  -- 5. Shutdown MCP server
  shutdown
  IO.println s!"=== Task {idx} done ===\n"

private def runHandler (p : Parsed) : IO UInt32 := do
  let taskFile := p.positionalArg! "task-file" |>.as! String
  let configPath := p.flag? "config" |>.map (·.as! String)
  let taskIdx := p.flag? "task" |>.map (·.as! Nat)
  let debug := p.hasFlag "debug"
  let appConfig ← loadAppConfig (configPath.map System.FilePath.mk)
  let taskFileData ← loadTaskFile taskFile
  if taskFileData.tasks.isEmpty then
    IO.eprintln "No tasks found in task file"
    return 1
  let tasks := match taskIdx with
    | some idx =>
      if h : idx < taskFileData.tasks.size then
        #[taskFileData.tasks[idx]]
      else
        #[]
    | none => taskFileData.tasks
  if tasks.isEmpty then
    IO.eprintln "Task index out of range"
    return 1
  for i in [:tasks.size] do
    try
      runTask appConfig tasks[i]! i debug
    catch e =>
      IO.eprintln s!"Task {i} failed: {e}"
  return (0 : UInt32)

private def mcpServerHandler (p : Parsed) : IO UInt32 := do
  let upstream := p.positionalArg! "upstream" |>.as! String
  let fork := p.positionalArg! "fork" |>.as! String
  let allowPR := p.hasFlag "allow_pr"
  let configPath := p.flag? "config" |>.map (·.as! String)
  let appConfig ← loadAppConfig (configPath.map System.FilePath.mk)
  let jwt ← GitHub.createJWT appConfig.appId appConfig.privateKeyPath
  let (forkOwner, _) ← Repo.splitRepo fork
  let installationId ← match appConfig.installationId with
    | some id => pure id
    | none => GitHub.getInstallationId jwt forkOwner
  let token ← GitHub.createInstallationToken jwt installationId
  GitHub.setupGhAuth token
  let serverState : Server.State := {
    upstream, fork, allowPR
    appId := appConfig.appId
    privateKeyPath := appConfig.privateKeyPath
    installationId
    pat := appConfig.pat
  }
  let (port, _shutdown) ← Server.start serverState
  IO.println s!"MCP server listening on port {port}"
  repeat do
    IO.sleep 60000
  return 0

private def prepareHandler (p : Parsed) : IO UInt32 := do
  let upstream := p.positionalArg! "upstream" |>.as! String
  let fork := p.positionalArg! "fork" |>.as! String
  let repoPath ← Repo.ensureCloned fork upstream
  IO.println repoPath.toString
  return 0

private def cleanupHandler (_ : Parsed) : IO UInt32 := do
  Repo.cleanup
  return 0

private def runCmd' : Cmd := `[Cli|
  run VIA runHandler; ["0.1.0"]
  "Run coding agent tasks from a task file."

  FLAGS:
    c, config : String; "Path to config file (default: ~/.agent/config.json)"
    t, task : Nat; "Run only the task at this index (0-based)"
    d, debug; "Print the landrun command before executing it"

  ARGS:
    "task-file" : String; "Path to the JSON task file"
]

private def mcpServerCmd : Cmd := `[Cli|
  «mcp-server» VIA mcpServerHandler; ["0.1.0"]
  "Start the MCP server and print the port it is listening on."

  FLAGS:
    c, config : String; "Path to config file (default: ~/.agent/config.json)"
    allow_pr; "Allow the create_pr tool (disabled by default)"

  ARGS:
    "upstream" : String; "Upstream repository in 'owner/repo' format"
    "fork" : String; "Fork repository in 'owner/repo' format"
]

private def prepareCmd : Cmd := `[Cli|
  prepare VIA prepareHandler; ["0.1.0"]
  "Clone the fork and configure the upstream remote."

  ARGS:
    "upstream" : String; "Upstream repository in 'owner/repo' format"
    "fork" : String; "Fork repository in 'owner/repo' format"
]

private def cleanupCmd : Cmd := `[Cli|
  cleanup VIA cleanupHandler; ["0.1.0"]
  "Remove all cloned repositories."
]

private def defaultHandler (_ : Parsed) : IO UInt32 := do
  IO.eprintln "Use a subcommand. Try 'agent --help'."
  return 1

def agentCmd : Cmd := `[Cli|
  agent VIA defaultHandler; ["0.1.0"]
  "CLI tool for managing and sandboxing coding agents."

  SUBCOMMANDS:
    runCmd';
    mcpServerCmd;
    prepareCmd;
    cleanupCmd
]

def main (args : List String) : IO UInt32 :=
  agentCmd.validate args
