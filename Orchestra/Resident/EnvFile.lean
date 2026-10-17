import Orchestra.Dirs
import Orchestra.Exec.Backend

/-!
# A session's env file

How `refresh_kleis_token` reaches the commands a running agent starts
(`docs/design/resident-sessions.md` §7.3). The agent's own environment cannot be changed from
outside, but Claude Code's Bash tool runs every command in a shell that first evaluates the file
`CLAUDE_ENV_FILE` names — and, as verified against the CLI in headless stream-json mode (§15.1),
it reads that file's *content* once, when the session starts, and replays it before each command.
A file rewritten mid-session is therefore not seen; a file that *sources another one* is, because
the `.` is replayed and reads the other file again every time.

So there are two files, both per session:

* the **stub**, which `CLAUDE_ENV_FILE` names, written before each process starts and never
  changed while it runs: `if [ -r <env> ]; then . <env>; fi`;
* the **env file** it sources: empty (a comment) while the process's own token is current, and
  holding `export HTTPS_PROXY=…` for a replacement token after `refresh_kleis_token`. It is
  emptied again at the cycle that puts the process itself on a new token, before the replaced one
  is revoked, so it never names a revoked token.

Where they live: in a directory nothing else shares. On a backend whose runs share this machine's
`$HOME` (landrun, local) that is `<data>/session-env/<session>/`, granted read-only to the run; on
one that gives each run a home of its own (Kubernetes) it is `~/.orchestra/env/` in that home,
written with `Session.putHomeFile`.
-/

namespace Orchestra.Resident.EnvFile

/-- `s` quoted for a POSIX shell, whatever it holds. -/
def shellQuote (s : String) : String :=
  "'" ++ s.replace "'" "'\\''" ++ "'"

/-- The variables that carry the kleis token, and so the only ones the env file sets: the rest of
    a launch's environment — the CA, the setup's placeholders — is the same for a new token, and
    re-exporting it would undo `Sandbox.specFor`'s rule that the agent's own variables win over a
    setup's. -/
def tokenVars : List String := ["HTTPS_PROXY", "https_proxy", "HTTP_PROXY", "http_proxy"]

/-- The env file for a replacement token, from the environment a launch would have used. -/
def contents (env : Array (String × String)) : String :=
  let lines := env.toList.filter (fun (k, _) => tokenVars.contains k)
    |>.map fun (k, v) => s!"export {k}={shellQuote v}"
  "# Written by orchestra (refresh_kleis_token): the kleis token for commands from now on.\n" ++
    "\n".intercalate lines ++ "\n"

/-- The env file while the process's own token is current: nothing to override. -/
def emptyContents : String :=
  "# Written by orchestra: nothing to override; the process's own kleis token is current.\n"

/-- The stub `CLAUDE_ENV_FILE` names: sources `envPath` if it is there. -/
def stubContents (envPath : String) : String :=
  s!"# Written by orchestra. Replayed before each command; reads the env file afresh each time.\n\
if [ -r {shellQuote envPath} ]; then . {shellQuote envPath}; fi\n"

/-- Where one session's two files are, and how to write the env file. -/
structure Location where
  /-- The stub, as the agent sees it: what `CLAUDE_ENV_FILE` is set to. -/
  stubPath : String
  /-- The env file, as the agent sees it. -/
  envPath : String
  /-- Write the stub (once per process, before it starts). -/
  writeStub : IO Unit
  /-- Replace the env file's contents. -/
  writeEnv : String → IO Unit
  /-- What the run must be granted to read them, on a backend that grants paths. -/
  grant : Option Exec.PathGrant

/-- The directory a session's files live in on this machine. -/
def hostDir (sessionId : String) : IO System.FilePath :=
  return (← Dirs.dataBase) / "session-env" / sessionId

/-- Home-relative directory of the files on a backend with a home per run. -/
def homeRel : String := ".orchestra/env"

/-- Where session `sessionId`'s files go in `session`. -/
def locate (session : Exec.Session) (sessionId : String) : IO Location := do
  match session.agentHome with
  | some home =>
    let stubRel := s!"{homeRel}/stub.sh"
    let envRel := s!"{homeRel}/kleis.sh"
    let envPath := s!"{home}/{envRel}"
    return {
      stubPath := s!"{home}/{stubRel}", envPath
      writeStub := session.putHomeFile stubRel "0600" (stubContents envPath)
      writeEnv := fun c => session.putHomeFile envRel "0600" c
      grant := none }
  | none =>
    let dir ← hostDir sessionId
    let envPath := (dir / "kleis.sh").toString
    let write (name content : String) : IO Unit := do
      IO.FS.createDirAll dir
      IO.Prim.setAccessRights dir 0o700
      Exec.writeUnderHome dir name 0o600 content
    return {
      stubPath := (dir / "stub.sh").toString, envPath
      writeStub := write "stub.sh" (stubContents envPath)
      writeEnv := write "kleis.sh"
      grant := some { path := dir.toString, access := .ro, from_ := .orchestra } }

/-- Remove session `sessionId`'s files from this machine, if they are here. -/
def removeHost (sessionId : String) : IO Unit := do
  let dir ← hostDir sessionId
  try if ← dir.pathExists then IO.FS.removeDirAll dir catch _ => pure ()

end Orchestra.Resident.EnvFile
