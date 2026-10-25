import Orchestra.AgentDef
import Orchestra.StreamFormat
import Lean.Data.Json

open Lean (Json)
open Orchestra.StreamFormat

namespace Orchestra.AgentDef

/-- Helpers for parsing JSON. -/
private def jStr (j : Json) (key : String) : String :=
  j.getObjValAs? String key |>.toOption |>.getD ""

private def jVal (j : Json) (key : String) : Option Json :=
  j.getObjVal? key |>.toOption

/-- Parse one line of opencode's JSON output into an Event. -/
def parseOpencodeOutput (line : String) : Option Event := do
  let json ← (Json.parse line.trimAscii.toString).toOption
  match jStr json "type" with
  | "step_start" =>
    some (.init (jStr json "sessionID") "opencode")
  | "tool_use" =>
    let part ← jVal json "part"
    let tool := jStr part "tool"
    let state := jVal part "state" |>.getD (Json.mkObj [])
    let input := jVal state "input" |>.getD (Json.mkObj [])
    -- No call id: opencode's part carries no field this can be read from, so the pairing
    -- between a call and its result is unknown rather than invented.
    some (.assistant (.toolUse tool input none))
  | "reasoning" =>
    let part ← jVal json "part"
    let text := jStr part "text"
    if text.isEmpty then none else some (.assistant (.thinking text))
  | "text" =>
    let part ← jVal json "part"
    let reasoning := jStr part "reasoning_content"
    if !reasoning.isEmpty then
      some (.assistant (.thinking reasoning))
    else
      let text := jStr part "text"
      if text.isEmpty then none else some (.assistant (.text text))
  | "step_finish" =>
    let part ← jVal json "part"
    let reason := jStr part "reason"
    if reason == "stop" then
      some (.result .success none none none "")
    else
      none
  | other => some (.unknown other)

/-- The opencode coding agent backend. -/
def opencode : AgentDef where
  command := "opencode"
  sandboxPaths := {
    rox     := ["/usr", "/lib", "/lib64", "/bin", "/sbin", "/nix", "/run", "/run/current-system/sw/bin"]
    -- Not `/home`, which this list used to grant read-only for opencode's sake: it is every user's
    -- home, orchestra's data and configuration directories among them, and any `~/.ssh`. The
    -- sandbox would refuse it now anyway (`Landrun.withoutGuarded`); an opencode that needs
    -- something there needs it named.
    ro      := ["/etc", "/run", "/dev", "/proc", "/sys"]
    rw      := ["/dev/null"]
    -- `~/.local/bin` and `~/.local/lib` (an npm prefix of `~/.local`), and the per-user install
    -- opencode's own script makes. Never `.local` whole: it holds orchestra's own data directory.
    -- Git's global configuration is read, never written: the daemon runs `git` as the same user,
    -- outside the sandbox, so an agent that could set `core.hooksPath` or a `url.*.insteadOf`
    -- there would run code, or redirect a fetch, on the daemon's next clone or checkout. And no
    -- `~/.config/gh`: its `hosts.yml` is the operator's own GitHub login, and the agent's `gh`
    -- authenticates with the `GH_TOKEN` orchestra gives it.
    homeRox := [".local/bin", ".local/lib", ".opencode", ".gitconfig", ".config/git"]
    homeRw  := [".config/opencode", ".local/share/opencode"]
    homeRwx := [".elan", ".cache"]
    extraPorts := [4096]
  }
  setupMcp mcp _ _ := do
    let (cmd, cmdArgs) := mcp.stdioCommand
    let mcpConfig := Json.mkObj [("mcp", Json.mkObj [
      ("agent", Json.mkObj [
        ("type", .str "local"),
        ("command", .arr ((#[cmd] ++ cmdArgs).map Json.str)),
        ("enabled", .bool true)
      ])
    ])]
    match ← IO.getEnv "HOME" with
    | none => return ("", #[], #[])
    | some home =>
      let configDir := System.FilePath.mk home / ".config" / "opencode"
      IO.FS.createDirAll configDir
      let configPath := configDir / "opencode.json"
      IO.FS.writeFile configPath mcpConfig.compress
      -- Home-scoped, not the absolute path just written: this is the daemon's `$HOME`, and the
      -- agent reads it from its own.
      return (configPath.toString, #[],
        #[{ path := ".config/opencode/opencode.json", access := .rw, scope := .home
          , from_ := .orchestra }])
  buildArgs _ctx _pluginDirs _subAgent model _systemPrompt resume _budget prompt := Id.run do
    let mut args : Array String := #[
      "run", "--format", "json",
      "--log-level", "ERROR", "--port", "4096",
      "--thinking", "--dangerously-skip-permissions"
    ]
    if let some m := model then
      args := args.push "--model" |>.push m
    if let some sid := resume then
      args := args.push "-s" |>.push sid
    -- opencode run takes the prompt as a positional argument
    args := args.push prompt
    return args
  buildInteractiveArgs _ctx _pluginDirs _subAgent model _systemPrompt resume _budget := Id.run do
    let mut args : Array String := #[
      "run", "--port", "4096", "--thinking", "--dangerously-skip-permissions"
    ]
    if let some m := model then
      args := args.push "--model" |>.push m
    if let some sid := resume then
      args := args.push "-s" |>.push sid
    return args
  -- Two fixed resources per run: the hard-coded port 4096 that `buildArgs` passes to
  -- `opencode run`, and the config file `~/.config/opencode/opencode.json` that `setupMcp`
  -- overwrites with this task's MCP port. A second concurrent run either fails to bind the
  -- port or attaches to the first run's server and executes inside its session.
  parallelSafe := false
  parseOutputLine := fun line => StreamFormat.one (parseOpencodeOutput line)
  extractSessionId _ := pure none
  cleanup path := try IO.FS.removeFile (System.FilePath.mk path) catch _ => pure ()
  isUsageLimitError := stdUsageLimitError
  envVarsOfAuthSource _ := #[]

end Orchestra.AgentDef
