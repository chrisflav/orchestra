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
    some (.assistant (.toolUse tool input))
  | "text" =>
    let part ← jVal json "part"
    let text := jStr part "text"
    if text.isEmpty then none else some (.assistant (.text text))
  | "step_finish" =>
    let part ← jVal json "part"
    let reason := jStr part "reason"
    if reason == "stop" then
      some (.result .success none none none "")
    else
      some (.unknown s!"step_finish({reason})")
  | other => some (.unknown other)


/-- The opencode coding agent backend. -/
def opencode : AgentDef where
  command := "opencode"
  sandboxPaths := {
    rox     := ["/usr", "/lib", "/lib64", "/bin", "/sbin", "/nix", "/run", "/run/current-system/sw/bin"]
    ro      := ["/etc", "/run", "/dev", "/proc", "/sys"]
    rw      := ["/dev/null"]
    homeRox := [".elan", ".cache", ".local"]
    homeRw  := [".config/opencode", ".gitconfig", ".local/share/opencode"]
    extraPorts := [4096]
  }

  setupMcp _ _ _ := do
    -- TODO: Implement MCP configuration for opencode
    return ("", #[])
  buildArgs _ _ _ model _ resume _ prompt := Id.run do
    let mut args : Array String := #[
      "run", "--format", "json",
      "--log-level", "ERROR", "--port", "4096"
    ]
    if let some m := model then
      args := args.push "--model" |>.push m
    if let some sid := resume then
      args := args.push "-s" |>.push sid
    -- opencode run takes the prompt as a positional argument
    args := args.push prompt
    return args
  parseOutputLine := parseOpencodeOutput
  extractSessionId _ := pure none
  cleanup _ := pure ()
  isUsageLimitError := stdUsageLimitError
  envVarsOfAuthSource _ := #[]

end Orchestra.AgentDef
