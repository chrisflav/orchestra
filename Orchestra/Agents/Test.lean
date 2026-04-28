import Orchestra.AgentDef
import Orchestra.StreamFormat
import Lean.Data.Json

open Lean (Json)

namespace Orchestra.AgentDef

/-- A deterministic test agent for verifying sandbox and MCP setup. -/
def testAgent : AgentDef where
  command := "python3"
  sandboxPaths := {
    rox     := ["/usr", "/lib", "/lib64", "/bin", "/sbin", "/nix"]
    ro      := ["/etc", "/run", "/dev", "/proc", "/sys"]
    rw      := ["/tmp"]
    homeRox := [".elan", ".cache", ".local"]
    homeRw  := [".test"]
  }
  setupMcp port _ _ := do
    -- Configure a dummy MCP server using nc
    let mcpConfig := Json.mkObj [("mcpServers", Json.mkObj [
      ("agent", Json.mkObj [
        ("command", .str "nc"),
        ("args", .arr #[.str "127.0.0.1", .str (toString port)])
      ])
    ])]
    let ts ← IO.monoNanosNow
    let path := s!"/tmp/test-agent-mcp-{ts}.json"
    IO.FS.writeFile (System.FilePath.mk path) mcpConfig.compress
    return (path, #[])
  buildArgs mcpConfigPath pluginDirs subAgent model systemPrompt resume budget prompt := Id.run do
    -- Run the test agent script
    return #["/home/agent/.agent/repos/chrisflav-agents/orchestra/Orchestra/Agents/TestAgent.py"]
  parseOutputLine := StreamFormat.parseEvent
  extractSessionId _ := pure none
  cleanup path := try IO.FS.removeFile (System.FilePath.mk path) catch _ => pure ()
  isUsageLimitError := stdUsageLimitError
  envVarsOfAuthSource _ := #[]

end Orchestra.AgentDef
