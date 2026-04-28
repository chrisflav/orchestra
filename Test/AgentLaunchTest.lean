import Test.TestM
import Orchestra
import Orchestra.Sandbox

open Orchestra

@[test]
def agentLaunchTest : Test := do
  let repoPath := System.FilePath.mk "."
  let prompt := "Hello, agent!"
  let serverPort := 8080
  let ghToken := "dummy-token"
  
  let result ← Sandbox.launchAgent 
    AgentDef.testAgent 
    repoPath 
    prompt 
    serverPort 
    ghToken

  TestM.assert (result.exitCode == 0) (msg := "agent should exit with code 0")
