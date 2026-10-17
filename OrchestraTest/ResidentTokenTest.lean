import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Interactive

/-!
# Resident sessions: tokens, the env file, and what a restart keeps

Phase 3 of `docs/design/resident-sessions.md`, checked without an agent: when a running resident is
cycled (§7, §8), the `token_refresh_margin` setting, the env file `refresh_kleis_token` writes and
the stub `CLAUDE_ENV_FILE` names — sourced by a real shell, as the CLI's command shell does — the
tool's place in the MCP server's list, the delivery loop's runtime surviving a restart, and a
repository-less resident waking without a GitHub App key.
-/

namespace OrchestraTest.ResidentTokens

private def mentions (hay needle : String) : Bool := (hay.splitOn needle).length > 1

private def parseApp (extra : String) : Except String AppConfig :=
  Json.parse s!"\{\"github_app\": \{\"app_id\": 1, \"private_key_path\": \"/k\"}{extra}}" >>=
    FromJson.fromJson?

private def settings : Identity.ResidentSettings :=
  (({} : Identity.ResidentSpec).settings {})

/-! ## When to cycle -/

@[test]
def aRunningResidentIsCycledForANewDayARefreshOrAnExpiringToken : Test := do
  let now : Int := 1_000_000
  let margin := 7200
  TestM.assertEqual (Resident.cycleReason now (some (now + 10_000)) margin false "d" "d") none
    (msg := "a token with hours to go, the same day, no refresh: handed over as it is")
  TestM.assertEqual (Resident.cycleReason now none margin false "d" "d") none
    (msg := "no token (no kleis): nothing to cycle for")
  let near := Resident.cycleReason now (some (now + 3600)) margin false "d" "d"
  TestM.assert (near.isSome && mentions (near.getD "") "token_refresh_margin")
    (msg := s!"an hour left, within a two-hour margin: cycled, and says why: {near}")
  let gone := Resident.cycleReason now (some (now - 5)) margin false "d" "d"
  TestM.assert (mentions (gone.getD "") "expired") (msg := s!"an expired one too: {gone}")
  TestM.assertEqual (Resident.cycleReason now (some (now + 7201)) margin false "d" "d") none
    (msg := "just outside the margin is not within it")
  let refreshed := Resident.cycleReason now (some (now + 10_000)) margin true "d" "d"
  TestM.assert (mentions (refreshed.getD "") "refresh_kleis_token")
    (msg := s!"a refresh during the last turn cycles it: {refreshed}")
  let day := Resident.cycleReason now (some (now + 10_000)) margin false "d1" "d2"
  TestM.assert (mentions (day.getD "") "budget day") (msg := s!"a new budget day: {day}")

@[test]
def theTokenRefreshMarginIsReadAsASpan : Test := do
  match parseApp "" with
  | .error e => TestM.fail e
  | .ok cfg => TestM.assertEqual cfg.residents.tokenRefreshMarginSeconds 7200
                 (msg := "two hours unless configured")
  for (written, secs) in [("\"2h\"", 7200), ("\"90m\"", 5400), ("600", 600), ("\"45s\"", 45),
                          ("\"1d\"", 86400)] do
    match parseApp s!", \"residents\": \{\"token_refresh_margin\": {written}}" with
    | .error e => TestM.fail s!"{written}: {e}"
    | .ok cfg => TestM.assertEqual cfg.residents.tokenRefreshMarginSeconds secs (msg := written)
  match parseApp ", \"residents\": {\"token_refresh_margin_seconds\": 30}" with
  | .error e => TestM.fail e
  | .ok cfg => TestM.assertEqual cfg.residents.tokenRefreshMarginSeconds 30
  match parseApp ", \"residents\": {\"token_refresh_margin\": \"soon\"}" with
  | .ok _ => TestM.fail "a margin that is not a span must not fall back to the default"
  | .error e => TestM.assert (mentions e "token_refresh_margin") (msg := s!"names the field: {e}")

/-! ## The env file -/

private def proxyEnv (token : String) : Array (String × String) :=
  let url := s!"http://orchestra:{token}@kleis:8080"
  #[("HTTPS_PROXY", url), ("https_proxy", url), ("HTTP_PROXY", url), ("http_proxy", url),
    ("SSL_CERT_FILE", "/ca.pem"), ("ANTHROPIC_BASE_URL", "https://elsewhere.example")]

@[test]
def theEnvFileSetsOnlyTheTokensVariablesQuoted : Test := do
  let c := Resident.EnvFile.contents (proxyEnv "it's")
  TestM.assert (mentions c "export HTTPS_PROXY='http://orchestra:it'\\''s@kleis:8080'")
    (msg := s!"quoted for the shell, whatever the token holds: {c}")
  TestM.assert (mentions c "export http_proxy=") (msg := "both spellings")
  TestM.assert (!mentions c "SSL_CERT_FILE" && !mentions c "ANTHROPIC_BASE_URL")
    (msg := "nothing but the token's variables: the agent's own environment wins")
  TestM.assert (!mentions Resident.EnvFile.emptyContents "export")
    (msg := "the empty file sets nothing")

/-- What `sh` prints for `$HTTPS_PROXY` after sourcing `stub`, as the CLI's command shell does
    before each command. -/
private def proxyAfterSourcing (stub : String) : IO String := do
  let out ← IO.Process.output { cmd := "/bin/sh", args := #["-c", s!". '{stub}'; printf %s \"$HTTPS_PROXY\""],
                                env := #[("HTTPS_PROXY", some "http://orchestra:own@kleis:8080")] }
  return out.stdout

@[test]
def theStubReadsTheEnvFileAfreshEachTime : Test := do
  -- The CLI keeps the stub's content from start-up and replays it before every command (§15.1),
  -- so what has to change between commands is the file the stub sources.
  let (before, refreshed, emptied, loc, gone) ← withTempData "resident-envfile" do
    let loc ← Resident.EnvFile.locate Exec.Landrun.session "resident-secretary"
    loc.writeStub
    loc.writeEnv Resident.EnvFile.emptyContents
    let before ← proxyAfterSourcing loc.stubPath
    loc.writeEnv (Resident.EnvFile.contents (proxyEnv "new"))
    let refreshed ← proxyAfterSourcing loc.stubPath
    loc.writeEnv Resident.EnvFile.emptyContents
    let emptied ← proxyAfterSourcing loc.stubPath
    let dir ← Resident.EnvFile.hostDir "resident-secretary"
    let mode := (← IO.Process.output { cmd := "stat", args := #["-c", "%a", loc.envPath] }).stdout.trimAscii.toString
    Resident.EnvFile.removeHost "resident-secretary"
    pure (before, refreshed, emptied, (loc, mode, dir), ← dir.pathExists)
  let (loc, md, dir) := loc
  TestM.assertEqual before "http://orchestra:own@kleis:8080"
    (msg := "an empty env file leaves the process's own token")
  TestM.assertEqual refreshed "http://orchestra:new@kleis:8080"
    (msg := "a rewritten env file is seen by the next command")
  TestM.assertEqual emptied "http://orchestra:own@kleis:8080"
    (msg := "and emptied again, it names no token at all")
  TestM.assert (loc.stubPath.startsWith dir.toString && mentions loc.stubPath "session-env")
    (msg := s!"under the daemon's data, not the shared home: {loc.stubPath}")
  TestM.assertEqual md "600" (msg := "readable by its owner alone")
  TestM.assertEqual (loc.grant.map (·.path)) (some dir.toString)
    (msg := "the run is granted the directory, read-only")
  TestM.assert ((loc.grant.map (·.access == .ro)).getD false)
  TestM.assert (!gone) (msg := "removed with the environment")

@[test]
def onABackendWithAHomeOfItsOwnTheFilesGoThere : Test := do
  let written ← IO.mkRef (#[] : Array (String × String × String))
  let session : Exec.Session := { Exec.Landrun.session with
    agentHome := some "/workspace/home"
    putHomeFile := fun rel mode content => written.modify (·.push (rel, mode, content)) }
  let loc ← Resident.EnvFile.locate session "resident-secretary"
  loc.writeStub
  loc.writeEnv "x"
  let w ← written.get
  TestM.assertEqual loc.stubPath "/workspace/home/.orchestra/env/stub.sh"
    (msg := "the path the agent sees, in its own home")
  TestM.assertEqual (w.map (·.1)).toList [".orchestra/env/stub.sh", ".orchestra/env/kleis.sh"]
    (msg := "written through putHomeFile, relative to that home")
  TestM.assert (w.all (·.2.1 == "0600")) (msg := "private")
  TestM.assert (mentions ((w[0]?.map (·.2.2)).getD "") "/workspace/home/.orchestra/env/kleis.sh")
    (msg := "the stub sources the env file by its path in the pod")
  TestM.assert loc.grant.isNone (msg := "nothing to grant: the home is the run's")

/-! ## The tool -/

private def residentState (kleis : Option KleisConfig) : Server.State :=
  { repo := none, allowedTools := [], appId := 0, privateKeyPath := "", installationId := none
    pat := "", kleis
    residentTools := some ("secretary", fun _ _ => pure (.ok "")) }

private def offered (st : Server.State) : List String :=
  match (Server.toolsList st).getObjVal? "tools" |>.toOption |>.bind (·.getArr? |>.toOption) with
  | none => []
  | some defs => defs.toList.filterMap (·.getObjValAs? String "name" |>.toOption)

@[test]
def refreshKleisTokenIsOfferedToAResidentBehindKleis : Test := do
  let kc : KleisConfig := { url := "http://kleis:8180" }
  TestM.assert ((offered (residentState (some kc))).contains "refresh_kleis_token")
    (msg := "a resident behind kleis is offered it")
  TestM.assert (!(offered (residentState none)).contains "refresh_kleis_token")
    (msg := "without kleis there is no token to refresh")
  TestM.assert (!(offered { residentState (some kc) with residentTools := none }).contains
      "refresh_kleis_token")
    (msg := "nor is anything that is not a resident")
  match Server.parseToolCall "refresh_kleis_token" (Json.mkObj []) with
  | .resident "refresh_kleis_token" _ => TestM.assert true
  | _ => TestM.fail "the call reaches the resident's own tools"
  TestM.assert (Server.residentToolNames.contains "refresh_kleis_token")

/-! ## What a restart keeps -/

@[test]
def theDeliveryLoopsRuntimeSurvivesARestart : Test := do
  let (loaded, failures, cleared) ← withTempData "resident-runtime" do
    let now ← Usage.nowEpoch
    Resident.saveRuntime "secretary"
      { failures := 2, retryAfter := now + 60, turnStamps := #[now - 7200, now - 10, now]
        tokenExpires := some (now + 3600) } now
    let loaded ← Resident.loadRuntime "secretary"
    -- A new manager is a restarted daemon: it reads what the last one recorded.
    let mgr ← Manager.new {} (fun _ => pure (some 0)) (fun _ _ => pure ())
    let _ ← ensureResidentSession "secretary" settings
    mgr.setConfigured #[("secretary", settings)]
    let failures ← mgr.residentFailures "secretary"
    -- Resuming forgets the failures, in the database too.
    let _ ← mgr.resumeResident "secretary"
    pure (loaded, failures, ← Resident.loadRuntime "secretary")
  TestM.assertEqual loaded.failures 2
  TestM.assertEqual loaded.turnStamps.size 2 (msg := "a stamp older than an hour is dropped")
  TestM.assert loaded.tokenExpires.isSome
  TestM.assertEqual failures.1 2 (msg := "a restarted daemon still knows it failed twice")
  TestM.assert (failures.2 > 0) (msg := "and when it may try again")
  TestM.assertEqual cleared.failures 0 (msg := "resumed: forgotten, durably")
  TestM.assertEqual cleared.retryAfter 0
  TestM.assert ((← withTempData "resident-runtime-none" (Resident.loadRuntime "nobody")) == ({} : Resident.Runtime))
    (msg := "nothing recorded reads as nothing")

@[test]
def watchesSurviveAReset : Test := do
  let mgr ← Manager.new {} (fun _ => pure (some 0)) (fun _ _ => pure ())
  let (before, after) ← withTempData "resident-reset-watches" do
    let _ ← ensureResidentSession "secretary" settings
    mgr.setConfigured #[("secretary", settings)]
    let _ ← Resident.addWatch "secretary" "zulip" { stream := "ops", topic := some "deploys" }
      (some "until the release") none
    let before ← Resident.listWatches "secretary"
    let _ ← mgr.resetResident "secretary"
    pure (before, ← Resident.listWatches "secretary")
  TestM.assertEqual before.size 1
  TestM.assertEqual after.size 1
    (msg := "a reset ends the conversation, not what the resident asked to be told about")

/-! ## Waking without a repository -/

private def withIdentities (act : IO α) : IO α := do
  let root : System.FilePath :=
    System.FilePath.mk "/tmp" / s!"orchestra-resident-token-test-{← IO.monoNanosNow}"
  IO.FS.createDirAll root
  let previous ← Identity.identitiesDirOverride.get
  Identity.setIdentitiesDirOverride (some root)
  try act
  finally
    Identity.setIdentitiesDirOverride previous
    try IO.FS.removeDirAll root catch _ => pure ()

@[test]
def aResidentWakesWithoutReadingTheGitHubAppKey : Test := do
  -- Before phase 3 a repository-less session signed a JWT on every wake, and a daemon whose App
  -- key could not be read could wake no resident at all. Behind kleis it signs none: what fails
  -- here is kleis (nobody listens on port 9), not the key.
  let mgr ← Manager.new {} (fun _ => pure (some 0)) (fun _ _ => pure ())
  let cfg : AppConfig := { appId := 1, privateKeyPath := "/nonexistent/orchestra-app-key.pem"
                           kleis := some { url := "http://127.0.0.1:9", issuerToken := "t" } }
  let outcome ← withIdentities <| withTempData "resident-nokey" do
    let dir := (← Identity.identitiesDir) / "secretary"
    IO.FS.createDirAll dir
    IO.FS.writeFile (dir / "identity.json") "{\"name\": \"secretary\", \"resident\": {}}"
    let _ ← ensureResidentSession "secretary" settings
    mgr.setConfigured #[("secretary", settings)]
    let _ ← Resident.insert "secretary" "human" "h" true (Json.mkObj [("text", "hi")])
    mgr.deliver cfg "secretary" (urgent := true)
  match outcome with
  | .failed why =>
    TestM.assert (!mentions why "orchestra-app-key" && !mentions why "JWT" && mentions why "connect")
      (msg := s!"the App key is not what a resident's wake depends on: {why}")
  | other => TestM.fail s!"expected the wake to fail at kleis, got {repr other}"

/-! ## The migration -/

@[test]
def theRuntimeMigrationIsTheLatest : Test := do
  let names := Store.migrations.map (·.name)
  TestM.assertEqual ((names.drop 7).head?) (some "0008_resident_runtime")

end OrchestraTest.ResidentTokens
