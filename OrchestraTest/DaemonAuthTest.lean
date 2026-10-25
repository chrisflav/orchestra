import OrchestraTest.TestM
import Orchestra

open Lean (Json)
open Orchestra

/-!
# The daemon's control socket, and what may be asked of it

Landlock does not mediate `connect(2)` on a pathname Unix socket, so every agent of the daemon's
user could reach the queue daemon's control socket and ask it for anything: queue a task, start a
concert, shut down, claim an issue. And a queued task's `config_path` decides which execution
backend confines it, so asking for `"execution": {"backend": "local"}` in a config written to
`/tmp` was a way out of the sandbox.

Three things are pinned here:

* the socket refuses a request without the daemon's token, or with the wrong one, before reading
  anything else of it (`DaemonRequest.gate`), and a client adds the token from the file only the
  daemon's user can read (`DaemonRequest.withToken`) — checked over a real Unix socket too;
* a `config_path` is refused unless it lies in the configuration directory, symlinks and `..`
  resolved, and sandboxes agents exactly as the daemon's own config does (`loadEntryConfig`);
* the CLI's proof that it talks to orchestrad (`Secret.daemonProof`) is an HMAC that depends on
  both the token and the nonce.
-/

namespace OrchestraTest.DaemonAuth

/-- Run `act` with `Dirs.configBase` pointed at a fresh directory, removed afterwards. -/
private def withConfigDir (label : String) (act : System.FilePath → IO α) : IO α := do
  let root := System.FilePath.mk "/tmp" / s!"orchestra-{label}-{← IO.monoNanosNow}"
  let config := root / "config"
  IO.FS.createDirAll config
  let previous ← Dirs.configBaseOverride.get
  Dirs.setConfigBaseOverride (some config)
  try act root
  finally
    Dirs.setConfigBaseOverride previous
    try IO.FS.removeDirAll root catch _ => pure ()

private def shutdownRequest : Json := Json.mkObj [("type", "shutdown")]

/-! ## The token -/

@[test]
def aRequestWithoutTheTokenIsRefused : Test := do
  let expected := "0123456789abcdef"
  let refused (j : Json) : Bool := match DaemonRequest.gate expected j.compress with
    | .error e => e.startsWith "unauthenticated"
    | .ok _    => false
  TestM.assert (refused shutdownRequest) "no token: refused"
  TestM.assert (refused (shutdownRequest.setObjVal! "token" "0123456789abcdeX"))
    "the wrong token: refused"
  TestM.assert (refused (shutdownRequest.setObjVal! "token" "0123456789abcde"))
    "a prefix of the token: refused"
  TestM.assert (refused (shutdownRequest.setObjVal! "token" (Json.num 1))) "not a string: refused"
  TestM.assert (refused (Json.mkObj [("type", "no_such_request")]))
    "an unknown request without a token is refused as unauthenticated, not parsed"
  match DaemonRequest.gate expected (shutdownRequest.setObjVal! "token" expected).compress with
  | .ok (.shutdown false false) => TestM.assert true
  | _ => TestM.fail "the right token lets the request through"
  match DaemonRequest.gate "" (shutdownRequest.setObjVal! "token" "").compress with
  | .error _ => TestM.assert true
  | .ok _ => TestM.fail "a daemon with no token refuses everything, an empty one included"

@[test]
def theTokenIsPrivateAndKeptAcrossStarts : Test := do
  let (t1, t2, mode, presented) ← withConfigDir "daemon-token" fun _ => do
    let t1 ← Secret.ensureDaemonToken
    let t2 ← Secret.ensureDaemonToken
    let path ← Secret.daemonTokenFile
    let out ← IO.Process.output { cmd := "stat", args := #["-c", "%a", path.toString] }
    let req ← DaemonRequest.withToken shutdownRequest
    return (t1, t2, out.stdout.trimAscii.toString, (req.getObjValAs? String "token").toOption)
  TestM.assertEqual t1.length 64 (msg := "32 random bytes, hex")
  TestM.assertEqual t1 t2 (msg := "a second start reuses the token rather than minting another")
  TestM.assertEqual mode "600" (msg := "only the daemon's user can read it")
  TestM.assertEqual presented (some t1) (msg := "a client presents what the file holds")

@[test]
def aClientWithoutTheTokenFileSaysWhere : Test := do
  let msg ← withConfigDir "daemon-token-missing" fun _ => do
    try
      discard <| DaemonRequest.withToken shutdownRequest
      return none
    catch e => return some (toString e)
  match msg with
  | some m => TestM.assert (m.contains "daemon.token") "the error names the file"
  | none => TestM.fail "a client with no token to present refuses to send"

/-- The gate over a real socket: a server that answers each line with what `gate` decided, and
    clients with and without the token. -/
@[test]
def theSocketRefusesUnauthenticatedPeers : Test := do
  let answers ← withConfigDir "daemon-socket" fun root => do
    let token ← Secret.ensureDaemonToken
    let path := root / "daemon.sock"
    let server ← Utils.UnixSocket.Server.listen path
    let serverTask ← IO.asTask (prio := .dedicated) do
      for _ in [0:3] do
        let conn ← server.accept
        let line ← conn.recvLine
        let reply := match DaemonRequest.gate token line with
          | .ok _    => "accepted"
          | .error e => e
        conn.sendLine reply
        conn.close
    let ask (req : Json) : IO String := do
      let conn ← Utils.UnixSocket.Connection.connect path
      conn.sendLine req.compress
      let r ← conn.recvLine
      conn.close
      return r
    let anonymous ← ask shutdownRequest
    let wrong ← ask (shutdownRequest.setObjVal! "token" (Json.str (String.ofList (List.replicate 64 'a'))))
    let right ← ask (← DaemonRequest.withToken shutdownRequest)
    discard <| IO.wait serverTask
    server.close
    return (anonymous, wrong, right)
  TestM.assert (answers.1.startsWith "unauthenticated") "a peer without the token is refused"
  TestM.assert (answers.2.1.startsWith "unauthenticated") "so is one with the wrong token"
  TestM.assertEqual answers.2.2 "accepted" (msg := "a client that read the file gets through")

/-! ## `config_path` -/

private def daemonConfig : AppConfig := { appId := 0, privateKeyPath := "" }

private def configJson (backend : String) (extra : String := "") : String :=
  s!"\{\"github_app\": \{\"app_id\": 0, \"private_key_path\": \"\"}, \
\"execution\": \{\"backend\": \"{backend}\"}{extra}}"

private def refusal (cfg : AppConfig) (path : String) : IO (Option String) := do
  try
    discard <| loadEntryConfig cfg path
    return none
  catch e => return some (toString e)

@[test]
def aConfigOutsideTheConfigurationDirectoryIsRefused : Test := do
  let (outside, viaLink, viaDots, inside) ← withConfigDir "entry-config" fun root => do
    let config := root / "config"
    IO.FS.writeFile (root / "evil.json") (configJson "local")
    IO.FS.writeFile (config / "other.json") (configJson "landrun")
    discard <| IO.Process.output { cmd := "ln", args := #["-s", (root / "evil.json").toString,
      (config / "link.json").toString] }
    return (← refusal daemonConfig (root / "evil.json").toString,
            ← refusal daemonConfig (config / "link.json").toString,
            ← refusal daemonConfig (config / ".." / "evil.json").toString,
            ← refusal daemonConfig (config / "other.json").toString)
  TestM.assert ((outside.getD "").contains "outside") "a config in /tmp is refused"
  TestM.assert ((viaLink.getD "").contains "outside")
    "a symlink in the configuration directory pointing out of it is refused"
  TestM.assert ((viaDots.getD "").contains "outside") "so is a path that walks out with `..`"
  TestM.assertEqual inside none
    (msg := "a config in the directory that sandboxes agents as the daemon does is used")

@[test]
def aConfigThatWeakensTheSandboxIsRefused : Test := do
  let (local_, paths, plugins) ← withConfigDir "entry-config-sec" fun root => do
    let config := root / "config"
    IO.FS.writeFile (config / "local.json") (configJson "local")
    IO.FS.writeFile (config / "paths.json")
      (configJson "landrun" ", \"additional_sandbox_paths\": {\"rw\": [\"/\"]}")
    IO.FS.writeFile (config / "plugins.json")
      (configJson "landrun" ", \"plugin_dirs\": [\"/home\"]")
    return (← refusal daemonConfig (config / "local.json").toString,
            ← refusal daemonConfig (config / "paths.json").toString,
            ← refusal daemonConfig (config / "plugins.json").toString)
  TestM.assert ((local_.getD "").contains "execution")
    "another execution backend is refused, naming the setting"
  TestM.assert ((paths.getD "").contains "additional_sandbox_paths") "so are more sandbox paths"
  TestM.assert ((plugins.getD "").contains "plugin_dirs") "and other plugin directories"

/-! ## The CLI's proof -/

@[test]
def theProofDependsOnTheTokenAndTheNonce : Test := do
  let p := Secret.daemonProof "token-a" "00112233445566778899aabbccddeeff"
  TestM.assertEqual p.length 64 (msg := "HMAC-SHA256, hex")
  TestM.assert (p != Secret.daemonProof "token-b" "00112233445566778899aabbccddeeff")
    "another token answers differently"
  TestM.assert (p != Secret.daemonProof "token-a" "00112233445566778899aabbccddeefe")
    "another nonce is answered differently"
  TestM.assert (!p.contains "token-a") "the token does not appear in the proof"

/-- The whole exchange: the dashboard answers the proof under the token it can read, a CLI that can
    read the same token accepts it, and a server on loopback that is not orchestrad — here a plain
    `python3 -m http.server` — is never sent the password. -/
@[test]
def theCliSendsThePasswordOnlyToOrchestrad : Test := do
  let (good, bad) ← withConfigDir "daemon-proof" fun root => do
    discard <| Secret.ensureDaemonToken
    let (port, stop) ← Dashboard.serve { password := "pw", port := 0 }
    let good ← try Client.verifyServer { baseUrl := s!"http://127.0.0.1:{port}", token := "pw" }
      finally stop
    -- A port nothing in this test holds, for the impostor.
    let impostorPort := port.toNat + 1
    let impostor ← IO.Process.spawn {
      cmd := "python3", cwd := root, stdout := .null, stderr := .null, stdin := .null
      args := #["-I", "-m", "http.server", "--bind", "127.0.0.1", toString impostorPort] }
    let mut bad : Except String Unit := .ok ()
    try
      -- Until it listens: a refused connection is a different failure from the one under test.
      for _ in [0:50] do
        let r ← IO.Process.output { cmd := "curl", args := #["-s", "-o", "/dev/null",
          s!"http://127.0.0.1:{impostorPort}/"] }
        if r.exitCode == 0 then break
        IO.sleep 100
      bad ← Client.verifyServer { baseUrl := s!"http://127.0.0.1:{impostorPort}", token := "pw" }
    finally
      impostor.kill
      discard impostor.wait
    return (good, bad)
  TestM.assert (good matches .ok ()) s!"orchestrad proves itself"
  match bad with
  | .error e => TestM.assert (e.contains "Not sending the orchestra password") "the impostor is refused"
  | .ok () => TestM.fail "a server that cannot prove it is orchestrad is not sent the password"

end OrchestraTest.DaemonAuth
