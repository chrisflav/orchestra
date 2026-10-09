import OrchestraTest.TestM
import Orchestra.Kleis
import Orchestra.Sandbox
import Orchestra.Server
import Orchestra.Agents.Claude

open Lean (Json FromJson)
open Orchestra
open Orchestra.Server

namespace OrchestraTest.Kleis

/-!
# Reaching GitHub through kleis

What can be checked without a proxy: the facts a task's token is asked to carry, the
environment a sandbox is given, that no GitHub token reaches it, which tools are withheld, and
how the `kleis` block of the configuration is read. The minting and revocation themselves are
HTTP calls to kleis, exercised by kleis's own integration test against the same endpoints.
-/

private def pair : RepoPair :=
  { upstream := { owner := "up", name := "proj" }, fork := { owner := "bot", name := "proj" } }

private def cfg : KleisConfig :=
  { url := "http://127.0.0.1:8080/", issuerToken := "issuer-secret" }

private def minted : Orchestra.Kleis.Minted :=
  { token := "En0K+abc/def=", revocationId := "abcd", expires := 0 }

/-- The `name`s of a list of facts. -/
private def names (facts : List Json) : List String :=
  facts.filterMap (·.getObjValAs? String "name" |>.toOption)

/-- The terms of the first fact with this name, rendered as JSON. -/
private def termsOf (facts : List Json) (name : String) : Option String :=
  facts.find? (fun f => (f.getObjValAs? String "name").toOption == some name)
    |>.bind (fun f => (f.getObjVal? "terms").toOption)
    |>.map (·.compress)

@[test]
def facts_nameTheTask : Test := do
  let facts := Orchestra.Kleis.TaskFacts.toFacts {
    taskId := "t1", repo := some pair, issueNumber := some 7
    tools := ["create_pr", "comment", "create_pr"], prLabels := ["orchestra"] }
  TestM.assert ((names facts).all (·.startsWith "task_"))
    "every fact is a task_ fact, the only prefix the issuer may use"
  TestM.assertEqual (termsOf facts "task_fork") (some "[\"bot\",\"proj\"]")
  TestM.assertEqual (termsOf facts "task_upstream") (some "[\"up\",\"proj\"]")
  TestM.assertEqual (termsOf facts "task_issue") (some "[7]")
  TestM.assertEqual ((names facts).filter (· == "task_tool")).length 2
  TestM.assert ((names facts).contains "task_writable") "a task that is not read-only may write"
  TestM.assertEqual (termsOf facts "task_pr_labels") (some "[[\"orchestra\"]]")
  TestM.assertEqual (termsOf facts "task_label_any") (some "[false]")

@[test]
def facts_readOnlyAndRepositoryIndependent : Test := do
  let facts := Orchestra.Kleis.TaskFacts.toFacts { taskId := "t2", readOnly := true }
  TestM.assert (!(names facts).contains "task_writable") "a read-only task may not write"
  TestM.assert (!(names facts).contains "task_fork") "no repository, no fork"
  -- Always present, possibly empty: the grant reads it to bound the labels a pull request gets.
  TestM.assertEqual (termsOf facts "task_pr_labels") (some "[[]]")

@[test]
def facts_triageAndRepositoryCreation : Test := do
  let facts := Orchestra.Kleis.TaskFacts.toFacts {
    taskId := "t3", tools := ["label_issue", "create_repository"], org := some "bot-org" }
  -- Stated either way, since the grant allows no label at all without it.
  TestM.assertEqual (termsOf facts "task_label_any") (some "[true]")
  TestM.assertEqual (termsOf facts "task_org") (some "[\"bot-org\"]")
  let p := Orchestra.Kleis.systemPrompt {
    taskId := "t3", tools := ["create_repository"], org := some "bot-org" }
  TestM.assert (p.contains "orgs/bot-org/repos") "the agent is told where it may create repositories"

@[test]
def launch_pointsEverythingAtTheProxy : Test := do
  let l := Orchestra.Kleis.launch cfg minted "/data/kleis/ca-bundle.pem"
  let get (k : String) := (l.env.find? (·.1 == k)).map (·.2)
  TestM.assertEqual (get "HTTPS_PROXY")
    (some "http://orchestra:En0K%2Babc%2Fdef%3D@127.0.0.1:8080")
  TestM.assertEqual (get "https_proxy") (get "HTTPS_PROXY")
  TestM.assertEqual (get "SSL_CERT_FILE") (some "/data/kleis/ca-bundle.pem")
  TestM.assertEqual (get "GIT_SSL_CAINFO") (some "/data/kleis/ca-bundle.pem")
  TestM.assertEqual (get "GH_TOKEN") (some "kleis")
  TestM.assertEqual l.port 8080
  TestM.assert (l.files.any fun g => g.path == "/data/kleis/ca-bundle.pem" && g.access == .ro)
    "the bundle is granted read-only"

@[test]
def launch_aConfiguredProxyAddressWins : Test := do
  let l := Orchestra.Kleis.launch { cfg with proxy := some "kleis.orchestra.svc:3128" } minted
    "/b.pem"
  TestM.assertEqual l.port 3128
  TestM.assert (l.env.any fun (k, v) => k == "HTTPS_PROXY" && v.endsWith "@kleis.orchestra.svc:3128")
    "the sandbox reaches the proxy where it was told to"

@[test]
def spec_carriesNoGitHubToken : Test := do
  let mcp : Exec.McpEndpoint := { host := "127.0.0.1", port := 4000 }
  let l := Orchestra.Kleis.launch cfg minted "/b.pem"
  let spec := Sandbox.specFor AgentDef.claude "/work" mcp "ghs_installation" #[] #[] #[] #[]
    false #[] {} (kleis := some l)
  TestM.assert (!spec.env.any fun (_, v) => v == "ghs_installation")
    "the installation token never reaches a sandbox behind the proxy"
  TestM.assert (spec.ports.connect.contains 8080) "the sandbox may connect to the proxy"
  TestM.assert (spec.grants.any (·.path == "/b.pem")) "and read the CA bundle"
  let plain := Sandbox.specFor AgentDef.claude "/work" mcp "ghs_installation" #[] #[] #[] #[]
    false #[] {}
  TestM.assert (plain.env.any fun (k, v) => k == "GH_TOKEN" && v == "ghs_installation")
    "without kleis the token is exported as before"

private def serverState (kleis : Option KleisConfig) : State :=
  { repo := some pair, allowedTools := ["create_pr", "comment", "merge_pr", "label_issue"]
    appId := 0, privateKeyPath := "", installationId := some 1, pat := "pat", kleis }

private def offered (st : State) : List String :=
  match (toolsList st).getObjVal? "tools" |>.toOption |>.bind (·.getArr? |>.toOption) with
  | none => []
  | some defs => defs.toList.filterMap (·.getObjValAs? String "name" |>.toOption)

@[test]
def server_withholdsTheGitHubTools : Test := do
  let behind := offered (serverState (some cfg))
  for tool in Server.kleisReplacedTools do
    TestM.assert (!behind.contains tool) s!"{tool} is not offered behind the proxy"
  let direct := offered (serverState none)
  for tool in ["create_pr", "comment", "refresh_token", "get_pr_comments"] do
    TestM.assert (direct.contains tool) s!"{tool} is still offered without kleis"

@[test]
def server_refusesThemByName : Test := do
  let result ← evalToolCall (serverState (some cfg)) (.createPr "t" "b" "branch" "" .upstream)
  TestM.assert (result.getObjValAs? Bool "isError" |>.toOption |>.getD false)
    "an agent naming create_pr anyway is turned away"
  let text := (result.getObjVal? "content" |>.toOption |>.bind (·.getArr? |>.toOption)
    |>.bind (·[0]?) |>.bind (·.getObjValAs? String "text" |>.toOption)).getD ""
  TestM.assert (text.contains "proxy") "and told how GitHub is reached instead"

@[test]
def systemPrompt_namesWhatTheProxyDecidesBy : Test := do
  let p := Orchestra.Kleis.systemPrompt {
    taskId := "t", repo := some pair, issueNumber := some 7, prLabels := ["orchestra"] }
  TestM.assert (p.contains "bot/proj") "the fork"
  TestM.assert (p.contains "up/proj") "the upstream"
  TestM.assert (p.contains "#7") "the issue it may comment on"
  TestM.assert (p.contains "`orchestra`") "the labels its pull requests carry"

private def parse (kleisBlock : String) : Except String AppConfig :=
  let text := "{\"github_app\": {\"app_id\": 1, \"private_key_path\": \"/k.pem\"}, \
               \"kleis\": " ++ kleisBlock ++ "}"
  match Json.parse text with
  | .error e => .error s!"test fixture is not JSON: {e}"
  | .ok j => FromJson.fromJson? j

@[test]
def config_readsTheKleisBlock : Test := do
  match parse "{\"url\": \"http://k:8080\", \"issuer_token\": \"tok\", \"grants\": [\"a\"], \
      \"push_prefix\": \"refs/heads/o/\"}" with
  | .error e => TestM.fail s!"a well-formed kleis block should load: {e}"
  | .ok c =>
    match c.kleis with
    | none => TestM.fail "the kleis block was dropped"
    | some k =>
      TestM.assertEqual k.grants ["a"]
      TestM.assertEqual k.pushPrefix (some "refs/heads/o/")
      TestM.assertEqual k.ttl "12h"
      TestM.assert (!(toString (repr k)).contains "tok") "the issuer token is redacted"

@[test]
def config_theIssuerCredentialMayComeFromAFile : Test := do
  match parse "{\"url\": \"http://kleis:8080\", \"issuer_token_file\": \"/kleis/orchestra.token\"}" with
  | .error e => TestM.fail s!"a block naming a token file should load: {e}"
  | .ok c => TestM.assertEqual (c.kleis.bind (·.issuerTokenFile)) (some "/kleis/orchestra.token")
  let dir ← IO.FS.createTempDir
  let f := dir / "orchestra.token"
  IO.FS.writeFile f "  En0K-file-token\n"
  let got ← Orchestra.Kleis.issuerCredential { cfg with issuerTokenFile := some f.toString }
  TestM.assertEqual got "En0K-file-token"

@[test]
def config_aMistypedOrUnknownSettingIsAnError : Test := do
  -- Read as absent, a mistyped push_prefix would lift the restriction without a word.
  TestM.assert (parse "{\"url\": \"http://k:8080\", \"issuer_token\": \"t\", \"push_prefix\": [\"refs/heads/o/\"]}"
      |>.toOption |>.isNone) "a push_prefix of the wrong type does not load"
  TestM.assert (parse "{\"url\": \"http://k:8080\", \"issuer_token\": \"t\", \"push_prefx\": \"refs/heads/o/\"}"
      |>.toOption |>.isNone) "nor a misspelt setting"
  TestM.assert (parse "{\"url\": \"http://k:8080/prefix\", \"issuer_token\": \"t\"}"
      |>.toOption |>.isNone) "nor a url with a path"

@[test]
def spec_reachesTheMcpServerDirectly : Test := do
  let mcp : Exec.McpEndpoint := { host := "orchestra.default.svc", port := 4000 }
  let spec := Sandbox.specFor AgentDef.claude "/work" mcp "" #[] #[] #[] #[] false #[] {}
    (kleis := some (Orchestra.Kleis.launch cfg minted "/b.pem"))
  TestM.assert (spec.env.any fun (k, v) => k == "NO_PROXY" && v.endsWith ",orchestra.default.svc")
    "the MCP server is not reached through the proxy"

@[test]
def config_aBrokenKleisBlockIsAnError : Test := do
  -- Dropped silently, the block would put the App's token back into every sandbox.
  TestM.assert (parse "{\"url\": \"http://k:8080\"}" |>.toOption |>.isNone)
    "a block with neither an issuer token nor a file does not load"
  TestM.assert (parse "{\"url\": \"http://k\", \"issuer_token\": \"{{kleis}}\"}" |>.toOption |>.isNone)
    "nor one naming a secret that secrets.json lacks"

end OrchestraTest.Kleis
