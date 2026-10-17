import OrchestraTest.TestM
import Orchestra.Kleis
import Orchestra.Identity
import Orchestra.Listener
import Orchestra.Project.Role
import Orchestra.Spawn
import Orchestra.WorkflowParser

open Lean (Json FromJson ToJson)
open Orchestra

namespace OrchestraTest.KleisSpec

/-!
# Task tokens derived from a source

What can be checked without a kleis to talk to: which source a task resolves (`Kleis.choose`), what
a queued task inherits, how every surface reads `kleis_spec`, `kleis_token` and `kleis_narrow`,
what a derive request says, what of a `/setup` answer is refused, what a sandbox is given, and
that setup files are written under the home without following a link. The HTTP calls themselves
are kleis's to test against the same endpoints.
-/

private def cfg : KleisConfig :=
  { url := "http://127.0.0.1:8080", issuerToken := "issuer-secret" }

private def mentions (hay needle : String) : Bool := (hay.splitOn needle).length > 1

private def parseApp (kleisBlock : String) : Except String AppConfig :=
  let text := "{\"github_app\": {\"app_id\": 1, \"private_key_path\": \"/k.pem\"}, \
               \"kleis\": " ++ kleisBlock ++ "}"
  match Json.parse text with
  | .error e => .error s!"test fixture is not JSON: {e}"
  | .ok j => FromJson.fromJson? j

/-! ## Configuration -/

@[test]
def config_readsTheDefaultSpec : Test := do
  match parseApp "{\"url\": \"http://k:8080\", \"issuer_token\": \"t\", \"default_spec\": \"worker\"}" with
  | .error e => TestM.fail s!"a block with a default spec should load: {e}"
  | .ok c => TestM.assertEqual (c.kleis.bind (·.defaultSpec)) (some "worker")
  match parseApp "{\"url\": \"http://k:8080\", \"issuer_token\": \"t\"}" with
  | .error e => TestM.fail s!"a block without one should load: {e}"
  | .ok c =>
    TestM.assertEqual (c.kleis.bind (·.defaultSpec)) none
      (msg := "without one, a task with no source keeps the legacy mint")
    TestM.assertEqual (c.kleis.map (·.grants)) (some ["orchestra-github"])
      (msg := "and grants keep their default")

@[test]
def config_aBadDefaultSpecIsAnError : Test := do
  TestM.assert (parseApp "{\"url\": \"http://k\", \"issuer_token\": \"t\", \"default_spec\": 3}"
    |>.toOption |>.isNone) "a default_spec that is not a string does not load"
  TestM.assert (parseApp "{\"url\": \"http://k\", \"issuer_token\": \"t\", \"default_spec\": \"../x\"}"
    |>.toOption |>.isNone) "nor one that is not a spec name"
  TestM.assert (parseApp "{\"url\": \"http://k\", \"issuer_token\": \"t\", \"default_specs\": \"w\"}"
    |>.toOption |>.isNone) "nor a misspelt key"

/-! ## Which source -/

@[test]
def choose_followsThePrecedence : Test := do
  let all : Kleis.SourceLevels := {
    taskToken := some "{{t}}", taskSpec := some "task-spec", identitySpec := some "id-spec"
    roleSpec := some "role-spec", defaultSpec := some "worker" }
  TestM.assertEqual (Kleis.choose all) (.taskToken "{{t}}") (msg := "the task's token first")
  TestM.assertEqual (Kleis.choose { all with taskToken := none }) (.spec "task-spec" "task")
    (msg := "then the task's spec")
  TestM.assertEqual (Kleis.choose { all with taskToken := none, taskSpec := none })
    (.spec "id-spec" "identity") (msg := "then the identity's, before the role's")
  TestM.assertEqual
    (Kleis.choose { all with taskToken := none, taskSpec := none, identitySpec := none
                             identityToken := true })
    .identityToken (msg := "the identity's token stands where its spec would")
  TestM.assertEqual
    (Kleis.choose { all with taskToken := none, taskSpec := none, identitySpec := none })
    (.spec "role-spec" "role") (msg := "then the role's (or listener action's)")
  TestM.assertEqual (Kleis.choose { defaultSpec := some "worker" }) (.spec "worker" "default_spec")
    (msg := "then the default")
  TestM.assertEqual (Kleis.choose {}) .none (msg := "and otherwise none")

@[test]
def inherited_isANameOrAReferenceNeverAToken : Test := do
  TestM.assertEqual (Kleis.Choice.taskToken "{{t}}").inherited (none, some "{{t}}")
  TestM.assertEqual (Kleis.Choice.spec "worker" "default_spec").inherited (some "worker", none)
  -- The identity's token is resolved; the queued task inherits the identity, which names it again.
  TestM.assertEqual Kleis.Choice.identityToken.inherited (none, none)

@[test]
def tokenRef_isResolvedFromTheSecrets : Test := do
  let secrets := [("ada_token", " En0K-ada \n")]
  TestM.assertEqual (Kleis.resolveTokenRef secrets "{{ada_token}}" |>.toOption) (some "En0K-ada")
  TestM.assert (Kleis.resolveTokenRef secrets "{{other}}" |>.toOption |>.isNone)
    "a secret secrets.json lacks is an error"
  TestM.assert (Kleis.resolveTokenRef secrets "En0K-literal" |>.toOption |>.isNone)
    "and a literal token is not a reference"

@[test]
def prepareFor_refusesWhatItCannotHonour : Test := do
  let facts : Kleis.TaskFacts := { taskId := "t" }
  -- Without kleis, a named source would run the task with whatever the sandbox holds instead.
  let noKleis ← try
      let _ ← Kleis.prepareFor none { taskSpec := some "secretary" } none facts []
      pure "prepared"
    catch e => pure (toString e)
  TestM.assert (mentions noKleis "no kleis is configured") s!"refused: {noKleis}"
  -- The legacy mint cannot narrow, so a narrowing with nothing to derive from is refused.
  let legacyNarrow ← try
      let _ ← Kleis.prepareFor (some cfg) {} none facts ["mail: only read"]
      pure "prepared"
    catch e => pure (toString e)
  TestM.assert (mentions legacyNarrow "cannot narrow") s!"refused: {legacyNarrow}"
  -- And with neither, the legacy path: nothing derived, nothing asked of kleis.
  let none? ← Kleis.prepareFor (some cfg) {} none facts []
  TestM.assert none?.isNone "no source under kleis is the legacy mint"
  let none? ← Kleis.prepareFor none {} none facts []
  TestM.assert none?.isNone "and no kleis at all is no token"

/-! ## Where a source is written -/

private def parseTask (fields : String) : Except String Task :=
  match Json.parse ("{\"prompt\": \"p\"" ++ fields ++ "}") with
  | .error e => .error e
  | .ok j => FromJson.fromJson? j

@[test]
def task_readsItsSourceStrictly : Test := do
  match parseTask ", \"kleis_token\": \"{{ada}}\", \"kleis_spec\": \"secretary\", \
      \"kleis_narrow\": [\"mail: only read\"]" with
  | .error e => TestM.fail s!"a task naming its source should load: {e}"
  | .ok t =>
    TestM.assertEqual t.ioTask.kleisToken (some "{{ada}}") (msg := "the reference, unresolved")
    TestM.assertEqual t.ioTask.kleisSpec (some "secretary")
    TestM.assertEqual t.ioTask.kleisNarrow ["mail: only read"]
  TestM.assert (parseTask ", \"kleis_token\": \"En0K-literal\"" |>.toOption |>.isNone)
    "a literal token is refused: it would be stored in the queue"
  TestM.assert (parseTask ", \"kleis_spec\": 5" |>.toOption |>.isNone) "a mistyped spec"
  TestM.assert (parseTask ", \"kleis_spec\": \"a b\"" |>.toOption |>.isNone) "a spec that is no name"
  TestM.assert (parseTask ", \"kleis_narrow\": \"mail: only read\"" |>.toOption |>.isNone)
    "a narrowing that is not a list"

@[test]
def identity_namesOneSource : Test := do
  let parse (s : String) : Except String Identity.Identity :=
    match Json.parse s with
    | .error e => .error e
    | .ok j => FromJson.fromJson? j
  match parse r#"{"name":"secretary","kleis_spec":"secretary","kleis_narrow":["mail: only read"]}"# with
  | .error e => TestM.fail s!"an identity naming a spec should load: {e}"
  | .ok i =>
    TestM.assertEqual i.kleisSpec (some "secretary")
    TestM.assertEqual i.kleisNarrow ["mail: only read"]
    TestM.assert (mentions (ToJson.toJson i).compress "secretary") "the spec is listed"
  match parse r#"{"name":"secretary","kleis_token":"En0K-secret"}"# with
  | .error e => TestM.fail s!"an identity holding a token should load: {e}"
  | .ok i =>
    TestM.assertEqual i.kleisToken (some "En0K-secret")
    TestM.assert (!mentions (ToJson.toJson i).compress "En0K") "the token is not serialized"
    TestM.assert (!mentions (toString (repr i)) "En0K") "nor printed"
  TestM.assert (parse r#"{"name":"s","kleis_spec":"a","kleis_token":"b"}"# |>.toOption |>.isNone)
    "both is refused"
  TestM.assert (parse r#"{"name":"s","kleis_token":"{{missing}}"}"# |>.toOption |>.isNone)
    "an unsubstituted secret is refused"

@[test]
def roleAndListenerAction_carryASpecAndNarrowings : Test := do
  let role : Except String Project.Role := Json.parse r#"{"name":"reviewer","permissions":[],
      "prompt_template":"x","kleis_spec":"reviewer","kleis_narrow":["github: only read"]}"#
    >>= FromJson.fromJson?
  match role with
  | .error e => TestM.fail s!"a role with a spec should load: {e}"
  | .ok r =>
    TestM.assertEqual r.kleisSpec (some "reviewer")
    TestM.assertEqual r.kleisNarrow ["github: only read"]
  let action : Except String Listener.ActionConfig := Json.parse r#"{"prompt_template":"x",
      "kleis_spec":"triager","kleis_narrow":["mail: only read"]}"# >>= FromJson.fromJson?
  match action with
  | .error e => TestM.fail s!"an action with a spec should load: {e}"
  | .ok a =>
    TestM.assertEqual a.kleisSpec (some "triager")
    TestM.assertEqual a.kleisNarrow ["mail: only read"]
  let bad : Except String Project.Role := Json.parse r#"{"name":"r","permissions":[],
      "prompt_template":"x","kleis_narrow":[1]}"# >>= FromJson.fromJson?
  TestM.assert (bad.toOption.isNone) "a narrowing that is not a string is refused"

@[test]
def workflowStep_carriesASource : Test := do
  let yaml := "name: w
steps:
  one:
    task:
      prompt: \"a\"
      kleis-spec: secretary
      kleis-token: \"{{ada}}\"
      kleis-narrow:
        - \"mail: only read\"
"
  match Workflow.WorkflowProgram.parseYaml yaml with
  | .error e => TestM.fail s!"parse failed: {e}"
  | .ok prog =>
    match prog.steps with
    | step :: _ =>
      match step.action with
      | .task spec =>
        TestM.assertEqual spec.kleisSpec (some "secretary")
        TestM.assertEqual spec.kleisToken (some "{{ada}}")
        TestM.assertEqual spec.kleisNarrow ["mail: only read"]
      | _ => TestM.fail "expected a task step"
    | [] => TestM.fail "expected a step"
  let literal := "name: w
steps:
  one:
    task:
      prompt: \"a\"
      kleis-token: En0K-literal
"
  TestM.assert (Workflow.WorkflowProgram.parseYaml literal |>.toOption |>.isNone)
    "a literal token in a workflow is refused"

/-! ## Spawned tasks (B.3) -/

@[test]
def spawn_inheritsTheSourceAndMayOnlyNarrow : Test := do
  let p : SpawnPolicy := { maxTasks := 1 }
  let ctx : SpawnContext := {
    kleisSpec := some "secretary", kleisNarrow := ["mail: only read, flag"] }
  match p.resolve ctx { prompt := "do it", kleisNarrow := ["caldav: only read"] } with
  | .error e => TestM.fail s!"the spawn should resolve: {e}"
  | .ok r =>
    TestM.assertEqual r.kleisSpec (some "secretary") (msg := "the parent's spec, by name")
    TestM.assertEqual r.kleisToken none
    TestM.assertEqual r.kleisNarrow ["mail: only read, flag", "caldav: only read"]
      (msg := "the parent's narrowings, then the child's own")
  let ctxToken : SpawnContext := { kleisToken := some "{{ada}}" }
  match p.resolve ctxToken { prompt := "do it" } with
  | .error e => TestM.fail s!"the spawn should resolve: {e}"
  | .ok r => TestM.assertEqual r.kleisToken (some "{{ada}}") (msg := "a token by its reference")

/-! ## derive -/

@[test]
def deriveBody_fromASpecCarriesTheTaskFacts : Test := do
  let facts : Kleis.TaskFacts := { taskId := "t-5678", identity := some "secretary" }
  let body := Kleis.deriveBody { cfg with ttl := "6h" } (.spec "secretary") facts ["mail: only read"]
  TestM.assertEqual (body.getObjValAs? String "spec" |>.toOption) (some "secretary")
  TestM.assertEqual (body.getObjValAs? String "bearer" |>.toOption) (some "orchestra:t-5678")
  TestM.assertEqual (body.getObjValAs? String "ttl" |>.toOption) (some "6h")
  TestM.assertEqual (body.getObjValAs? (List String) "narrow" |>.toOption) (some ["mail: only read"])
  TestM.assert (mentions (body.getObjVal? "facts" |>.toOption |>.map (·.compress) |>.getD "")
    "task_identity") "the task facts orchestra states today"
  TestM.assert (body.getObjVal? "token" |>.toOption |>.isNone) "and no token"

@[test]
def deriveBody_fromATokenCarriesNoFacts : Test := do
  let body := Kleis.deriveBody cfg (.token "En0K-src") { taskId := "t" } []
  TestM.assertEqual (body.getObjValAs? String "token" |>.toOption) (some "En0K-src")
  TestM.assert (body.getObjVal? "facts" |>.toOption |>.isNone) "no facts: kleis adds checks only"
  TestM.assert (body.getObjVal? "spec" |>.toOption |>.isNone) "no spec"
  TestM.assert (body.getObjVal? "narrow" |>.toOption |>.isNone) "no narrowing when there is none"

@[test]
def parseDerived_wantsOneRevocationId : Test := do
  match Kleis.parseDerived r#"{"token":"T","revocation_id":"r1","expires":"2026-10-11T18:00:00Z","card":"This token acts as worker."}"# with
  | .error e => TestM.fail s!"a derive answer should parse: {e}"
  | .ok m =>
    TestM.assertEqual m.revocationId "r1"
    TestM.assertEqual m.card (some "This token acts as worker.")
  TestM.assert (Kleis.parseDerived r#"{"token":"T"}"# |>.toOption |>.isNone)
    "a token that cannot be revoked is refused"
  TestM.assert (Kleis.parseDerived r#"{"token":"T","revocation_ids":["a","b"]}"# |>.toOption |>.isNone)
    "and so is one with several ids, any of which could be the source's"

/-! ## setup -/

private def setupOf (s : String) : Except String Kleis.Setup :=
  Json.parse s >>= Kleis.parseSetup

@[test]
def setup_isReadAsKleisSendsIt : Test := do
  match setupOf r#"{"proxy":{"ca_pem":"-----BEGIN CERTIFICATE-----"},
      "env":{"GH_TOKEN":"kleis","GIT_CONFIG_COUNT":"1"},
      "files":[{"path":".zuliprc","mode":"0600","content":"[api]\nkey=kleis\n"},
               {"path":".config/khal/config","content":"x"}],
      "no_proxy":["dav.example.org"],"card":"This token acts as secretary."}"# with
  | .error e => TestM.fail s!"a setup should parse: {e}"
  | .ok s =>
    TestM.assertEqual s.caPem (some "-----BEGIN CERTIFICATE-----")
    TestM.assertEqual s.env.size 2
    TestM.assertEqual (s.files.map (·.path)) #[".zuliprc", ".config/khal/config"]
    TestM.assertEqual (s.files.map (·.mode)) #["0600", "0600"]
    TestM.assertEqual s.noProxy ["dav.example.org"]
    TestM.assertEqual s.card (some "This token acts as secretary.")

@[test]
def setup_refusesWhatIsTheConsumers : Test := do
  for v in Kleis.reservedSetupEnv do
    TestM.assert (setupOf ("{\"env\":{\"" ++ v ++ "\":\"x\"}}") |>.toOption |>.isNone)
      s!"setup may not set {v}"
  TestM.assert (setupOf r#"{"files":[{"path":"/etc/passwd","content":"x"}]}"# |>.toOption |>.isNone)
    "an absolute path is refused"
  TestM.assert (setupOf r#"{"files":[{"path":".config/../../x","content":"x"}]}"# |>.toOption |>.isNone)
    "so is a '..' segment"
  TestM.assert (setupOf r#"{"files":[{"path":"..","content":"x"}]}"# |>.toOption |>.isNone)
    "and '..' alone"
  TestM.assert (setupOf r#"{"files":[{"path":"x","mode":"4755","content":"x"}]}"# |>.toOption |>.isNone)
    "and a setuid mode"
  TestM.assert (setupOf r#"{"env":{"A B":"x"}}"# |>.toOption |>.isNone)
    "and a variable that has no valid name"

@[test]
def launchDerived_setsNothingAboutAnyOneService : Test := do
  let setup : Kleis.Setup := {
    env := #[("GH_TOKEN", "kleis")], noProxy := ["dav.example.org"] }
  let p : Kleis.Prepared := {
    minted := { token := "En0K+abc", revocationId := "r", expires := 0 }
    setup, card := "card", choice := .spec "worker" "default_spec" }
  let grant : Exec.PathGrant := { path := ".zuliprc", access := .ro, scope := .home }
  let l := Kleis.launchDerived cfg p "/data/kleis/ca-bundle.pem" #[grant]
  let get (k : String) := (l.env.find? (·.1 == k)).map (·.2)
  TestM.assertEqual (get "HTTPS_PROXY") (some "http://orchestra:En0K%2Babc@127.0.0.1:8080")
  TestM.assertEqual (get "SSL_CERT_FILE") (some "/data/kleis/ca-bundle.pem")
  TestM.assert ((get "NO_PROXY").getD "" |>.endsWith "dav.example.org") "setup's no_proxy is added"
  TestM.assertEqual (get "GH_TOKEN") (some "kleis") (msg := "from setup, not from orchestra")
  TestM.assertEqual (get "GIT_CONFIG_KEY_0") none (msg := "no git configuration of orchestra's own")
  TestM.assert (l.files.any (·.path == ".zuliprc")) "the setup file is granted"
  TestM.assert (l.files.any (·.path == "/data/kleis/ca-bundle.pem")) "and the bundle"
  let noGh := Kleis.launchDerived cfg { p with setup := {} } "/b.pem" #[]
  TestM.assert (!noGh.env.any (·.1 == "GH_TOKEN")) "without setup saying so, no GH_TOKEN at all"
  TestM.assert (mentions (Kleis.cardPrompt "This token acts as worker.") "This token acts as worker.")
    "the card reaches the system prompt"

/-! ## Files under the home -/

private def run (cmd : String) (args : Array String) : IO Unit := do
  let _ ← IO.Process.output { cmd, args }


@[test]
def aCardNoteReplacesTheSystemPromptsCard : Test := do
  -- A resumed conversation keeps its first system prompt; the note is how a later card reaches it.
  let n := Kleis.cardNote "This token acts as secretary.\n\nMail — mail/secretary@merten.dev\n"
  TestM.assert (n.startsWith Kleis.cardNoteHeading) "the note is found by its heading"
  TestM.assert (mentions n "Mail — mail/secretary@merten.dev") "and carries the card"
  TestM.assert (mentions n "replaces the one in your system prompt") "and says what it replaces"
@[test]
def writeUnderHome_followsNoLink : Test := do
  let home ← IO.FS.createTempDir
  let outside ← IO.FS.createTempDir
  try
    Exec.writeUnderHome home ".config/khal/config" 0o600 "[calendars]\n"
    TestM.assertEqual (← IO.FS.readFile (home / ".config" / "khal" / "config")) "[calendars]\n"
      (msg := "written, with the directories on the way made")
    -- A link at the destination is replaced, not written through.
    IO.FS.writeFile (outside / "victim") "untouched"
    run "ln" #["-s", (outside / "victim").toString, (home / ".zuliprc").toString]
    Exec.writeUnderHome home ".zuliprc" 0o600 "[api]\n"
    TestM.assertEqual (← IO.FS.readFile (outside / "victim")) "untouched"
      (msg := "the link's target is left alone")
    TestM.assertEqual (← IO.FS.readFile (home / ".zuliprc")) "[api]\n"
    -- A link on the way is refused.
    run "ln" #["-s", outside.toString, (home / ".linked").toString]
    let refused ← try
        Exec.writeUnderHome home ".linked/x" 0o600 "x"
        pure false
      catch _ => pure true
    TestM.assert refused "a directory that is a link is not written under"
    TestM.assert (!(← (outside / "x").pathExists)) "and nothing reached the other side"
    let traversal ← try
        Exec.writeUnderHome home "../escape" 0o600 "x"
        pure false
      catch _ => pure true
    TestM.assert traversal "a '..' is refused here too"
  finally
    IO.FS.removeDirAll home
    IO.FS.removeDirAll outside

@[test]
def homePath_allowsOnlyWhatNoShellReads : Test := do
  for ok in [".zuliprc", ".config/zulip/weil.zulip.merten.dev.zuliprc", ".local/bin/kleis-mail",
             ".config/vdirsyncer/dav.example.invalid.conf", "a/b@c+d=e,f:g%h"] do
    TestM.assertEqual (Exec.homePathError? ok) none (msg := s!"{ok} is a home path")
  -- `>` would be a redirection on the daemon's host if it reached a shell unquoted, and with a
  -- `/` after it the segments look harmless: `a>` `tmp` `x`.
  for bad in ["a>/tmp/x", "a<b", "*", "x?", "#c", "a b", "$(id)", "`id`", "a;b", "a|b", "~/x",
              "a\nb", "a\\b", "ä"] do
    TestM.assert (Exec.homePathError? bad).isSome s!"{bad.quote} is refused"

@[test]
def setup_refusesHomePathAndTheLoader : Test := do
  for v in ["HOME", "PATH", "ALL_PROXY", "all_proxy", "SSL_CERT_DIR", "LD_PRELOAD",
            "LD_LIBRARY_PATH", "NODE_TLS_REJECT_UNAUTHORIZED", "GIT_SSL_NO_VERIFY"] do
    TestM.assert (setupOf ("{\"env\":{\"" ++ v ++ "\":\"x\"}}") |>.toOption |>.isNone)
      s!"setup may not set {v}"

@[test]
def installFiles_grantsExecuteToAnExecutable : Test := do
  TestM.assert (Kleis.installFiles.grantAccess "0755" matches .rox) "0755 is run, so rox"
  TestM.assert (Kleis.installFiles.grantAccess "0600" matches .ro) "0600 is only read"
  TestM.assert (Kleis.installFiles.grantAccess "0700" matches .rox) "0700 too is run"

@[test]
def deriveBody_fromATokenNamesTheBearer : Test := do
  let body := Kleis.deriveBody cfg (.token "En0K-src") { taskId := "t-9" } []
  TestM.assertEqual (body.getObjValAs? String "bearer" |>.toOption) (some "orchestra:t-9")
    (msg := "for kleis's issued record and audit log")

@[test]
def launchDerived_setupEnvIsNotDoubled : Test := do
  let p : Kleis.Prepared := {
    minted := { token := "T", revocationId := "r", expires := 0 }
    setup := { env := #[("GIT_TERMINAL_PROMPT", "0"), ("GH_TOKEN", "kleis")] }
    card := "c", choice := .spec "worker" "default_spec" }
  let l := Kleis.launchDerived cfg p "/b.pem" #[]
  TestM.assertEqual (l.env.filter (·.1 == "GIT_TERMINAL_PROMPT")).size 1
    (msg := "one GIT_TERMINAL_PROMPT")
  TestM.assertEqual l.setupEnvNames ["GIT_TERMINAL_PROMPT", "GH_TOKEN"]

@[test]
def workflow_listenerKleisReachesEveryTaskStep : Test := do
  let step (n : String) (narrow : List String) : Step :=
    { name := n, action := .task { prompt := "p", kleisNarrow := narrow } }
  let prog : WorkflowProgram := { name := "w", steps := [step "a" [], step "b" ["mail: only read"]] }
  let prog := prog.withListenerKleis (some "worker") ["caldav: only read"]
  for s in prog.steps do
    match s.action with
    | .task t =>
      TestM.assertEqual t.kleisRoleSpec (some "worker") (msg := s!"{s.name}: the action's spec")
      TestM.assert (t.kleisNarrow.contains "caldav: only read") s!"{s.name}: the action's narrowing"
    | _ => TestM.fail "a task step stayed a task step"
  match prog.steps with
  | [_, b] => match b.action with
    | .task t => TestM.assert (t.kleisNarrow.contains "mail: only read") "the step's own kept"
    | _ => TestM.fail "b is a task"
  | _ => TestM.fail "two steps"

@[test]
def fileMode_isOctalWithoutSpecialBits : Test := do
  TestM.assertEqual (Exec.fileModeOf? "0600") (some 0o600)
  TestM.assertEqual (Exec.fileModeOf? "644") (some 0o644)
  TestM.assertEqual (Exec.fileModeOf? "4755") none
  TestM.assertEqual (Exec.fileModeOf? "0800") none
  TestM.assertEqual (Exec.fileModeOf? "rw") none

end OrchestraTest.KleisSpec
