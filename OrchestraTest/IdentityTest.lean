import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Identity

/-! Identities: the record, where its memory lives, and the two things a task carries it for.

    Nothing here talks to taxis. What an identity does to a tracker write is swap one bearer
    token for another (`Taxis.getConfigAs`), which is worth pinning as the pure fact it is; that
    taxis then attributes the write to that token's actor is taxis's own behaviour and is tested
    where the rest of the tracker integration is, against a real instance. -/

namespace OrchestraTest.Identities

private def withIdentities (act : IO α) : IO α := do
  let root : System.FilePath :=
    System.FilePath.mk "/tmp" / s!"orchestra-identity-test-{← IO.monoNanosNow}"
  IO.FS.createDirAll root
  let previous ← identitiesDirOverride.get
  setIdentitiesDirOverride (some root)
  try act
  finally
    setIdentitiesDirOverride previous
    try IO.FS.removeDirAll root catch _ => pure ()

private def writeIdentity (name : String) (json : String) (agents : Option String := none) :
    IO Unit := do
  let dir := (← identitiesDir) / name
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "identity.json") json
  if let some body := agents then IO.FS.writeFile (dir / "AGENTS.md") body

/-- Whether `needle` occurs in `hay`. Used to check that an error message names the thing it is
    about, without pinning the whole sentence. -/
private def mentions (hay needle : String) : Bool := (hay.splitOn needle).length > 1

/-- Run `act` against `cfg` as the active taxis configuration, restoring whatever was there. -/
private def withTaxisConfig (cfg : Taxis.Config) (act : IO α) : IO α := do
  let previous ← Orchestra.Taxis.configRef.get
  Orchestra.Taxis.setConfig (some cfg)
  try act
  finally Orchestra.Taxis.setConfig previous

/-- Run `act`, returning either its result or the message it threw. -/
private def outcomeOf (act : IO String) : IO String := do
  try act catch e => return toString e

@[test]
def loadsTheRecordAsConfigured : Test := do
  let got ← (withIdentities do
    writeIdentity "maintainer"
      r#"{"name":"maintainer","description":"Keeps the tracker tidy.","taxis_token":"t-123"}"#
    let record ← loadIdentity "maintainer"
    return record.map fun r => (r.name, r.description, r.taxisToken))
  TestM.assertEqual got (some ("maintainer", some "Keeps the tracker tidy.", some "t-123"))
    (msg := "name, description and token as written")

/-- A record with nothing but a name is a usable identity: one with a memory of its own that
    writes to the tracker on orchestra's token like any other task. The token is what makes its
    tracker writes its own, and it is optional. -/
@[test]
def aTokenIsOptional : Test := do
  let got ← (withIdentities do
    writeIdentity "archivist" r#"{"name":"archivist"}"#
    let record ← loadIdentity "archivist"
    return record.map fun r => (r.name, r.taxisToken))
  TestM.assertEqual got (some ("archivist", none)) (msg := "an identity without a taxis token")

@[test]
def anAbsentIdentityIsNone : Test := do
  let got ← (withIdentities do
    let record ← loadIdentity "nobody"
    return record.map (·.name))
  TestM.assertEqual got none (msg := "no record, no identity")

/-- A record stored under a name it does not agree with is refused rather than loaded under
    either. Every other surface — a task, a role, a listener action — names an identity by the
    filename, so a disagreement makes the identity reachable under one spelling and self-describing
    under another. -/
@[test]
def aMisnamedRecordIsRefused : Test := do
  let outcome ← (withIdentities do
    writeIdentity "reviewer" r#"{"name":"maintainer"}"#
    outcomeOf do
      let _ ← loadIdentity "reviewer"
      return "loaded")
  TestM.assert (outcome != "loaded") (msg := "a misnamed record must not load")
  TestM.assert (mentions outcome "maintainer" && mentions outcome "reviewer")
    (msg := s!"the refusal should name both spellings, got: {outcome}")

/-- The failure that matters most, because the alternative is silent: a task naming an identity
    that is not configured must stop, and say which ones are. Running it anyway would run the
    work as the instance — orchestra's own tracker token, none of the identity's memory — and
    look like it had worked. -/
@[test]
def requiringAMissingIdentityNamesTheOnesThereAre : Test := do
  let outcome ← (withIdentities do
    writeIdentity "maintainer" r#"{"name":"maintainer"}"#
    writeIdentity "reviewer"   r#"{"name":"reviewer"}"#
    outcomeOf do
      let _ ← requireIdentity "planner"
      return "resolved")
  TestM.assert (outcome != "resolved") (msg := "an unconfigured identity must not resolve")
  TestM.assert (mentions outcome "maintainer" && mentions outcome "reviewer")
    (msg := s!"the refusal should list the configured identities, got: {outcome}")

@[test]
def listingSkipsWhatItCannotRead : Test := do
  let names ← (withIdentities do
    writeIdentity "maintainer" r#"{"name":"maintainer"}"#
    writeIdentity "broken"     r#"{"nome":"broken"}"#
    let ids ← loadAllIdentities
    return ids.map (·.name))
  TestM.assertEqual names #["maintainer"]
    (msg := "one unreadable record must not stop the listing answering")

/-! ## The standing instructions -/

@[test]
def theAgentsFileIsReadBesideTheRecord : Test := do
  let got ← (withIdentities do
    writeIdentity "maintainer" r#"{"name":"maintainer"}"#
      (agents := some "# maintainer\n\nYou look after the tracker.\n")
    let record ← loadIdentity "maintainer"
    return record.bind (·.agents))
  TestM.assertEqual got (some "# maintainer\n\nYou look after the tracker.\n")
    (msg := "AGENTS.md arrives verbatim")

@[test]
def anIdentityNeedNotHaveInstructions : Test := do
  let got ← (withIdentities do
    writeIdentity "archivist" r#"{"name":"archivist"}"#
    let record ← loadIdentity "archivist"
    return record.bind (·.agents))
  TestM.assertEqual got none (msg := "a name and a memory is a whole identity")

/-- The instructions reach the agent as a section of its system prompt, under a heading naming
    whose they are, with the operator's file inside it unedited. -/
@[test]
def instructionsBecomeASystemPromptSection : Test := do
  let idn : Identity := { name := "maintainer", agents := some "Split what is too big." }
  match TaskRunner.identityInstructions idn with
  | none         => TestM.fail "an identity with an AGENTS.md should contribute a section"
  | some section? =>
    TestM.assert (mentions section? "maintainer's instructions")
      (msg := s!"the section should name whose instructions these are, got: {section?}")
    TestM.assert (mentions section? "Split what is too big.")
      (msg := "the operator's file should arrive verbatim")

/-- A file that is there but blank contributes nothing, rather than a heading with nothing under
    it — which reads to an agent as instructions it has failed to find. -/
@[test]
def blankInstructionsContributeNoSection : Test := do
  let idn : Identity := { name := "maintainer", agents := some "   \n\n  " }
  TestM.assert (TaskRunner.identityInstructions idn).isNone
    (msg := "a blank AGENTS.md is no instructions")

/-- The layout decision, pinned because it is the whole of "dedicated" and nothing else would
    catch it going wrong. Global memory is the memory *root* — `TaskRunner.resolveMemoryDirs`
    hands `<data>/memory` itself to any task whose `memory` is `global` or `both` — so an
    identity's memory kept anywhere below it would be readable and writable by every ordinary
    task on the instance. -/
@[test]
def memoryLivesOutsideTheSharedMemoryRoot : Test := do
  let (memory, sharedRoot, created) ← Orchestra.withTempData "identity-memory" do
    let memory ← ensureMemoryDir "maintainer"
    let sharedRoot := (← Dirs.dataBase) / "memory"
    let created ← memory.pathExists
    return (memory.toString, sharedRoot.toString, created)
  TestM.assert created (msg := "the memory directory is created for the run that needs it")
  TestM.assert (!memory.startsWith sharedRoot)
    (msg := s!"an identity's memory must not sit under the shared memory root, got {memory}")

@[test]
def twoIdentitiesDoNotShareAMemory : Test := do
  let (a, b) ← Orchestra.withTempData "identity-memory-distinct" do
    let a ← ensureMemoryDir "maintainer"
    let b ← ensureMemoryDir "reviewer"
    return (a.toString, b.toString)
  TestM.assert (a != b) (msg := "each identity gets its own directory")

/-! ## Carrying the identity through the surfaces that queue work -/

@[test]
def aTaskFileCarriesTheIdentity : Test := do
  match Json.parse r#"{"prompt":"do the thing","identity":"maintainer"}"# with
  | .error e => TestM.fail s!"fixture is not JSON: {e}"
  | .ok j =>
    match (FromJson.fromJson? j : Except String Task) with
    | .error e => TestM.fail s!"task did not decode: {e}"
    | .ok t    =>
      TestM.assertEqual t.ioTask.identity (some "maintainer") (msg := "identity on the task")

@[test]
def aTaskWithoutOneRunsAsTheInstance : Test := do
  match Json.parse r#"{"prompt":"do the thing"}"# with
  | .error e => TestM.fail s!"fixture is not JSON: {e}"
  | .ok j =>
    match (FromJson.fromJson? j : Except String Task) with
    | .error e => TestM.fail s!"task did not decode: {e}"
    | .ok t    => TestM.assertEqual t.ioTask.identity none (msg := "absent stays absent")

/-- An entry can wait hours for a slot, and it is written to disk in between: the identity has to
    survive the round trip or a queued run would come back as the instance. -/
@[test]
def aQueueEntryRoundTripsTheIdentity : Test := do
  let entry : Queue.QueueEntry :=
    { id := "q1", createdAt := "2026-01-01T00:00:00Z", repo := none, prompt := "go"
    , identity := some "maintainer" }
  match (FromJson.fromJson? (ToJson.toJson entry) : Except String Queue.QueueEntry) with
  | .error e  => TestM.fail s!"entry did not decode: {e}"
  | .ok back  =>
    TestM.assertEqual back.identity (some "maintainer") (msg := "identity survives the queue")

@[test]
def aRoleCarriesTheIdentityItDispatchesUnder : Test := do
  let fixture :=
    r#"{"name":"reviewer","permissions":["review_issues"],"prompt_template":"R",
        "identity":"maintainer"}"#
  match Json.parse fixture with
  | .error e => TestM.fail s!"fixture is not JSON: {e}"
  | .ok j =>
    match (FromJson.fromJson? j : Except String Project.Role) with
    | .error e => TestM.fail s!"role did not decode: {e}"
    | .ok r    => TestM.assertEqual r.identity (some "maintainer") (msg := "identity on the role")

/-- `queue_task` has no identity field, so the only identity a queued task can get is the one the
    task queueing it is already running under. This is what stops an agent putting on somebody
    else's — an identity carries a memory and a tracker token the operator handed to a
    particular piece of work. -/
@[test]
def aQueuedTaskInheritsTheIdentityOfTheTaskThatQueuedIt : Test := do
  let policy : SpawnPolicy := { maxTasks := 2 }
  let ctx : SpawnContext := { identity := some "maintainer" }
  match SpawnPolicy.resolve policy ctx { prompt := "carry on" } with
  | .error e => TestM.fail s!"the spawn was refused: {e}"
  | .ok r    => TestM.assertEqual r.identity (some "maintainer") (msg := "inherited, not chosen")

@[test]
def aTaskWithNoIdentityQueuesTasksWithNone : Test := do
  match SpawnPolicy.resolve { maxTasks := 2 } {} { prompt := "carry on" } with
  | .error e => TestM.fail s!"the spawn was refused: {e}"
  | .ok r    => TestM.assertEqual r.identity none (msg := "nothing to inherit")

/-- A dormant session is woken from what is on disk, so the identity has to be on the record and
    not only in the request that started it: a conversation that came back as the instance would
    carry on writing to the tracker under a different actor than the half already in its
    transcript. -/
@[test]
def aSessionRecordRoundTripsTheIdentity : Test := do
  let record : Interactive.SessionRecord :=
    { id := "s1", createdAt := "2026-01-01T00:00:00Z", lastActivityAt := "2026-01-01T00:00:00Z"
    , upstream := { owner := "o", name := "r" }, fork := { owner := "o", name := "r" }
    , identity := some "maintainer" }
  match (FromJson.fromJson? (ToJson.toJson record) : Except String Interactive.SessionRecord) with
  | .error e => TestM.fail s!"session record did not decode: {e}"
  | .ok back =>
    TestM.assertEqual back.identity (some "maintainer")
      (msg := "identity survives a session going dormant")

/-- `orchestra chat --identity` reaches the daemon as one field of the start request. -/
@[test]
def aSessionRequestCarriesTheIdentity : Test := do
  let body := Json.mkObj
    [ ("type", Json.str "interactive_start")
    , ("spec", Json.mkObj
        [ ("upstream", Json.str "o/r"), ("fork", Json.str "o/r")
        , ("identity", Json.str "maintainer") ]) ]
  match (FromJson.fromJson? body : Except String DaemonRequest.DaemonRequest) with
  | .error e => TestM.fail s!"start request did not decode: {e}"
  | .ok (.interactiveStart spec) =>
      TestM.assertEqual spec.identity (some "maintainer") (msg := "identity on the spec")
  | .ok _ => TestM.fail "decoded as some other request"

/-! ## Acting on the tracker as the identity -/

/-- The one thing running under an identity changes about a tracker write: the bearer token, and
    nothing else about where the write goes. -/
@[test]
def actingAsAnIdentitySwapsOnlyTheToken : Test := do
  let (asInstance, asIdentity) ←
    withTaxisConfig { url := "http://taxis.example", token := some "instance-token" } do
      let a ← Orchestra.Taxis.getConfigAs none
      let b ← Orchestra.Taxis.getConfigAs (some "identity-token")
      return ((a.url, a.token), (b.url, b.token))
  TestM.assertEqual asInstance ("http://taxis.example", some "instance-token")
    (msg := "no identity: orchestra's own token")
  TestM.assertEqual asIdentity ("http://taxis.example", some "identity-token")
    (msg := "same instance, the identity's token")

end OrchestraTest.Identities
