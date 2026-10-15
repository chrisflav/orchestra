import OrchestraTest.TestM
import Orchestra.Kleis

open Lean (Json)
open Orchestra

/-!
# Task tokens, against a running kleis

`OrchestraTest/KleisSpecTest.lean` checks what orchestra *sends* and how it reads what it is sent,
against answers written down in the test. This file sends the same requests to a real kleisd and
reads its real answers, through the same functions a task launch uses (`Kleis.prepare`,
`Kleis.derive`, `Kleis.fetchCard`, `Kleis.revoke`, `Kleis.launchDerived`): a derive from a spec
with task facts and a narrowing, its setup and card, the setup's files written under a home, a
derive from that token, the proxy honouring both, and revocation.

It opts in, like the cluster and tracker tests, and skips itself otherwise:

```sh
ORCHESTRA_KLEIS_URL=http://127.0.0.1:18190 \
ORCHESTRA_KLEIS_ISSUER_TOKEN_FILE=/path/to/issuers/orchestra.token lake test
```

The kleis it names is expected to be configured like kleis's end-to-end fixture: an issuer
`orchestra` allowed `task_*` facts and the specs `secretary` (an account `mail/christian` on the
`mail` service, reachable as `http://kleis-mail:8025` behind the proxy, and `caldav/christian` on
`dav.example.invalid`) and `alice`, and not `nobody-allowed-this`. A mail gateway need not be
reachable: a request that kleis let through reaches the gateway (or fails to reach the account
behind it); one kleis refused says `kleis:`, and one on a revoked token says `revoked`.
-/

namespace OrchestraTest.KleisLive

private def mentions (hay needle : String) : Bool := (hay.splitOn needle).length > 1

/-- The kleis under test, or `none` when this machine was not given one. -/
private def liveConfig? : IO (Option KleisConfig) := do
  let some url ← IO.getEnv "ORCHESTRA_KLEIS_URL" | return none
  let some file ← IO.getEnv "ORCHESTRA_KLEIS_ISSUER_TOKEN_FILE" | return none
  return some { url, issuerTokenFile := some file, ttl := "1h" }

/-- A request through the proxy, as a sandbox would make it: with the `http_proxy` orchestra put
    in the sandbox's environment. Returns the body and status, whatever they are. -/
private def throughProxy (launch : Kleis.Launch) (url : String) (extra : Array String := #[])
    : IO String := do
  let some proxy := (launch.env.find? (·.1 == "http_proxy")).map (·.2)
    | throw (IO.userError "the launch has no http_proxy")
  let out ← IO.Process.output {
    cmd := "curl", args := #["-sS", "--max-time", "20", "-x", proxy] ++ extra ++ #[url] }
  return out.stdout ++ out.stderr

private def facts : Kleis.TaskFacts :=
  { taskId := "live-1", tools := ["queue_task"], identity := some "secretary" }

@[test]
def live_deriveSetupCardAndRevoke : Test := do
  let some cfg ← liveConfig?
    | TestM.skip "ORCHESTRA_KLEIS_URL and ORCHESTRA_KLEIS_ISSUER_TOKEN_FILE are not set"
  let mailboxes := "http://kleis-mail:8025/mailboxes"
  -- From a spec, with the task's facts and a narrowing, as a task launch does.
  let p ← Kleis.prepare cfg (.spec "secretary" "task") (.spec "secretary") facts
    ["caldav: only read"]
  try
    TestM.assert (!p.minted.revocationId.isEmpty) "derive names a revocation id"
    TestM.assert (p.minted.expires > 0) "and an expiry"
    TestM.assert (mentions p.card "acts as \"secretary\"") s!"the card says who it acts as:\n{p.card}"
    TestM.assert (mentions p.card "narrowed further: caldav: only read")
      s!"the card names the narrowing:\n{p.card}"
    TestM.assertEqual p.minted.card.isSome true (msg := "derive sent a card")
    -- The setup, as orchestra read and checked it.
    let paths := p.setup.files.map (·.path)
    TestM.assert (paths.contains ".local/bin/kleis-mail") s!"setup delivers the mail client: {paths}"
    TestM.assert (paths.any (·.startsWith ".config/vdirsyncer/"))
      s!"and a vdirsyncer configuration: {paths}"
    TestM.assert ((p.setup.caPem.getD "").startsWith "-----BEGIN CERTIFICATE-----")
      "setup names kleis's CA"
    TestM.assert (p.setup.files.all fun f => (Exec.fileModeOf? f.mode).isSome)
      "every file has a mode orchestra can apply"
    -- Written under a home the way the host backends write them.
    let home ← IO.FS.createTempDir
    try
      for f in p.setup.files do
        let some bits := Exec.fileModeOf? f.mode | TestM.fail s!"{f.path}: mode {f.mode}"
        Exec.writeUnderHome home f.path bits f.content
      let client := home / ".local" / "bin" / "kleis-mail"
      TestM.assert (← client.pathExists) "the mail client is in the home"
      let mode ← IO.Process.output { cmd := "stat", args := #["-c", "%a", client.toString] }
      TestM.assertEqual mode.stdout.trimAscii.toString "755" (msg := "and executable")
    finally
      IO.FS.removeDirAll home
    -- The card endpoint agrees with what derive and setup said.
    let card ← Kleis.fetchCard cfg p.minted.token
    TestM.assertEqual card.trimAscii.toString p.card.trimAscii.toString
      (msg := "/card says what setup said")
    -- The proxy, as the sandbox is pointed at it.
    let launch := Kleis.launchDerived cfg p "/nonexistent/bundle.pem" #[]
    let r ← throughProxy launch mailboxes
    TestM.assert (mentions r "IMAP" && !mentions r "kleis:" && !mentions r "revoked")
      s!"the task token reads mail through the proxy, to the gateway: {r}"
    let r ← throughProxy launch "https://dav.example.invalid/calendars/christian/personal/e.ics"
      #["-k", "-X", "PUT", "--data-binary", "BEGIN:VCALENDAR"]
    TestM.assert (mentions r "kleis:") s!"the narrowed calendar refuses a write: {r}"
    -- From the token: a copy narrowed further, revoked, and the source left alive.
    let copy ← Kleis.derive cfg (.token p.minted.token) { taskId := "live-2" } ["mail: only read"]
    TestM.assert (copy.revocationId != p.minted.revocationId) "the copy has its own revocation id"
    TestM.assert (mentions (copy.card.getD "") "narrowed further")
      s!"the copy's card names its narrowings: {copy.card}"
    let copyLaunch := Kleis.launchDerived cfg { p with minted := copy } "/nonexistent/bundle.pem" #[]
    let r ← throughProxy copyLaunch mailboxes
    TestM.assert (mentions r "IMAP" && !mentions r "kleis:" && !mentions r "revoked")
      s!"the copy reads mail: {r}"
    let r ← throughProxy copyLaunch "http://kleis-mail:8025/send"
      #["-H", "Content-Type: application/json", "-d", r#"{"to": ["a@merten.dev"], "text": "t"}"#]
    TestM.assert (mentions r "kleis:") s!"but may not send: {r}"
    Kleis.revoke cfg copy
    let r ← throughProxy copyLaunch mailboxes
    TestM.assert (mentions r "revoked") s!"the revoked copy is refused by the proxy: {r}"
    let cardAfter ← try
        let _ ← Kleis.fetchCard cfg copy.token
        pure false
      catch _ => pure true
    TestM.assert cardAfter "and by kleis's own endpoints"
    let r ← throughProxy launch mailboxes
    TestM.assert (mentions r "IMAP" && !mentions r "revoked") s!"the source outlives its copy: {r}"
  finally
    Kleis.revoke cfg p.minted
  let r ← throughProxy (Kleis.launchDerived cfg p "/nonexistent/bundle.pem" #[]) mailboxes
  TestM.assert (mentions r "revoked") s!"the task token, revoked when the task ends, is refused: {r}"
  -- Revoking twice is not an error (the `finally` of a task that already revoked).
  Kleis.revoke cfg p.minted

@[test]
def live_deriveRefusals : Test := do
  let some cfg ← liveConfig?
    | TestM.skip "ORCHESTRA_KLEIS_URL and ORCHESTRA_KLEIS_ISSUER_TOKEN_FILE are not set"
  let refusal (src : Kleis.Source) (narrow : List String) : IO String := do
    try
      let m ← Kleis.derive cfg src facts narrow
      Kleis.revoke cfg m
      return ""
    catch e => return toString e
  let e ← refusal (.spec "nobody-allowed-this") []
  TestM.assert (mentions e "may not derive") s!"a spec the issuer may not name: {e}"
  let e ← refusal (.spec "secretary") ["mail: only teleport"]
  TestM.assert (mentions e "neither an operation") s!"an unknown operation in a narrowing: {e}"
  let issuer ← Kleis.issuerCredential cfg
  let e ← refusal (.token issuer) []
  TestM.assert (mentions e "not something to derive") s!"an issuer credential as a source: {e}"
  TestM.assert (!mentions e issuer) "and the refusal does not repeat the credential"

end OrchestraTest.KleisLive
