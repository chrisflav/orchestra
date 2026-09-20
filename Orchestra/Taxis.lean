import Taxis.Client

/-!
# taxis client

Orchestra's project/issue/claim subsystem (`Orchestra.Project`) is backed by a taxis instance
instead of its own file-based storage (see the "Migrate to taxis" tracking issue). This module
used to hand-roll a second typed HTTP client mirroring taxis's own (`Taxis.Client`) — that
duplication is gone: everything below is a re-export of `Taxis.Client`/`Taxis`'s own domain types
and wrapper functions, plus the one piece of genuinely Orchestra-specific machinery, an
active-config ref so `Orchestra.Project.*`/`Orchestra.Project.Claim.*` don't need to thread a
`Config` value through every call site.
-/

namespace Orchestra.Taxis

export Taxis.Client (Config listIssues createIssue getIssueDetail getIssue updateIssue deleteIssue
  createArtifact updateArtifact deleteArtifact listComments createComment listLabels createLabel
  ensureLabel
  listActors getMe epochToIso8601)

export _root_.Taxis (Issue IssueDetail IssueInput IssueUpdate IssueState Label LabelId Comment
  ReviewState Actor ActorId Artifact ArtifactId ArtifactView ArtifactDisplay)

/-! ## Active configuration

`Orchestra.Project.Basic`'s functions (`loadProject`, `saveIssue`, ...) keep the same signatures
they had as file-backed operations — no `Config` parameter — so they read the active taxis config
from this process-wide ref instead, exactly mirroring the pre-migration `projectsDirOverride`
pattern in `Project.Basic`. Set once at startup from `AppConfig.taxis`; tests override it directly
via `setConfig` instead of pointing at a temp directory. -/
initialize configRef : IO.Ref (Option Config) ← IO.mkRef none

def setConfig (cfg : Option Config) : IO Unit := configRef.set cfg

/-- The active taxis config, or an error if none is set — callers throw this straight through
    (unconfigured taxis is a hard error, not a silently-skipped feature, once anything tries to
    use it). -/
def getConfig : IO Config := do
  match ← configRef.get with
  | some cfg => pure cfg
  | none => throw (.userError
      "taxis is not configured — set a \"taxis\": {\"url\": ..., \"token\": ...} section in config.json")

/-- The active taxis config, writing as `asToken`'s owner when a token is given.

    This is how a task run under an identity reaches the tracker as that identity: same instance,
    same everything else, a different bearer token, so taxis attributes the write to the actor the
    token belongs to (`Orchestra.Identity`).

    It is a per-call argument rather than a second ref because it cannot be process-wide. Several
    tasks run at once inside one daemon, each possibly under a different identity, and they share
    this process — a ref swapped for the duration of a call would be swapped underneath every
    other task running at the time. -/
def getConfigAs (asToken : Option String) : IO Config := do
  let cfg ← getConfig
  match asToken with
  | some token => return { cfg with token := some token }
  | none       => return cfg

end Orchestra.Taxis
