/-
Relabelling arithmetic, shared by the two trackers orchestra labels things in.

`Orchestra.GitHub` grew the original version of this against api.github.com; it is lifted here
so the taxis side (`Orchestra.Project.Basic.setIssueLabels`) can reuse it instead of keeping a
second copy of the same rules. The two write labels very differently — GitHub takes one call per
label, taxis takes the whole set in a `PATCH` — but *deciding* what a relabelling request comes
down to is the same problem in both, and it is the part worth getting right once: matching the
tracker's spelling, refusing a name it does not define, and reporting the halves of the request
that turned out to be nothing to do.

Pure, so the whole mapping is tested without a network (`OrchestraTest.LabelIssue`).
-/

namespace Orchestra.Utils.Labels

/-- What relabelling an issue actually changes, once the labels it already carries are taken
    into account. `add` and `remove` are the calls that will be made; the other two fields are
    the requests that turned out to be nothing to do, kept so they can be reported rather than
    silently dropped. -/
structure LabelChange where
  /-- Labels to add, spelled as the tracker spells them. None is already on the issue. -/
  add : List String := []
  /-- Labels to remove, spelled as the tracker spells them. All are on the issue. -/
  remove : List String := []
  /-- Requested additions the issue already carried. -/
  alreadyPresent : List String := []
  /-- Requested removals the issue did not carry. -/
  notPresent : List String := []
deriving Repr, Inhabited, DecidableEq

private def dedup (xs : List String) : List String :=
  xs.foldl (fun acc x => if acc.contains x then acc else acc ++ [x]) []

/-- Work out which label calls a relabelling request actually needs, or why it cannot be served.
    `known` is the tracker's labels, `current` the ones already on the issue.

    Names are matched case-insensitively and answered in the tracker's spelling, because an
    agent asking for `T-Feature` means the `t-feature` the repository defines and GitHub would
    create a second label rather than say so.

    A name the tracker does not define is an error, not a label to create: the point of
    triage is to sort into the vocabulary a project already has, and `create_pr`'s label
    auto-creation exists for labels the *configuration* names, not ones an agent invented.

    `owner` names whatever holds that vocabulary, since the refusal quotes it back and the two
    callers do not share one — a taxis label belongs to the tracker as a whole, a GitHub label
    to one repository. -/
def planLabelChange (known current add remove : List String)
    (owner : String := "the repository") : Except String LabelChange :=
  let canon? (name : String) : Option String :=
    known.find? (·.toLower == name.toLower)
  let unknown := dedup ((add ++ remove).filter (canon? · |>.isNone))
  if !unknown.isEmpty then
    let names := String.intercalate ", " unknown
    let vocabulary :=
      if known.isEmpty then s!"{owner} defines no labels at all"
      else s!"{owner} defines: {String.intercalate ", " known}"
    .error s!"no such label: {names} — {vocabulary}"
  else
    let canonAdd := dedup (add.filterMap canon?)
    let canonRemove := dedup (remove.filterMap canon?)
    let contradictory := canonAdd.filter canonRemove.contains
    if !contradictory.isEmpty then
      .error s!"asked to both add and remove {String.intercalate ", " contradictory}"
    else
      let present (label : String) : Bool := current.any (·.toLower == label.toLower)
      .ok {
        add            := canonAdd.filter (!present ·)
        remove         := canonRemove.filter present
        alreadyPresent := canonAdd.filter present
        notPresent     := canonRemove.filter (!present ·)
      }

/-- One line saying what the relabelling did, for whoever asked for it. `subject` names the
    issue, e.g. `owner/repo#12`. -/
def LabelChange.summary (change : LabelChange) (subject : String) : String :=
  let added := String.intercalate ", " change.add
  let removed := String.intercalate ", " change.remove
  let had := String.intercalate ", " change.alreadyPresent
  let lacked := String.intercalate ", " change.notPresent
  let done :=
    (if change.add.isEmpty then [] else [s!"added {added}"]) ++
    (if change.remove.isEmpty then [] else [s!"removed {removed}"])
  let notes :=
    (if change.alreadyPresent.isEmpty then [] else [s!"already had {had}"]) ++
    (if change.notPresent.isEmpty then [] else [s!"did not have {lacked}"])
  let head :=
    if done.isEmpty then s!"{subject}: nothing to change"
    else s!"{subject}: {String.intercalate "; " done}"
  if notes.isEmpty then head else s!"{head} (it {String.intercalate "; " notes})"

end Orchestra.Utils.Labels
