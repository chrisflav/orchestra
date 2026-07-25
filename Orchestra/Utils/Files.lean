namespace Orchestra.Utils

/-- Write `contents` to `path` so that no reader ever sees a half-written file.

    Every configuration file the API writes is read by a process that is not the one writing it:
    the daemon re-reads listener configs on each tick, and role and skill files are read when a
    task is dispatched. A plain `IO.FS.writeFile` truncates first, so a reader landing in that
    window gets an empty or partial file and — for the daemon — a listener that silently stops
    parsing. Writing a sibling temp file and renaming avoids the window entirely: `rename` within
    one directory is atomic, so a reader sees either the old file or the new one.

    The temp file carries the process id so two writers cannot collide on it. -/
def writeFileAtomically (path : System.FilePath) (contents : String) : IO Unit := do
  let dir := path.parent.getD "."
  IO.FS.createDirAll dir
  let tmp := System.FilePath.mk s!"{path}.{← IO.Process.getPID}.tmp"
  try
    IO.FS.writeFile tmp contents
    IO.FS.rename tmp path
  catch e =>
    try IO.FS.removeFile tmp catch _ => pure ()
    throw e

/-- Whether `name` may be used as the filename of a configuration record.

    Listeners, roles and skills are all stored one-per-file under a directory, named by the value
    the user chose. That makes the name a path component, so it has to be one that can only name
    itself: no separator, no dot-segment, no control character, and nothing empty. The check is
    here rather than at the HTTP boundary because it is a property of the store — the CLI writes
    through the same functions and must be held to the same rule.

    Deliberately narrower than "what the filesystem accepts": a leading `.` would hide the file
    from every listing that skips dotfiles, and a name with a newline in it cannot be printed in
    the tabular output the CLI uses. -/
def validConfigName (name : String) : Bool :=
  !name.isEmpty
    && !name.startsWith "."
    && !name.any (fun c => c == '/' || c == '\\' || c.toNat < 0x20 || c.toNat == 0x7f)

/-- `validConfigName` as an error message, so every caller rejects with the same sentence. -/
def checkConfigName (what name : String) : Except String Unit :=
  if validConfigName name then .ok ()
  else .error s!"'{name}' is not a usable {what} name: it must be non-empty, must not begin \
with '.', and must not contain a path separator or a control character"

/-- `checkConfigName` as a throw, for the store functions themselves to stand on.

    The validators run this and report it as a `400`, which is where a person sees it. This is
    the backstop underneath them: every function that turns a name into a path calls it, so the
    property above is one the *store* holds rather than one each caller is trusted to have
    established. A name that reaches here unchecked is a bug in a caller, which is why it throws
    rather than returning — there is no sensible way to continue, and a 500 naming the file that
    was refused beats a file written somewhere nobody was looking.

    This is not hypothetical. The first version of the skill store validated the front matter and
    forgot this check, and because a `POST` names its record from the request *body* rather than
    from a path segment, nothing upstream had rejected a traversal either: `{"name": "../../…"}`
    wrote a `SKILL.md` outside the skills root and answered `201`. Two of the three stores were
    correct and the third was not, which is exactly the shape of mistake a shared backstop
    removes. -/
def ensureConfigName (what name : String) : IO Unit :=
  match checkConfigName what name with
  | .ok _    => pure ()
  | .error e => throw (.userError e)

end Orchestra.Utils
