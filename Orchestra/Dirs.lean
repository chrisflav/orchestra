namespace Orchestra.Dirs

private def getHome : IO System.FilePath := do
  match ← IO.getEnv "HOME" with
  | some h => return System.FilePath.mk h
  | none   => throw (.userError "HOME not set")

/-- XDG config base: $XDG_CONFIG_HOME/orchestra or ~/.config/orchestra -/
def orchestraConfigDir : IO System.FilePath := do
  match ← IO.getEnv "XDG_CONFIG_HOME" with
  | some x => return System.FilePath.mk x / "orchestra"
  | none   => return (← getHome) / ".config" / "orchestra"

/-- XDG data base: $XDG_DATA_HOME/orchestra or ~/.local/share/orchestra -/
def orchestraDataDir : IO System.FilePath := do
  match ← IO.getEnv "XDG_DATA_HOME" with
  | some x => return System.FilePath.mk x / "orchestra"
  | none   => return (← getHome) / ".local" / "share" / "orchestra"

private initialize deprecationWarned : IO.Ref Bool ← IO.mkRef false

/--
Config base with legacy fallback.
Returns `orchestraConfigDir` if it already exists.
Falls back to `~/.agent/` with a one-time deprecation warning if the XDG dir is
absent but the legacy dir is present. Returns `orchestraConfigDir` on a fresh install
(neither exists yet).
-/
def configBase : IO System.FilePath := do
  let xdgDir ← orchestraConfigDir
  if ← xdgDir.pathExists then return xdgDir
  let legacy := (← getHome) / ".agent"
  if ← legacy.pathExists then
    unless ← deprecationWarned.get do
      IO.eprintln s!"[orchestra] WARNING: Config not found at '{xdgDir}'; \
        falling back to '{legacy}'. Run 'orchestra migrate' to upgrade."
      deprecationWarned.set true
    return legacy
  return xdgDir

private initialize dataDirOverride : IO.Ref (Option System.FilePath) ← IO.mkRef none

/-- Override the data base directory (for testing). -/
def setDataDirOverride (p : Option System.FilePath) : IO Unit :=
  dataDirOverride.set p

/-- Data base: always uses the XDG data dir, no legacy fallback. -/
def dataBase : IO System.FilePath := do
  match ← dataDirOverride.get with
  | some p => return p
  | none   => orchestraDataDir

end Orchestra.Dirs
