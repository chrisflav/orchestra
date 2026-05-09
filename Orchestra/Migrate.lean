import Orchestra.Dirs

namespace Orchestra.Migrate

private def runCp (args : Array String) : IO Unit := do
  let child ← IO.Process.spawn {
    cmd    := "cp"
    args
    stdout := .piped
    stderr := .piped
  }
  let stderr ← child.stderr.readToEnd
  let code   ← child.wait
  unless code == 0 do
    throw (.userError s!"cp failed: {stderr.trimAscii}")

private def copyDir (src dst : System.FilePath) (label : String) : IO Unit := do
  if !(← src.pathExists) then
    IO.println s!"  skip  {label} (not found)"; return
  if ← dst.pathExists then
    IO.println s!"  skip  {label} (already exists at destination)"; return
  IO.println s!"  copy  {label}"
  IO.FS.createDirAll dst
  runCp #["-r", (src / ".").toString, dst.toString]

private def copyFile (src dst : System.FilePath) (label : String) : IO Unit := do
  if !(← src.pathExists) then
    IO.println s!"  skip  {label} (not found)"; return
  if ← dst.pathExists then
    IO.println s!"  skip  {label} (already exists at destination)"; return
  IO.println s!"  copy  {label}"
  runCp #[src.toString, dst.toString]

private def copyJsonFiles (src dst : System.FilePath) (label : String) : IO Unit := do
  if !(← src.pathExists) then
    IO.println s!"  skip  {label} (not found)"; return
  let entries ← src.readDir
  let jsonEntries := entries.filter (fun e => e.fileName.endsWith ".json")
  if jsonEntries.isEmpty then
    IO.println s!"  skip  {label} (no .json files)"; return
  IO.FS.createDirAll dst
  let mut copied := 0
  for e in jsonEntries do
    let destFile := dst / e.fileName
    unless ← destFile.pathExists do
      runCp #[e.path.toString, destFile.toString]
      copied := copied + 1
  let skipped := jsonEntries.size - copied
  let note := if skipped > 0 then s!" ({skipped} already at destination)" else ""
  IO.println s!"  copy  {label} ({copied} files{note})"

def run : IO Unit := do
  let home ← match ← IO.getEnv "HOME" with
    | some h => pure (System.FilePath.mk h)
    | none   => throw (.userError "HOME not set")
  let legacy     := home / ".agent"
  let configDest ← Dirs.orchestraConfigDir
  let dataDest   ← Dirs.orchestraDataDir

  unless ← legacy.pathExists do
    IO.println s!"Nothing to migrate: '{legacy}' does not exist."
    return

  IO.println s!"Migrating from '{legacy}'"
  IO.println s!"  Config → {configDest}"
  IO.println s!"  Data   → {dataDest}"
  IO.println ""

  IO.FS.createDirAll configDest
  IO.FS.createDirAll dataDest

  IO.println "Config:"
  copyFile      (legacy / "config.json") (configDest / "config.json") "config.json"
  copyJsonFiles (legacy / "listeners")   (configDest / "listeners")   "listeners/*.json"
  copyDir       (legacy / "prompts")     (configDest / "prompts")     "prompts/"
  copyDir       (legacy / "roles")       (configDest / "roles")       "roles/"

  IO.println ""
  IO.println "State:"
  copyDir (legacy / "tasks")               (dataDest / "tasks")               "tasks/"
  copyDir (legacy / "queue")               (dataDest / "queue")               "queue/"
  copyDir (legacy / "series")              (dataDest / "series")              "series/"
  copyDir (legacy / "concerts")            (dataDest / "concerts")            "concerts/"
  copyDir (legacy / "listeners" / "state") (dataDest / "listeners" / "state") "listeners/state/"
  copyDir (legacy / "repos")               (dataDest / "repos")               "repos/"
  copyDir (legacy / "memory")              (dataDest / "memory")              "memory/"
  copyDir (legacy / "logs")                (dataDest / "logs")                "logs/"
  copyDir (legacy / "projects")            (dataDest / "projects")            "projects/"

  IO.println ""
  IO.println s!"Done. '{legacy}' has been left in place."
  IO.println s!"After verifying the new setup, you may remove it with:  rm -rf {legacy}"

end Orchestra.Migrate
