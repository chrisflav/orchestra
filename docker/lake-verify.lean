/-
Check files in Lake's artifact cache against their names, with Lake's own hash.

    lean --run lake-verify.lean < paths

Reads one path per line on stdin, each a file in `<LAKE_CACHE_DIR>/artifacts` named
`<16-hex hash>[.ext]`, and prints the path of every one whose content does not hash to its name,
or that cannot be read. An artifact's name is `Hash.ofByteArray` of its bytes (Lake/Config/Cache.lean
`saveArtifact`; `downloadArtifactCore` checks a download the same way), or for one Lake cached as
text, `Hash.ofString` of its line-ending-normalised contents -- either is accepted. A name that is
not a hash is left alone. Run by lean-cache-warm with the toolchain the artifacts were built by,
whose Lake this imports, so the hash is exactly Lake's.
-/
import Lake
open Lake

def bad (path : String) : IO Bool := do
  let name := (System.FilePath.mk path).fileName.getD ""
  let some want := Hash.ofString? (name.take 16).toString | return false
  try
    let bytes ← IO.FS.readBinFile path
    if Hash.ofByteArray bytes == want then return false
    match String.fromUTF8? bytes with
    | some s => return Hash.ofText s != want
    | none => return true
  catch _ =>
    -- Gone since it was listed (pruned, or replaced): nothing to check.
    return !(← System.FilePath.pathExists path)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  repeat
    let line ← stdin.getLine
    if line.isEmpty then break
    let path := line.trimAsciiEnd.toString
    unless path.isEmpty do
      if ← bad path then
        stdout.putStrLn path
        stdout.flush
