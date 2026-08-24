import Lake
open Lake DSL

package orchestra where
  version := v!"0.1.0"
  testDriver := "orchestraTest"

require Cli from git "https://github.com/leanprover/lean4-cli.git" @ "main"
require Yaml from git "https://github.com/chrisflav/lean-yaml" @ "master"
require Taxis from git "https://github.com/chrisflav/taxis" @ "d029b06a912a39fef52734ef90f29537561c7785"

/-- Compile a single C shim under `ffi/` into a static library of the same name. -/
private def ffiStaticLib (pkg : Package) (name : String) : FetchM (Job System.FilePath) := do
  let cFile := pkg.dir / "ffi" / s!"{name}.c"
  let cSrc  ← inputTextFile cFile
  let oFile := pkg.buildDir / "ffi" / s!"{name}.o"
  let oJob  ← buildFileAfterDep oFile cSrc fun _ => do
    compileO oFile cFile #["-I", (← getLeanIncludeDir).toString, "-fPIC"]
  let libFile := pkg.buildDir / "lib" / nameToStaticLib name
  liftM <| buildFileAfterDep libFile oJob fun oFile => do
    compileStaticLib libFile #[oFile]

/-- Unix domain socket shim, backing `Orchestra.Utils.UnixSocket`. -/
extern_lib UnixSocket pkg := ffiStaticLib pkg "UnixSocket"

/-- Termination-signal shim, backing `Orchestra.Utils.Signals`. -/
extern_lib Signal pkg := ffiStaticLib pkg "Signal"

@[default_target]
lean_lib Orchestra

lean_lib OrchestraTest

/-- The client. Everything a person types: one-shot runs, the interactive sandbox, and the
    commands that read and change orchestra's configuration — which since taxis #433 go over the
    HTTP API rather than touching the daemon's state directly. -/
@[default_target]
lean_exe orchestra where
  root := `Main

/-- The backend. The queue daemon and the HTTP API, in one binary that runs continuously and
    holds the credentials. Separate from `orchestra` so that neither ships the other's job:
    see the module docs in `Orchestrad.lean`. -/
@[default_target]
lean_exe orchestrad where
  root := `Orchestrad

lean_exe orchestraTest where
  root := `TestDriver
