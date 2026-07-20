import Lake
open Lake DSL

package orchestra where
  version := v!"0.1.0"
  testDriver := "orchestraTest"

require Cli from git "https://github.com/leanprover/lean4-cli.git" @ "main"
require Yaml from git "https://github.com/chrisflav/lean-yaml" @ "master"
require Taxis from git "https://github.com/chrisflav/taxis" @ "388642241531548eb45218c091d785aa891dc573"

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

@[default_target]
lean_exe orchestra where
  root := `Main

lean_exe orchestraTest where
  root := `TestDriver
