import Lake
open Lake DSL

package orchestra where
  version := v!"0.1.0"
  testDriver := "orchestraTest"

require Cli from git "https://github.com/leanprover/lean4-cli.git" @ "main"
require Yaml from git "https://github.com/chrisflav/lean-yaml" @ "278306ce5603c5120adf51409cf71f3990ce6859"
require verso from git "https://github.com/leanprover/verso" @ "170af40e495d97361a59fcf8e8295a4f26116aea"

/-- Compile the Unix domain socket C shim into a static library. -/
extern_lib udsFFI pkg := do
  let cFile := pkg.dir / "ffi" / "UnixSocket.c"
  let cSrc  ← inputTextFile cFile
  let oFile := pkg.buildDir / "ffi" / "UnixSocket.o"
  let oJob  ← buildFileAfterDep oFile cSrc fun _ => do
    compileO oFile cFile #["-I", (← getLeanIncludeDir).toString, "-fPIC"]
  let libFile := pkg.buildDir / "lib" / nameToStaticLib "udsFFI"
  buildFileAfterDep libFile oJob fun oFile => do
    compileStaticLib libFile #[oFile]

/-- Front-end CSS/JS shipped with the dashboard. Declared as an input
    directory so `lake build` rebuilds `Orchestra.Dashboard` whenever a
    file inside it changes (the `include_str` calls in `Orchestra/Dashboard.lean`
    are otherwise invisible to Lake's dependency tracker). -/
input_dir dashboardAssets where
  text := true
  path := "Orchestra/Dashboard"

@[default_target]
lean_lib Orchestra where
  needs := #[dashboardAssets]

lean_lib OrchestraTest

@[default_target]
lean_exe orchestra where
  root := `Main

lean_exe orchestraTest where
  root := `TestDriver
