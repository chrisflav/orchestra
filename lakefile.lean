import Lake
open Lake DSL

package orchestra where
  version := v!"0.1.0"
  testDriver := "orchestraTest"

require Cli from git "https://github.com/leanprover/lean4-cli.git" @ "main"
require Yaml from git "https://github.com/chrisflav/lean-yaml" @ "master"

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

@[default_target]
lean_lib Orchestra

lean_lib OrchestraTest

@[default_target]
lean_exe orchestra where
  root := `Main

@[default_target]
lean_exe «orchestra-mcp-test» where
  root := `McpTest

lean_exe orchestraTest where
  root := `TestDriver
