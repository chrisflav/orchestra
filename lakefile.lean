import Lake
open System Lake DSL

/-- Run `cmd args` and return its trimmed standard output, or `none` if it is not installed or
    fails. -/
private def toolOutput? (cmd : String) (args : Array String) : IO (Option String) := do
  let out ← IO.Process.output { cmd, args }
    |>.catchExceptions fun _ => pure { exitCode := 1, stdout := "", stderr := "" }
  let text := out.stdout.trimAscii.toString
  if out.exitCode == 0 && !text.isEmpty then return some text else return none

/-- The linker arguments the record store needs: the absolute path of libpq, discovered by
    asking the tools that know where it is.

    Copied from `db`'s own lakefile rather than imported, because Lake does not propagate a
    dependency's link arguments to the packages that depend on it — only the FFI object, which
    then has nothing to resolve its `PQ*` calls against. Discovered rather than written down
    because this is built in two places that disagree about where libpq lives: a nix store path
    on the development host, `/usr/lib/<triple>` in the Debian-based image `docker/Dockerfile`
    produces.

    An absolute path rather than `-L<dir> -lpq`: the Lean toolchain ships its own clang and C
    runtime, and putting the system library directory on its search path makes it resolve glibc
    there too, which then fails to link against the toolchain's own `Scrt1.o`. Naming the one
    library takes libpq without the directory around it.

    `pkg-config` is asked before `pg_config`, which is where this differs from the copy in `db`.
    nixpkgs' libpq ships no `pg_config` at all — it is part of the server package, not the
    client library — so the pg_config-only version finds nothing on the development host and
    falls through to `-lpq`, which then fails to link for the reason in the paragraph above. The
    `.pc` file is present in both places this builds.

    Falls back to `-lpq` when neither tool knows, which is right wherever libpq already sits
    somewhere the linker searches. -/
private def libpqLinkArgs : IO (Array String) := do
  let dirs := (← toolOutput? "pkg-config" #["--variable=libdir", "libpq"]).toArray
    ++ (← toolOutput? "pg_config" #["--libdir"]).toArray
  for dir in dirs do
    for ext in ["so", "dylib", "a"] do
      let candidate : FilePath := FilePath.mk dir / s!"libpq.{ext}"
      if ← candidate.pathExists then
        return #[candidate.toString]
  return #["-lpq"]

package orchestra where
  version := v!"0.1.0"
  testDriver := "orchestraTest"

require Cli from git "https://github.com/leanprover/lean4-cli.git" @ "main"
require Yaml from git "https://github.com/chrisflav/lean-yaml" @ "master"
require Taxis from git "https://github.com/chrisflav/taxis" @ "d029b06a912a39fef52734ef90f29537561c7785"

-- The record store's database layer: `Orchestra.Store` is written against it. Pinned to the
-- revision of its master that closes a PostgreSQL connection when it is done with it.
--
-- `postgres = "on"` builds the libpq FFI shim, which is off by default so that a package using
-- only the vendored SQLite needs no PostgreSQL headers. Orchestra keeps its records in
-- PostgreSQL (see `Orchestra/Store/Connection.lean`), so it needs both the shim and, on every
-- executable that calls into it, the libpq to resolve it against.
require db from git "https://github.com/chrisflav/db" @ "2361cf0994610ebb7ad32770ffa042c860338d0f"
  with NameMap.empty.insert `postgres "on"

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
  moreLinkArgs := run_io libpqLinkArgs

/-- The backend. The queue daemon and the HTTP API, in one binary that runs continuously and
    holds the credentials. Separate from `orchestra` so that neither ships the other's job:
    see the module docs in `Orchestrad.lean`. -/
@[default_target]
lean_exe orchestrad where
  root := `Orchestrad
  moreLinkArgs := run_io libpqLinkArgs

lean_exe orchestraTest where
  root := `TestDriver
  moreLinkArgs := run_io libpqLinkArgs

/-- The one-off that carries an installation's records out of the SQLite file they used to be
    kept in and into PostgreSQL. Its own executable rather than a subcommand because it is the
    only thing left that opens SQLite, and neither the daemon nor the client should carry a
    second database driver for a migration each installation runs once. Not a default target for
    the same reason: it is built when it is needed. -/
lean_exe «orchestra-migrate-sqlite» where
  root := `MigrateSqlite
  moreLinkArgs := run_io libpqLinkArgs
