/-!
# Landlock scopes around landrun

landrun confines the filesystem and TCP ports, and leaves two things open that Landlock can also
close since ABI 6 (Linux 6.12): connecting to an *abstract* Unix socket — an X server's
`@/tmp/.X11-unix/X0`, a container runtime's command socket — and signalling processes outside the
sandbox. landrun 0.1.15 has no flag for either. So orchestra applies those two scopes itself, in a
process that then executes landrun: `orchestrad __landlock-scope -- landrun …` (and the same
subcommand in `orchestra`). Landlock layers stack, so whatever landrun adds the agent stays in this
domain too. See `ffi/Landlock.c`.

The subcommand is handled before anything else in `main`, so it costs neither binary a dependency:
it parses nothing, opens no database, reads no configuration.
-/

namespace Orchestra.Exec.Scope

/-- The Landlock ABI version this kernel offers, or `0` when it offers none. -/
@[extern "lean_orchestra_landlock_abi"]
opaque landlockAbi : IO UInt32

/-- Restrict this process to the abstract-socket and signal scopes and execute `argv` (the first
    element looked up on `PATH`). Returns only by throwing. -/
@[extern "lean_orchestra_scope_exec"]
opaque scopeExec (argv : @& Array String) : IO Unit

/-- The subcommand both binaries recognise as their first argument. Not a user-facing command:
    the landrun backend puts it in front of `landrun` (`Landrun.argvWith`). -/
def subcommand : String := "__landlock-scope"

/-- Whether this kernel has the scopes (ABI 6 or later). -/
def supported : IO Bool := return (← landlockAbi) ≥ 6

/-- Handle `args` if they are the scope subcommand — `__landlock-scope -- <command> <args…>` —
    by executing the command inside the scopes, and return `none` otherwise. Called first thing in
    each binary's `main`; on success it does not return at all. -/
def handle? (args : List String) : IO (Option UInt32) := do
  match args with
  | sub :: "--" :: cmd :: rest =>
    if sub != subcommand then return none
    try
      scopeExec (cmd :: rest).toArray
      return some 127
    catch e =>
      IO.eprintln s!"orchestra: {e}"
      return some 126
  | sub :: _ =>
    if sub != subcommand then return none
    IO.eprintln s!"orchestra: usage: {subcommand} -- <command> [args...]"
    return some 2
  | [] => return none

end Orchestra.Exec.Scope
