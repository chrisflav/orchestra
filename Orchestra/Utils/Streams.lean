namespace Orchestra.Utils

/-- Wrap a stream so that every write is flushed immediately.

    Lean block-buffers its output streams when they are not a TTY. A long-running process writing
    to a pipe therefore produces *nothing* until it exits or fills the buffer — so the queue
    daemon, which is meant to run for days, shows no output at all under `docker compose logs`,
    systemd's journal, `tee`, or anything else that isn't a terminal. Short-lived commands hide
    the problem by flushing on exit.

    Only applied when the stream is not already a TTY: an interactive terminal is line-buffered
    by default and does not need a flush per write. -/
private def autoFlushing (s : IO.FS.Stream) : IO.FS.Stream :=
  { s with
    write  := fun bs  => do s.write bs;  s.flush
    putStr := fun str => do s.putStr str; s.flush }

/-- Make stdout and stderr unbuffered when they are pipes.

    Shared by both binaries: the server needs it because it is the long-running one, and the CLI
    needs it because its output is routinely piped into something that reads incrementally. -/
def unbufferIfPiped : IO Unit := do
  let out ← IO.getStdout
  unless ← out.isTty do discard <| IO.setStdout (autoFlushing out)
  let err ← IO.getStderr
  unless ← err.isTty do discard <| IO.setStderr (autoFlushing err)

end Orchestra.Utils
