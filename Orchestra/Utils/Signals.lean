/-
Termination-signal handling for the queue daemon.

The daemon runs as PID 1 in the container image, so `docker stop` delivers SIGTERM to it directly
and Lean installs no handler. PID 1 is exempt from signals carrying the default disposition, so
SIGTERM is ignored outright and Docker falls back to SIGKILL once the grace period expires —
taking in-flight agent tasks with it and leaving their queue entries stuck in `running`.
Installing a handler makes the signal deliverable, and routes it into the same graceful drain as
`orchestra queue shutdown`.

The C side only bumps a counter (nothing else is async-signal-safe); callers poll `count` from an
ordinary thread and act there.
-/

namespace Orchestra.Utils.Signals

@[extern "lean_orchestra_install_signal_handlers"]
private opaque installHandlers : IO Unit

@[extern "lean_orchestra_signal_count"]
private opaque signalCount : IO UInt32

/-- Start catching SIGTERM and SIGINT. Idempotent. -/
def install : IO Unit := installHandlers

/-- Number of termination signals received so far. The daemon reads the first as "drain and
    exit" and a second as "stop now", mirroring `queue shutdown` and `queue shutdown --force`. -/
def count : IO UInt32 := signalCount

end Orchestra.Utils.Signals
