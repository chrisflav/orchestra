import Orchestra.Exec.Spec

/-!
# Runs that outlive the daemon: the in-pod run helper

A `kubectl exec` is a stream from the daemon into the pod, and everything orchestra ran in a task's
pod used to *be* one: the agent, `validation.sh`, the hooks. A stream is only as long-lived as the
two processes at its ends, and the daemon is restarted with every deploy — so a restart killed the
agent mid-turn and the build `validation.sh` was half way through, and the best the next daemon
could do was resume the conversation in a new pod (`Queue.resumeInterrupted`).

The fix is to stop making the command the far end of the stream. A small POSIX `sh` script,
`runnerScript`, is staged into the pod's control directory when the session opens, and every
command orchestra runs there is started *through* it: detached, in a session of its own, with its
output going to files in the control directory rather than to the connection. The daemon then
reads those files with a second exec that only *follows* them. That exec can die — the daemon
restarting, an API server dropping an idle stream — and nothing happens to the command; a new
daemon, or the same one after a dropped connection, follows the same files again.

```
  daemon ──exec── run start <id> -- <cmd…>      returns at once
                    └─ setsid wrapper ─ setsid <cmd…>   <in  >runs/<id>/out  2>runs/<id>/err
                                        on exit: runs/<id>/exit (written atomically)
  daemon ──exec── run attach <id> out <offset>  streams out from offset, until exit and drained
  daemon ──exec── run attach <id> err <offset>  the same for stderr
  daemon ──exec── run status | kill | term | write | close-stdin <id>
```

The script is staged by orchestra itself rather than baked into the agent image: a task runs in
whatever image its repository asked for, and all this needs from it is `sh`, `setsid` (optional —
without it a cancellation reaches the command and not its children), `mkfifo`, `tail`, `head` and
`wc`.

## why the run id is chosen by the daemon

`start` is idempotent per id: starting an id that already exists does nothing and says
`existing <bytes of stdout so far>`. Ids are deterministic in the task — `agent-0`, `validate-0`,
`before-1`, `after` — so a daemon that comes back to a task re-executes the step it was at, and
the step finds its command already running (or already finished, with its exit code and output on
disk) instead of starting a second one. That is what lets a restart resume the task loop by
replaying it (`TaskRunner`) rather than by saving every variable the loop holds.

## following from an offset, and why the daemon re-reads from zero

`attach` takes a byte offset so that a dropped connection is picked up exactly where it stopped:
the daemon-side `followScript` counts what it has passed on and reattaches from there, so the agent's
stream reaches the parser whole whether the connection held or not. A daemon that *restarts*
attaches from zero instead and parses the whole stream again. That is cheap — it is the same
work it did the first time, on a file — and it rebuilds exactly the state the old daemon held (the
session id, the result, a usage limit) without any of it having to be persisted. Nothing the
parser does is counted twice: the stream feeds the task's log file (rewritten whole, which leaves
it complete across the restart), the console (`Handle.replayBytes` keeps the replayed part off it)
and a handful of idempotent records. Usage is not tallied from the stream at all.

## stdin

A command started `--stdin` reads from a FIFO, `runs/<id>/in`, whose write end is held open by a
small holder process for as long as the run lives. The daemon writes to it with `write <id>`; when
the daemon goes away its writer closes, and the holder is what keeps that from reaching the command
as EOF. `close-stdin` stops the holder, which is how "no more turns" is said deliberately. Queued
agents take their prompt as an argument and run with no stdin; the FIFO is there for a streaming
session, which today still runs as a plain exec (`docs/kubernetes.md`).
-/

namespace Orchestra.Exec.Runner

/-- The version of the protocol below. Part of the file name the script is staged under, so a
    daemon never runs a different version's script in a pod a previous daemon set up — and never
    overwrites a script a running wrapper is still reading. -/
def protocol : Nat := 2

/-- Where the run helper lives in a pod, given the control directory. -/
def runnerPath (controlDir : String) : String := s!"{controlDir}/run-{protocol}"

/-- The run helper. POSIX `sh`, tested under dash and busybox (`OrchestraTest.ReattachTest` runs it).

    Files per run, under `runs/<id>/` beside the script: `out`, `err`, `pid` (the command's process
    id — and, with `setsid`, its process group), `wrapper` (the wrapper's), `starter` (the `start`
    that created the directory, so a second `start` takes over only one that died before it
    launched anything), `exit` (its exit status, written by the wrapper when the command ends:
    `mv`d into place, so a reader sees it whole or not at all), and with `--stdin` the FIFO `in`
    and the holder's pid in `holder`.

    A run whose wrapper and command are both gone with no `exit` on disk — `kill -9`, the OOM
    killer — is settled by whoever looks next (`status`, `attach`): `exit` is written as 255, so
    nothing waits for a run that can no longer end. -/
def runnerScript : String := "# orchestra's in-pod run helper (protocol 2). Staged by the daemon; run as `sh <this file> ...`.
# See Orchestra/Exec/Runner.lean for what each subcommand promises.
set -u
self=$0
runs=$(dirname \"$self\")/runs
die() { echo \"orchestra-run: $*\" >&2; exit 125; }
nap() { sleep \"$1\" 2>/dev/null || sleep 1; }
detach() {
  if command -v setsid >/dev/null 2>&1; then setsid \"$@\"; else nohup \"$@\"; fi
}
dir_of() {
  case $1 in ''|*/*|.*) die \"bad run id '$1'\" ;; esac
  d=$runs/$1
}
size_of() { if [ -f \"$1\" ]; then wc -c <\"$1\" | tr -d ' '; else echo 0; fi; }
put() { printf '%s\\n' \"$2\" >\"$1.tmp\" && mv \"$1.tmp\" \"$1\"; }
alive() { [ -n \"$1\" ] && kill -0 \"$1\" 2>/dev/null; }
# A run whose wrapper and command are both gone without an exit status on disk was killed from
# outside (kill -9, the OOM killer): record that, so nobody waits for it forever.
settle() {
  [ -f \"$d/exit\" ] && return 0
  [ -f \"$d/pid\" ] || return 0
  if ! alive \"$(cat \"$d/wrapper\" 2>/dev/null)\" && ! alive \"$(cat \"$d/pid\" 2>/dev/null)\"; then
    [ -f \"$d/exit\" ] || put \"$d/exit\" 255
  fi
}

# start <id> [--stdin] -- <cmd...>: start the command detached, once per id.
cmd_start() {
  dir_of \"$1\"; shift
  stdin=
  while [ $# -gt 0 ]; do
    case $1 in
      --stdin) stdin=1; shift ;;
      --) shift; break ;;
      *) die \"start: unexpected '$1'\" ;;
    esac
  done
  [ $# -gt 0 ] || die \"start: no command\"
  mkdir -p \"$runs\" || die \"start: cannot create $runs\"
  if ! mkdir \"$d\" 2>/dev/null; then
    # Started before: by a daemon that has since gone away, or by this very request retried.
    # Starting it again would be two of it. Wait out a start still in progress.
    i=0
    while [ ! -f \"$d/pid\" ] && [ ! -f \"$d/exit\" ] && [ $i -lt 50 ]; do nap 0.2; i=$((i + 1)); done
    if [ -f \"$d/pid\" ] || [ -f \"$d/exit\" ]; then echo \"existing $(size_of \"$d/out\")\"; exit 0; fi
    # A start that is still going (its starter alive) is not taken over; one that died before it
    # launched anything left nothing running, so it is done again.
    if alive \"$(cat \"$d/starter\" 2>/dev/null)\"; then die \"start: this run is being started by another request\"; fi
    rm -rf \"$d\"
    mkdir \"$d\" || die \"start: cannot create $d\"
  fi
  put \"$d/starter\" \"$$\"
  if [ -n \"$stdin\" ]; then
    mkfifo \"$d/in\" || die \"start: cannot create $d/in\"
    detach sh \"$self\" _hold \"$d\" </dev/null >/dev/null 2>&1 &
    i=0
    while [ ! -f \"$d/holder\" ] && [ $i -lt 100 ]; do nap 0.1; i=$((i + 1)); done
    [ -f \"$d/holder\" ] || die \"start: the stdin holder did not come up\"
  fi
  detach sh \"$self\" _wrap \"$d\" \"$@\" </dev/null >/dev/null 2>&1 &
  i=0
  while [ ! -f \"$d/pid\" ] && [ ! -f \"$d/exit\" ] && [ $i -lt 100 ]; do nap 0.1; i=$((i + 1)); done
  [ -f \"$d/pid\" ] || [ -f \"$d/exit\" ] || die \"start: the command did not come up\"
  echo started
}

# The holder: keeps a write end of the FIFO open, so a daemon that goes away is not an EOF.
cmd_hold() {
  d=$1
  exec 3<>\"$d/in\"
  put \"$d/holder\" \"$$\"
  while :; do sleep 3600 3>&-; done
}

# The wrapper: runs the command in a process group of its own and records how it ended.
cmd_wrap() {
  d=$1; shift
  put \"$d/wrapper\" \"$$\"
  if [ -p \"$d/in\" ]; then in=$d/in; else in=/dev/null; fi
  if command -v setsid >/dev/null 2>&1; then
    setsid \"$@\" <\"$in\" >\"$d/out\" 2>\"$d/err\" &
  else
    \"$@\" <\"$in\" >\"$d/out\" 2>\"$d/err\" &
  fi
  pid=$!
  put \"$d/pid\" \"$pid\"
  wait \"$pid\"
  code=$?
  stop_holder
  put \"$d/exit\" \"$code\"
}

stop_holder() {
  if [ -f \"$d/holder\" ]; then
    kill \"$(cat \"$d/holder\")\" 2>/dev/null
    rm -f \"$d/holder\"
  fi
}

# status <id>: exited <code> <out bytes> <err bytes> | running <out> <err> | starting 0 0 |
# unknown 0 0
cmd_status() {
  dir_of \"$1\"
  settle
  if [ -f \"$d/exit\" ]; then echo \"exited $(cat \"$d/exit\") $(size_of \"$d/out\") $(size_of \"$d/err\")\"
  elif [ -f \"$d/pid\" ]; then echo \"running $(size_of \"$d/out\") $(size_of \"$d/err\")\"
  elif [ -d \"$d\" ]; then echo \"starting 0 0\"
  else echo \"unknown 0 0\"; fi
}

# attach <id> out|err [offset]: the stream from offset on, until the command has exited and all
# of it is out; then exit with the command's own status. Polls quickly while output flows and
# backs off to a second while it does not.
cmd_attach() {
  dir_of \"$1\"
  case ${2:-} in out|err) f=$d/$2 ;; *) die \"attach: stream must be out or err\" ;; esac
  off=${3:-0}
  [ -d \"$d\" ] || die \"attach: no run $1\"
  idle=0
  while :; do
    settle
    fin=; [ -f \"$d/exit\" ] && fin=1
    size=$(size_of \"$f\")
    if [ \"$size\" -gt \"$off\" ]; then
      tail -c +$((off + 1)) \"$f\" | head -c $((size - off)) || exit 141
      off=$size
      idle=0
    else
      idle=$((idle + 1))
    fi
    if [ -n \"$fin\" ]; then exit \"$(cat \"$d/exit\")\"; fi
    if [ $idle -lt 10 ]; then nap 0.2; else nap 1; fi
  done
}

# write <id>: this command's stdin, into the run's.
cmd_write() {
  dir_of \"$1\"
  [ -p \"$d/in\" ] || die \"write: run $1 has no stdin\"
  [ -f \"$d/exit\" ] && die \"write: run $1 has exited\"
  exec cat >\"$d/in\"
}

# close-stdin <id>: the command sees EOF once the last writer has gone.
cmd_close_stdin() {
  dir_of \"$1\"
  stop_holder
}

signal_run() {
  pid=$(cat \"$d/pid\" 2>/dev/null) || return 1
  kill \"-$1\" \"-$pid\" 2>/dev/null || kill \"-$1\" \"$pid\" 2>/dev/null
}

# term <id>: SIGTERM to the command's process group, and nothing more.
cmd_term() {
  dir_of \"$1\"
  [ -f \"$d/exit\" ] && exit 0
  signal_run TERM
  exit 0
}

# kill <id> [grace]: SIGTERM, then SIGKILL after grace seconds. An exit status is on disk when
# this returns, whatever happened to the wrapper.
cmd_kill() {
  dir_of \"$1\"
  grace=${2:-5}
  [ -d \"$d\" ] || exit 0
  [ -f \"$d/exit\" ] && exit 0
  signal_run TERM
  i=0
  while [ ! -f \"$d/exit\" ] && [ $i -lt $((grace * 5)) ]; do nap 0.2; i=$((i + 1)); done
  [ -f \"$d/exit\" ] && exit 0
  signal_run KILL
  i=0
  while [ ! -f \"$d/exit\" ] && [ $i -lt 25 ]; do nap 0.2; i=$((i + 1)); done
  if [ ! -f \"$d/exit\" ]; then stop_holder; put \"$d/exit\" 137; fi
  exit 0
}

sub=${1:-}
[ $# -gt 0 ] && shift
case $sub in
  start) cmd_start \"$@\" ;;
  _hold) cmd_hold \"$@\" ;;
  _wrap) cmd_wrap \"$@\" ;;
  status) cmd_status \"$@\" ;;
  attach) cmd_attach \"$@\" ;;
  write) cmd_write \"$@\" ;;
  close-stdin) cmd_close_stdin \"$@\" ;;
  term) cmd_term \"$@\" ;;
  kill) cmd_kill \"$@\" ;;
  *) die \"unknown subcommand '$sub'\" ;;
esac
"

/-- Whether `id` can name a run: what `dir_of` in the script accepts, and a little less — the
    characters a deterministic id is spelled with. Checked on the daemon's side so a bad id is a
    clear error here rather than `bad run id` from inside a pod. -/
def validRunId (id : String) : Bool :=
  !id.isEmpty && id.length ≤ 100 && !id.startsWith "."
    && id.all (fun c => c.isAlphanum || c == '-' || c == '_' || c == '.')

/-- The argument vector, after `sh <runner>`, that starts `argv` as run `id`. -/
def startArgs (id : String) (argv : Array String) (stdin : Bool := false) : Array String :=
  #["start", id] ++ (if stdin then #["--stdin"] else #[]) ++ #["--"] ++ argv

/-- The argument vector, after `sh <runner>`, that follows one of run `id`'s streams. -/
def attachArgs (id : String) (stream : String) (offset : Nat := 0) : Array String :=
  #["attach", id, stream, toString offset]

/-- What `start` said: a new run, or one that was already there with this much on stdout — the
    part of the stream a daemon attaching to it is replaying rather than seeing for the first
    time. -/
inductive StartReply where
  | started
  | existing (outBytes : Nat)
deriving Repr, BEq, Inhabited

/-- Read `start`'s one line. `none` for anything else, which the caller reports with what was
    said. -/
def parseStartReply (out : String) : Option StartReply :=
  match (out.trimAscii.toString.splitOn " ").filter (!·.isEmpty) with
  | ["started"] => some .started
  | ["existing", n] => n.toNat?.map .existing
  | _ => none

/-- What `status` says about a run. The byte counts are of `out`, then `err`, so far: what a
    follower compares its offset with before it believes it has passed the whole stream on. -/
inductive RunStatus where
  /-- Over, with this exit status. -/
  | exited (code : UInt32) (outBytes errBytes : Nat)
  /-- Still going. -/
  | running (outBytes errBytes : Nat)
  /-- Being started right now. -/
  | starting
  /-- Never started in this pod. -/
  | unknown
deriving Repr, BEq, Inhabited

/-- Read `status`'s one line. -/
def parseStatus (out : String) : Option RunStatus :=
  match (out.trimAscii.toString.splitOn " ").filter (!·.isEmpty) with
  | ["exited", c, n, e] => do pure (.exited (UInt32.ofNat (← c.toNat?)) (← n.toNat?) (← e.toNat?))
  | ["running", n, e]   => do pure (.running (← n.toNat?) (← e.toNat?))
  | ["starting", _, _]  => some .starting
  | ["unknown", _, _]   => some .unknown
  | _ => none

/-- The argument vector, after `sh <runner>`, that asks how run `id` is. -/
def statusArgs (id : String) : Array String := #["status", id]

/-- The daemon-side half: follow one stream of a run, across dropped connections, and exit with
    the run's own exit status.

    Run here as `sh -c <this> follow <kubectl> <namespace> <pod> <runner> <id> <out|err> <offset>`,
    with its stdout being what the daemon reads. Each round is one `kubectl exec … attach` from the
    offset reached so far — counted by passing everything through `tee` into a FIFO `wc -c` reads,
    since POSIX `sh` has no other way to count a stream it forwards — and when that exec ends it
    asks the pod how the run is:
    * over, and everything it wrote to this stream passed on (the byte counts `status` gives):
      exit with its status. Over with bytes still unforwarded — the attach was cut off before the
      end — is another round, since the last line is the one with the result in it;
    * still running: the connection dropped, attach again from where it stopped;
    * not answerable: try again every ten seconds for fifteen minutes, then give up.

    Giving up — that, a pod that is gone, or a run the pod has never heard of — exits 255 with the
    reason on stderr. 255 is also a status a command can exit with, so the daemon never takes it at
    its word: it asks the pod itself, and treats a run it cannot confirm as over as *lost*, which
    fails the step rather than reading as a finished one (`Kubernetes.confirmExit`).

    Its own reader going away ends it too (141), so a follower is never left behind looping on a
    run nobody reads. -/
def followScript : String := "set -u
k=$1; ns=$2; pod=$3; runner=$4; id=$5; stream=$6; off=$7
tmp=$(mktemp -d \"${TMPDIR:-/tmp}/orchestra-follow.XXXXXX\") || exit 125
trap 'rm -rf \"$tmp\"' EXIT
fails=0
while :; do
  mkfifo \"$tmp/c\" || exit 125
  wc -c <\"$tmp/c\" >\"$tmp/n\" &
  \"$k\" -n \"$ns\" exec \"$pod\" -- sh \"$runner\" attach \"$id\" \"$stream\" \"$off\" 2>\"$tmp/e\" | tee \"$tmp/c\"
  piped=$?
  wait
  n=$(tr -d ' ' <\"$tmp/n\")
  off=$((off + ${n:-0}))
  rm -f \"$tmp/c\"
  [ \"$piped\" -eq 0 ] || exit 141
  st=$(\"$k\" -n \"$ns\" exec \"$pod\" -- sh \"$runner\" status \"$id\" 2>>\"$tmp/e\") || st=
  set -- $st
  case ${1:-} in
    exited)
      # Over -- but only done once everything it wrote has been passed on: the attach that just
      # ended may have been cut off before the end, and the last line is the one with the result.
      if [ \"$stream\" = out ]; then want=${3:-0}; else want=${4:-0}; fi
      if [ \"$off\" -ge \"$want\" ]; then exit \"$2\"; fi
      fails=0 ;;
    running|starting) fails=0; sleep 1 ;;
    unknown) echo \"orchestra: run $id is not in pod $pod\" >&2; exit 255 ;;
    *)
      if grep -qi 'not found' \"$tmp/e\"; then echo \"orchestra: pod $pod is gone\" >&2; exit 255; fi
      fails=$((fails + 1))
      if [ \"$fails\" -ge 90 ]; then
        echo \"orchestra: lost run $id in pod $pod: $(tail -n 3 \"$tmp/e\")\" >&2; exit 255
      fi
      sleep 10 ;;
  esac
  : >\"$tmp/e\"
done
"

/-- The argument vector for running `followScript` with `/bin/sh`. -/
def followArgs (kubectl ns pod runner id stream : String) (offset : Nat := 0) : Array String :=
  #["-c", followScript, "follow", kubectl, ns, pod, runner, id, stream, toString offset]

end Orchestra.Exec.Runner
