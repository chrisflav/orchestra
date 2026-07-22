#include <lean/lean.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>

/* Termination-signal handling for the queue daemon.

   The daemon is PID 1 in the container image, so `docker stop` delivers SIGTERM straight to it,
   and Lean installs no handler of its own. That combination has no graceful path at all: the
   kernel drops signals with the default disposition when the target is PID 1, so SIGTERM is
   ignored outright, Docker waits out the whole grace period and then SIGKILLs -- killing
   in-flight agent tasks mid-run and leaving their queue entries stuck in `running`. Installing a
   handler is what makes the signal deliverable in the first place, and lets the daemon take the
   same graceful path as `orchestra queue shutdown`.

   The handler does the least it can: bump a counter. Anything else (logging, cancelling tokens,
   touching the Lean heap) is not async-signal-safe. The daemon polls `lean_orchestra_signal_count`
   from an ordinary thread and does the real work there. */

static volatile sig_atomic_t g_signal_count = 0;

static void orchestra_signal_handler(int signum) {
    (void)signum;
    g_signal_count++;
}

static lean_obj_res io_error_from_errno(const char *prefix) {
    char buf[256];
    snprintf(buf, sizeof(buf), "%s: %s", prefix, strerror(errno));
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(buf)));
}

/* lean_orchestra_install_signal_handlers: catch SIGTERM and SIGINT.

   SA_RESTART matters here: the daemon blocks in `accept` on its control socket and in `waitpid`
   on agent subprocesses, and without it those calls would fail with EINTR the moment a signal
   arrives -- turning a graceful shutdown into a burst of spurious errors on every other thread. */
LEAN_EXPORT lean_obj_res lean_orchestra_install_signal_handlers(lean_obj_arg world) {
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = orchestra_signal_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_RESTART;

    if (sigaction(SIGTERM, &sa, NULL) < 0) return io_error_from_errno("sigaction(SIGTERM)");
    if (sigaction(SIGINT, &sa, NULL) < 0) return io_error_from_errno("sigaction(SIGINT)");
    return lean_io_result_mk_ok(lean_box(0));
}

/* lean_orchestra_signal_count: how many termination signals have been received so far.
   0 means none; the daemon treats the first as "drain" and a second as "stop now". */
LEAN_EXPORT lean_obj_res lean_orchestra_signal_count(lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)g_signal_count));
}
