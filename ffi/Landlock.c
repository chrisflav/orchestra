#include <lean/lean.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
#include <unistd.h>

/* Landlock scoping for the landrun backend.

   landrun builds a Landlock ruleset for the filesystem and TCP ports, and nothing else. Two things
   Landlock can also restrict are left open by it (as of landrun 0.1.15): connecting to an abstract
   Unix socket (an X server's `@/tmp/.X11-unix/X0`, a D-Bus or container runtime's command socket)
   and sending signals to processes outside the sandbox. Both are "scopes" since Landlock ABI 6
   (Linux 6.12): a ruleset with `scoped` set stops the processes it is applied to from reaching
   anything outside their own Landlock domain that way.

   So orchestra applies one more ruleset itself, holding only those two scopes and handling no
   filesystem or network access (those stay landrun's), and then executes landrun. Landlock layers
   stack: whatever landrun adds on top, the agent stays inside this domain too.

   The constants and the syscall numbers are written out rather than taken from
   <linux/landlock.h>, which the Lean toolchain's sysroot does not carry. The numbers are the same
   on every architecture (the landlock syscalls were added after the tables were unified). */

#ifndef SYS_landlock_create_ruleset
#define SYS_landlock_create_ruleset 444
#endif
#ifndef SYS_landlock_restrict_self
#define SYS_landlock_restrict_self 446
#endif
#define ORCHESTRA_LANDLOCK_CREATE_RULESET_VERSION (1U << 0)
#define ORCHESTRA_LANDLOCK_SCOPE_ABSTRACT_UNIX_SOCKET (1ULL << 0)
#define ORCHESTRA_LANDLOCK_SCOPE_SIGNAL (1ULL << 1)

struct orchestra_landlock_ruleset_attr {
    uint64_t handled_access_fs;
    uint64_t handled_access_net;
    uint64_t scoped;
};

static lean_obj_res io_error(const char *prefix) {
    char buf[512];
    snprintf(buf, sizeof(buf), "%s: %s", prefix, strerror(errno));
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(buf)));
}

/* The Landlock ABI version this kernel offers, or 0 when it offers none. */
static int landlock_abi(void) {
    long abi = syscall(SYS_landlock_create_ruleset, NULL, 0,
                       ORCHESTRA_LANDLOCK_CREATE_RULESET_VERSION);
    return abi < 0 ? 0 : (int)abi;
}

/* lean_orchestra_landlock_abi : IO UInt32 */
LEAN_EXPORT lean_obj_res lean_orchestra_landlock_abi(lean_obj_arg world) {
    (void)world;
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)landlock_abi()));
}

/* lean_orchestra_scope_exec : Array String -> IO Unit

   Restricts this process to the abstract-socket and signal scopes, then executes argv[0] (looked
   up on PATH) with argv. Returns only on failure. A kernel below ABI 6 has no scopes; the caller
   decides beforehand whether that is acceptable (`Landrun.scopeSupported`), so here it is an
   error rather than a silent exec without them. */
LEAN_EXPORT lean_obj_res lean_orchestra_scope_exec(b_lean_obj_arg args, lean_obj_arg world) {
    (void)world;
    size_t n = lean_array_size(args);
    if (n == 0) {
        errno = EINVAL;
        return io_error("landlock scope: nothing to execute");
    }
    if (landlock_abi() < 6) {
        errno = ENOSYS;
        return io_error("landlock scope: this kernel offers no Landlock scopes (ABI 6, Linux 6.12)");
    }
    struct orchestra_landlock_ruleset_attr attr;
    memset(&attr, 0, sizeof(attr));
    attr.scoped = ORCHESTRA_LANDLOCK_SCOPE_ABSTRACT_UNIX_SOCKET | ORCHESTRA_LANDLOCK_SCOPE_SIGNAL;
    long fd = syscall(SYS_landlock_create_ruleset, &attr, sizeof(attr), 0);
    if (fd < 0) return io_error("landlock scope: landlock_create_ruleset");
    if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0) {
        close((int)fd);
        return io_error("landlock scope: PR_SET_NO_NEW_PRIVS");
    }
    if (syscall(SYS_landlock_restrict_self, fd, 0) != 0) {
        close((int)fd);
        return io_error("landlock scope: landlock_restrict_self");
    }
    close((int)fd);
    char **argv = calloc(n + 1, sizeof(char *));
    if (argv == NULL) return io_error("landlock scope: calloc");
    for (size_t i = 0; i < n; i++) {
        argv[i] = (char *)lean_string_cstr(lean_array_get_core(args, i));
    }
    argv[n] = NULL;
    execvp(argv[0], argv);
    lean_obj_res err = io_error("landlock scope: exec");
    free(argv);
    return err;
}
