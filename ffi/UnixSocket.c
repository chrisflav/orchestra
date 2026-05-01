#include <lean/lean.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

static lean_obj_res io_error_from_errno(const char *prefix) {
    char buf[256];
    snprintf(buf, sizeof(buf), "%s: %s", prefix, strerror(errno));
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(buf)));
}

/* lean_uds_listen: create AF_UNIX SOCK_STREAM socket, bind to path, start listening.
   Returns the server fd as UInt32. */
LEAN_EXPORT lean_obj_res lean_uds_listen(lean_obj_arg path_obj, uint32_t backlog, lean_obj_arg world) {
    const char *path = lean_string_cstr(path_obj);
    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0) { lean_dec(path_obj); return io_error_from_errno("socket"); }

    int opt = 1;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    struct sockaddr_un addr;
    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, path, sizeof(addr.sun_path) - 1);
    lean_dec(path_obj);

    if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        close(fd); return io_error_from_errno("bind");
    }
    if (listen(fd, (int)backlog) < 0) {
        close(fd); return io_error_from_errno("listen");
    }
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)fd));
}

/* lean_uds_accept: block until a client connects.
   Returns the client fd as UInt32.
   When the server fd is closed from another thread, returns an error
   (EBADF/EINVAL) which the caller uses to exit its accept loop. */
LEAN_EXPORT lean_obj_res lean_uds_accept(uint32_t server_fd, lean_obj_arg world) {
    int client = accept((int)server_fd, NULL, NULL);
    if (client < 0) return io_error_from_errno("accept");
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)client));
}

/* lean_uds_connect: connect to a Unix-domain server at path.
   Returns the connected fd as UInt32. */
LEAN_EXPORT lean_obj_res lean_uds_connect(lean_obj_arg path_obj, lean_obj_arg world) {
    const char *path = lean_string_cstr(path_obj);
    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0) { lean_dec(path_obj); return io_error_from_errno("socket"); }

    struct sockaddr_un addr;
    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, path, sizeof(addr.sun_path) - 1);
    lean_dec(path_obj);

    if (connect(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        close(fd); return io_error_from_errno("connect");
    }
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)fd));
}

/* lean_uds_send_line: write a Lean String followed by '\n' to fd. */
LEAN_EXPORT lean_obj_res lean_uds_send_line(uint32_t fd, lean_obj_arg str_obj, lean_obj_arg world) {
    const char *str = lean_string_cstr(str_obj);
    size_t len = strlen(str);
    lean_dec(str_obj);

    const char *p = str;
    size_t rem = len;
    while (rem > 0) {
        ssize_t n = write((int)fd, p, rem);
        if (n < 0) return io_error_from_errno("write");
        p += n; rem -= (size_t)n;
    }
    char nl = '\n';
    if (write((int)fd, &nl, 1) < 0) return io_error_from_errno("write (newline)");
    return lean_io_result_mk_ok(lean_box(0));
}

/* lean_uds_recv_line: read bytes from fd until '\n' or EOF.
   Returns a Lean String (without the trailing newline). */
LEAN_EXPORT lean_obj_res lean_uds_recv_line(uint32_t fd, lean_obj_arg world) {
    size_t cap = 256, len = 0;
    char *buf = malloc(cap);
    if (!buf) return io_error_from_errno("malloc");

    char ch;
    while (1) {
        ssize_t n = read((int)fd, &ch, 1);
        if (n <= 0) break;
        if (ch == '\n') break;
        if (len + 2 > cap) {
            cap *= 2;
            char *nb = realloc(buf, cap);
            if (!nb) { free(buf); return io_error_from_errno("realloc"); }
            buf = nb;
        }
        buf[len++] = ch;
    }
    buf[len] = '\0';
    lean_obj_arg s = lean_mk_string(buf);
    free(buf);
    return lean_io_result_mk_ok(s);
}

/* lean_uds_close: close an fd. */
LEAN_EXPORT lean_obj_res lean_uds_close(uint32_t fd, lean_obj_arg world) {
    close((int)fd);
    return lean_io_result_mk_ok(lean_box(0));
}

/* lean_uds_unlink: remove the socket file. Ignores ENOENT. */
LEAN_EXPORT lean_obj_res lean_uds_unlink(lean_obj_arg path_obj, lean_obj_arg world) {
    const char *path = lean_string_cstr(path_obj);
    if (unlink(path) < 0 && errno != ENOENT) {
        lean_dec(path_obj);
        return io_error_from_errno("unlink");
    }
    lean_dec(path_obj);
    return lean_io_result_mk_ok(lean_box(0));
}
