/-
Unix domain socket bindings for intra-machine IPC between the queue daemon and CLI clients.
The API is intentionally minimal: one line-delimited JSON round-trip per connection.
-/

namespace Orchestra.UnixSocket

-- Low-level FFI bindings

@[extern "lean_uds_listen"]
private opaque udsListen (path : String) (backlog : UInt32) : IO UInt32

/-- Block until a client connects; errors (e.g. EBADF) when the server fd is closed
    from another thread, which is how the daemon interrupts its accept loop on shutdown. -/
@[extern "lean_uds_accept"]
private opaque udsAccept (serverFd : UInt32) : IO UInt32

@[extern "lean_uds_connect"]
private opaque udsConnect (path : String) : IO UInt32

@[extern "lean_uds_send_line"]
private opaque udsSendLine (fd : UInt32) (line : String) : IO Unit

@[extern "lean_uds_recv_line"]
private opaque udsRecvLine (fd : UInt32) : IO String

@[extern "lean_uds_close"]
private opaque udsClose (fd : UInt32) : IO Unit

@[extern "lean_uds_unlink"]
private opaque udsUnlink (path : String) : IO Unit

-- Mid-level API

structure Server where fd : UInt32

structure Connection where fd : UInt32

namespace Server

def listen (path : System.FilePath) (backlog : UInt32 := 16) : IO Server :=
  return { fd := ← udsListen path.toString backlog }

/-- Block until a client connects.  Throws when the server fd has been closed
    (e.g. by the daemon's shutdown path), which exits the accept loop. -/
def accept (s : Server) : IO Connection :=
  return { fd := ← udsAccept s.fd }

def close (s : Server) : IO Unit := udsClose s.fd

/-- Remove the socket file; ignores missing-file errors. -/
def unlink (path : System.FilePath) : IO Unit := udsUnlink path.toString

end Server

namespace Connection

def connect (path : System.FilePath) : IO Connection :=
  return { fd := ← udsConnect path.toString }

def sendLine (c : Connection) (line : String) : IO Unit := udsSendLine c.fd line

def recvLine (c : Connection) : IO String := udsRecvLine c.fd

def close (c : Connection) : IO Unit := udsClose c.fd

end Connection

end Orchestra.UnixSocket
