#!/usr/bin/env python3
import json
import sys
import os
import socket

def print_event(event_type, **kwargs):
    event = {"type": event_type}
    event.update(kwargs)
    print(json.dumps(event), flush=True)

def check_path(path, expected):
    readable = os.access(path, os.R_OK)
    writable = os.access(path, os.W_OK)
    executable = os.access(path, os.X_OK)
    
    res = f"Path {path}: R={readable}, W={writable}, X={executable} (expected {expected})"
    
    passed = False
    if expected == "ro":
        passed = readable and not writable
    elif expected == "rw":
        passed = readable and writable
    elif expected == "rox":
        passed = readable and executable
    elif expected == "none":
        passed = not readable and not writable
        
    return res, passed

def main():
    # 1. Init event
    print_event("system", subtype="init", session_id="test-session", model="test-agent")

    # 2. Check MCP server
    # The port is passed as an environment variable by the runner (we'll configure this in AgentDef)
    mcp_port = os.environ.get("ORCHESTRA_MCP_PORT")
    if mcp_port:
        try:
            # Try to connect to the MCP server
            with socket.create_connection(("127.0.0.1", int(mcp_port)), timeout=1):
                print_event("assistant", item={"type": "text", "text": "MCP server is available"})
        except Exception as e:
            print_event("assistant", item={"type": "text", "text": f"MCP server is NOT available: {e}"})
    else:
        print_event("assistant", item={"type": "text", "text": "MCP port not provided in environment"})

    # 3. Check paths
    # We check a set of paths that should be configured in the AgentDef
    paths_to_check = {
        "/usr": "rox",
        "/etc": "ro",
        "/tmp": "rw",
        "/root": "none",
    }
    
    all_passed = True
    for path, expected in paths_to_check.items():
        msg, passed = check_path(path, expected)
        print_event("assistant", item={"type": "text", "text": msg})
        if not passed:
            all_passed = False

    # 4. Final result
    if all_passed:
        print_event("result", subtype="success", result="All deterministic tests passed")
    else:
        print_event("result", subtype="error", result="Some deterministic tests failed")

if __name__ == "__main__":
    main()
