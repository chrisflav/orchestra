#!/usr/bin/env python3
"""Write sample data into orchestra's store, so the dashboard has something to show.

For development only. Everything it creates is prefixed `demo-`, so `--remove` can take it
back out again without touching real runs:

    scripts/seed-dashboard-data.py            # write it
    scripts/seed-dashboard-data.py --remove   # take it back out

The layout mirrors what the daemon writes: task records under `tasks/`, queue entries under
`queue/`, concert runs under `concerts/`, and per-task JSONL logs under `logs/<fork>/`. Paths
follow `$XDG_DATA_HOME` exactly as `Orchestra.Dirs.dataBase` does, so pointing that at a
scratch directory seeds a throwaway store instead of the real one.
"""

from __future__ import annotations

import argparse
import json
import os
import random
import shutil
from datetime import datetime, timedelta, timezone
from pathlib import Path

PREFIX = "demo-"
FORK = "chrisflav/orchestra"
UPSTREAM = "chrisflav/orchestra"


def data_base() -> Path:
    xdg = os.environ.get("XDG_DATA_HOME")
    base = Path(xdg) if xdg else Path.home() / ".local" / "share"
    return base / "orchestra"


def iso(when: datetime) -> str:
    return when.strftime("%Y-%m-%dT%H:%M:%SZ")


# Prompts that read like real work on this repository, so the lists are scannable rather than
# filled with "test 1", "test 2".
PROMPTS = [
    "Port the concert step outputs onto the typed workflow schema",
    "Fix the landrun mount for review tasks so the clone stays read-only",
    "Add a regression test for the usage-limit router",
    "Rework the MCP review tool to carry inline annotations",
    "Triage the open backlog and label anything blocked",
    "Retry the failed merger run on chrisflav/taxis#412",
    "Update the docker entrypoint to wait for the queue socket",
    "Document the role dispatcher claim locks",
    "Trim the task log tail to the last 500 events",
    "Bump the pinned Lean toolchain and rebuild the cache",
    "Make the listener poll interval configurable per source",
    "Investigate the dropped SSE frames on reconnect",
    "Give the dashboard an OpenAPI description",
    "Replace the hand-rolled HTTP server with Std.Http",
    "Cache the taxis issue graph between dispatcher ticks",
    "Report per-task spend against its budget",
]

TOOLS = [
    ("Bash", {"command": "lake build 2>&1 | tail -20", "description": "Build the project"}),
    ("Read", {"file_path": "Orchestra/Queue.lean"}),
    ("Grep", {"pattern": "loadAllEntries"}),
    ("Edit", {"file_path": "Orchestra/Dashboard.lean"}),
    ("Bash", {"command": "lake exe orchestraTest", "description": "Run the test suite"}),
]

THOUGHTS = [
    "The queue loader sorts by id descending, so the newest entry is first. That means the "
    "paging window can be taken from the front without another sort.",
    "This only reproduces when the SSE stream reconnects mid-frame, which is why the retry "
    "path never showed it.",
    "Two callers depend on the old field name. Renaming it means touching both, or keeping a "
    "compatibility shim for one release.",
]


def log_events(seed: int, status: str) -> list[dict]:
    """A plausible run: init, some thinking and tool calls, then a result."""
    rng = random.Random(seed)
    events: list[dict] = [
        {
            "type": "init",
            "model": rng.choice(["claude-opus-4-8", "claude-sonnet-5"]),
            "session_id": f"sess_{seed:08x}",
        },
        {"type": "system", "subtype": "context_loaded"},
        {"type": "assistant", "item": {"type": "thinking", "text": rng.choice(THOUGHTS)}},
    ]
    for _ in range(rng.randint(2, 5)):
        name, args = rng.choice(TOOLS)
        events.append({"type": "assistant", "item": {"type": "tool_use", "name": name, "input": args}})
        if name == "Bash":
            events.append(
                {
                    "type": "tool_result",
                    "stdout": "Build completed successfully (598 jobs).\n",
                    "stderr": "" if rng.random() > 0.25 else "warning: unused variable `p`\n",
                }
            )
        else:
            events.append({"type": "tool_result", "stdout": "ok\n", "stderr": ""})
    events.append(
        {
            "type": "assistant",
            "item": {"type": "text", "text": "Done. The build is clean and the tests pass."},
        }
    )
    if status in ("completed", "done"):
        events.append(
            {
                "type": "result",
                "subtype": "success",
                "result": "Opened a pull request with the change.",
                "num_turns": rng.randint(6, 40),
                "duration_ms": rng.randint(30_000, 900_000),
                "total_cost_usd": round(rng.uniform(0.08, 3.4), 2),
            }
        )
    elif status == "failed":
        events.append(
            {
                "type": "result",
                "subtype": "error_during_execution",
                "result": "lake build failed: Orchestra/Queue.lean:301:7: unknown identifier",
                "num_turns": rng.randint(3, 20),
                "duration_ms": rng.randint(20_000, 400_000),
                "total_cost_usd": round(rng.uniform(0.05, 1.2), 2),
            }
        )
    return events


def write_json(path: Path, payload: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2) + "\n")


def seed(base: Path) -> None:
    rng = random.Random(20260723)
    now = datetime.now(timezone.utc)

    # Concerts, oldest first, so a running one sits alongside two that finished.
    concerts = []
    for i, (name, status) in enumerate(
        [("nightly-review", "done"), ("release-cut", "failed"), ("backlog-sweep", "running")]
    ):
        started = now - timedelta(hours=30 - i * 11)
        cid = f"{PREFIX}c{i:03d}"
        run = {
            "id": cid,
            "started_at": iso(started),
            "status": status,
            "name": name,
            "workflow_file": f"workflows/{name}.yaml",
        }
        if status != "running":
            run["finished_at"] = iso(started + timedelta(minutes=rng.randint(12, 90)))
        write_json(base / "concerts" / f"{cid}.json", run)
        concerts.append(cid)

    # Queue entries: what is in flight now. Three running, one failed sitting in the middle of
    # the backlog, and a tail of pending work at descending priority.
    statuses = ["running"] * 3 + ["failed"] + ["pending"] * 9
    for i, status in enumerate(statuses):
        qid = f"{PREFIX}q{900 - i:04d}"
        entry = {
            "id": qid,
            "created_at": iso(now - timedelta(minutes=9 * i + 3)),
            "status": status,
            "upstream": UPSTREAM,
            "fork": FORK,
            "mode": "fork",
            "prompt": PROMPTS[i % len(PROMPTS)],
            "backend": rng.choice(["claude", "claude", "vibe"]),
            "priority": max(1, 12 - i),
        }
        if i % 2 == 0:
            entry["series"] = rng.choice(["nightly", "review", "triage"])
        if i % 4 == 0:
            entry["concert_id"] = concerts[2]
            entry["concert_step_key"] = f"step-{i // 4 + 1}"
        if status == "running":
            entry["task_id"] = f"{PREFIX}t{800 - i:04d}"
        write_json(base / "queue" / f"{qid}.json", entry)

    # Task history, spread over the last few days so `since` and the relative timestamps in the
    # UI both have something to bite on.
    for i in range(24):
        tid = f"{PREFIX}t{800 - i:04d}"
        status = ["running", "completed", "completed", "completed", "failed", "unfinished",
                  "cancelled"][i % 7]
        created = now - timedelta(hours=i * 7 + rng.randint(0, 5))
        record = {
            "id": tid,
            "created_at": iso(created),
            "upstream": UPSTREAM,
            "fork": FORK,
            "mode": "fork",
            "prompt": PROMPTS[(i + 3) % len(PROMPTS)],
            "status": status,
            "backend": rng.choice(["claude", "claude", "vibe", "opencode"]),
            "model": rng.choice(["claude-opus-4-8", "claude-sonnet-5"]),
            "session_id": f"sess_{i:08x}",
            "budget": round(rng.uniform(1.0, 8.0), 2),
        }
        if i % 3:
            record["series"] = rng.choice(["nightly", "review", "triage"])
        if i % 8 == 5:
            record["continues_from"] = f"{PREFIX}t{800 - i - 1:04d}"
        write_json(base / "tasks" / f"{tid}.json", record)

        # A log for most of them, so task detail is not an empty panel. One gets a long one, to
        # exercise the tail and the "showing N of M" line.
        if i % 5 != 4:
            events = log_events(i, status)
            # One run long enough to exceed the default 500-event tail, so the detail page's
            # "showing N of M" line and the `logLimit` parameter both have something to do.
            if i == 1:
                events = (events[:3] + events[3:-1] * 160 + events[-1:])
            log = base / "logs" / FORK / f"{tid}.log"
            log.parent.mkdir(parents=True, exist_ok=True)
            log.write_text("".join(json.dumps(e) + "\n" for e in events))

    print(f"seeded into {base}")
    for sub in ("concerts", "queue", "tasks"):
        n = len(list((base / sub).glob(f"{PREFIX}*.json")))
        print(f"  {sub:9} {n}")
    logs = list((base / "logs" / FORK).glob(f"{PREFIX}*.log"))
    print(f"  logs      {len(logs)}")


def remove(base: Path) -> None:
    removed = 0
    for sub in ("concerts", "queue", "tasks"):
        for path in (base / sub).glob(f"{PREFIX}*.json"):
            path.unlink()
            removed += 1
    log_dir = base / "logs" / FORK
    if log_dir.is_dir():
        for path in log_dir.glob(f"{PREFIX}*.log"):
            path.unlink()
            removed += 1
        # Only if this script is what created it.
        if not any(log_dir.iterdir()):
            shutil.rmtree(log_dir, ignore_errors=True)
    print(f"removed {removed} demo files from {base}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--remove", action="store_true", help="delete the demo data instead")
    args = parser.parse_args()
    base = data_base()
    remove(base) if args.remove else seed(base)


if __name__ == "__main__":
    main()
