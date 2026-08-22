import { useEffect, useRef } from "react";
import type { ReactNode } from "react";
import type { LogEvent } from "../api";

/** Cap on how much of a single stdout/stderr blob is rendered inline.

    Sized for a task log, where a blob is the output of a command and a runaway one would wedge
    the page. A conversation is the other case — there the blob is the model's prose, and 4000
    characters is an ordinary long answer — so the chat raises it; see `maxBlob`. */
const MAX_BLOB = 4000;

function clamp(text: string, max: number): string {
  return text.length <= max ? text : `${text.slice(0, max)}\n… ${text.length - max} more characters`;
}

function truncate(text: string, max: number): string {
  return text.length <= max ? text : `${text.slice(0, max)}…`;
}

/**
 * One log line: what kind of event it was, then the event.
 *
 * The kind sits in a fixed left column and is right-aligned against the body, so the eye can
 * run down the boundary between them and find the tool calls without reading any of the text.
 * Its colour comes from the same five sections used for status everywhere else.
 */
function Entry({ kind, tone = "", children }: { kind: string; tone?: string; children: ReactNode }) {
  return (
    <div className="log-entry">
      <div className={`log-kind ${tone}`}>{kind}</div>
      <div className="log-body">{children}</div>
    </div>
  );
}

/**
 * Renders a tool call's input.
 *
 * Tools differ enough that a generic JSON dump is unreadable for the common ones, so the few
 * fields that carry the meaning — the command, the path, the pattern — are pulled out and
 * everything else falls back to compact JSON.
 */
function ToolInput({ input, maxBlob }: { input: Record<string, unknown>; maxBlob: number }) {
  const str = (key: string): string => {
    const v = input[key];
    return typeof v === "string" ? v : "";
  };
  const command = str("command");
  const filePath = str("file_path") || str("filePath");
  const pattern = str("pattern");
  const description = str("description");

  if (command) {
    return (
      <>
        {description && <div className="log-meta">{description}</div>}
        <pre>{clamp(command, maxBlob)}</pre>
      </>
    );
  }
  if (filePath) return <div className="log-meta">{filePath}</div>;
  if (pattern) return <div className="log-meta">pattern: {pattern}</div>;
  if (description) return <div className="log-meta">{description}</div>;
  return <pre>{truncate(JSON.stringify(input), 280)}</pre>;
}

function LogEntry({ event, maxBlob }: { event: LogEvent; maxBlob: number }) {
  switch (event.type) {
    case "init":
      return (
        <Entry kind="start">
          {event.model ?? "unknown model"}
          <div className="log-meta">session {truncate(event.session_id ?? "", 12)}</div>
        </Entry>
      );

    case "system":
      return <Entry kind="system">{event.subtype ?? ""}</Entry>;

    case "assistant": {
      const item = event.item ?? {};
      if (item.type === "thinking")
        return (
          <Entry kind="thinking" tone="k-think">
            <div className="log-think">{clamp(item.text ?? "", maxBlob)}</div>
          </Entry>
        );
      if (item.type === "tool_use")
        return (
          <Entry kind="tool" tone="k-tool">
            <span className="log-tool">{item.name ?? "?"}</span>
            <ToolInput input={item.input ?? {}} maxBlob={maxBlob} />
          </Entry>
        );
      if (item.type === "text")
        return (
          <Entry kind="says" tone="k-text">
            {clamp(item.text ?? "", maxBlob)}
          </Entry>
        );
      return (
        <Entry kind={item.type ?? "unknown"}>{truncate(JSON.stringify(item), 240)}</Entry>
      );
    }

    case "tool_result": {
      const stdout = event.stdout ?? "";
      const stderr = event.stderr ?? "";
      return (
        <Entry kind="output">
          {stdout && <pre>{clamp(stdout, maxBlob)}</pre>}
          {stderr && (
            <>
              <div className="log-meta log-err">stderr</div>
              <pre>{clamp(stderr, maxBlob)}</pre>
            </>
          )}
        </Entry>
      );
    }

    case "result": {
      const subtype = event.subtype ?? "";
      const failed = subtype.startsWith("error");
      const parts: string[] = [];
      if (event.num_turns != null) parts.push(`${event.num_turns} turns`);
      if (event.duration_ms != null) parts.push(`${Math.floor(event.duration_ms / 1000)}s`);
      if (event.total_cost_usd != null) parts.push(`$${event.total_cost_usd}`);
      return (
        <Entry kind={subtype || "result"} tone={failed ? "k-err" : "k-text"}>
          {event.result && <pre>{clamp(event.result, maxBlob)}</pre>}
          {parts.length > 0 && <div className="log-meta">{parts.join(" · ")}</div>}
        </Entry>
      );
    }

    default:
      return (
        <Entry kind={event.type ?? "unknown"}>{truncate(JSON.stringify(event), 240)}</Entry>
      );
  }
}

/**
 * The task log, pinned to the bottom while the reader is already there.
 *
 * A running task appends events every couple of seconds. Scrolling back up to read something
 * must not be undone by the next frame, so the scroll is only forced when the reader was
 * within a few pixels of the end when the update arrived.
 *
 * `flow` turns all of that off — the panel, the box, and the pinning — for a caller that draws
 * several of these as part of its own page. A chat transcript is that caller: a conversation is
 * continuous prose, and every turn arriving as a bordered card with its own scrollbar reads as a
 * stack of documents rather than as something someone said. It also costs: each block would read
 * `scrollHeight` on every frame, a synchronous layout per block, for a page that has exactly one
 * place worth pinning to — the window.
 */
export function LogView({
  events,
  total,
  truncated,
  empty = "This task has no log file.",
  flow = false,
  maxBlob = MAX_BLOB,
}: {
  events: LogEvent[];
  total: number;
  truncated: boolean;
  /** Draw as part of the caller's page — no panel, no scroll box, no pinning. */
  flow?: boolean;
  /** How much of one blob to render before cutting it. See `MAX_BLOB`. */
  maxBlob?: number;
  /** What to say when there is nothing to show. Only the page knows why there isn't. */
  empty?: string;
}) {
  const ref = useRef<HTMLDivElement>(null);
  const pinned = useRef(true);

  useEffect(() => {
    if (flow) return;
    const el = ref.current;
    if (el && pinned.current) el.scrollTop = el.scrollHeight;
  }, [events, flow]);

  if (events.length === 0) {
    return <p className="empty">{empty}</p>;
  }

  const onScroll = () => {
    const el = ref.current;
    if (el) pinned.current = el.scrollHeight - el.scrollTop - el.clientHeight < 8;
  };

  return (
    <div className={flow ? "log-plain" : "panel"}>
      {truncated && (
        <div className="caption">
          Showing the last {events.length} of {total} events. The tail follows as the task runs.
        </div>
      )}
      <div className={flow ? "log log-flow" : "log"} ref={ref} onScroll={onScroll}>
        {events.map((event, i) => (
          // Log events are append-only and carry no id, so position is a stable key here:
          // index `i` always names the same event for as long as the tail window holds.
          <LogEntry key={i} event={event} maxBlob={maxBlob} />
        ))}
      </div>
    </div>
  );
}
