import { useEffect, useRef } from "react";
import type { ReactNode } from "react";
import type { LogEvent } from "../api";

/** Cap on how much of a single stdout/stderr blob is rendered inline. */
const MAX_BLOB = 4000;

function clamp(text: string, max = MAX_BLOB): string {
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
function ToolInput({ input }: { input: Record<string, unknown> }) {
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
        <pre>{clamp(command)}</pre>
      </>
    );
  }
  if (filePath) return <div className="log-meta">{filePath}</div>;
  if (pattern) return <div className="log-meta">pattern: {pattern}</div>;
  if (description) return <div className="log-meta">{description}</div>;
  return <pre>{truncate(JSON.stringify(input), 280)}</pre>;
}

function LogEntry({ event }: { event: LogEvent }) {
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
            <div className="log-think">{clamp(item.text ?? "")}</div>
          </Entry>
        );
      if (item.type === "tool_use")
        return (
          <Entry kind="tool" tone="k-tool">
            <span className="log-tool">{item.name ?? "?"}</span>
            <ToolInput input={item.input ?? {}} />
          </Entry>
        );
      if (item.type === "text")
        return (
          <Entry kind="says" tone="k-text">
            {clamp(item.text ?? "")}
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
          {stdout && <pre>{clamp(stdout)}</pre>}
          {stderr && (
            <>
              <div className="log-meta log-err">stderr</div>
              <pre>{clamp(stderr)}</pre>
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
          {event.result && <pre>{clamp(event.result)}</pre>}
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
 */
export function LogView({
  events,
  total,
  truncated,
}: {
  events: LogEvent[];
  total: number;
  truncated: boolean;
}) {
  const ref = useRef<HTMLDivElement>(null);
  const pinned = useRef(true);

  useEffect(() => {
    const el = ref.current;
    if (el && pinned.current) el.scrollTop = el.scrollHeight;
  }, [events]);

  if (events.length === 0) {
    return (
      <p className="empty">This task has no log file.</p>
    );
  }

  const onScroll = () => {
    const el = ref.current;
    if (el) pinned.current = el.scrollHeight - el.scrollTop - el.clientHeight < 8;
  };

  return (
    <div className="panel">
      {truncated && (
        <div className="caption">
          Showing the last {events.length} of {total} events. The tail follows as the task runs.
        </div>
      )}
      <div className="log" ref={ref} onScroll={onScroll}>
        {events.map((event, i) => (
          // Log events are append-only and carry no id, so position is a stable key here:
          // index `i` always names the same event for as long as the tail window holds.
          <LogEntry key={i} event={event} />
        ))}
      </div>
    </div>
  );
}
