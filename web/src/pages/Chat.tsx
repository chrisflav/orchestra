import { useEffect, useMemo, useRef, useState } from "react";
import { useNavigate, useParams } from "react-router-dom";
import {
  ApiError,
  endSession,
  interruptSession,
  sendTurn,
  startSession,
  UnauthorizedError,
} from "../api";
import type { SessionDetail, TranscriptEvent } from "../api";
import { Empty, List, Row } from "../components/List";
import { LogView } from "../components/LogView";
import { Facts, LivePage, Section } from "../components/Page";
import { Status } from "../components/Status";
import { Time } from "../components/Time";
import { useTranscript } from "../useTranscript";

/**
 * The one page in this console that talks back.
 *
 * Everything else here reads: it shows what the daemon did and, at most, stops one task. A
 * session is the other kind of thing — the browser is one of three clients of the same five
 * routes, and what it does here it could do from `orchestra chat` or a phone instead.
 */

/** What a caller can do to a session, and when. */
function turnState(session: SessionDetail): {
  canType: boolean;
  canInterrupt: boolean;
  note: string;
} {
  switch (session.status) {
    case "idle":
      return { canType: true, canInterrupt: false, note: "" };
    case "running":
      // Refused rather than queued, and the input says so rather than accepting a turn that
      // would be rejected: the agent would take the second line as soon as it finished the
      // first, and someone who typed twice would get two answers in an order nobody chose.
      return {
        canType: false,
        canInterrupt: true,
        note: "Working. Interrupt it, or wait for this turn to finish.",
      };
    case "starting":
      return { canType: false, canInterrupt: false, note: "Starting the agent…" };
    case "failed":
      return {
        canType: false,
        canInterrupt: false,
        note: session.error ?? "This session failed.",
      };
    default:
      return { canType: false, canInterrupt: false, note: "This session has ended." };
  }
}

function errorText(err: unknown): string {
  if (err instanceof ApiError) return err.message;
  if (err instanceof UnauthorizedError) return "the session expired";
  return err instanceof Error ? err.message : String(err);
}

/** The events the agent itself produced, for `LogView` — which already knows how to draw them. */
function agentEvents(events: TranscriptEvent[]) {
  return events.flatMap((e) => (e.kind === "agent" && e.event ? [e.event] : []));
}

/**
 * The conversation.
 *
 * Agent output goes through `LogView`, the same component a task's log uses, so a turn looks
 * the same here as it does there. What is rendered around it is the part the agent's own stream
 * cannot say: what the person typed, and what the daemon did.
 */
function Conversation({ events }: { events: TranscriptEvent[] }) {
  // Grouped so a run of agent events becomes one `LogView` rather than one per line, which is
  // what keeps the tool calls lined up the way they are on a task page. Memoised because the
  // arrays it builds are `LogView`'s props: rebuilt on every render they would make every block
  // in the conversation look new on every frame.
  const blocks = useMemo(() => {
    const out: { kind: "agent" | "other"; events: TranscriptEvent[] }[] = [];
    for (const e of events) {
      const kind = e.kind === "agent" ? "agent" : "other";
      const last = out[out.length - 1];
      if (last && last.kind === kind) last.events.push(e);
      else out.push({ kind, events: [e] });
    }
    return out;
  }, [events]);

  // One scroll region for the whole conversation rather than one per block, pinned to the
  // bottom only while the reader is already there — scrolling back to read something must not
  // be undone by the next turn.
  const ref = useRef<HTMLDivElement>(null);
  const pinned = useRef(true);
  useEffect(() => {
    const el = ref.current;
    if (el && pinned.current) el.scrollTop = el.scrollHeight;
  }, [blocks]);
  const onScroll = () => {
    const el = ref.current;
    if (el) pinned.current = el.scrollHeight - el.scrollTop - el.clientHeight < 8;
  };

  if (events.length === 0) {
    return <p className="empty">Nothing said yet. Type a turn below.</p>;
  }

  return (
    <div className="chat-transcript" ref={ref} onScroll={onScroll}>
      {blocks.map((block, i) =>
        block.kind === "agent" ? (
          <LogView
            key={i}
            events={agentEvents(block.events)}
            total={block.events.length}
            truncated={false}
            autoScroll={false}
          />
        ) : (
          <div key={i}>
            {block.events.map((e) => {
              if (e.kind === "user") {
                return (
                  <blockquote key={e.seq} className="chat-turn">
                    {e.text}
                  </blockquote>
                );
              }
              if (e.kind === "notice") {
                return (
                  <p key={e.seq} className={`chat-notice chat-notice-${e.level ?? "info"}`}>
                    {e.message}
                  </p>
                );
              }
              if (e.kind === "turnEnded") {
                const cost = typeof e.costUsd === "number" ? ` · $${e.costUsd.toFixed(4)}` : "";
                const dur =
                  typeof e.durationSeconds === "number" ? ` · ${e.durationSeconds}s` : "";
                return (
                  <p key={e.seq} className="chat-turn-end">
                    turn {e.turn} {e.subtype}
                    {dur}
                    {cost}
                  </p>
                );
              }
              // `turnStarted` is structure, not content: the status already says a turn is in
              // flight, and drawing both says the same thing twice.
              return null;
            })}
          </div>
        ),
      )}
    </div>
  );
}

/** The list of conversations. */
export function Chat() {
  return (
    <LivePage endpoint="interactive" title="Sessions">
      {(data) => (
        <>
          <NewSession />
          <List>
            {data.items.length === 0 ? (
              <Empty>No sessions. Start one above.</Empty>
            ) : (
              data.items.map((s) => (
                <Row
                  key={s.id}
                  to={`/chat/${encodeURIComponent(s.id)}`}
                  title={s.title ?? "(nothing said yet)"}
                  end={<Status status={s.status} />}
                  meta={[
                    s.id,
                    s.fork,
                    `${s.turnCount} turn${s.turnCount === 1 ? "" : "s"}`,
                    <Time key="t" iso={s.lastActivityAt} />,
                  ]}
                />
              ))
            )}
          </List>
        </>
      )}
    </LivePage>
  );
}

/** Starting one. The only form in this console that runs an agent. */
function NewSession() {
  const navigate = useNavigate();
  const [upstream, setUpstream] = useState("");
  const [fork, setFork] = useState("");
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const submit = (e: React.FormEvent) => {
    e.preventDefault();
    if (busy || upstream.trim() === "" || fork.trim() === "") return;
    setBusy(true);
    setError(null);
    startSession({ upstream: upstream.trim(), fork: fork.trim() })
      .then((s) => navigate(`/chat/${encodeURIComponent(s.id)}`))
      .catch((err: unknown) => setError(errorText(err)))
      .finally(() => setBusy(false));
  };

  return (
    <Section title="Start a session">
      <form className="chat-start" onSubmit={submit}>
        <input
          value={upstream}
          onChange={(e) => setUpstream(e.target.value)}
          placeholder="upstream (owner/repo)"
          aria-label="Upstream repository"
        />
        <input
          value={fork}
          onChange={(e) => setFork(e.target.value)}
          placeholder="fork (owner/repo)"
          aria-label="Fork repository"
        />
        <button type="submit" disabled={busy}>
          {busy ? "Starting…" : "Start"}
        </button>
      </form>
      {/* The daemon's sentence, verbatim: it is the only thing that knows why it refused. */}
      {error !== null && <p className="chat-error">{error}</p>}
    </Section>
  );
}

/** One conversation, and the box to say something into. */
export function ChatDetail() {
  const { id = "" } = useParams();
  const navigate = useNavigate();
  const { events, error: streamError, live } = useTranscript(id);

  return (
    <LivePage
      endpoint={`interactive/${id}`}
      title="Session"
      qualifier={id}
      missing={{
        title: "No such session",
        note: "This id is not in the session store.",
      }}
    >
      {(session) => (
        <>
          <Facts
            items={[
              { key: "Status", value: <Status status={session.status} /> },
              { key: "Repository", value: session.fork },
              { key: "Backend", value: session.model ?? session.backend },
              { key: "Turns", value: String(session.turnCount) },
              { key: "Spent", value: `$${session.costUsd.toFixed(4)} of $${session.budget}` },
              { key: "Last activity", value: <Time iso={session.lastActivityAt} /> },
            ]}
          />
          <Section title="Conversation">
            {streamError !== null && <p className="chat-error">{streamError}</p>}
            {!live && streamError === null && <p className="empty">Attaching…</p>}
            <Conversation events={events} />
          </Section>
          <Compose session={session} onEnded={() => navigate("/chat")} />
        </>
      )}
    </LivePage>
  );
}

/** The turn box, plus the two things you can do to a session that is not a turn. */
function Compose({ session, onEnded }: { session: SessionDetail; onEnded: () => void }) {
  const [text, setText] = useState("");
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const { canType, canInterrupt, note } = turnState(session);

  const act = (what: Promise<unknown>, onOk: () => void) => {
    setBusy(true);
    setError(null);
    what
      .then(onOk)
      .catch((err: unknown) => setError(errorText(err)))
      .finally(() => setBusy(false));
  };

  const submit = (e: React.FormEvent) => {
    e.preventDefault();
    if (busy || !canType || text.trim() === "") return;
    // Cleared optimistically — the turn appears in the transcript on the next frame, and
    // leaving it in the box as well would show it twice — and put back if the send failed. A
    // refused turn is not a reason to lose what someone wrote, and "not idle" and "the daemon
    // is not answering" are both answers this box gets and both temporary.
    const turn = text;
    setText("");
    setBusy(true);
    setError(null);
    sendTurn(session.id, turn)
      .catch((err: unknown) => {
        setError(errorText(err));
        setText((current) => (current === "" ? turn : current));
      })
      .finally(() => setBusy(false));
  };

  return (
    <Section title="Say something">
      <form className="chat-compose" onSubmit={submit}>
        <textarea
          value={text}
          onChange={(e) => setText(e.target.value)}
          // Enter sends, shift-enter is a newline: a chat box behaves like a chat box, and a
          // turn is usually one line.
          onKeyDown={(e) => {
            if (e.key === "Enter" && !e.shiftKey) submit(e);
          }}
          placeholder={canType ? "Type a turn; enter sends, shift-enter is a newline." : note}
          disabled={!canType || busy}
          rows={3}
          aria-label="Your turn"
        />
        <div className="chat-actions">
          <button type="submit" disabled={!canType || busy || text.trim() === ""}>
            Send
          </button>
          {canInterrupt && (
            <button
              type="button"
              disabled={busy}
              onClick={() => act(interruptSession(session.id), () => undefined)}
            >
              Interrupt
            </button>
          )}
          {/* Ending releases the clone slot and the process, and cannot be undone — so it asks,
              exactly as cancelling a task does. */}
          {!["ended", "failed"].includes(session.status) && (
            <button
              type="button"
              className="chat-end"
              disabled={busy}
              onClick={() => {
                if (window.confirm("End this session? The transcript is kept.")) {
                  act(endSession(session.id), onEnded);
                }
              }}
            >
              End session
            </button>
          )}
        </div>
      </form>
      {error !== null && <p className="chat-error">{error}</p>}
    </Section>
  );
}
