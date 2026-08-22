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
import { orDash } from "../format";
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

  // The page is the scroll region — there is no box around the conversation and none inside it —
  // so the thing to keep at the bottom is the window. Only while the reader is already there:
  // scrolling back to read something must not be undone by the next turn. The margin is
  // generous because the composer sits below the transcript, and someone at the composer is at
  // the bottom of the conversation whatever the pixel arithmetic says.
  const pinned = useRef(true);
  useEffect(() => {
    const onScroll = () => {
      const doc = document.documentElement;
      pinned.current = doc.scrollHeight - window.scrollY - doc.clientHeight < 120;
    };
    window.addEventListener("scroll", onScroll, { passive: true });
    return () => window.removeEventListener("scroll", onScroll);
  }, []);
  useEffect(() => {
    if (pinned.current) window.scrollTo({ top: document.documentElement.scrollHeight });
  }, [blocks]);

  if (events.length === 0) {
    return <p className="empty">Nothing said yet. Type a turn below.</p>;
  }

  return (
    <div className="chat-transcript">
      {blocks.map((block, i) =>
        block.kind === "agent" ? (
          <LogView
            key={i}
            events={agentEvents(block.events)}
            total={block.events.length}
            truncated={false}
            flow
            // The task log's 4000-character cap is a guard against a runaway command; here the
            // blob is the answer, and cutting it at 4000 is the same "shown partially" the
            // transcript's own layout used to be guilty of. Still bounded, but well past any
            // real turn.
            maxBlob={100000}
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

/**
 * Suggestions for the model box — not a closed list.
 *
 * orchestra passes this string through to the backend CLI's `--model` and has never had an
 * opinion about what is valid, so the field stays a text input and this is only a `datalist`:
 * anything the CLI accepts can be typed, and a suggestion that goes stale costs a keystroke
 * rather than blocking a session. The family aliases lead because they are the durable half —
 * they name a family rather than a release, so they keep meaning something as models come and
 * go. The pinned ids are for a conversation that has to stay on one model.
 *
 * `claude` is the only backend that can host a session today; if that changes, these become
 * suggestions for the wrong CLI, which is another reason not to make them a closed set.
 */
const MODEL_SUGGESTIONS = [
  "fable",
  "opus",
  "sonnet",
  "haiku",
  "claude-fable-5",
  "claude-opus-5",
  "claude-opus-4-8",
  "claude-sonnet-5",
  "claude-haiku-4-5",
];

/** Starting one. The only form in this console that runs an agent. */
function NewSession() {
  const navigate = useNavigate();
  const [upstream, setUpstream] = useState("");
  const [fork, setFork] = useState("");
  const [model, setModel] = useState("");
  const [budget, setBudget] = useState("");
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const submit = (e: React.FormEvent) => {
    e.preventDefault();
    if (busy || upstream.trim() === "" || fork.trim() === "") return;
    const chosen = model.trim();
    const spend = budget.trim();
    // Parsed here rather than by `type="number"`, which hands back an empty string for anything
    // it cannot read: "abour" would leave the box reading what was typed while the request
    // carried no budget at all, and the session would run on the default with the person
    // believing they had capped it. The one field whose whole purpose is to bound spending is
    // the one that must not fail quietly — the same reason `orchestra chat --budget` refuses a
    // value it cannot parse instead of dropping it.
    //
    // Matched against the JSON number grammar rather than handed to `Number`, which reads
    // `0x10` as sixteen and `0b101` as five. A box labelled "budget USD" that quietly charges
    // $16 for `0x10` is the same class of surprise, and it is a grammar the CLI's own
    // `--budget` already holds to, so the two clients agree on what an amount is.
    //
    // Only the shape and a floor of one cent are checked here — anything smaller reaches the
    // agent as `0.000000`. The ceiling is the server's, and asking it is cheaper than keeping a
    // copy of `maxSessionBudgetUsd` in the browser that can drift out of date.
    let amount: number | undefined;
    if (spend !== "") {
      if (!/^[+-]?\d+(\.\d+)?([eE][+-]?\d+)?$/.test(spend)) {
        setError(`The budget is an amount in dollars, like 5 or 12.50; '${spend}' is not one.`);
        return;
      }
      amount = Number(spend);
      if (!Number.isFinite(amount) || amount < 0.01) {
        setError(`The budget must be at least 0.01 USD; '${spend}' is not.`);
        return;
      }
    }
    setBusy(true);
    setError(null);
    startSession({
      upstream: upstream.trim(),
      fork: fork.trim(),
      // Left out rather than sent empty. Absent means "whatever the backend runs by default",
      // which is a different request from asking it to resolve a model named "".
      ...(chosen === "" ? {} : { model: chosen }),
      ...(amount === undefined ? {} : { budget: amount }),
    })
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
        <input
          className="chat-start-model"
          list="chat-models"
          value={model}
          onChange={(e) => setModel(e.target.value)}
          placeholder="model (optional)"
          // The placeholder is not part of the accessible name once `aria-label` is set, and
          // "optional" is the one thing about this field a listener cannot otherwise tell.
          aria-label="Model (optional)"
        />
        <datalist id="chat-models">
          {MODEL_SUGGESTIONS.map((m) => (
            <option key={m} value={m} />
          ))}
        </datalist>
        <input
          className="chat-start-budget"
          inputMode="decimal"
          value={budget}
          onChange={(e) => setBudget(e.target.value)}
          placeholder="budget USD (optional)"
          aria-label="Budget in USD (optional)"
        />
        <button type="submit" disabled={busy}>
          {busy ? "Starting…" : "Start"}
        </button>
      </form>
      {/* The daemon's sentence, verbatim: it is the only thing that knows why it refused.
          `role="alert"` because a refusal is the whole outcome of pressing Start, and without it
          a reader who cannot see the form is told nothing at all. */}
      {error !== null && (
        <p className="chat-error" role="alert">
          {error}
        </p>
      )}
    </Section>
  );
}

/**
 * The transcript of one session, live while there is anything left to be live about.
 *
 * Its own component so that it can be told the session's status. `useTranscript` had no way to
 * know a conversation was over: the server closes the stream of a finished session deliberately,
 * `EventSource` treats every close as something to retry, and the page said "Attaching…" over a
 * transcript that was already complete — for as long as the tab stayed open, at a reconnect
 * every three seconds, each one costing a full read of the transcript on the server.
 */
function Transcript({ session }: { session: SessionDetail }) {
  const finished = ["ended", "failed"].includes(session.status);
  const { events, error: streamError, live } = useTranscript(session.id, finished);
  return (
    <Section title="Conversation">
      {streamError !== null && <p className="chat-error">{streamError}</p>}
      {!live && !finished && streamError === null && <p className="empty">Attaching…</p>}
      <Conversation events={events} />
    </Section>
  );
}

/** One conversation, and the box to say something into. */
export function ChatDetail() {
  const { id = "" } = useParams();
  const navigate = useNavigate();

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
              { key: "Backend", value: session.backend },
              // The model as it was asked for, not as the agent resolved it: a session started
              // without one runs on the backend's default, and saying "—" is the honest way to
              // show that rather than naming a model nobody chose.
              { key: "Model", value: orDash(session.model) },
              { key: "Turns", value: String(session.turnCount) },
              // Both halves to the same precision. `${session.budget}` was fine while every
              // session had the 20.0 default; now that a budget can be set it reads `$4.5` and
              // `$3.141593` against a four-decimal spend.
              {
                key: "Spent",
                value: `$${session.costUsd.toFixed(4)} of $${session.budget.toFixed(2)}`,
              },
              { key: "Last activity", value: <Time iso={session.lastActivityAt} /> },
            ]}
          />
          <Transcript session={session} />
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
