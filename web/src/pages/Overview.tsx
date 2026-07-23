import type { ReactNode } from "react";
import { Link } from "react-router-dom";
import type { Overview as OverviewData, QueueEntry } from "../api";
import { Empty, List, Row } from "../components/List";
import { PageHead, Section } from "../components/Page";
import { sectionOf, Status } from "../components/Status";
import { Time } from "../components/Time";
import { relativeTime, truncate } from "../format";
import { useOverview } from "../overview";

interface Verdict {
  /** The finding, in the display face. */
  lead: string;
  /** The evidence for it, quieter. */
  trail: string;
  /** What to do about it. */
  note: ReactNode;
  /** Whether the finding is waiting on a person, which is the only thing that goes red. */
  stuck: boolean;
}

/**
 * Read the counts the way an operator would, and say what they mean.
 *
 * This is the page's whole reason to exist. The states are ordered by how much they should
 * worry someone: an exhausted account silently holding up the queue is the case the README
 * names as the reason to open this dashboard at all, so it is checked first and is the only
 * one that points somewhere else to act.
 */
function readVerdict(d: OverviewData): Verdict {
  const c = d.counts;
  const queued = c.pending;
  const tasks = (n: number) => `${n} ${n === 1 ? "task" : "tasks"}`;

  if (c.authTotal > 0 && c.authFree === 0) {
    return {
      lead: "Nothing can start.",
      trail: "Every auth source is exhausted.",
      note: (
        <>
          {queued > 0
            ? `${tasks(queued)} will stay queued until a limit lifts. `
            : "New work will queue until a limit lifts. "}
          <Link to="/auth">See which limit is binding</Link>.
        </>
      ),
      stuck: true,
    };
  }

  if (c.running === 0 && queued > 0) {
    return {
      lead: "Nothing is running.",
      trail: `${tasks(queued)} waiting to be claimed.`,
      note: "The queue has work but no agent has taken it. Check that the daemon is running.",
      stuck: true,
    };
  }

  if (c.running > 0) {
    return {
      lead: `${tasks(c.running)} running.`,
      trail: queued > 0 ? `${queued} queued behind them.` : "Nothing else queued.",
      note:
        c.failed > 0
          ? `${tasks(c.failed)} failed and ${c.failed === 1 ? "is" : "are"} still in the queue.`
          : "The queue is draining normally.",
      stuck: false,
    };
  }

  if (c.failed > 0) {
    return {
      lead: "Idle.",
      trail: `${tasks(c.failed)} failed and still in the queue.`,
      note: "Retry or cancel them to clear the queue.",
      stuck: false,
    };
  }

  return {
    lead: "Idle.",
    trail: "Nothing queued, nothing running.",
    note:
      c.totalTasks > 0
        ? `${c.totalTasks} tasks in history.`
        : "Enqueue work, or let a listener do it.",
    stuck: false,
  };
}

/**
 * Past this many entries the ticks are thinner than the gaps between them and the strip stops
 * being readable, so the remainder is stated in the caption instead of drawn.
 */
const STRIP_MAX = 72;

/**
 * The queue, drawn at page scale: one tick per entry, in the order it is held.
 *
 * The figures below say how much work there is. This says what shape it is in — how many
 * agents are actually turning, how deep the tail behind them runs, whether a failure is
 * sitting in the middle of it — which is what the counts alone make the reader assemble in
 * their head. It encodes nothing the API does not send: order, and status. The wordmark is
 * four bars; at this size, so is the queue.
 */
function Strip({ entries }: { entries: QueueEntry[] }) {
  if (entries.length === 0) {
    return (
      <div className="strip">
        <div className="strip-empty" />
      </div>
    );
  }

  const shown = entries.slice(0, STRIP_MAX);
  const hidden = entries.length - shown.length;
  // The queue is held newest-last, so the entry that has waited longest is the first one
  // still pending.
  const oldest = entries.find((e) => e.status === "pending");

  return (
    <div className="strip">
      <div className="strip-ticks">
        {shown.map((e) => (
          <Link
            key={e.id}
            className={`tick tick-${sectionOf(e.status)}`}
            to={`/tasks/${encodeURIComponent(e.id)}`}
            title={`${e.status} — ${truncate(e.prompt, 90)}`}
            aria-label={`${e.status}: ${truncate(e.prompt, 90)}`}
          >
            <span className="tick-bar" />
          </Link>
        ))}
      </div>
      {/* Only what the ledger below cannot say. The counts live there; this is the one fact
          about the queue that no count carries. */}
      <p className="strip-caption">
        {oldest !== undefined && <span>oldest waiting {relativeTime(oldest.createdAt)}</span>}
        {hidden > 0 && <span>{hidden} more not drawn</span>}
      </p>
    </div>
  );
}

function Figure({
  value,
  label,
  tone = "",
  to,
}: {
  value: number | string;
  label: string;
  tone?: string;
  to?: string;
}) {
  const cls = value === 0 ? "zero" : tone;
  const body = (
    <>
      <div className={`figure-value ${cls}`}>{value}</div>
      <div className="figure-label">{label}</div>
    </>
  );
  return to === undefined ? (
    <div className="figure">{body}</div>
  ) : (
    <Link className="figure" to={to}>
      {body}
    </Link>
  );
}

export function Overview() {
  const { data, error, live } = useOverview();

  if (error !== null) {
    return (
      <>
        <PageHead title="Overview" />
        <div className="panel">
          <div className="notice">
            <div className="notice-title">Could not load the overview</div>
            <p className="notice-note data">{error}</p>
          </div>
        </div>
      </>
    );
  }

  if (data === null) {
    return (
      <>
        <PageHead title="Overview" />
        <p className="empty">Loading…</p>
      </>
    );
  }

  const c = data.counts;
  const verdict = readVerdict(data);

  return (
    <>
      {/* No page title: the verdict *is* the heading. A reader who lands here wants the
          finding, not the name of the screen they can see they are on. */}
      <p className="eyebrow">
        <span>Queue</span>
        <span className="eyebrow-sep">/</span>
        <span className={`pulse ${live ? "on" : "off"}`}>
          <span className="pulse-dot" />
          {live ? "live" : "reconnecting"}
        </span>
      </p>

      <h1 className={`verdict ${verdict.stuck ? "stuck" : ""}`}>
        <span className="lead">{verdict.lead}</span>{" "}
        <span className="trail">{verdict.trail}</span>
      </h1>
      <p className="lede">{verdict.note}</p>

      <Strip entries={data.activeQueue} />

      <div className="ledger">
        <Figure value={c.running} label="Running" tone="brass" to="/queue" />
        <Figure value={c.pending} label="Queued" to="/queue" />
        <Figure value={c.failed} label="Failed" tone="perc" to="/queue" />
        <Figure value={c.concerts} label="Concerts" to="/queue" />
        <Figure value={c.listeners} label="Listeners" to="/listeners" />
        <Figure value={c.totalTasks} label="Tasks" to="/tasks" />
        {c.authTotal > 0 && (
          <Figure
            value={`${c.authFree}/${c.authTotal}`}
            label="Auth sources"
            tone={c.authFree === 0 ? "perc" : ""}
            to="/auth"
          />
        )}
      </div>

      <Section title="In flight" meta={data.activeQueue.length > 0 ? "live" : undefined}>
        <List>
          {data.activeQueue.length === 0 ? (
            <Empty>Nothing running or queued.</Empty>
          ) : (
            data.activeQueue.map((e) => (
              <Row
                key={e.id}
                to={`/tasks/${encodeURIComponent(e.id)}`}
                title={e.prompt}
                end={<Status status={e.status} />}
                meta={[
                  e.fork,
                  <Time key="t" iso={e.createdAt} />,
                  `priority ${e.priority}`,
                  e.concertId ? `concert ${e.concertId}` : "",
                ]}
              />
            ))
          )}
        </List>
      </Section>

      <Section title="Recently" meta={`${data.recentTasks.length} of ${c.totalTasks}`}>
        <List>
          {data.recentTasks.length === 0 ? (
            <Empty>No tasks have run yet.</Empty>
          ) : (
            data.recentTasks.map((t) => (
              <Row
                key={t.id}
                to={`/tasks/${encodeURIComponent(t.id)}`}
                title={t.prompt}
                end={<Status status={t.status} />}
                meta={[t.fork, <Time key="t" iso={t.createdAt} />, t.series]}
              />
            ))
          )}
        </List>
      </Section>
    </>
  );
}
