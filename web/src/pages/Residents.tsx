import { useState } from "react";
import { Link, useParams } from "react-router-dom";
import { controlResident } from "../api";
import type { ResidentAction, ResidentSummary } from "../api";
import { Empty, List, Row } from "../components/List";
import { Facts, LivePage, Section } from "../components/Page";
import type { Fact } from "../components/Page";
import { Status } from "../components/Status";
import { Time } from "../components/Time";
import { orDash, truncate, untilTime } from "../format";

/** The state as the design names it, with why a paused one is paused. */
function stateLabel(r: ResidentSummary): string {
  if (r.state === "paused" && r.pausedReason !== null) return `paused: ${r.pausedReason}`;
  return r.state;
}

/** The section colour follows the session status; the label is the resident's own word. */
function ResidentStatus({ r }: { r: ResidentSummary }) {
  const section = r.state === "cold" ? "dormant" : r.state === "waking" ? "starting" : r.state;
  return (
    <span title={stateLabel(r)}>
      <Status status={section} />
      {r.state === "cold" && <span className="sep"> cold</span>}
      {r.state === "waking" && <span className="sep"> waking</span>}
    </span>
  );
}

function usd(n: number): string {
  return `$${n.toFixed(2)}`;
}

/** Today's spend against the day's budget, as a short bar and the two figures. */
function SpendMeter({ r }: { r: ResidentSummary }) {
  if (r.budgetPerDay === null || r.budgetPerDay <= 0) return <>{usd(r.spentToday)} today</>;
  const frac = Math.min(1, r.spentToday / r.budgetPerDay);
  return (
    <span title={`${usd(r.spentToday)} of ${usd(r.budgetPerDay)} on ${r.budgetDay}`}>
      <span
        aria-hidden="true"
        style={{
          display: "inline-block",
          width: "48px",
          height: "6px",
          borderRadius: "3px",
          marginRight: "6px",
          verticalAlign: "middle",
          background: "var(--rule, rgba(127,127,127,0.25))",
          overflow: "hidden",
        }}
      >
        <span
          style={{
            display: "block",
            height: "100%",
            width: `${Math.round(frac * 100)}%`,
            background: frac >= 0.9 ? "var(--percussion, #c0392b)" : "var(--brass, #b8860b)",
          }}
        />
      </span>
      {usd(r.spentToday)} / {usd(r.budgetPerDay)}
    </span>
  );
}

function failuresLabel(r: ResidentSummary): string | null {
  const n = r.failures ?? 0;
  if (n === 0) return null;
  const retry = r.retryAfter ? `, retry ${untilTime(r.retryAfter)}` : "";
  return `${n} failure${n === 1 ? "" : "s"} in a row${retry}`;
}

function lastEventLabel(r: ResidentSummary) {
  const e = r.lastEvent;
  if (!e) return null;
  return (
    <>
      last {e.source} <Time iso={e.receivedAt} />
    </>
  );
}

export function Residents() {
  return (
    <LivePage endpoint="residents" title="Residents">
      {(data) => (
        <List>
          {data.items.length === 0 ? (
            <Empty>No identity is configured as a resident.</Empty>
          ) : (
            data.items.map((r) => (
              <Row
                key={r.name}
                to={`/residents/${encodeURIComponent(r.name)}`}
                title={r.name}
                end={<ResidentStatus r={r} />}
                meta={[
                  r.state === "paused" ? stateLabel(r) : null,
                  r.configured ? null : "no longer configured",
                  `${r.inboxReady} in inbox`,
                  lastEventLabel(r),
                  <SpendMeter key="spend" r={r} />,
                  failuresLabel(r),
                  r.tokenExpiresAt ? `token ${untilTime(r.tokenExpiresAt)}` : null,
                ]}
              />
            ))
          )}
        </List>
      )}
    </LivePage>
  );
}

/** Pause, resume and reset; reset asks first, since it ends the conversation. */
function Controls({ r }: { r: ResidentSummary }) {
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const run = async (action: ResidentAction) => {
    if (
      action === "reset" &&
      !window.confirm(
        `Reset ${r.name}? Its conversation ends and the next turn starts a new one; its memory directory, inbox and watches are kept.`,
      )
    ) {
      return;
    }
    setBusy(true);
    setError(null);
    try {
      await controlResident(r.name, action);
    } catch (err) {
      setError(err instanceof Error ? err.message : String(err));
    } finally {
      setBusy(false);
    }
  };
  return (
    <div className="chat-actions" style={{ display: "flex", gap: "8px", alignItems: "center" }}>
      {r.state === "paused" ? (
        <button type="button" disabled={busy} onClick={() => void run("resume")}>
          Resume
        </button>
      ) : (
        <button type="button" disabled={busy} onClick={() => void run("pause")}>
          Pause
        </button>
      )}
      <button
        type="button"
        className="cancel-button danger"
        disabled={busy}
        onClick={() => void run("reset")}
      >
        Reset
      </button>
      <Link className="text-button" to={`/chat/${encodeURIComponent(r.sessionId)}`}>
        Open chat
      </Link>
      {error !== null && <span className="notice-note data">{error}</span>}
    </div>
  );
}

export function ResidentDetail() {
  const { name = "" } = useParams();
  return (
    <LivePage
      endpoint={`residents/${name}`}
      title="Resident"
      qualifier={name}
      missing={{
        title: "No such resident",
        note: "No resident session goes by this name.",
      }}
    >
      {(data) => {
        const facts: Fact[] = [
          { key: "State", value: <ResidentStatus r={data} /> },
          { key: "Inbox", value: `${data.inboxReady} ready`, data: true },
          { key: "Today", value: <SpendMeter r={data} />, data: true },
          { key: "Turns", value: data.turnCount, data: true },
          { key: "Last activity", value: <Time iso={data.lastActivityAt} />, data: true },
          {
            key: "Token expires",
            value: data.tokenExpiresAt ? <Time iso={data.tokenExpiresAt} /> : "—",
            data: true,
          },
          { key: "Failures", value: orDash(failuresLabel(data)), data: true },
          { key: "Model", value: orDash(data.model), data: true },
        ];
        return (
          <>
            <Facts items={facts} />
            {data.pausedReason !== null && (
              <p className="notice-note">Paused: {data.pausedReason}</p>
            )}
            {data.error !== null && <p className="notice-note data">{data.error}</p>}
            <Controls r={data} />

            <Section title="Feeds" meta={`${data.feeds.length}`}>
              <List>
                {data.feeds.length === 0 ? (
                  <Empty>No feeds: people and listeners wake it.</Empty>
                ) : (
                  data.feeds.map((f) => (
                    <Row
                      key={f.listener}
                      to={`/listeners/${encodeURIComponent(f.listener)}`}
                      title={`${f.name} (${f.site})`}
                      end={<Status status={f.error ? "failed" : f.enabled ? f.connection : "cancelled"} />}
                      meta={[
                        f.connection,
                        f.error,
                        f.lastEventAt ? (
                          <>
                            last event <Time key="t" iso={f.lastEventAt} />
                          </>
                        ) : null,
                      ]}
                    />
                  ))
                )}
              </List>
            </Section>

            <Section title="Watches" meta={`${data.watches.length}`}>
              <List>
                {data.watches.length === 0 ? (
                  <Empty>It is not watching anything.</Empty>
                ) : (
                  data.watches.map((w) => (
                    <Row
                      key={w.id}
                      title={`#${w.stream}${w.topic !== null ? ` > ${w.topic}` : " (every topic)"}`}
                      meta={[
                        w.feed,
                        w.note,
                        w.until !== null ? `until ${w.until}` : "until unwatched",
                      ]}
                    />
                  ))
                )}
              </List>
            </Section>

            <Section title="Inbox" meta={`${data.inbox.total} in all`}>
              <List>
                {data.inbox.items.length === 0 ? (
                  <Empty>Nothing has arrived yet.</Empty>
                ) : (
                  data.inbox.items.map((i) => {
                    const text =
                      i.payload !== null &&
                      typeof i.payload === "object" &&
                      "text" in i.payload &&
                      typeof (i.payload as { text: unknown }).text === "string"
                        ? (i.payload as { text: string }).text
                        : JSON.stringify(i.payload);
                    return (
                      <Row
                        key={i.id}
                        title={truncate(text, 140)}
                        end={i.state}
                        meta={[
                          `#${i.id}`,
                          i.source,
                          i.trusted ? null : "untrusted",
                          <Time key="t" iso={i.receivedAt} />,
                          i.sentIn !== null ? `turn ${i.sentIn}` : null,
                        ]}
                      />
                    );
                  })
                )}
              </List>
            </Section>
          </>
        );
      }}
    </LivePage>
  );
}
