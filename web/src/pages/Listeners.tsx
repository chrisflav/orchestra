import { useParams } from "react-router-dom";
import type { RateLimitStatus } from "../api";
import { Empty, List, Row } from "../components/List";
import { Facts, LivePage, Section } from "../components/Page";
import type { Fact } from "../components/Page";
import { EnabledStatus } from "../components/Status";
import { Time } from "../components/Time";
import { interval, orDash, untilTime } from "../format";

/**
 * A listener's dispatch ceilings, in the one line a list row has for them.
 *
 * A full ceiling is the only thing about them worth interrupting a scan for — it is the answer
 * to "why has this listener gone quiet" — so a full one is what the row says, and when there is
 * none the row says how much of each is spent instead. `null` for a listener that is not paced,
 * which `Row` then drops rather than rendering an empty separator around.
 */
function rateLimitLabel(limits: RateLimitStatus[]): string | null {
  if (limits.length === 0) return null;
  const full = limits.find((l) => l.remaining === 0);
  if (full !== undefined) return `at ${full.description}, room ${untilTime(full.nextAllowedAt)}`;
  return limits.map((l) => `${l.used} of ${l.description}`).join(", ");
}

export function Listeners() {
  return (
    <LivePage endpoint="listeners" title="Listeners">
      {(data) => (
        <List>
          {data.items.length === 0 ? (
            <Empty>No listeners are configured.</Empty>
          ) : (
            data.items.map((l) => (
              <Row
                key={l.name}
                to={`/listeners/${encodeURIComponent(l.name)}`}
                title={l.name}
                end={<EnabledStatus enabled={l.enabled} />}
                meta={[
                  l.sourceType,
                  `every ${interval(l.intervalSeconds)}`,
                  <>
                    checked <Time key="t" iso={l.lastCheckedAt ?? ""} />
                  </>,
                  `${l.eventCount} events`,
                  rateLimitLabel(l.rateLimits),
                ]}
              />
            ))
          )}
        </List>
      )}
    </LivePage>
  );
}

export function ListenerDetail() {
  const { name = "" } = useParams();
  return (
    <LivePage
      endpoint={`listeners/${name}`}
      title="Listener"
      qualifier={name}
      missing={{
        title: "No such listener",
        note: "Nothing by this name is configured under the listeners directory.",
      }}
    >
      {(data) => {
        const facts: Fact[] = [
          { key: "State", value: <EnabledStatus enabled={data.enabled} /> },
          { key: "Source", value: data.sourceType, data: true },
          { key: "Polls every", value: interval(data.intervalSeconds), data: true },
          { key: "Last checked", value: <Time iso={data.lastCheckedAt ?? ""} />, data: true },
          { key: "Events seen", value: data.eventCount, data: true },
          ...data.sourceExtras.map(([k, v]): Fact => ({ key: k, value: v, data: true })),
        ];

        return (
          <>
            <Facts items={facts} />

            <Section title="Watching">
              <div className="panel">
                <pre className="pre">{data.sourceDetail}</pre>
              </div>
            </Section>

            <Section title="What it queues">
              <Facts
                items={[
                  { key: "Mode", value: data.action.mode, data: true },
                  { key: "Upstream", value: orDash(data.action.upstream), data: true },
                  { key: "Fork", value: orDash(data.action.fork), data: true },
                  { key: "Backend", value: orDash(data.action.backend), data: true },
                  { key: "Model", value: orDash(data.action.model), data: true },
                  { key: "Series", value: orDash(data.action.series), data: true },
                  { key: "Workflow", value: orDash(data.action.workflowPath), data: true },
                  { key: "Priority", value: data.action.priority, data: true },
                ]}
              />
              <div className="panel" style={{ marginTop: "18px" }}>
                <div className="caption">Prompt template</div>
                <pre className="pre">{data.action.promptTemplate}</pre>
              </div>
            </Section>

            {data.rateLimits.length > 0 && (
              <Section
                title="Rate limits"
                meta={`${data.recentDispatches.length} dispatches on record`}
              >
                <List>
                  {data.rateLimits.map((l) => (
                    <Row
                      key={l.description}
                      title={l.description}
                      meta={[
                        `${l.used} used`,
                        `${l.remaining} left`,
                        l.remaining === 0 ? `room ${untilTime(l.nextAllowedAt)}` : null,
                      ]}
                      end={l.remaining === 0 ? "full" : "open"}
                    />
                  ))}
                </List>
              </Section>
            )}

            <Section title="Processed events" meta={`${data.eventCount} seen`}>
              <List>
                {data.recentEvents.length === 0 ? (
                  <Empty>This listener has not queued anything yet.</Empty>
                ) : (
                  data.recentEvents.map((e, i) => (
                    <Row key={`${i}:${e}`} title={e} titleIsData />
                  ))
                )}
              </List>
            </Section>
          </>
        );
      }}
    </LivePage>
  );
}
