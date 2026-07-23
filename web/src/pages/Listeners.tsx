import { useParams } from "react-router-dom";
import { Empty, List, Row } from "../components/List";
import { Facts, LivePage, Section } from "../components/Page";
import type { Fact } from "../components/Page";
import { EnabledStatus } from "../components/Status";
import { Time } from "../components/Time";
import { interval, orDash } from "../format";

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
