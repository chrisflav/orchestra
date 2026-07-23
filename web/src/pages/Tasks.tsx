import { useParams } from "react-router-dom";
import { Empty, List, Row } from "../components/List";
import { LogView } from "../components/LogView";
import { Facts, LivePage, Section } from "../components/Page";
import { Status } from "../components/Status";
import { Time } from "../components/Time";

export function Tasks() {
  return (
    <LivePage endpoint="tasks" title="Tasks">
      {(data) => (
        <List>
          {data.items.length === 0 ? (
            <Empty>No tasks have run yet.</Empty>
          ) : (
            data.items.map((t) => (
              <Row
                key={t.id}
                to={`/tasks/${encodeURIComponent(t.id)}`}
                title={t.prompt}
                end={<Status status={t.status} />}
                meta={[
                  t.id,
                  t.fork,
                  <Time key="t" iso={t.createdAt} />,
                  t.series ? `series ${t.series}` : "",
                ]}
              />
            ))
          )}
        </List>
      )}
    </LivePage>
  );
}

export function TaskDetail() {
  const { id = "" } = useParams();
  return (
    <LivePage
      endpoint={`tasks/${id}`}
      title="Task"
      qualifier={id}
      missing={{
        title: "No such task",
        note: "This id is not in the queue or the task store.",
      }}
    >
      {(data) => (
        <>
          <Facts
            items={[
              { key: "Status", value: <Status status={data.status} /> },
              { key: "Fork", value: data.fork, data: true },
              { key: "Started", value: <Time iso={data.createdAt} />, data: true },
              { key: "Log events", value: data.logTotal, data: true },
            ]}
          />

          <Section title="Prompt">
            <div className="panel">
              <pre className="pre">{data.prompt}</pre>
            </div>
          </Section>

          <Section
            title="Log"
            meta={data.logTruncated ? `last ${data.log.length} of ${data.logTotal}` : undefined}
          >
            <LogView events={data.log} total={data.logTotal} truncated={data.logTruncated} />
          </Section>
        </>
      )}
    </LivePage>
  );
}
