import { useParams } from "react-router-dom";
import { Empty, List, Row } from "../components/List";
import { Facts, LivePage, Section } from "../components/Page";
import { Status } from "../components/Status";
import { Time } from "../components/Time";
import { orDash } from "../format";

/**
 * A single concert run and the tasks it is stepping through.
 *
 * There is no concert *list* page: `Queue` carries that, since a concert is queue activity.
 * This page is reached by clicking a run there.
 */
export function ConcertDetail() {
  const { id = "" } = useParams();
  return (
    <LivePage
      endpoint={`concerts/${id}`}
      title="Concert"
      qualifier={id}
      missing={{
        title: "No such concert",
        note: "This run is not in the store. It may have been cleaned up.",
      }}
    >
      {(data) => (
        <>
          <Facts
            items={[
              { key: "Status", value: <Status status={data.concert.status} /> },
              { key: "Name", value: orDash(data.concert.name) },
              { key: "Workflow", value: orDash(data.concert.workflowFile), data: true },
              { key: "Started", value: <Time iso={data.concert.startedAt} />, data: true },
              {
                key: "Finished",
                value: data.concert.finishedAt ? <Time iso={data.concert.finishedAt} /> : "—",
                data: true,
              },
              { key: "Steps", value: data.steps.length, data: true },
            ]}
          />

          {/* Order is real information here — a concert suspends and resumes between steps —
              so this list stays in the order the workflow defines. */}
          <Section title="Steps">
            <List>
              {data.steps.length === 0 ? (
                <Empty>This concert has no steps yet.</Empty>
              ) : (
                data.steps.map((s) => (
                  <Row
                    key={s.id}
                    to={`/tasks/${encodeURIComponent(s.id)}`}
                    title={s.concertStepKey || s.prompt}
                    end={<Status status={s.status} />}
                    meta={[s.id, s.fork, <Time key="t" iso={s.createdAt} />]}
                  />
                ))
              )}
            </List>
          </Section>
        </>
      )}
    </LivePage>
  );
}
