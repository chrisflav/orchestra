import { Empty, List, Row } from "../components/List";
import { LivePage, Section } from "../components/Page";
import { Status } from "../components/Status";
import { Time } from "../components/Time";

/**
 * Queue and concerts on one page.
 *
 * A concert is queue activity — its steps are queue entries with a back-reference — so a
 * separate Concerts page was listing the same runs a second time. Merging them removed a
 * top-level destination without losing anything; concert detail is still a page, reached by
 * clicking a run.
 */
export function Queue() {
  return (
    <LivePage endpoint="queue" title="Queue">
      {(data) => (
        <>
          <Section title="Concert runs" meta={`${data.concerts.length}`}>
            <List>
              {data.concerts.length === 0 ? (
                <Empty>No concerts have run.</Empty>
              ) : (
                data.concerts.map((c) => (
                  <Row
                    key={c.id}
                    to={`/concerts/${encodeURIComponent(c.id)}`}
                    title={c.name || c.workflowFile || c.id}
                    end={<Status status={c.status} />}
                    meta={[
                      c.id,
                      <Time key="s" iso={c.startedAt} />,
                      c.finishedAt ? "finished" : "",
                    ]}
                  />
                ))
              )}
            </List>
          </Section>

          <Section title="Entries" meta={`${data.entries.length}`}>
            <List>
              {data.entries.length === 0 ? (
                <Empty>The queue is empty.</Empty>
              ) : (
                data.entries.map((e) => (
                  <Row
                    key={e.id}
                    to={`/tasks/${encodeURIComponent(e.id)}`}
                    title={e.prompt}
                    end={<Status status={e.status} />}
                    meta={[
                      e.fork,
                      <Time key="t" iso={e.createdAt} />,
                      `priority ${e.priority}`,
                      e.series ? `series ${e.series}` : "",
                      e.concertId ? `concert ${e.concertId}` : "",
                    ]}
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
