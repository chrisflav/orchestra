import { Empty, List, Row } from "../components/List";
import { PageHead, Section } from "../components/Page";
import { Status } from "../components/Status";
import { Time } from "../components/Time";
import { useLiveData } from "../useLiveData";

/**
 * Queue and concerts on one page.
 *
 * A concert is queue activity — its steps are queue entries with a back-reference — so a
 * separate Concerts page was listing the same runs a second time. Merging them removed a
 * top-level destination without losing anything; concert detail is still a page, reached by
 * clicking a run.
 *
 * Two subscriptions rather than one, because the API has two collections here rather than one
 * endpoint shaped for this page. That is the trade the resource-shaped API makes: a page that
 * shows two things asks for two things, and each is independently pageable.
 */
export function Queue() {
  const entries = useLiveData("queue");
  const concerts = useLiveData("concerts");

  const error = entries.error ?? concerts.error;
  if (error !== null) {
    return (
      <>
        <PageHead title="Queue" />
        <div className="panel">
          <div className="notice">
            <div className="notice-title">Could not load the queue</div>
            <p className="notice-note data">{error}</p>
          </div>
        </div>
      </>
    );
  }

  if (entries.data === null || concerts.data === null) {
    return (
      <>
        <PageHead title="Queue" />
        <p className="empty">Loading…</p>
      </>
    );
  }

  const shown = (n: number, total: number) => (n === total ? `${total}` : `${n} of ${total}`);

  return (
    <>
      <PageHead title="Queue" live={entries.live && concerts.live} />

      <Section
        title="Concert runs"
        meta={shown(concerts.data.items.length, concerts.data.total)}
      >
        <List>
          {concerts.data.items.length === 0 ? (
            <Empty>No concerts have run.</Empty>
          ) : (
            concerts.data.items.map((c) => (
              <Row
                key={c.id}
                to={`/concerts/${encodeURIComponent(c.id)}`}
                title={c.name ?? c.workflowFile ?? c.id}
                end={<Status status={c.status} />}
                meta={[c.id, <Time key="s" iso={c.startedAt} />, c.finishedAt ? "finished" : ""]}
              />
            ))
          )}
        </List>
      </Section>

      <Section title="Entries" meta={shown(entries.data.items.length, entries.data.total)}>
        <List>
          {entries.data.items.length === 0 ? (
            <Empty>The queue is empty.</Empty>
          ) : (
            entries.data.items.map((e) => (
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
  );
}
