import type {
  AuthSource,
  UsageBlock,
  UsageHistory,
  UsageHistorySource,
  UsageLimit,
  UsageWindow,
} from "../api";
import type { Bar } from "../components/Chart";
import { Bars } from "../components/Chart";
import { LivePage, Section } from "../components/Page";
import { Status } from "../components/Status";
import { relativeTime, sinceTime, untilTime } from "../format";
import { useLiveData } from "../useLiveData";

const SEVERITY: Record<string, string> = {
  normal: "",
  warning: "warn",
  critical: "crit",
};

/**
 * One reported limit as a labelled track.
 *
 * A subscription runs several limits at once — a rolling session window, a weekly total, and
 * weekly limits scoped to one model family — and usually only one is the one biting. A track
 * per limit shows which at a glance where a single percentage could not.
 */
function Limit({ limit }: { limit: UsageLimit }) {
  const percent = Math.max(0, Math.min(100, Number(limit.percent) || 0));
  // A scoped limit closes exactly one model family; naming it is the difference between
  // "the account is done" and "Opus is done, Sonnet is fine".
  const name = limit.scope ? `${limit.kind} · ${limit.scope}` : limit.kind;
  return (
    <tr>
      <td className="limit-name">
        {name}
        {limit.active && <span className="tag tag-alert">binding</span>}
      </td>
      <td className="limit-track">
        <div className="track">
          <div
            className={`track-fill ${SEVERITY[limit.severity] ?? ""}`}
            style={{ width: `${percent}%` }}
          />
        </div>
      </td>
      <td className="limit-pct">{percent}%</td>
      <td className="limit-reset">{untilTime(limit.resetsAt)}</td>
    </tr>
  );
}

/**
 * One observed block as a line of prose.
 *
 * No track, because there is no percentage to draw: a block is not a meter reading but a thing
 * that happened, and all a reader needs is what closed, why, and until when. It sits above the
 * limit tracks because it is the fact those tracks cannot show — a poll sees the account's own
 * windows and nothing scoped to a model family, so a block is the only evidence that one family
 * has stopped running while the source still reports itself available for the rest.
 */
function Block({ block }: { block: UsageBlock }) {
  const scope = block.model ?? "whole account";
  return (
    <li className="source-block">
      <span className="tag tag-alert">{scope}</span> {block.reason}
      {block.until !== null && ` — lifts ${untilTime(block.until)}`}
    </li>
  );
}

/**
 * A window as a bar.
 *
 * A *closed* window is drawn at its peak, which is what it consumed. The one still filling is
 * drawn at where it stands now, because that is the number the limit tracks above report and a
 * reader compares the two: a reading that came back down inside the window leaves the peak
 * stuck above the live number, and two different figures for "this week", with nothing to say
 * which is which, reads as a bug in the page rather than as the two facts it is. The peak it
 * has already reached is kept as a mark above the bar, so nothing is lost by drawing the lower
 * number.
 *
 * The exact instants go in the hover line rather than under the bars — sixty of them would be a
 * wall of text where the shape is the thing being read.
 */
function barsOf(windows: UsageWindow[]): Bar[] {
  return windows.map((window) => {
    const value = window.open ? window.percent : window.peakPercent;
    return {
      // Keyed by the series as well as the instant: one poll opens every window it reports at
      // the same `startedAt`, so a kind and a scope are both part of what makes a window itself.
      key: `${window.kind}:${window.scope ?? "*"}:${window.startedAt}`,
      value,
      ...(window.open ? { open: true } : {}),
      ...(window.peakPercent > value ? { peak: window.peakPercent } : {}),
      title: [
        window.startedAt,
        // Both numbers on the open window, named: "now" is what the tracks above show, "peak"
        // is the mark on the bar, and a hover is where the difference between them is settled.
        window.open ? `now ${window.percent}%` : null,
        `peak ${window.peakPercent}%`,
        window.open ? "still filling" : null,
        // A window built from one poll is a glimpse of it rather than a measurement, and the
        // bar cannot say so on its own.
        `${window.samples} ${window.samples === 1 ? "poll" : "polls"}`,
      ]
        .filter(Boolean)
        .join(" · "),
    };
  });
}

/** The ends of the time axis. The open window is "now", which is what it is. */
function axisOf(windows: UsageWindow[]): [string, string] | undefined {
  const first = windows[0];
  const last = windows[windows.length - 1];
  if (first === undefined || last === undefined) return undefined;
  return [relativeTime(first.startedAt), last.open ? "now" : relativeTime(last.updatedAt)];
}

function Chart({ title, windows, empty }: { title: string; windows: UsageWindow[]; empty: string }) {
  return <Bars title={title} bars={barsOf(windows)} empty={empty} axis={axisOf(windows)} />;
}

/**
 * What this source has spent, window by window.
 *
 * Two graphs because a subscription is two counters: the session window is the one that
 * decides whether the next task starts, and the weekly total is the one that decides whether
 * the rest of the week does. A weekly limit scoped to a model family is a third counter and
 * gets its own graph rather than being averaged into the account's.
 */
function History({ history }: { history: UsageHistorySource }) {
  const scopes = [...new Set(history.weeks.map((week) => week.scope))];
  return (
    <div className="charts">
      <Chart
        title="usage per session"
        windows={history.sessions}
        empty="Nothing recorded yet. A window opens the first time this source is polled."
      />
      {scopes.length === 0 ? (
        <Chart
          title="usage per week"
          windows={[]}
          empty="Nothing recorded yet. A window opens the first time this source is polled."
        />
      ) : (
        scopes.map((scope) => (
          <Chart
            // `null` is the account-wide window, which is not the same series as a limit
            // scoped to a model family that happens to be named with an empty string.
            key={scope === null ? "*" : `scope:${scope}`}
            title={scope === null ? "usage per week" : `usage per week · ${scope}`}
            windows={history.weeks.filter((week) => week.scope === scope)}
            empty="No weeks recorded yet."
          />
        ))
      )}
    </div>
  );
}

function Source({
  source,
  history,
}: {
  source: AuthSource;
  history: UsageHistorySource | undefined;
}) {
  // The API sends instants; the phrasing is this page's business, which is what keeps
  // "4m ago" true four minutes after the frame that carried it arrived.
  const notes = [
    source.pollable ? `polled ${sinceTime(source.polledAt)}` : "",
    `last used ${sinceTime(source.lastUsedAt)}`,
    source.baseUrl ? `base ${source.baseUrl}` : "",
    source.backoffUntil ? `not polling until ${untilTime(source.backoffUntil)}` : "",
  ].filter(Boolean);

  return (
    <div className="source">
      <div className="source-head">
        <span className="source-label">{source.label}</span>
        <span className="tag">{source.kind}</span>
        {source.isDefault && <span className="tag tag-default">default</span>}
        <div style={{ marginLeft: "auto" }}>
          <Status status={source.state} />
        </div>
      </div>

      {source.state === "blocked" && (
        <p className="source-why">
          {source.reason}
          {source.availableAt !== null && ` — frees up ${untilTime(source.availableAt)}`}
        </p>
      )}

      {source.blocks.length > 0 && (
        <ul className="source-blocks">
          {source.blocks.map((block, i) => (
            <Block key={`${block.model ?? "account"}:${i}`} block={block} />
          ))}
        </ul>
      )}

      {source.limits.length > 0 ? (
        <table className="limits">
          <tbody>
            {source.limits.map((limit) => (
              <Limit key={`${limit.kind}:${limit.scope}`} limit={limit} />
            ))}
          </tbody>
        </table>
      ) : (
        // An API-key source has no subscription window to poll, so an empty limit list on one
        // is the expected state and not a gap in the data.
        <p className="source-none">
          {source.pollable
            ? "No usage data yet. Nothing has polled this source."
            : "Billed per token. No subscription limits to report."}
        </p>
      )}

      {/*
        Only for a source that has a subscription to spend: an API-key source bills per token
        and has no window to fill, so an empty pair of graphs on one would be inviting the
        reader to look for data that will never exist.
      */}
      {source.pollable && history !== undefined && <History history={history} />}

      <div className="source-foot">
        {notes.join(" · ")}
        {source.lastError && (
          <div className="source-error">Last poll failed: {source.lastError}</div>
        )}
      </div>
    </div>
  );
}

/** The recorded history of one source, by the pair of names that identifies it. */
function historyOf(
  history: UsageHistory | null,
  backend: string,
  label: string,
): UsageHistorySource | undefined {
  return history?.backends
    .find((entry) => entry.name === backend)
    ?.sources.find((source) => source.label === label);
}

export function Auth() {
  // History rides its own endpoint, and so its own stream: `auth` is the current verdict and
  // changes with every poll, while this is a log that only ever grows. Read here rather than
  // inside the render callback below, which runs conditionally — a hook cannot.
  const history = useLiveData("usage");

  return (
    <LivePage endpoint="auth" title="Auth">
      {(data) => {
        if (data.configError) {
          return (
            <div className="panel">
              <div className="notice">
                <div className="notice-title">Could not read the configuration</div>
                <p className="notice-note">
                  This page reads config.json on every request, so fixing the file is enough — no
                  restart needed.
                </p>
                <p className="notice-note data">{data.configError}</p>
              </div>
            </div>
          );
        }

        if (data.backends.length === 0) {
          return (
            <div className="panel">
              <div className="notice">
                <div className="notice-title">No auth sources configured</div>
                <p className="notice-note">
                  Add an <code>agents</code> block to config.json to give the queue something to
                  run tasks with.
                </p>
              </div>
            </div>
          );
        }

        return (
          <>
            {data.backends.map((backend) => (
              <Section
                key={backend.name}
                title={backend.name}
                meta={`${backend.sources.length} ${
                  backend.sources.length === 1 ? "source" : "sources"
                }`}
              >
                {backend.sources.length > 0 ? (
                  backend.sources.map((source) => (
                    <Source
                      key={source.label}
                      source={source}
                      history={historyOf(history.data, backend.name, source.label)}
                    />
                  ))
                ) : (
                  <p className="empty">This backend has no sources.</p>
                )}
              </Section>
            ))}
            <p className="lede" style={{ marginTop: "28px" }}>
              Availability is judged without a model in hand, so a source limited only for one
              model family still reads as available — the tracks above show which. These values
              come from the usage store the daemon refreshes; this page never polls the API
              itself, so opening it costs nothing.
            </p>
          </>
        );
      }}
    </LivePage>
  );
}
