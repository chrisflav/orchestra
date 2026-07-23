import type { AuthSource, UsageLimit } from "../api";
import { LivePage, Section } from "../components/Page";
import { Status } from "../components/Status";

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
      <td className="limit-reset">{limit.resets || "—"}</td>
    </tr>
  );
}

function Source({ source }: { source: AuthSource }) {
  const notes = [
    source.pollable ? `polled ${source.polled}` : "",
    `last used ${source.lastUsed}`,
    source.baseUrl ? `base ${source.baseUrl}` : "",
    source.backoff ? `not polling until ${source.backoff}` : "",
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
          {source.resets && ` — frees up ${source.resets}`}
        </p>
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

      <div className="source-foot">
        {notes.join(" · ")}
        {source.lastError && (
          <div className="source-error">Last poll failed: {source.lastError}</div>
        )}
      </div>
    </div>
  );
}

export function Auth() {
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
                  backend.sources.map((source) => <Source key={source.label} source={source} />)
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
