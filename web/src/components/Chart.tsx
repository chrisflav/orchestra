/**
 * The one chart in the app: a bar per limit window, on a fixed 0–100 axis.
 *
 * Everything graphed here is a percentage of a limit, so the axis is the limit itself and
 * never the data's own range. That is the whole reason the shape is readable at a glance: a
 * bar at half height means half a window spent, on every source and in every week, rather
 * than "half of the busiest bar in this particular chart". Nothing is rescaled, and the
 * ceiling is drawn so that a bar touching it reads as the window that ran out.
 *
 * A single series needs no legend — the title names it — and a number over every bar would
 * bury the shape it exists to show, so the figures live in the caption and on hover.
 */

/** One window's consumption. `value` is a percentage of the limit, not of the other bars. */
export interface Bar {
  key: string;
  value: number;
  /** The hover line: which window this is, and what it cost. */
  title: string;
  /** The window still filling. Its number is not final, and it is drawn as though unfinished. */
  open?: boolean;
}

/** The same three steps the limit tracks use, so a colour means one thing on this page. */
function fillOf(value: number): string {
  if (value >= 100) return "crit";
  if (value >= 75) return "warn";
  return "";
}

export function Bars({
  title,
  bars,
  empty,
  axis,
}: {
  title: string;
  bars: Bar[];
  /** What to say when nothing has been recorded yet. */
  empty: string;
  /** The two ends of the time axis, oldest first. */
  axis?: [string, string] | undefined;
}) {
  if (bars.length === 0) {
    return (
      <div className="chart">
        <div className="chart-head">
          <span className="chart-title">{title}</span>
        </div>
        <p className="chart-empty">{empty}</p>
      </div>
    );
  }

  const peak = bars.reduce((worst, bar) => Math.max(worst, bar.value), 0);
  // The newest bar, named for what it is. Every bar is a window's consumption, so this is what
  // the most recent window has cost — not where the source stands right now, which is the
  // limit tracks' job and would be a different number on the window still filling.
  const newest = bars[bars.length - 1]?.value ?? 0;

  return (
    <div className="chart">
      <div className="chart-head">
        <span className="chart-title">{title}</span>
        <span className="chart-note">
          {bars.length} {bars.length === 1 ? "window" : "windows"} · highest {peak}% · newest{" "}
          {newest}%
        </span>
      </div>
      {/*
        One image with one label rather than a hundred labelled bars: read aloud, the shape is
        the summary, and the exact numbers are in the caption above it either way.
      */}
      <div
        className="chart-plot"
        role="img"
        aria-label={`${title}: ${bars.length} windows, highest ${peak} percent, newest ${newest} percent`}
      >
        {bars.map((bar) => (
          <div className="chart-col" key={bar.key} title={bar.title}>
            <div
              className={["chart-bar", fillOf(bar.value), bar.open ? "open" : ""]
                .filter(Boolean)
                .join(" ")}
              // Clamped, never rescaled: a reading over 100% is still a full bar, and a
              // window with almost nothing in it keeps a hairline so that "polled and quiet"
              // does not look like "never polled".
              style={{ height: `${Math.max(2, Math.min(100, bar.value))}%` }}
            />
          </div>
        ))}
      </div>
      {axis !== undefined && (
        <div className="chart-axis">
          <span>{axis[0]}</span>
          <span>{axis[1]}</span>
        </div>
      )}
    </div>
  );
}
