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

/**
 * One window, as a bar. `value` is what the bar is drawn at — a percentage of the limit, not
 * of the other bars — which is what a closed window consumed, and where the window still
 * filling stands right now.
 */
export interface Bar {
  key: string;
  value: number;
  /** The hover line: which window this is, and what it cost. */
  title: string;
  /** The window still filling. Its number is not final, and it is drawn as though unfinished. */
  open?: boolean;
  /**
   * The window's high-water mark, when the bar is drawn at something lower — which is the
   * window still filling, drawn at where it stands now. Marked rather than drawn as the bar,
   * because a counter that can fall has two numbers and only one of them is "how much is
   * gone right now".
   */
  peak?: number;
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

  const peak = bars.reduce((worst, bar) => Math.max(worst, bar.peak ?? bar.value), 0);
  // The newest bar, named for what it is. A closed window is what it cost; the one still
  // filling is drawn at where the source stands now, so that it is the same number as the
  // limit track above it rather than a second, higher one with no way to tell them apart.
  const last = bars[bars.length - 1];
  const newest = last?.value ?? 0;
  const newestLabel = last?.open ? "now" : "newest";

  return (
    <div className="chart">
      <div className="chart-head">
        <span className="chart-title">{title}</span>
        <span className="chart-note">
          {bars.length} {bars.length === 1 ? "window" : "windows"} · highest {peak}% ·{" "}
          {newestLabel} {newest}%
        </span>
      </div>
      {/*
        One image with one label rather than a hundred labelled bars: read aloud, the shape is
        the summary, and the exact numbers are in the caption above it either way.
      */}
      <div
        className="chart-plot"
        role="img"
        aria-label={`${title}: ${bars.length} windows, highest ${peak} percent, ${newestLabel} ${newest} percent`}
      >
        {bars.map((bar) => (
          <div className="chart-col" key={bar.key} title={bar.title}>
            {/*
              The high-water mark of a bar drawn below it: the window still filling, whose
              counter has come back down. Without it, redrawing that bar at the current
              reading would lose what the window has already been up to.
            */}
            {bar.peak !== undefined && bar.peak > bar.value && (
              <div
                className={["chart-peak", fillOf(bar.peak)].filter(Boolean).join(" ")}
                style={{ bottom: `${Math.max(2, Math.min(100, bar.peak))}%` }}
              />
            )}
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
