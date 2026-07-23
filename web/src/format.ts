/** Shared display helpers. */

export function truncate(text: string, max: number): string {
  return text.length <= max ? text : `${text.slice(0, max)}…`;
}

/** An empty string reads as an em dash, so a blank value never looks like a missing one. */
export function orDash(value: string): string {
  return value === "" ? "—" : value;
}

const MINUTE = 60_000;
const HOUR = 60 * MINUTE;
const DAY = 24 * HOUR;

/**
 * An ISO timestamp as the distance from now: "4m ago", "yesterday", "3d ago".
 *
 * The server sends timestamps in ISO 8601 UTC, which is exact and unreadable. A person
 * scanning a list wants to know whether something happened just now or last week, and only
 * reaches for the exact value when something is wrong — so that stays available as the title
 * attribute (see `Time`). An unparseable value is passed through untouched rather than
 * rendered as "Invalid Date".
 */
export function relativeTime(iso: string, now: number = Date.now()): string {
  if (iso === "") return "never";
  const then = Date.parse(iso);
  if (Number.isNaN(then)) return iso;

  const delta = now - then;
  if (delta < 0) return "just now";
  if (delta < MINUTE) return "just now";
  if (delta < HOUR) return `${Math.floor(delta / MINUTE)}m ago`;
  if (delta < DAY) return `${Math.floor(delta / HOUR)}h ago`;
  if (delta < 2 * DAY) return "yesterday";
  if (delta < 30 * DAY) return `${Math.floor(delta / DAY)}d ago`;
  return new Date(then).toISOString().slice(0, 10);
}

/** Seconds as an interval a person reads at a glance: "45s", "2m", "1.5h". */
export function interval(seconds: number): string {
  if (seconds < 60) return `${seconds}s`;
  if (seconds < 3600) return `${Math.round(seconds / 60)}m`;
  return `${Math.round(seconds / 360) / 10}h`;
}
