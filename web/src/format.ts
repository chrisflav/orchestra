/** Shared display helpers. */

export function truncate(text: string, max: number): string {
  return text.length <= max ? text : `${text.slice(0, max)}…`;
}

/**
 * An absent value reads as an em dash, so a blank never looks like a missing one.
 *
 * Takes `null` as well as `""`: the API sends `null` for absent, and a page that prints
 * "null" in a fact row is worse than one that prints nothing.
 */
export function orDash(value: string | null | undefined): string {
  return value === null || value === undefined || value === "" ? "—" : value;
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

/**
 * An instant in the future as the distance to it: "in 45s", "in 2h 5m", "in 3d 4h".
 *
 * The counterpart to `relativeTime`, for the values that say when something frees up — a
 * usage limit resetting, a polling backoff lapsing. Days get a bucket of their own because
 * the weekly limits are what people read this for and their windows run up to seven days
 * wide: "in 3d 4h" is an answer, "in 76h 12m" is arithmetic homework.
 *
 * The API sends these as RFC 3339 instants, so the phrasing is the client's business and
 * stays correct as time passes rather than as of whenever the payload was built.
 */
export function untilTime(iso: string | null, now: number = Date.now()): string {
  if (iso === null || iso === "") return "—";
  const then = Date.parse(iso);
  if (Number.isNaN(then)) return iso;

  const delta = then - now;
  if (delta <= 0) return "now";
  if (delta < MINUTE) return `in ${Math.floor(delta / 1000)}s`;
  if (delta < HOUR) return `in ${Math.floor(delta / MINUTE)}m`;
  if (delta < DAY) {
    const hours = Math.floor(delta / HOUR);
    return `in ${hours}h ${Math.floor((delta % HOUR) / MINUTE)}m`;
  }
  return `in ${Math.floor(delta / DAY)}d ${Math.floor((delta % DAY) / HOUR)}h`;
}

/** `relativeTime`, but a `null` instant reads as "never" rather than being a type error. */
export function sinceTime(iso: string | null, now: number = Date.now()): string {
  return relativeTime(iso ?? "", now);
}

/** Seconds as an interval a person reads at a glance: "45s", "2m", "1.5h". */
export function interval(seconds: number): string {
  if (seconds < 60) return `${seconds}s`;
  if (seconds < 3600) return `${Math.round(seconds / 60)}m`;
  return `${Math.round(seconds / 360) / 10}h`;
}
