import { relativeTime } from "../format";

/**
 * A timestamp shown as a distance from now, with the exact value on hover.
 *
 * `<time dateTime=…>` rather than a bare span so the machine-readable value stays in the
 * markup even though the visible text is approximate.
 */
export function Time({ iso }: { iso: string }) {
  if (iso === "") return <span>never</span>;
  return (
    <time dateTime={iso} title={iso}>
      {relativeTime(iso)}
    </time>
  );
}
