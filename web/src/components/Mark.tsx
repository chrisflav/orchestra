/**
 * The wordmark's bars, matching `public/favicon.svg`.
 *
 * Inline rather than an `<img>` so it inherits the page's rendering and stays crisp at any
 * size; the two must be kept identical, since a favicon that does not match the wordmark is
 * how a browser tab stops being recognisable.
 */
export function Mark({ size = 22 }: { size?: number }) {
  return (
    <svg
      width={size}
      height={size}
      viewBox="0 0 32 32"
      aria-hidden="true"
      focusable="false"
    >
      <rect x="5" y="14" width="4" height="12" rx="2" fill="var(--strings)" />
      <rect x="11" y="8" width="4" height="18" rx="2" fill="var(--brass)" />
      <rect x="17" y="11" width="4" height="15" rx="2" fill="var(--winds)" />
      <rect x="23" y="17" width="4" height="9" rx="2" fill="var(--perc)" />
    </svg>
  );
}
