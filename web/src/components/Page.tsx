import type { ReactNode } from "react";
import type { Endpoint, PayloadOf } from "../api";
import { useLiveData } from "../useLiveData";

export function PageHead({
  title,
  qualifier,
  live,
}: {
  title: string;
  qualifier?: string;
  live?: boolean;
}) {
  return (
    <div className="page-head">
      <h1 className="page-title">
        {title}
        {qualifier !== undefined && <span className="page-qualifier">{qualifier}</span>}
      </h1>
      {live !== undefined && (
        <span className={`pulse ${live ? "on" : "off"}`}>
          <span className="pulse-dot" />
          {live ? "live" : "reconnecting"}
        </span>
      )}
    </div>
  );
}

export function Section({
  title,
  meta,
  children,
}: {
  title: string;
  // Explicitly `| undefined`: callers pass a conditional, and `exactOptionalPropertyTypes`
  // distinguishes "absent" from "present and undefined".
  meta?: string | undefined;
  children: ReactNode;
}) {
  return (
    <section className="section">
      <div className="section-head">
        <h2 className="section-title">{title}</h2>
        {meta !== undefined && <span className="section-meta">{meta}</span>}
      </div>
      {children}
    </section>
  );
}

export interface Fact {
  key: string;
  value: ReactNode;
  /** Renders the value in the data face — for ids, paths and timestamps. */
  data?: boolean;
}

/**
 * The header block on a detail page.
 *
 * A row of labelled facts rather than a grid of bordered cells: detail pages carry six to
 * eight short values, and giving each one a box pushed the thing the reader came for — the
 * log, the graph, the steps — below the fold.
 */
export function Facts({ items }: { items: Fact[] }) {
  return (
    <div className="facts">
      {items.map((item) => (
        <div className="fact" key={item.key}>
          <div className="fact-key">{item.key}</div>
          <div className={item.data ? "fact-value data" : "fact-value"}>{item.value}</div>
        </div>
      ))}
    </div>
  );
}

/**
 * Wraps a page in its data lifecycle: load, fail, then render.
 *
 * Hoisting the three states here keeps them identical everywhere — a page component only ever
 * describes the shape of a payload that is known to exist by the time it runs.
 */
export function LivePage<E extends Endpoint>({
  endpoint,
  title,
  qualifier,
  missing,
  children,
}: {
  endpoint: E;
  title: string;
  qualifier?: string;
  /** What to say when a detail endpoint resolves to nothing. */
  missing?: { title: string; note: string };
  children: (data: PayloadOf<E>) => ReactNode;
}) {
  const { data, error, live } = useLiveData(endpoint);

  if (error !== null) {
    const gone = missing !== undefined && error === "not found";
    return (
      <>
        <PageHead title={title} {...(qualifier === undefined ? {} : { qualifier })} />
        <div className="panel">
          <div className="notice">
            <div className="notice-title">{gone ? missing.title : "Could not load this page"}</div>
            <p className="notice-note">
              {gone ? missing.note : "The server returned an error."}
            </p>
            {!gone && <p className="notice-note data">{error}</p>}
          </div>
        </div>
      </>
    );
  }

  if (data === null) {
    return (
      <>
        <PageHead title={title} {...(qualifier === undefined ? {} : { qualifier })} />
        <p className="empty">Loading…</p>
      </>
    );
  }

  return (
    <>
      <PageHead title={title} {...(qualifier === undefined ? {} : { qualifier })} live={live} />
      {children(data)}
    </>
  );
}
