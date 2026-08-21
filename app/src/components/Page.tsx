import type { ReactNode } from "react";

import { useLive } from "../core/live";
import type { QueryParams } from "../core/api";
import type { Endpoint, PayloadOf } from "../core/types";

export function PageHead({
  title,
  qualifier,
  live,
  actions,
}: {
  title: string;
  qualifier?: string;
  live?: boolean;
  actions?: ReactNode;
}) {
  return (
    <div className="page-head">
      <h1 className="page-title">
        {title}
        {qualifier !== undefined && <span className="page-qualifier">{qualifier}</span>}
      </h1>
      <div className="page-head-end">
        {actions}
        {live !== undefined && (
          <span className={`pulse ${live ? "on" : "off"}`}>
            <span className="pulse-dot" />
            {live ? "live" : "reconnecting"}
          </span>
        )}
      </div>
    </div>
  );
}

export function Section({
  title,
  meta,
  children,
}: {
  title: string;
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
 * The header block on a detail page: a row of labelled facts rather than a grid of boxes, so
 * the thing the reader came for — the log, the steps, the transcript — stays above the fold.
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

export function Notice({
  title,
  children,
}: {
  title: string;
  children?: ReactNode;
}) {
  return (
    <div className="panel">
      <div className="notice">
        <div className="notice-title">{title}</div>
        {children}
      </div>
    </div>
  );
}

/**
 * Wraps a page in its data lifecycle: load, fail, then render.
 *
 * The app's version carries one state the dashboard's does not have to: a backend whose stored
 * password no longer works. The dashboard answers that by showing its login screen, because it
 * has one backend and being logged out of it is the whole app's state. Here it is *one*
 * backend's state — the others are fine — so it is a notice on this page pointing at the
 * Backends screen, and nothing else changes.
 */
export function LivePage<E extends Endpoint>({
  endpoint,
  query,
  title,
  qualifier,
  actions,
  missing,
  children,
}: {
  endpoint: E;
  query?: QueryParams;
  title: string;
  qualifier?: string;
  actions?: ReactNode;
  /** What to say when a detail endpoint resolves to nothing. */
  missing?: { title: string; note: string };
  children: (data: PayloadOf<E>) => ReactNode;
}) {
  const { data, error, live, unauthorized } = useLive(endpoint, query);
  const head = (extra?: { live?: boolean }) => (
    <PageHead
      title={title}
      {...(qualifier === undefined ? {} : { qualifier })}
      {...(actions === undefined ? {} : { actions })}
      {...(extra?.live === undefined ? {} : { live: extra.live })}
    />
  );

  if (error !== null) {
    const gone = missing !== undefined && error.includes("not found");
    return (
      <>
        {head()}
        {unauthorized ? (
          <Notice title="This backend rejected the password">
            <p className="notice-note">
              Its password has changed, or the one stored for it was never right. Open Backends
              and give it the current one.
            </p>
          </Notice>
        ) : (
          <Notice title={gone ? missing.title : "Could not load this page"}>
            <p className="notice-note">{gone ? missing.note : "The backend answered with an error."}</p>
            {!gone && <p className="notice-note data">{error}</p>}
          </Notice>
        )}
      </>
    );
  }

  if (data === null) {
    return (
      <>
        {head()}
        <p className="empty">Loading…</p>
      </>
    );
  }

  return (
    <>
      {head({ live })}
      {children(data)}
    </>
  );
}
