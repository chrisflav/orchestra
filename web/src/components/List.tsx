import { Fragment } from "react";
import type { ReactNode } from "react";
import { Link } from "react-router-dom";

/**
 * The one list pattern in the app.
 *
 * Every list — tasks, queue entries, concerts, listeners, projects, issues — is rows of "what
 * this is" over "what is true about it". Leading with the prompt or the name rather than the id
 * is the point: `t20260723T1030` is the least recognisable thing about a task, and putting it
 * in the first column is what made the old tables read as a database viewer.
 */
export function List({ children }: { children: ReactNode }) {
  return <div className="list">{children}</div>;
}

export function Empty({ children }: { children: ReactNode }) {
  return <p className="empty">{children}</p>;
}

interface RowProps {
  /** The recognisable thing: a prompt, a name, an issue title. */
  title: ReactNode;
  /** Set when the title is an id or a path rather than prose. */
  titleIsData?: boolean;
  /** Supporting facts, joined with separators. Falsy entries are dropped. */
  meta?: ReactNode[];
  /** Status or count, held to the right. */
  end?: ReactNode;
  /** Makes the whole row a link. */
  to?: string;
}

export function Row({ title, titleIsData = false, meta = [], end, to }: RowProps) {
  const shown = meta.filter((item) => item !== null && item !== undefined && item !== "");
  const body = (
    <>
      <div className="row-main">
        <div className={titleIsData ? "row-title data" : "row-title"}>{title}</div>
        {shown.length > 0 && (
          <div className="row-meta">
            {shown.map((item, i) => (
              <Fragment key={i}>
                {i > 0 && <span className="sep">·</span>}
                {item}
              </Fragment>
            ))}
          </div>
        )}
      </div>
      {end !== undefined && <div className="row-end">{end}</div>}
    </>
  );

  return to === undefined ? (
    <div className="row">{body}</div>
  ) : (
    <Link className="row" to={to}>
      {body}
    </Link>
  );
}
