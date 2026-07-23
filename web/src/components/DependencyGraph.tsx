import type { IssueNode, IssueStatus } from "../api";

// Issue status maps onto the same five sections as every other status in the console, so a
// node's colour means here exactly what a status dot means in a table.
const NODE_STROKE: Record<IssueStatus, string> = {
  open: "var(--strings)",
  claimed: "var(--brass)",
  completed: "var(--winds)",
  abandoned: "var(--rest)",
};

interface Edge {
  from: string;
  to: string;
  kind: "dep" | "parent";
}

/**
 * Assign each node a layer = the longest dependency chain reaching it, so a node always sits
 * to the right of everything it depends on.
 *
 * Cycles are possible in the data model even though the dispatcher never queues one, so the
 * topological pass is followed by appending whatever it could not order; those nodes keep
 * layer 0 rather than sending the layout into a loop.
 */
function layerNodes(ids: string[], edges: Edge[]): Map<string, number> {
  const adjacency = new Map<string, string[]>();
  const indegree = new Map<string, number>();
  for (const id of ids) {
    adjacency.set(id, []);
    indegree.set(id, 0);
  }
  for (const edge of edges) {
    const out = adjacency.get(edge.from);
    if (out === undefined || !indegree.has(edge.to)) continue;
    out.push(edge.to);
    indegree.set(edge.to, (indegree.get(edge.to) ?? 0) + 1);
  }

  const remaining = new Map(indegree);
  const queue = ids.filter((id) => remaining.get(id) === 0);
  const order: string[] = [];
  while (queue.length > 0) {
    const node = queue.shift();
    if (node === undefined) break;
    order.push(node);
    for (const next of adjacency.get(node) ?? []) {
      const left = (remaining.get(next) ?? 0) - 1;
      remaining.set(next, left);
      if (left === 0) queue.push(next);
    }
  }
  const ordered = new Set(order);
  for (const id of ids) if (!ordered.has(id)) order.push(id);

  const layer = new Map(ids.map((id) => [id, 0]));
  for (const node of order) {
    for (const next of adjacency.get(node) ?? []) {
      const candidate = (layer.get(node) ?? 0) + 1;
      if ((layer.get(next) ?? 0) < candidate) layer.set(next, candidate);
    }
  }
  return layer;
}

const COL_WIDTH = 210;
const ROW_HEIGHT = 78;
const NODE_WIDTH = 168;
const NODE_HEIGHT = 52;
const PAD = 20;

export function DependencyGraph({ issues }: { issues: IssueNode[] }) {
  if (issues.length === 0) {
    return (
      <p className="empty">This project has no issues to graph.</p>
    );
  }

  const byId = new Map(issues.map((issue) => [issue.id, issue]));
  // Dependency edges (solid): dep -> issue, i.e. the dep must finish first.
  // Parent edges (dashed): parent -> child decomposition.
  const edges: Edge[] = [];
  for (const issue of issues) {
    for (const dep of issue.dependencies) {
      if (byId.has(dep)) edges.push({ from: dep, to: issue.id, kind: "dep" });
    }
    if (issue.parentId && byId.has(issue.parentId)) {
      edges.push({ from: issue.parentId, to: issue.id, kind: "parent" });
    }
  }

  const ids = issues.map((issue) => issue.id);
  const layer = layerNodes(ids, edges);

  const columns = new Map<number, string[]>();
  for (const id of ids) {
    const l = layer.get(id) ?? 0;
    const column = columns.get(l);
    if (column === undefined) columns.set(l, [id]);
    else column.push(id);
  }

  const positions = new Map<string, { x: number; y: number }>();
  let maxRow = 0;
  for (const [l, column] of columns) {
    column.forEach((id, row) => {
      positions.set(id, { x: PAD + l * COL_WIDTH, y: PAD + row * ROW_HEIGHT });
      if (row > maxRow) maxRow = row;
    });
  }
  const maxLayer = Math.max(0, ...columns.keys());
  const width = PAD * 2 + maxLayer * COL_WIDTH + NODE_WIDTH;
  const height = PAD * 2 + maxRow * ROW_HEIGHT + NODE_HEIGHT;

  return (
    <>
      <div className="graph-legend">
        <span>
          <i className="graph-rule" />
          depends on
        </span>
        <span>
          <i className="graph-rule dashed" />
          parent to child
        </span>
      </div>
      <div className="panel graph">
        <svg width={width} height={height} viewBox={`0 0 ${width} ${height}`}>
          <defs>
            <marker
              id="arrow"
              markerWidth="8"
              markerHeight="8"
              refX="7"
              refY="4"
              orient="auto"
              markerUnits="userSpaceOnUse"
            >
              <path d="M0,0 L8,4 L0,8 z" fill="var(--faint)" />
            </marker>
          </defs>

          {/* Edges first, so nodes paint on top of them. */}
          {edges.map((edge) => {
            const a = positions.get(edge.from);
            const b = positions.get(edge.to);
            if (!a || !b) return null;
            const x1 = a.x + NODE_WIDTH;
            const y1 = a.y + NODE_HEIGHT / 2;
            const x2 = b.x;
            const y2 = b.y + NODE_HEIGHT / 2;
            const mid = (x1 + x2) / 2;
            const dashed = edge.kind === "parent";
            return (
              <path
                key={`${edge.from}->${edge.to}:${edge.kind}`}
                d={`M${x1},${y1} C${mid},${y1} ${mid},${y2} ${x2},${y2}`}
                fill="none"
                stroke={dashed ? "var(--strings)" : "var(--faint)"}
                strokeWidth={1.5}
                strokeDasharray={dashed ? "4 3" : undefined}
                markerEnd="url(#arrow)"
              />
            );
          })}

          {ids.map((id) => {
            const issue = byId.get(id);
            const p = positions.get(id);
            if (!issue || !p) return null;
            const claim = issue.claimedBy ? ` · ${issue.claimedBy}` : "";
            return (
              <g key={id}>
                <title>
                  {issue.title} — {issue.status}
                  {issue.claimedBy ? ` (claimed by ${issue.claimedBy})` : ""}
                </title>
                <rect
                  x={p.x}
                  y={p.y}
                  width={NODE_WIDTH}
                  height={NODE_HEIGHT}
                  rx={8}
                  fill="var(--riser)"
                  stroke={NODE_STROKE[issue.status] ?? "var(--rest)"}
                  strokeWidth={1.5}
                />
                <text x={p.x + 10} y={p.y + 20} className="graph-id">
                  {id.slice(0, 7)}
                  {claim}
                </text>
                <text x={p.x + 10} y={p.y + 38} className="graph-title">
                  {issue.title.length > 26 ? `${issue.title.slice(0, 26)}…` : issue.title}
                </text>
              </g>
            );
          })}
        </svg>
      </div>
    </>
  );
}
