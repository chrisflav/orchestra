import { useParams } from "react-router-dom";
import { DependencyGraph } from "../components/DependencyGraph";
import { Empty, List, Row } from "../components/List";
import { Facts, LivePage, Section } from "../components/Page";
import { Status } from "../components/Status";
import { Time } from "../components/Time";
import { orDash } from "../format";

export function Projects() {
  return (
    <LivePage endpoint="projects" title="Projects">
      {(data) => (
        <List>
          {data.projects.length === 0 ? (
            <Empty>No projects are tracked.</Empty>
          ) : (
            data.projects.map((p) => (
              <Row
                key={p.id}
                to={`/projects/${encodeURIComponent(p.id)}`}
                title={p.name || p.id}
                end={<span className="tag">{p.issueCount} issues</span>}
                meta={[
                  `${p.counts.open} open`,
                  `${p.counts.claimed} claimed`,
                  `${p.counts.completed} done`,
                  p.defaultTarget,
                ]}
              />
            ))
          )}
        </List>
      )}
    </LivePage>
  );
}

export function ProjectDetail() {
  const { id = "" } = useParams();
  return (
    <LivePage
      endpoint={`projects/${id}`}
      title="Project"
      qualifier={id}
      missing={{ title: "No such project", note: "This id is not in the tracker." }}
    >
      {(data) => (
        <>
          <Facts
            items={[
              { key: "Name", value: orDash(data.project.name) },
              { key: "Created", value: <Time iso={data.project.createdAt} />, data: true },
              { key: "Default target", value: orDash(data.project.defaultTarget), data: true },
              { key: "Open", value: data.project.counts.open, data: true },
              { key: "Claimed", value: data.project.counts.claimed, data: true },
              { key: "Completed", value: data.project.counts.completed, data: true },
              { key: "Abandoned", value: data.project.counts.abandoned, data: true },
            ]}
          />

          {data.project.description && (
            <Section title="About">
              <p className="lede">{data.project.description}</p>
            </Section>
          )}

          <Section title="Dependencies" meta={`${data.issues.length} issues`}>
            <DependencyGraph issues={data.issues} />
          </Section>

          <Section title="Issues">
            <List>
              {data.issues.length === 0 ? (
                <Empty>This project has no issues.</Empty>
              ) : (
                data.issues.map((i) => (
                  <Row
                    key={i.id}
                    title={i.title}
                    end={<Status status={i.status} />}
                    meta={[
                      i.id,
                      i.dependencies.length > 0 ? `${i.dependencies.length} deps` : "",
                      i.parentId ? `parent ${i.parentId}` : "",
                      i.claimedBy ? `claimed by ${i.claimedBy}` : "",
                      i.prCount > 0 ? `${i.prCount} PRs` : "",
                    ]}
                  />
                ))
              )}
            </List>
          </Section>
        </>
      )}
    </LivePage>
  );
}
