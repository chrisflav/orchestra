import VersoBlog
import Lean.Data.Json
import Orchestra.Dirs
import Orchestra.Dashboard.Pages.Overview
import Orchestra.Dashboard.Pages.Queue
import Orchestra.Dashboard.Pages.Concerts
import Orchestra.Dashboard.Pages.Listeners
import Orchestra.Dashboard.Pages.Tasks
import Orchestra.Dashboard.Pages.TaskDetail
import Orchestra.Dashboard.Pages.ConcertDetail
import Orchestra.Dashboard.Pages.ListenerDetail
import Orchestra.Dashboard.Pages.Projects
import Orchestra.Dashboard.Pages.ProjectDetail
import Orchestra.Dashboard.Pages.Auth
import Orchestra.Dashboard.Pages.About

/-!
# Static dashboard site

The dashboard's static pages are authored as Verso `#doc (Page)` documents
(`Orchestra/Dashboard/Pages/*.lean`), assembled into a `Site` tree and emitted
with Verso's Blog pipeline (`blogMain`) — the same approach as
[`leanprover/verso-website`](https://github.com/leanprover/verso-website).

The shared page chrome (header/nav, `<main>`, asset links) lives in the theme's
`primaryTemplate`. The runtime `--api-url` value is threaded through by making the
theme a function of `apiBase`, whose closure bakes `window.ORCHESTRA_API_BASE`
into every page's `<head>`; `dashboard.js` reads it to build the `/api/…` and
`/sse/…` URLs.

Static assets (`dashboard.css`/`dashboard.js`) are embedded in the binary with
`include_str` and written out after `blogMain`, so the installed `orchestra`
executable is self-contained (Verso's `static … ← path` would instead copy from
disk at run time, which is unavailable for an installed binary).
-/

namespace Orchestra.Dashboard

open Verso Genre Blog Site Syntax

open Output Html in
/-- A single navigation link; `active` is the currently-selected nav group. -/
private def navItem (active href label key : String) : Html :=
  if key == active then
    {{ <a href=s!"{href}" class="active"> s!"{label}" </a> }}
  else
    {{ <a href=s!"{href}"> s!"{label}" </a> }}

open Output Html Template Theme in
/-- The single layout shared by every generated page. `apiBase` is the base URL of
    the JSON API backend (`dashboard serve`), baked into every page. The API token
    is *not* baked in — the front-end prompts the user for it at runtime. -/
def theme (apiBase : String) : Theme := { Theme.default with
  primaryTemplate := do
    -- Relative path back to the site root, so root-level assets resolve from
    -- nested page directories (e.g. `queue/index.html`). Mirrors `builtinHeader`.
    let path ← currentPath
    let siteRoot := String.join (path.toList.map (fun _ => "../")) ++ "./"
    -- The active nav group; detail pages highlight their parent list.
    let active :=
      match path[0]? with
      | none            => "overview"
      | some "task"     => "tasks"
      | some "concert"  => "concerts"
      | some "project"  => "projects"
      | some "listener" => "listeners"
      | some s          => s
    let pageTitle ← param (α := String) "title"
    let apiBaseJs := s!"window.ORCHESTRA_API_BASE = {(Lean.Json.str apiBase).compress};"
    return {{
      <html lang="en">
        <head>
          <meta charset="utf-8"/>
          <meta name="viewport" content="width=device-width,initial-scale=1"/>
          <base href=s!"{siteRoot}"/>
          <title> s!"{pageTitle} — Orchestra" </title>
          <link rel="stylesheet" href="dashboard.css"/>
          <script> {{ Html.text false apiBaseJs }} </script>
        </head>
        <body>
          <header>
            <span class="logo"> "Orchestra" </span>
            <nav>
              {{ navItem active "."          "Overview"  "overview"  }}
              {{ navItem active "queue/"     "Queue"     "queue"     }}
              {{ navItem active "concerts/"  "Concerts"  "concerts"  }}
              {{ navItem active "projects/"  "Projects"  "projects"  }}
              {{ navItem active "listeners/" "Listeners" "listeners" }}
              {{ navItem active "tasks/"     "Tasks"     "tasks"     }}
              {{ navItem active "auth/"      "Auth"      "auth"      }}
              {{ navItem active "about/"     "About"     "about"     }}
            </nav>
          </header>
          <main>
            {{ ← param "content" }}
          </main>
          <script src="dashboard.js"> "" </script>
        </body>
      </html>
    }}
  -- Emit the page content directly, without the default `<article><h1>` wrapper.
  pageTemplate := do return (← param "content") }

/-- The site's URL structure. The Overview document is the root (`index.html`);
    every other page is a named subdirectory (`queue/index.html`, …). -/
def versoSite : Site :=
  site Orchestra.Dashboard.Pages.Overview /
    "queue"     Orchestra.Dashboard.Pages.Queue
    "concerts"  Orchestra.Dashboard.Pages.Concerts
    "listeners" Orchestra.Dashboard.Pages.Listeners
    "projects"  Orchestra.Dashboard.Pages.Projects
    "project"   Orchestra.Dashboard.Pages.ProjectDetail
    "tasks"     Orchestra.Dashboard.Pages.Tasks
    "task"      Orchestra.Dashboard.Pages.TaskDetail
    "concert"   Orchestra.Dashboard.Pages.ConcertDetail
    "listener"  Orchestra.Dashboard.Pages.ListenerDetail
    "auth"      Orchestra.Dashboard.Pages.Auth
    "about"     Orchestra.Dashboard.Pages.About

/-- Light theme stylesheet, written out by `generate`. -/
def dashCss : String := include_str "dashboard.css"

/-- Front-end JS bundle, written out by `generate`. -/
def dashJs : String := include_str "dashboard.js"

/-- Run `act` from a scratch directory that looks like a Lean project.

    Verso's pipeline insists on being run from inside one: `blogMain` refreshes its
    cross-reference remotes on every run, and that walks up from the working directory
    looking for a `lean-toolchain`, throwing "No 'lean-toolchain' found in a parent
    directory" when there is none (`MultiVerso.findProject`). An installed `orchestra` is
    run from wherever the user happens to stand — and in a container, from `/`.

    Only the file's *existence* is checked, never its contents, and the dashboard site has
    no cross-references to resolve, so an empty marker under the data dir is enough. -/
private def withProjectDir (act : IO α) : IO α := do
  let dir := (← Dirs.dataBase) / "dashboard-build"
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "lean-toolchain") ""
  let previous ← IO.currentDir
  IO.Process.setCurrentDir dir
  try act finally IO.Process.setCurrentDir previous

/-- Generate the complete static dashboard site into `targetDir`. The generated
    front-end fetches (and streams over SSE) from the JSON API at `apiBase` — the
    base URL of a running `orchestra dashboard serve`, or the empty string to use
    whatever origin served the page. -/
def generate (targetDir : System.FilePath) (apiBase : String) : IO Unit := do
  -- Resolved before `withProjectDir` changes what a relative path means.
  let cwd ← IO.currentDir
  let target := if targetDir.isAbsolute then targetDir else cwd / targetDir
  IO.FS.createDirAll target
  withProjectDir do
    let _ ← blogMain (theme apiBase) versoSite (options := ["--output", target.toString])
  IO.FS.writeFile (target / "dashboard.css") dashCss
  IO.FS.writeFile (target / "dashboard.js") dashJs

end Orchestra.Dashboard
