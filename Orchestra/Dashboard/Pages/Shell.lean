import Verso.Output.Html

/-!
Shared HTML "shell" for the dynamic dashboard pages.

Every dynamic page is a near-empty static shell: a wrapper element carrying the
`data-*` attributes that `Dashboard/dashboard.js` reads (`data-page` selects the
client-side renderer; `data-endpoint`/`data-param` tell it which JSON API to
fetch and stream from), with a "Loading…" placeholder that the JS replaces once
the first payload arrives.

Each page module (`Orchestra/Dashboard/Pages/*.lean`) embeds the matching shell
into its Verso `#doc (Page)` via the `blob` directive, so the whole site is still
authored as Verso documents and emitted by `blogMain`.
-/

namespace Orchestra.Dashboard.Pages.Shell

open Verso.Output (Html)
open Verso.Output.Html

/-- The wrapper the JS targets. `page` picks the renderer; `endpoint`/`param`
    build the `/api/…` and `/sse/…` URLs (see `dashboard.js`). -/
def shell (page endpoint param : String) : Html :=
  {{ <div data-page=s!"{page}" data-endpoint=s!"{endpoint}" data-param=s!"{param}">
       <div class="loading"> "Loading…" </div>
     </div> }}

def overviewShell        : Html := shell "overview"        "overview"    ""
def queueShell           : Html := shell "queue"           "queue"       ""
def concertsShell        : Html := shell "concerts"        "concerts"    ""
def listenersShell       : Html := shell "listeners"       "listeners"   ""
def tasksShell           : Html := shell "tasks"           "tasks"       ""
def taskDetailShell      : Html := shell "task-detail"     "tasks/"      "id"
def concertDetailShell   : Html := shell "concert-detail"  "concerts/"   "id"
def listenerDetailShell  : Html := shell "listener-detail" "listeners/"  "name"

end Orchestra.Dashboard.Pages.Shell
