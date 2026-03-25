import Lean.Data.Json

open Lean (Json FromJson ToJson)

namespace Orchestra.InformalContext

/-- A single annotation attaching a comment to a range of diff lines. -/
structure Annotation where
  startLine : Nat
  endLine : Nat
  comment : String
deriving Repr, Inhabited

instance : FromJson Annotation where
  fromJson? j := do
    let startLine ← j.getObjValAs? Nat "startLine"
    let endLine   ← j.getObjValAs? Nat "endLine"
    let comment   ← j.getObjValAs? String "comment"
    return { startLine, endLine, comment }

instance : ToJson Annotation where
  toJson a := Json.mkObj [
    ("startLine", .num ⟨a.startLine, 0⟩),
    ("endLine",   .num ⟨a.endLine,   0⟩),
    ("comment",   .str a.comment)
  ]

/-- Parse and validate an informal context JSON string.
    Returns an error message or the array of annotations. -/
def parseAnnotations (jsonStr : String) : Except String (Array Annotation) := do
  let j ← Json.parse jsonStr |>.mapError (s!"JSON parse error: " ++ ·)
  match j with
  | .arr items =>
    items.mapIdxM fun i item => do
      let a ← (FromJson.fromJson? item : Except String Annotation)
        |>.mapError (fun e => s!"annotation {i}: {e}")
      if a.endLine < a.startLine then
        throw s!"annotation {i}: endLine ({a.endLine}) must be >= startLine ({a.startLine})"
      return a
  | _ => throw "expected a JSON array of annotations"

private def runCmd (cmd : String) (args : Array String)
    (cwd : Option System.FilePath := none)
    (input : Option String := none) : IO String := do
  let child ← IO.Process.spawn {
    cmd
    args
    cwd := cwd.map (·.toString)
    stdin  := if input.isSome then .piped else .null
    stdout := .piped
    stderr := .piped
  }
  if let some s := input then
    child.stdin.putStr s
    child.stdin.flush
  let stdout ← child.stdout.readToEnd
  let stderr ← child.stderr.readToEnd
  let code ← child.wait
  if code != 0 then
    throw (IO.Error.userError s!"{cmd} failed (exit {code}):\n{stderr}")
  return stdout.trimAscii.toString

private def runCmd' (cmd : String) (args : Array String)
    (cwd : Option System.FilePath := none) : IO Unit := do
  let _ ← runCmd cmd args cwd

/-- Sanitize a `owner/repo` string into a safe directory name, e.g. `owner-repo`. -/
private def sanitizeRepo (repo : String) : String :=
  repo.map (fun c => if c == '/' then '-' else c)

/-- Clone or update the web repo into a temporary location, then return its path. -/
private def ensureWebRepo (webRepo : String) (pat : String) : IO System.FilePath := do
  let home ← IO.getEnv "HOME" >>= fun
    | some h => pure h
    | none   => throw (IO.Error.userError "HOME not set")
  let repoDir := System.FilePath.mk home / ".agent" / "informal-context" / sanitizeRepo webRepo
  IO.FS.createDirAll repoDir
  if ← (repoDir / ".git").pathExists then
    -- Pull latest changes
    let child ← IO.Process.spawn {
      cmd := "git"
      args := #["pull", "--ff-only"]
      cwd := some repoDir.toString
      stdin := .null
      stdout := .piped
      stderr := .piped
    }
    let _ ← child.stdout.readToEnd
    let _ ← child.stderr.readToEnd
    let _ ← child.wait
  else
    -- Fresh clone
    let cloneUrl :=
      if pat.isEmpty then s!"https://github.com/{webRepo}.git"
      else s!"https://x-access-token:{pat}@github.com/{webRepo}.git"
    runCmd' "git" #["clone", cloneUrl, repoDir.toString]
  return repoDir

/-- Path within the web repo for a given upstream repo and PR number:
    `{owner}-{repo}/pr-{prNumber}/annotations.json` -/
private def annotationPath (upstreamRepo : String) (prNumber : Nat) : System.FilePath :=
  System.FilePath.mk (sanitizeRepo upstreamRepo) / s!"pr-{prNumber}" / "annotations.json"

private def gitConfigUser (repoDir : System.FilePath) : IO Unit := do
  let child1 ← IO.Process.spawn {
    cmd := "git"
    args := #["config", "user.email", "orchestra@localhost"]
    cwd := some repoDir.toString
    stdin := .null
    stdout := .null
    stderr := .null
  }
  let _ ← child1.wait
  let child2 ← IO.Process.spawn {
    cmd := "git"
    args := #["config", "user.name", "Orchestra"]
    cwd := some repoDir.toString
    stdin := .null
    stdout := .null
    stderr := .null
  }
  let _ ← child2.wait

/-- Push informal context annotations to the web repository.
    Clones/updates the web repo, writes the JSON file, commits and pushes. -/
def pushAnnotations (webRepo : String) (upstreamRepo : String) (prNumber : Nat)
    (annotations : Array Annotation) (pat : String) : IO Unit := do
  let repoDir ← ensureWebRepo webRepo pat
  -- Write the annotations JSON
  let relPath := annotationPath upstreamRepo prNumber
  let fullPath := repoDir / relPath
  IO.FS.createDirAll (fullPath.parent.getD repoDir)
  let jsonContent := ToJson.toJson annotations |>.pretty
  IO.FS.writeFile fullPath (jsonContent ++ "\n")
  -- Configure git user for the commit if not set
  gitConfigUser repoDir
  -- Stage, commit, and push
  runCmd' "git" #["add", relPath.toString] repoDir
  let commitMsg := s!"Add informal context for {upstreamRepo} PR #{prNumber}"
  runCmd' "git" #["commit", "-m", commitMsg] repoDir
  -- Push; set remote URL with token if PAT is available
  if !pat.isEmpty then
    let pushUrl := s!"https://x-access-token:{pat}@github.com/{webRepo}.git"
    runCmd' "git" #["push", pushUrl, "HEAD"] repoDir
  else
    runCmd' "git" #["push"] repoDir

-- ── CI / GitHub Pages setup ──────────────────────────────────────────────────

/-- The GitHub Actions workflow that builds and deploys the informal-context site. -/
private def deployWorkflow : String :=
"name: Deploy informal context site

on:
  push:
    branches: [main, master]
  workflow_dispatch:

permissions:
  contents: read
  pages: write
  id-token: write

concurrency:
  group: pages
  cancel-in-progress: true

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Generate site
        run: python3 generate.py
      - name: Upload Pages artifact
        uses: actions/upload-pages-artifact@v3
        with:
          path: _site

  deploy:
    needs: build
    runs-on: ubuntu-latest
    environment:
      name: github-pages
      url: ${{ steps.deployment.outputs.page_url }}
    steps:
      - name: Deploy to GitHub Pages
        id: deployment
        uses: actions/deploy-pages@v4
"

/-- Python script that walks the repo tree, fetches PR diffs from GitHub,
    and generates an HTML page for each annotated pull request. -/
private def generateScript : String :=
"#!/usr/bin/env python3
\"\"\"Generate the informal-context website from annotations.json files.\"\"\"
import json
import os
import subprocess
import sys
import html as html_mod
from pathlib import Path

SITE = Path('_site')
SITE.mkdir(exist_ok=True)


def fetch_diff(repo, pr_number):
    \"\"\"Fetch the unified diff for a pull request via gh CLI.\"\"\"
    try:
        result = subprocess.run(
            ['gh', 'pr', 'diff', str(pr_number), '--repo', repo],
            capture_output=True, text=True, timeout=30
        )
        if result.returncode == 0:
            return result.stdout
    except Exception:
        pass
    return ''


def parse_diff(diff_text):
    \"\"\"Parse unified diff into a list of (line_number, kind, text) tuples.
    kind: 'context', 'add', 'remove', 'header'\"\"\"
    import re
    lines = []
    line_no = 0
    for raw in diff_text.splitlines():
        if raw.startswith('@@'):
            m = re.search(r'\\\\+([0-9]+)', raw)
            if m:
                line_no = int(m.group(1)) - 1
            lines.append((None, 'header', raw))
        elif raw.startswith('---') or raw.startswith('+++'):
            lines.append((None, 'header', raw))
        elif raw.startswith('+'):
            line_no += 1
            lines.append((line_no, 'add', raw[1:]))
        elif raw.startswith('-'):
            lines.append((None, 'remove', raw[1:]))
        else:
            line_no += 1
            lines.append((line_no, 'context', raw[1:] if raw.startswith(' ') else raw))
    return lines


def build_page(repo_slug, pr_number, annotations, diff_lines):
    \"\"\"Render the two-column HTML page for one pull request.\"\"\"
    ann_by_start = {}
    for ann in annotations:
        start = ann.get('startLine', 0)
        ann_by_start.setdefault(start, []).append(ann)

    rows = []
    style_map = {
        'add':     'background:#e6ffed',
        'remove':  'background:#ffeef0',
        'header':  'background:#f1f8ff;color:#586069',
        'context': ''
    }
    for (lno, kind, text) in diff_lines:
        esc = html_mod.escape(text)
        style = style_map.get(kind, '')
        lno_str = str(lno) if lno else ''
        lno_cell = '<td style=\"color:#999;text-align:right;padding:0 8px;user-select:none\">' + lno_str + '</td>'
        code_cell = '<td style=\"font-family:monospace;white-space:pre;' + style + '\">' + esc + '</td>'

        ann_html = ''
        if lno and lno in ann_by_start:
            for ann in ann_by_start[lno]:
                end = ann.get('endLine', lno)
                span = ('lines ' + str(lno) + '\xe2\x80\x93' + str(end)) if end != lno else ('line ' + str(lno))
                ann_html += (
                    '<div style=\"border-left:3px solid #f9c513;padding:4px 8px;'
                    'margin:2px 0;background:#fffbea;font-size:0.9em\">'
                    '<em style=\"color:#888\">' + span + '</em><br>'
                    + html_mod.escape(ann.get('comment', '')) + '</div>'
                )
        ann_cell = '<td style=\"vertical-align:top;padding:2px 8px\">' + ann_html + '</td>'
        rows.append('<tr>' + lno_cell + code_cell + ann_cell + '</tr>')

    rows_html = '\\n'.join(rows)
    repo_display = repo_slug.replace('-', '/', 1)
    return ('<!DOCTYPE html>\\n'
            '<html lang=\"en\">\\n'
            '<head>\\n'
            '<meta charset=\"utf-8\">\\n'
            '<title>Informal context: ' + repo_display + ' PR #' + str(pr_number) + '</title>\\n'
            '<style>\\n'
            '  body { font-family: sans-serif; margin: 0; padding: 16px; }\\n'
            '  h1 { font-size: 1.2em; }\\n'
            '  table { border-collapse: collapse; width: 100%; }\\n'
            '  td { border: none; padding: 1px 4px; }\\n'
            '</style>\\n'
            '</head>\\n'
            '<body>\\n'
            '<h1>Informal context &mdash; '
            '<a href=\"https://github.com/' + repo_display + '/pull/' + str(pr_number) + '\">'
            + repo_display + ' PR #' + str(pr_number) + '</a></h1>\\n'
            '<table>\\n'
            '<colgroup>\\n'
            '  <col style=\"width:4em\">\\n'
            '  <col style=\"width:55%\">\\n'
            '  <col style=\"width:40%\">\\n'
            '</colgroup>\\n'
            '<tbody>\\n'
            + rows_html + '\\n'
            '</tbody>\\n'
            '</table>\\n'
            '</body>\\n'
            '</html>')


def main():
    index_links = []
    for ann_file in sorted(Path('.').glob('*/pr-*/annotations.json')):
        parts = ann_file.parts
        repo_slug = parts[0]
        pr_part   = parts[1]
        pr_number = int(pr_part.split('-', 1)[1])
        repo_full = repo_slug.replace('-', '/', 1)

        with open(ann_file) as f:
            try:
                annotations = json.load(f)
            except json.JSONDecodeError as e:
                print('[warn] ' + str(ann_file) + ': ' + str(e), file=sys.stderr)
                continue

        diff_text = fetch_diff(repo_full, pr_number)
        diff_lines = parse_diff(diff_text)

        out_dir = SITE / repo_slug / pr_part
        out_dir.mkdir(parents=True, exist_ok=True)
        page = build_page(repo_slug, pr_number, annotations, diff_lines)
        (out_dir / 'index.html').write_text(page, encoding='utf-8')
        print('Generated ' + str(out_dir / 'index.html'))
        index_links.append((repo_full, pr_number, repo_slug + '/' + pr_part + '/'))

    links_html = ''.join(
        '<li><a href=\"' + path + '\">' + repo + ' PR #' + str(pr) + '</a></li>'
        for (repo, pr, path) in index_links
    )
    (SITE / 'index.html').write_text(
        '<!DOCTYPE html>\\n'
        '<html lang=\"en\">\\n'
        '<head><meta charset=\"utf-8\"><title>Informal context</title></head>\\n'
        '<body>\\n'
        '<h1>Informal context</h1>\\n'
        '<ul>' + links_html + '</ul>\\n'
        '</body>\\n'
        '</html>',
        encoding='utf-8'
    )
    print('Generated ' + str(SITE) + '/index.html (' + str(len(index_links)) + ' entries)')


if __name__ == '__main__':
    main()
"

/-- Set up the `.github/workflows/deploy.yml` CI and `generate.py` script
    in a local clone of the web repository. -/
def setupWebRepo (webRepo : String) (pat : String) : IO Unit := do
  let repoDir ← ensureWebRepo webRepo pat
  -- Create .github/workflows directory
  let workflowDir := repoDir / ".github" / "workflows"
  IO.FS.createDirAll workflowDir
  IO.FS.writeFile (workflowDir / "deploy.yml") deployWorkflow
  IO.FS.writeFile (repoDir / "generate.py") generateScript
  -- Configure git user
  gitConfigUser repoDir
  -- Stage and commit
  runCmd' "git" #["add", ".github/workflows/deploy.yml", "generate.py"] repoDir
  -- Check if there are staged changes before committing
  let status ← runCmd "git" #["status", "--porcelain"] repoDir
  if !status.isEmpty then
    runCmd' "git" #["commit", "-m", "Set up informal-context CI and site generator"] repoDir
    if !pat.isEmpty then
      let pushUrl := s!"https://x-access-token:{pat}@github.com/{webRepo}.git"
      runCmd' "git" #["push", pushUrl, "HEAD"] repoDir
    else
      runCmd' "git" #["push"] repoDir

end Orchestra.InformalContext
