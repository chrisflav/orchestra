# Agents through kleis

By default a task's sandbox holds a GitHub App installation token (`GH_TOKEN`) good for every
repository in the installation, and orchestra's MCP tools spend the PAT on the agent's behalf for
pull requests, comments, labels and merges. Nothing stops the agent from using that token for
anything the installation can do.

With [kleis](https://github.com/chrisflav/kleis) configured, no sandbox holds a credential at
all:

- kleis, a credential proxy, holds the credentials: the GitHub App's private key and the PAT, and
  whatever other accounts its operator installs (mail, calendars, Zulip, …).
- Each task gets a kleis token of its own when it starts, revoked when it ends.
- Every program in the sandbox is pointed at the proxy with `HTTPS_PROXY` and trusts kleis's CA.
  The agent uses ordinary tools — `git`, `gh api`, a mail or calendar client — against the real
  addresses.
- kleis decides each request. What the token allows is written in kleis, not in orchestra.
- The GitHub tools — `refresh_token`, `get_pr_comments`, `create_pr`, `merge_pr`, `label_issue`,
  `comment` and `create_repository` — are not offered. Keeping them would be a second way to
  spend the credentials, past kleis. A tool named in a task's `tools` becomes a fact on its token
  instead (`task_tool`): `create_repository` lets the token create a repository in
  `default_organization`, and kleis then lets the same token push to it.

A task's token is made in one of two ways. With a **source** — a kleis spec, or a token handed to
the task — it is *derived* (`/.kleis/v1/derive`), and kleis also says what the sandbox needs and
what the token allows. Without one it is *minted* from `kleis.grants` (`/.kleis/v1/tokens`), the
older path, which reaches GitHub only. Setting `kleis.default_spec` gives every task a source.

## Sources

A task resolves one source when it starts. The first that is set wins:

1. the task's `kleis_token` — a `{{secret}}` reference, resolved from `secrets.json` at launch;
2. the task's `kleis_spec`;
3. its identity's `kleis_spec` or `kleis_token`;
4. its role's `kleis_spec`, or, for a task a listener queued, its listener action's;
5. `kleis.default_spec`;
6. none: the legacy mint from `kleis.grants`, or no token at all when `kleis` is unset.

The identity comes before the role: a secretary task dispatched for a role keeps the secretary's
access. A role or listener action that wants less sets `kleis_narrow` instead.

**Narrowings** (`kleis_narrow`, a list of strings in kleis's `allow` syntax, one per service —
`"mail: only read, flag"`) may be set on a task, its identity, its role and its listener action,
and accumulate: every level's narrowing is sent, so the token holds their intersection. A
narrowing needs a source; on the legacy mint, which cannot narrow, it is an error.

```json
// identities/secretary/identity.json
{"name": "secretary", "description": "Christian's secretary", "kleis_spec": "secretary"}

// a task file
{"prompt": "…", "identity": "secretary", "kleis_narrow": ["mail: only read"]}

// a task handed a token of its own
{"prompt": "…", "kleis_token": "{{zulip_bot_token}}"}
```

| where | `kleis_spec` | `kleis_token` | `kleis_narrow` |
| --- | --- | --- | --- |
| task file, queue entry (`kleis_spec`, …) | yes | a `{{secret}}` reference only | yes |
| workflow step (`kleis-spec`, `kleis-token`, `kleis-narrow`) | yes | a quoted `"{{secret}}"` reference only | yes |
| identity (`identity.json`) | yes | yes, usually as `{{secret}}`, substituted when the record is read | yes |
| role | yes | no | yes |
| listener action | yes | no | yes |
| `queue_task` | no — inherited | no — inherited | yes, added to what is inherited |

A task's `kleis_token` is held as the reference and never as the token: a task is stored in the
queue and the task store, which the API and the dashboard list. A literal token there is refused
when the task is read. The same goes for a workflow step. An identity's token lives in its record
on disk, like its `taxis_token`, and is neither serialized nor printed.

Every one of these fields is read strictly: present and mistyped, or naming something that is not
a spec name, is an error rather than being read as absent — a dropped source runs the task under
the next level's, and a dropped narrowing hands it access somebody took away. A task naming a
source when no `kleis` is configured fails rather than running with the sandbox's own credentials.

A continuation (`orchestra queue retry`, a restart resume, `orchestra issue continue`) carries
the task's own source and narrowings with it, as it carries the identity.

### Spawned tasks

A task queued by a task (`queue_task`) inherits its parent's **source** — the spec's name, or the
`{{secret}}` reference of the token — and every narrowing its parent's token carried, and derives
a token of its own when it is dispatched. It never receives the parent's token, which is revoked
when the parent ends. The agent may add narrowings (`kleis_narrow` on `queue_task`) but may not
name another source. A parent whose source was its identity's token passes on nothing: the child
inherits the identity, which names the token again.

## At task start, with a source

1. `POST /.kleis/v1/derive` with the issuer credential. From a spec: the spec, `ttl`,
   `bearer = "orchestra:<task id>"`, the narrowings, and the task facts — `task_id`,
   `task_identity`, and for a task on a repository `task_fork`, `task_upstream`, `task_issue`,
   `task_tool`, `task_writable`, `task_pr_labels`, `task_label_any`, `task_org`,
   `task_push_prefix`. From a token: the token, `ttl` and the narrowings, nothing else. kleis
   answers with the task token and the one `revocation_id` that revokes it.
2. `GET /.kleis/v1/setup` with `Authorization: Bearer <task token>`: the environment, files and CA
   the sandbox needs, extra `no_proxy` hosts, and the token's **card**.
3. The sandbox gets the proxy with the task token in it, the CA bundle (system roots plus the
   setup's `proxy.ca_pem`), `NO_PROXY` from configuration and the setup, `setup.env`, and
   `setup.files` written into its home. orchestra refuses a setup that sets the proxy, CA or
   `NO_PROXY` variables (`HTTPS_PROXY`, `HTTP_PROXY`, `NO_PROXY` in either case,
   `SSL_CERT_FILE`, `GIT_SSL_CAINFO`, `CURL_CA_BUNDLE`, `REQUESTS_CA_BUNDLE`,
   `NODE_EXTRA_CA_CERTS`), or a file whose path is absolute or has a `..` segment.
4. The system prompt is orchestra's own text, the identity's `AGENTS.md`, then the card, then the
   task prompt. orchestra's own GitHub section and its `GH_TOKEN=kleis` and git proxy settings are
   not added: the card and the GitHub extension's setup carry them.
5. When the task ends, however it ends, `POST /.kleis/v1/revoke` with that `revocation_id`.

The token is derived before the sandbox's environment is opened, since the CA bundle is one of
the paths it opens with; a failure there fails the task before anything runs, and the token is
revoked.

### Setup files

Each file is written relative to the sandbox user's home, creating directories on the way,
without following a symbolic link anywhere below the home: a directory on the way that is a link
is refused, and the file is written beside its destination and renamed over it, so a link at the
destination is replaced rather than written through. The mode defaults to `0600`.

- **landrun** and **local** run the agent on this machine, in orchestra's own `$HOME`. The files
  are written there, and on landrun granted read-only to the sandbox. That home is shared by
  every task and by the daemon, so two tasks whose setups write the same path with different
  contents overwrite each other, last one first; a setup should only write files whose content
  depends on the accounts, not on the task. Prefer a backend with a home per task when tasks with
  different specs run side by side.
- **kubernetes** writes them into the pod's home (`home_path`, or the task volume's), through
  `kubectl exec`, with the same checks done by a shell script in the pod.

Interactive sessions resolve their source from their identity, then `default_spec`;
`orchestra interactive` (which names no identity) from `default_spec` alone. Both otherwise do
what a queued task does.

## Setting up kleis

kleis's `examples/orchestra/` is the setup this was written against. It contains:

- a `config.toml` with an `orchestra` issuer and `passthrough` for hosts kleis has no manifest
  for;
- the GitHub manifest;
- one grant, `orchestra-github`, which decides what a task may do and then which credential each
  request goes out on:
  1. an operator-written route by owner or repository (`resources = ["acme/*"]`), first;
  2. otherwise the task's fork, the repositories it created, and GraphQL on the GitHub App;
  3. otherwise the task's upstream on the PAT;
  4. otherwise nothing: public dependencies are read anonymously.

Its README has the `kleis credential add` commands. Then mint orchestra's issuer credential:

```sh
kleis issuer token orchestra --ttl 90d
```

To move to derived tokens, give the issuer the specs it may derive from (`specs = ["worker", …]`
in its `[[issuer]]`), write a spec `worker` that includes the grant unchanged
(`grant orchestra-github`), and set `default_spec = "worker"` in orchestra's `kleis` block. The
task facts are where the grant reads them today, so it needs no change.

## Configuring orchestra

```json
"kleis": {
  "url": "http://127.0.0.1:8080",
  "issuer_token": "{{kleis_issuer}}",
  "default_spec": "worker"
}
```

| key | default | |
| --- | --- | --- |
| `url` | required | where orchestra reaches kleisd, to derive, mint and revoke tokens and fetch the CA |
| `issuer_token` | one of these two | from `kleis issuer token orchestra`; put it in `secrets.json` |
| `issuer_token_file` | | a file kleisd keeps the issuer credential in (`token_file` in its `[[issuer]]`), read on every use; what the NixOS module sets up |
| `proxy` | `url`'s host and port | `host:port` the sandboxes reach the proxy at, if different — a service name for pods |
| `ca_file` | fetched from kleisd | kleis's CA certificate, for the legacy path |
| `default_spec` | none | the spec a task with no other source runs under; without it such a task takes the legacy mint |
| `grants` | `["orchestra-github"]` | legacy: the grants a minted token names, in the order kleis tries them |
| `ttl` | `12h` | how long a token lives if its task never revokes it |
| `push_prefix` | none | limit pushes to refs under this prefix (`task_push_prefix`) |
| `no_proxy` | none | more hosts the sandbox reaches directly, besides the loopback |

A `kleis` block that does not parse, or that has a key it does not know, is a configuration error
rather than being ignored: ignoring it would hand every sandbox the App's token again.

## What a sandbox gets

- `HTTPS_PROXY`/`HTTP_PROXY`, in both spellings, naming the proxy with the task's token in it.
- `NO_PROXY` for the loopback, `no_proxy` from configuration, and the setup's `no_proxy`.
- `SSL_CERT_FILE`, `GIT_SSL_CAINFO`, `CURL_CA_BUNDLE`, `REQUESTS_CA_BUNDLE` and
  `NODE_EXTRA_CA_CERTS` all pointing at a bundle of the system roots plus kleis's CA, under
  orchestra's data directory. The bundle carries both because `SSL_CERT_FILE` replaces a
  program's trust store rather than adding to it.
- `GIT_TERMINAL_PROMPT=0`.
- With a source: the setup's environment and files.
- On the legacy path: `GH_TOKEN=kleis`, a placeholder (`gh` will not run without a token, and kleis
  removes whatever `Authorization` a client sends), and git told through `GIT_CONFIG_*` to answer
  the proxy with basic authentication.

On landrun the sandbox may connect to the proxy's port, and the bundle is granted read-only. On
Kubernetes the bundle is staged into the pod like the MCP configuration. The pod must be able to
route to `proxy`, and only orchestra's pods should be able to. Every program in the sandbox goes
through the proxy, including the agent's own model API, so kleis must either pass those hosts
through (`passthrough`, as in the example) or have them listed in `no_proxy`.

On the legacy path the agent's system prompt names its fork, upstream, issue and pull request
labels; with a source, the card does. The `orchestra-pull-requests` skill describes the `gh api`
calls for each of the old tools, and why not `gh pr create`: kleis refuses every GraphQL mutation,
and most `gh pr` and `gh issue` commands are mutations.

## What is not covered

- `label_issue` can add a label the repository does not have; GitHub creates it. The tool
  refused unknown labels, but a grant cannot ask the repository which labels exist.
- One App installation per credential. Forks in a second organisation need a second
  `github-app` credential in kleis and a `credential_route` to it.
- The daemon's own GitHub calls — cloning into a slot, the merger, triage, listeners, approving
  an issue — still use the App token and the PAT directly. None of them run in a sandbox.
- taxis is not behind kleis yet. Its tools stay as they are.
- A token-sourced task carries no task facts (kleis adds checks only), so a grant that needs them
  — the GitHub one — does not apply to it.
- A listener action that starts a workflow passes neither its identity nor its kleis fields to
  the workflow's steps; each step names its own.
