# GitHub through kleis

By default a task's sandbox holds a GitHub App installation token (`GH_TOKEN`) good for every
repository in the installation, and orchestra's MCP tools spend the PAT on the agent's behalf for
pull requests, comments, labels and merges. Nothing stops the agent from using that token for
anything the installation can do.

With [kleis](https://github.com/chrisflav/kleis) configured, no sandbox holds a GitHub
credential at all:

- kleis, a credential proxy, holds the App's private key and the PAT.
- Each task gets a kleis token of its own, minted when the task starts and revoked when it ends.
  The token carries facts about the task: its fork and upstream, the issue it was launched from,
  its tools, whether it may write, and its pull request labels.
- Every program in the sandbox is pointed at the proxy with `HTTPS_PROXY` and trusts kleis's CA.
  The agent uses the real `git` and `gh api` against the real URLs.
- kleis decides each request against grants written over those facts. A task can push to its
  own fork, open a pull request from it on its upstream, comment on its own issue, and so on, as
  its tools allow. It cannot do anything else.
- The GitHub tools — `refresh_token`, `get_pr_comments`, `create_pr`, `merge_pr`, `label_issue`,
  `comment` and `create_repository` — are not offered. Keeping them would be a second way to
  spend the credentials, past the grants. A tool named in a task's `tools` becomes a fact on its
  token instead: `create_repository` lets the token create a repository in
  `default_organization`, and kleis then lets the same token push to it.

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

## Configuring orchestra

```json
"kleis": {
  "url": "http://127.0.0.1:8080",
  "issuer_token": "{{kleis_issuer}}"
}
```

| key | default | |
| --- | --- | --- |
| `url` | required | where orchestra reaches kleisd, to mint and revoke tokens and fetch the CA |
| `issuer_token` | required | from `kleis issuer token orchestra`; put it in `secrets.json` |
| `proxy` | `url`'s host and port | `host:port` the sandboxes reach the proxy at, if different — a service name for pods |
| `ca_file` | fetched from kleisd | kleis's CA certificate |
| `grants` | `["orchestra-github"]` | the grants a task's token names, in the order kleis tries them |
| `ttl` | `12h` | how long a token lives if its task never revokes it |
| `push_prefix` | none | limit pushes to refs under this prefix |
| `no_proxy` | none | more hosts the sandbox reaches directly, besides the loopback |

A `kleis` block that does not parse is a configuration error rather than being ignored: ignoring
it would hand every sandbox the App's token again.

## What a sandbox gets

- `HTTPS_PROXY`/`HTTP_PROXY`, in both spellings, naming the proxy with the task's token in it.
- `NO_PROXY` for the loopback.
- `SSL_CERT_FILE`, `GIT_SSL_CAINFO`, `CURL_CA_BUNDLE`, `REQUESTS_CA_BUNDLE` and
  `NODE_EXTRA_CA_CERTS` all pointing at a bundle of the system roots plus kleis's CA, under
  orchestra's data directory. The bundle carries both because `SSL_CERT_FILE` replaces a
  program's trust store rather than adding to it.
- `GH_TOKEN=kleis`. This is a placeholder: `gh` will not run without a token, and kleis removes
  whatever `Authorization` a client sends.
- git is told, through `GIT_CONFIG_*`, to answer the proxy with basic authentication.

On landrun the sandbox may connect to the proxy's port, and the bundle is granted read-only. On
Kubernetes the bundle is staged into the pod like the MCP configuration. The pod must be able to
route to `proxy`, and only orchestra's pods should be able to. Every program in the sandbox goes
through the proxy, including the agent's own model API, so kleis must either pass those hosts
through (`passthrough`, as in the example) or have them listed in `no_proxy`.

The agent's system prompt names its fork, upstream, issue and pull request labels. The
`orchestra-pull-requests` skill describes the `gh api` calls for each of the old tools, and why
not `gh pr create`: kleis refuses every GraphQL mutation, and most `gh pr` and `gh issue`
commands are mutations.

## What is not covered

- `label_issue` can add a label the repository does not have; GitHub creates it. The tool
  refused unknown labels, but a grant cannot ask the repository which labels exist.
- One App installation per credential. Forks in a second organisation need a second
  `github-app` credential in kleis and a `credential_route` to it.
- The daemon's own GitHub calls — cloning into a slot, the merger, triage, listeners, approving
  an issue — still use the App token and the PAT directly. None of them run in a sandbox.
- taxis is not behind kleis yet. Its tools stay as they are.
