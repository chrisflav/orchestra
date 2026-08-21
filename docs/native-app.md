# a native app for orchestra

`orchestra` the CLI reaches one backend, the one whose secret sits on the same host. The
dashboard reaches one backend too — the one that served it, because a browser page and its API
are the same origin by construction. Neither is what you want on a laptop or a phone that
watches *several* orchestras: a machine at home, a box in a datacentre, a container on the
company network.

The native app is that client. It holds a list of backends, talks to whichever one is selected,
and switches between them without a reload and without logging in again. Everything the
dashboard shows, it shows; everything the dashboard can change, it can change; and the one thing
a browser page cannot host well — a conversation with an agent that survives the client going
away — it hosts on a phone.

This document is the plan: what it is, what shell it is built on and why, how a backend is
described and switched, where credentials live, how requests and streams travel, and what ships
in which order.

## the three walls a browser puts up

It is worth being precise about why this is a native app and not a second web page, because the
three reasons are the same three that dictate its architecture.

**A browser cannot talk to a backend that did not serve it.** The dashboard API sends no
`Access-Control-Allow-Origin` header, deliberately — see "Authentication" in
`Orchestra/Dashboard.lean`. A page loaded from anywhere else is refused before the request is
made. A client that reaches *many* backends therefore cannot make its requests from a web
context at all; they have to come from a process that is not subject to the same-origin policy.
That single fact is what decides the shape below: **the transport lives in native code, and the
UI never issues a request itself.**

**`EventSource` cannot carry a bearer token.** Every read in the API is also an SSE stream, and
liveness is most of what this app is for. In the dashboard the stream authenticates with the
session cookie, which the browser attaches on its own. Cross-origin there is no cookie to
attach, and `EventSource` — alone among the fetch APIs — takes no headers. A token in the query
string is the usual workaround and is not acceptable: it lands in proxy logs. So streams are
read by the same native transport, over an ordinary HTTP request with an `Authorization` header,
and the frames are handed up to the UI.

**A browser has nowhere safe to keep a credential.** The dashboard never holds its password:
it trades it for an `HttpOnly` cookie and forgets it. An app that must re-authenticate to five
backends after every restart has to *store* five secrets, and `localStorage` is not where a
secret goes. The OS keychain is, and reaching it needs native code too.

Three walls, one conclusion: a native shell around a web UI, where the shell owns the network
and the keychain. That is exactly what Tauri is.

## the shell

**Tauri v2**, with a Rust core and a React/TypeScript front-end.

| | desktop | mobile | native transport | keychain | ships a runtime |
|---|---|---|---|---|---|
| **Tauri v2** | macOS, Windows, Linux | iOS, Android | yes, Rust | yes | no — the system webview |
| Electron | macOS, Windows, Linux | no | yes, Node | via a module | ~150 MB of Chromium |
| React Native | via a second project | iOS, Android | yes | yes | — |
| Flutter | all five | all five | yes | yes | — |
| a PWA | all five | all five | **no** | **no** | — |

A PWA is out on the first wall alone. Electron gives up the phone, which is where a session
you want to check on from a train is actually read. Flutter clears every column and is the real
alternative — the cost is that nothing in this repository is Dart, and that the dashboard's
screens, types and design language would be rewritten rather than reused. React Native is the
same trade with worse desktop support.

Tauri wins on fit rather than on any single column: the API client, the payload types, the log
renderer and the whole visual language already exist in `web/`, in TypeScript, and the app is
the same product wearing a different frame. The Rust core is small — it is a bearer-token HTTP
client, an SSE reader and a keychain — and Rust is where "hold a credential and never log it" is
most easily made true.

The one real cost is honest: on Linux the webview is `webkit2gtk`, which is not Chromium, and
the front-end has to stay inside what it supports. The dashboard's CSS already does.

## the shape

```
   ┌──────────────────────────── the app ────────────────────────────┐
   │                                                                 │
   │   React UI  ──invoke──►  Rust core  ──HTTPS──►  orchestrad #1   │
   │      ▲                      │  │  │                             │
   │      └────events────────────┘  │  └──────────►  orchestrad #2   │
   │                                │                                │
   │                          ┌─────┴─────┐                          │
   │                          │  keychain │  backends.json           │
   │                          └───────────┘  (no secrets)            │
   └─────────────────────────────────────────────────────────────────┘
```

Two rules hold the design together, and every screen is built on them:

1. **The UI never opens a socket.** It calls `invoke("api_request", …)` and gets a status and a
   JSON body back. There is no `fetch` in the front-end, no `EventSource`, and no URL of a
   backend anywhere in the React tree except as a label to display.
2. **The UI never sees a token.** It names a backend by its id. The core resolves the id to a
   base URL and a secret, attaches the header, and returns only the response.

Together they mean a bug in the front-end — a stray log, an XSS through a task's prompt, a
dependency that phones home — cannot exfiltrate a credential, because the credential was never
in that process.

## a backend

A backend is one `orchestrad dashboard` instance. It is described by:

| field | | stored |
|---|---|---|
| `id` | opaque, generated | `backends.json` |
| `name` | what you call it: "home", "prod" | `backends.json` |
| `url` | origin, e.g. `https://orchestra.example.com` | `backends.json` |
| `color` | one of the four section hues, for the switcher | `backends.json` |
| `allowInsecureTls` | off by default; for a self-signed box on a LAN | `backends.json` |
| the secret | the dashboard password | **keychain only** |

`backends.json` lives in the platform config directory (`~/.config/orchestra-app/` on Linux,
`~/Library/Application Support/…` on macOS, `%APPDATA%` on Windows) and holds no secret, so it
can be copied between machines, checked into a dotfile repository, or read to see what an app is
configured with. The secrets sit beside it in the OS keychain under service `orchestra-app`,
account `<id>`.

Adding one is a **probe, then save**: the core sends `GET /api/session` with the secret, and only
a `200` writes anything. A wrong password, a wrong port, a TLS name mismatch and a daemon that
is simply down are four different errors, and the add dialog says which — "saved, then every
page is empty" is the failure this avoids.

### switching

One backend is *selected*. The selection is app state, not per-window state, and it is
remembered across restarts.

Switching is a hard cut, not a merge:

- every open stream is torn down before the new one's are opened, so a frame from the old
  backend can never land in a view of the new one (each stream carries its backend id, and the
  UI drops any frame whose id is not the selected one — belt and braces, because a torn-down
  stream may have a frame already in flight);
- cached payloads are dropped rather than shown stale under a new name;
- the route is kept where it makes sense and reset where it does not. `/queue` survives a
  switch; `/tasks/t2026…` does not, because that id belongs to the backend you just left.

The switcher is in the title bar with the backend's colour beside it, `⌘K` / `Ctrl-K` opens it,
and `⌘1…9` jumps straight to one. On a phone it is a sheet from the top bar.

A backend is more than a dropdown entry, though: **it is watched even when it is not selected.**
The core keeps a slow poll (60 s, `GET /api/v1/overview`, no stream) against every configured
backend, which is what makes the switcher useful — each entry shows running/pending counts and a
dot for reachable, and a failure somewhere else is visible before you go looking for it. That is
also what feeds notifications.

## transport

Two commands cover every screen.

**`api_request { backend, method, path, body? } -> { status, json }`.** One round trip. Path is
the API path (`v1/queue`, `v1/interactive/<id>/messages`), never a full URL — the core joins it
to the base, so the front-end cannot be made to send a token somewhere else. A `401` comes back
as a typed error and puts that backend into a *needs-credential* state rather than logging the
whole app out; the others are untouched, which is the point of holding several.

**`stream_start { backend, path, cursor? } -> streamId`** and **`stream_stop { streamId }`.**
The core issues the same authenticated request against `/sse/v1/…`, parses the event stream
incrementally, and emits each frame to the front-end as a Tauri event named `stream://<id>`. A
dropped connection is retried with backoff, and for the transcript stream the retry carries the
last seq it saw as `?after=`, so a reconnect resumes rather than replays — the cursor the
`/events` stream already takes for exactly this. The front-end sees a hook, `useLive(path)`,
whose shape matches `useLiveData` in the dashboard so the screens port over.

Both are per backend, so a session running on "home" keeps streaming while you read the queue on
"prod" — the UI just is not rendering it.

## the screens

Parity with the dashboard, plus the two things a multi-backend client adds.

- **Backends** — the list, add/edit/remove, reachability, the running/pending counts, and which
  one is selected. This is the app's home when nothing is selected yet.
- **Overview** — the selected backend's counts, active queue, recent tasks.
- **Queue** — entries by priority and status; cancel a running one, with the same confirm the
  dashboard asks for.
- **Tasks** — history, and a run's full structured log. The log renderer is the dashboard's,
  ported.
- **Listeners** — what is configured, when each last checked, and the on/off switch. Editing a
  listener's config is a later phase (it is a text editor over a YAML document, and a phone is
  not where that is done).
- **Chat** — sessions on this backend: start one against a repository pair, read the transcript
  live, post a turn, interrupt, end. On a phone this is the screen that justifies the app.
- **Auth** — sources, the limit binding on each, and the usage history bars.

Two are new:

- **Everywhere** — a cross-backend strip on the Backends screen: what is running *anywhere*, in
  one list, each row tagged with its backend. Clicking a row switches and navigates.
- **Notifications** — a task fails, a session's turn ends, an auth source blocks. Off by default,
  per backend, delivered through the OS notifier. This is why the idle poll exists.

## what carries over from `web/`, and how

The payload types, the log renderer, the status vocabulary and the design tokens are shared in
substance. They are **not** imported across directory boundaries: `web/` builds standalone and is
served by the Lean binary, and coupling its build to the app's would make either one harder to
change than it is. The app keeps its own copy of the types, headed by a comment naming
`web/src/api.ts` and `Orchestra/Dashboard.lean` as the two sides they mirror — which is the
arrangement those two already have with each other.

`docs/openapi.json` is the contract of record. If drift becomes a problem the answer is to
generate both copies from it rather than to make one import the other.

## security

- The token is written once, into the keychain, and read only inside the core. It is never
  returned by a command, never logged, never in a URL, never in a crash report.
- The core sends `Authorization: Bearer`. It never asks for a cookie and never stores one.
- TLS verification is on. `allowInsecureTls` is per backend, defaults to off, is spelled out in
  the UI as what it is, and is refused for anything but a private-range or loopback host.
- The webview loads only bundled assets; the CSP forbids remote script, and remote images are
  not rendered. A prompt, a log line and a listener name are all attacker-influenced text and are
  rendered as text.
- No telemetry. The app talks to the backends in its list and to nothing else.

## packaging

`app/` is its own workspace, built by `npm` and `cargo`, and does not participate in the Lake
build — `lake build` stays what it is.

```sh
cd app
npm install
npm run tauri dev            # the app against a local orchestrad
npm run tauri build          # a bundle for the host platform
```

Bundles: `.dmg`/`.app` (macOS, universal), `.msi`/`.exe` (Windows), `.deb`/`.rpm`/`.AppImage`
(Linux), `.apk`/`.aab` (Android), `.ipa` (iOS). CI builds the three desktop targets on every push
to `app/` — that is what keeps "platform independent" a fact rather than a claim. Signing and
notarisation are release-time concerns and are not in CI's path; mobile targets are built
locally until there is a signing story.

Linux needs `libwebkit2gtk-4.1-dev`, `libgtk-3-dev`, `libayatana-appindicator3-dev`,
`librsvg2-dev` and `libsoup-3.0-dev` to build. The CI job installs them.

## phases

1. **The core and the registry.** Rust: registry, keychain, `api_request`, `stream_start`,
   probe. React: shell, Backends screen, switcher, Overview. The app is useful here — it
   answers "what is running, where".
2. **Read-only parity.** Queue, Tasks with the log renderer, Listeners, Auth with the usage
   bars.
3. **Writing.** Cancel a task, toggle a listener.
4. **Chat.** Sessions list, transcript, turns, interrupt, end.
5. **The multi-backend dividends.** The Everywhere strip, idle polling, notifications.
6. **Mobile.** iOS and Android targets, touch layouts, background refresh.

Phases 1–4 are what makes it a client. 5 is what makes it *this* client rather than the
dashboard in a window.

## the phone

Phase 6, in the state described below: the targets are configured, the interface is laid out for
a handset, and the two places the core has to behave differently on a phone are handled. What has
*not* happened is a build — see "getting an APK".

**Layout.** Nothing is hidden on a small screen. Every destination, every status and the switcher
are all still there, because the reason to hold orchestra on a phone is that something is wrong
and you are not at the desk where the other client lives. What changes is what a finger needs:
44px targets, 16px text fields (below that, iOS zooms the page on focus and leaves it zoomed),
the switcher hard right on the first row where a thumb reaches it, its menu spanning the width,
and the chat compose box sticky at the bottom, measured in `dvh` — which is the difference
between it sitting above the keyboard and behind it. `viewport-fit=cover` puts the page under the
notch, so every edge that meets one is padded by `env(safe-area-inset-*)` rather than by a
guessed number. Pull-to-refresh is disabled: it would reload the bundle and drop every stream
mid-conversation.

**Where the secrets go.** There is no keychain to speak of on Android, and the crate that reads
one does not build for it. The fallback in `secrets.rs` is the app's own data directory, which on
a phone is already private to the app — mode `0600` on top of that. The Backends screen says
which store is in use, so this is never assumed away.

**Which roots TLS trusts.** This is the one place mobile is not merely the desktop with bigger
buttons. On the desktop the app verifies against the operating system's own store, which is what
makes a company CA that is already installed there work with no second place to put it. Android
keeps its trust store behind the Java APIs, where the Rust side cannot read it — so the two
mobile targets carry Mozilla's roots with them instead. The consequence, stated plainly: **a
backend whose certificate is signed by a private CA works from the desktop app and not from the
phone.** There, the answer is a publicly-rooted certificate, or `allowInsecureTls` on your own
network.

### getting an APK

The Android toolchain is not a small thing to install, and every piece of it — the SDK, the NDK,
Google's Maven repository — lives on hosts that a locked-down development environment may not be
allowed to reach. So the APK is built in CI, by the `android` job in
[`.github/workflows/app.yml`](../.github/workflows/app.yml).

Two ways out of that job, and they are not equivalent. The run's **artifact**
(`orchestra-app-android-apk`) is a zip GitHub serves only to a signed-in browser — fine from a
desktop, awkward for a file whose entire purpose is to be opened on a phone. Running the workflow
with **publish** ticked also attaches the APK to the `android-latest` prerelease, where it is a
plain link. That is off by default, because a CI run should not write to a repository's releases
unless it was asked to.

It is a **debug** APK, arm64 only, signed with the debug keystore Gradle generates — which is
what makes it installable on a handset without a signing identity. Installing it means allowing
your phone to install from that source; it is not, and cannot be, a Play Store build.

Size is why it is one architecture, and why the Rust library goes in stripped. The default builds
every ABI into the APK, most of it code the phone installing it cannot execute; and `--debug`,
which is what makes the APK installable at all, otherwise leaves a `libapp.so` carrying full
debug symbols, which was the majority of a 161 MB file. One ABI and
`CARGO_PROFILE_DEV_STRIP=symbols` between them take the bulk of that out.

The two sizes reported for the same build disagree, and it is worth knowing why: an Actions
artifact is a **zip** of the APK, which compresses to roughly a third, while a release asset is
the APK itself.

Locally, with a JDK, the SDK and the NDK in place (`NDK_HOME` set):

```sh
cd app
npm install
npm run android:init     # writes the Gradle project under src-tauri/gen/android
npm run android:apk      # a debug APK under gen/android/app/build/outputs/apk/
npm run android:dev      # or run it on a device or emulator, with hot reload
```

`gen/` is generated from `tauri.conf.json` and the icons and is not tracked: regenerating it is
cheaper than reviewing a diff of it, and it is the Tauri CLI's output rather than anyone's
source. iOS is the same shape (`npm run ios:init`) and needs a Mac and Xcode, which is why no CI
job builds it.

**Signing** — a release APK or AAB, and an `.ipa` — needs a keystore and an Apple developer
identity, which are release-time secrets this repository does not hold. That is deliberate and
the reason CI stops at a debug build.

## deliberately not in scope

- **Running orchestra.** The app manages backends; it does not start a daemon, hold a queue, or
  run an agent. There is no embedded `orchestrad`.
- **Enqueueing tasks.** Nothing in the HTTP API enqueues work — that is the daemon's control
  socket, which stays off the network. A client that could enqueue would need a new route
  first, and that decision belongs to the API, not to this app.
- **Editing configuration by hand on a phone.** Listener and role configs are documents; the
  dashboard edits them. The app toggles what is toggleable.
- **A second authentication scheme.** The bearer token the dashboard already defines is the
  credential. The app stores it better; it does not replace it.
