# the native app

A desktop and mobile client for **several** orchestra backends at once. The dashboard under
[`web/`](../web) is a view of the one backend that served it; this is the same views over a list
of them, with a switcher in the title bar.

The design — why it is native at all, what a backend is, how switching works, and what ships in
which phase — is [`docs/native-app.md`](../docs/native-app.md). This file is how to build it.

```
app/
  src/            the React front-end
    core/         the registry, the transport, the typed API, the live hooks
    components/   chrome shared with the dashboard, ported
    pages/        one per destination
  src-tauri/      the Rust core: registry, keychain, HTTP, SSE
```

## running it

```sh
cd app
npm install
npm run tauri dev      # the app, against whatever backends you add to it
```

`tauri dev` starts Vite on port 1420 and the Rust binary against it. Point it at a backend by
adding one on the **Backends** screen: the address an `orchestrad dashboard` is served on, and
the password it was started with — `--password`, `$ORCHESTRA_DASHBOARD_PASSWORD`, or
`<data>/dashboard.secret` on that host. Nothing is stored until the address and the password
have both been proved to work.

## the phone

Android and iOS are the same codebase and the same core; what differs is the toolchain that
builds them and where the secrets end up. `docs/native-app.md` has the detail — including the one
behaviour that is genuinely different on a phone, which trust roots TLS is verified against.

```sh
npm run android:init     # writes the Gradle project under src-tauri/gen/android
npm run android:dev      # on a device or emulator, with hot reload
npm run android:apk      # a debug APK under gen/android/app/build/outputs/apk/
npm run ios:init         # the Xcode project; needs a Mac
npm run ios:dev
```

This needs a JDK, the Android SDK (platform 34, build-tools 34) and the NDK, with `NDK_HOME`
pointing at it, plus the four Rust targets:

```sh
rustup target add aarch64-linux-android armv7-linux-androideabi \
                  i686-linux-android x86_64-linux-android
```

`gen/` is not tracked. It is generated from `tauri.conf.json` and the icons, so regenerating it
costs less than reviewing a diff of it.

**Not building it yourself:** the `android` job in CI produces a debug APK on every push that
touches `app/`, and on demand through *Run workflow*. Two ways to get it:

- the run's artifacts, as `orchestra-app-android-apk` — a zip, and GitHub only serves it to a
  signed-in browser, which usually means fetching it on a desktop and moving it across;
- *Run workflow* with **publish** ticked, which also attaches it to the `android-latest`
  prerelease. A release asset is a plain link, so that is the one to use when the phone is where
  you want to open it.

It is arm64 only (`--target aarch64`), which is every handset made in roughly the last decade —
an emulator wants `--target aarch64 x86_64`. It is signed with Gradle's debug keystore, which is
what makes it installable without a signing identity, and what makes it not a Play Store build:
the phone will ask you to allow installing from that source.

For a desktop bundle:

```sh
npm run tauri build    # .dmg / .msi / .deb, .rpm, .AppImage, for the host platform
```

Linux needs the webview and its dependencies to build:

```sh
sudo apt-get install libwebkit2gtk-4.1-dev libgtk-3-dev libayatana-appindicator3-dev \
                     librsvg2-dev libsoup-3.0-dev
```

macOS needs Xcode's command line tools; Windows needs the MSVC build tools and WebView2, which
ships with Windows 11.

## checks

The three that CI runs, and the three to run before pushing:

```sh
npm run typecheck                            # tsc, no emit
cd src-tauri && cargo test                   # the core, including its wire-level tests
cd src-tauri && cargo clippy --all-targets && cargo fmt --check
```

`cargo test` covers the parts that are easy to get subtly wrong and hard to notice: URL
normalisation, the refusal to skip TLS verification for a public host, SSE frame parsing and
cursor rewriting, and — against a stub server on a real socket — that a request carries the
bearer token and no cookie, that a write is `application/json`, that a `401` is raised as
`Unauthorized`, and that a failure carries the server's own words.

## the two rules

Everything in `src-tauri/src` exists to hold these:

1. **The front-end never opens a socket.** No `fetch`, no `EventSource`. It calls
   `api_request` / `stream_start` and gets a status and a payload.
2. **The front-end never sees a token.** It names a backend by its id; the core resolves the id
   to an origin and a secret and returns only the answer.

A change that puts a URL or a password in the React tree breaks both. There is no third way to
reach a backend, and adding one is a change to the design, not to a file.

## icons

`src-tauri/icons/` holds the PNGs the Linux bundles use. macOS wants an `.icns` and Windows an
`.ico`; generate the whole set from the master PNG with

```sh
npm run tauri icon src-tauri/icons/icon.png
```

which CI does before it builds.

## the inline theme script

`index.html` carries one inline script, which applies a saved theme before first paint. The CSP
in `src-tauri/tauri.conf.json` allows exactly that script, by hash — so **editing it, even by a
character, stops it running**. After changing it, recompute:

```sh
node -e 'const h=require("crypto").createHash("sha256");
  const m=require("fs").readFileSync("index.html","utf8").match(/<script>([\s\S]*?)<\/script>/);
  h.update(m[1]); console.log("sha256-" + h.digest("base64"))'
```

and put the result in `script-src`.

## what is not here

The app manages backends; it does not run one. There is no embedded `orchestrad`, and nothing in
it enqueues a task — the HTTP API has no route that does, because enqueueing goes over the
daemon's control socket, which stays off the network. See the last section of
[`docs/native-app.md`](../docs/native-app.md).
