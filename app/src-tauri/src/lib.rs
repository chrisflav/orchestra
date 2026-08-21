//! The core: a backend registry, a keychain, an HTTP client and an SSE reader.
//!
//! Every command here is the front-end's only way out of the webview. Two rules hold:
//!
//! 1. **The UI never opens a socket.** It calls `api_request` or `stream_start` and gets a
//!    status and a payload back. There is no `fetch` in the front-end and no `EventSource`.
//! 2. **The UI never sees a token.** It names a backend by its id; the base URL and the secret
//!    are resolved here, put in a header, and dropped. No command returns a secret — the
//!    closest anything comes is `hasSecret`, a boolean.
//!
//! Together they mean a bug in the front-end cannot exfiltrate a credential, because the
//! credential was never in that process. That is the whole reason this app is native rather
//! than a second web page: see `docs/native-app.md`.

mod client;
mod error;
mod registry;
mod secrets;
mod stream;

use serde::{Deserialize, Serialize};
use tauri::{AppHandle, Manager, State};

use crate::client::{ApiResponse, Clients, Probe};
use crate::error::{Error, Result};
use crate::registry::Backend;
use crate::secrets::Secrets;
use crate::stream::Streams;

/// Everything the core owns, held by Tauri and handed to each command.
struct Core {
    registry: registry::Store,
    secrets: Secrets,
    clients: Clients,
    streams: Streams,
}

/// A backend as the front-end sees it: the record, minus anything secret, plus whether a
/// secret is stored at all. A backend without one is reachable-but-unusable and the Backends
/// screen says so rather than letting every page fail with a 401.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct BackendView {
    #[serde(flatten)]
    backend: Backend,
    has_secret: bool,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct RegistryView {
    backends: Vec<BackendView>,
    selected: Option<String>,
    /// Which store the secrets are in — `keychain` or `file`. Shown on the Backends screen, so
    /// a fallback is never assumed away.
    secret_store: secrets::Store,
}

impl Core {
    fn view(&self) -> RegistryView {
        let snapshot = self.registry.snapshot();
        let backends = snapshot
            .backends
            .into_iter()
            .map(|backend| {
                let has_secret = self
                    .secrets
                    .get(&backend.id)
                    .ok()
                    .flatten()
                    .is_some_and(|s| !s.is_empty());
                BackendView {
                    backend,
                    has_secret,
                }
            })
            .collect();
        // A selection naming a backend that is no longer there is treated as no selection,
        // which is what makes removing the selected one a one-line operation.
        let selected = snapshot.selected.filter(|id| {
            self.registry
                .snapshot()
                .backends
                .iter()
                .any(|b| &b.id == id)
        });
        RegistryView {
            backends,
            selected,
            secret_store: self.secrets.store(),
        }
    }

    /// Resolve a backend id to the pair a request needs. The only place the two are held
    /// together, and it is not `pub`.
    fn resolve(&self, id: &str) -> Result<(Backend, String, reqwest::Client)> {
        let backend = self.registry.get(id)?;
        let secret = self
            .secrets
            .get(id)?
            .filter(|s| !s.is_empty())
            .ok_or_else(|| {
                Error::NoSuchBackend(format!(
                    "no password is stored for {} — add one on the Backends screen",
                    backend.name
                ))
            })?;
        let client = self.clients.get(&backend)?;
        Ok((backend, secret, client))
    }
}

/* ── the registry ───────────────────────────────────────────────────────────────────────── */

#[tauri::command]
fn backends_list(core: State<'_, Core>) -> RegistryView {
    core.view()
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct BackendInput {
    name: String,
    url: String,
    secret: String,
    #[serde(default)]
    color: Option<String>,
    #[serde(default)]
    allow_insecure_tls: bool,
}

/// Try an address and a password without storing anything.
///
/// This is what the add dialog calls before it offers to save, and what the Backends screen
/// calls to re-test one that has gone quiet. Nothing it does is visible on disk.
#[tauri::command]
async fn backend_probe(input: BackendInput) -> Result<Probe> {
    let url = registry::normalise_url(&input.url)?;
    reject_insecure(&url, input.allow_insecure_tls)?;
    let candidate = Backend {
        id: "probe".into(),
        name: input.name,
        url,
        color: input.color.unwrap_or_else(|| "strings".into()),
        allow_insecure_tls: input.allow_insecure_tls,
        notify: false,
        added_at: String::new(),
    };
    Ok(client::probe(&candidate, &input.secret).await)
}

/// Add a backend, after proving it works.
///
/// Probe-then-save rather than save-then-discover: a wrong password, a wrong port, a name the
/// certificate does not cover and a daemon that is simply down are four different problems,
/// and "it saved, and then every page was empty" tells you none of them apart.
#[tauri::command]
async fn backend_add(core: State<'_, Core>, input: BackendInput) -> Result<RegistryView> {
    let url = registry::normalise_url(&input.url)?;
    reject_insecure(&url, input.allow_insecure_tls)?;
    let name = input.name.trim().to_string();
    if name.is_empty() {
        return Err(Error::BadRequest("give the backend a name".into()));
    }
    let candidate = Backend {
        id: registry::new_id(),
        name,
        url,
        color: input.color.unwrap_or_else(|| "strings".into()),
        allow_insecure_tls: input.allow_insecure_tls,
        notify: false,
        added_at: now(),
    };
    let probe = client::probe(&candidate, &input.secret).await;
    if !probe.ok {
        return Err(Error::BadRequest(probe.message));
    }
    core.secrets.set(&candidate.id, &input.secret)?;
    let id = candidate.id.clone();
    core.registry.update(|reg| {
        reg.backends.push(candidate);
        // The first backend added is the one selected: an app with exactly one backend should
        // never open on a chooser.
        if reg.selected.is_none() {
            reg.selected = Some(id.clone());
        }
        Ok(())
    })?;
    Ok(core.view())
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct BackendPatch {
    id: String,
    #[serde(default)]
    name: Option<String>,
    #[serde(default)]
    url: Option<String>,
    #[serde(default)]
    color: Option<String>,
    #[serde(default)]
    allow_insecure_tls: Option<bool>,
    #[serde(default)]
    notify: Option<bool>,
    /// A new password. Absent leaves the stored one alone; this is how a rename does not ask
    /// for the credential again.
    #[serde(default)]
    secret: Option<String>,
}

#[tauri::command]
fn backend_update(core: State<'_, Core>, patch: BackendPatch) -> Result<RegistryView> {
    let normalised = patch
        .url
        .as_deref()
        .map(registry::normalise_url)
        .transpose()?;
    core.registry.update(|reg| {
        let backend = reg
            .backends
            .iter_mut()
            .find(|b| b.id == patch.id)
            .ok_or_else(|| Error::NoSuchBackend(format!("no backend with id {}", patch.id)))?;
        if let Some(name) = &patch.name {
            let name = name.trim();
            if name.is_empty() {
                return Err(Error::BadRequest("give the backend a name".into()));
            }
            backend.name = name.to_string();
        }
        if let Some(url) = normalised {
            backend.url = url;
        }
        if let Some(color) = &patch.color {
            backend.color = color.clone();
        }
        if let Some(allow) = patch.allow_insecure_tls {
            backend.allow_insecure_tls = allow;
        }
        if let Some(notify) = patch.notify {
            backend.notify = notify;
        }
        reject_insecure(&backend.url, backend.allow_insecure_tls)
    })?;
    if let Some(secret) = &patch.secret {
        core.secrets.set(&patch.id, secret)?;
    }
    // The address or the TLS policy may have moved, and every open stream is still pointed at
    // where it was. Drop both rather than leave a stream reading the old address.
    core.clients.forget(&patch.id);
    core.streams.stop_backend(&patch.id);
    Ok(core.view())
}

/// Remove a backend, its secret and its connections.
#[tauri::command]
fn backend_remove(core: State<'_, Core>, id: String) -> Result<RegistryView> {
    core.streams.stop_backend(&id);
    core.clients.forget(&id);
    core.registry.update(|reg| {
        reg.backends.retain(|b| b.id != id);
        if reg.selected.as_deref() == Some(id.as_str()) {
            reg.selected = reg.backends.first().map(|b| b.id.clone());
        }
        Ok(())
    })?;
    // After the registry, so a failure to clear the keychain leaves an orphaned secret rather
    // than a backend that cannot be reached and cannot be removed.
    core.secrets.delete(&id)?;
    Ok(core.view())
}

/// Select a backend, and tear down what the previous one was streaming.
///
/// The teardown is the point. Switching is a hard cut: no frame from the backend you left can
/// land in a view of the one you moved to, and nothing is left reading an address nothing is
/// showing.
#[tauri::command]
fn backend_select(core: State<'_, Core>, id: Option<String>) -> Result<RegistryView> {
    let previous = core.registry.snapshot().selected;
    core.registry.update(|reg| {
        match &id {
            Some(id) if !reg.backends.iter().any(|b| &b.id == id) => {
                return Err(Error::NoSuchBackend(format!("no backend with id {id}")))
            }
            _ => {}
        }
        reg.selected = id.clone();
        Ok(())
    })?;
    if let Some(previous) = previous {
        if Some(&previous) != id.as_ref() {
            core.streams.stop_backend(&previous);
        }
    }
    Ok(core.view())
}

/* ── talking to a backend ───────────────────────────────────────────────────────────────── */

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct ApiInput {
    backend: String,
    #[serde(default = "get")]
    method: String,
    /// A path under `/api/`, without a leading slash: `v1/queue`, `v1/interactive/<id>/messages`.
    path: String,
    #[serde(default)]
    body: Option<serde_json::Value>,
}

fn get() -> String {
    "GET".into()
}

#[tauri::command]
async fn api_request(core: State<'_, Core>, input: ApiInput) -> Result<ApiResponse> {
    let (backend, secret, http) = core.resolve(&input.backend)?;
    let method = reqwest::Method::from_bytes(input.method.to_uppercase().as_bytes())
        .map_err(|_| Error::BadRequest(format!("{} is not an HTTP method", input.method)))?;
    let url = client::url_for(&backend, "api", &input.path)?;
    client::request(&http, method, &url, &secret, input.body).await
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct StreamInput {
    backend: String,
    /// A path under `/sse/`, without a cursor — the cursor is `cursor`, so that a reconnect
    /// can move it without the front-end being involved.
    path: String,
    #[serde(default)]
    cursor: Option<u64>,
}

#[tauri::command]
fn stream_start(app: AppHandle, core: State<'_, Core>, input: StreamInput) -> Result<String> {
    let (backend, secret, _) = core.resolve(&input.backend)?;
    core.streams
        .start(app, backend, secret, input.path, input.cursor)
}

#[tauri::command]
fn stream_stop(core: State<'_, Core>, id: String) {
    core.streams.stop(&id);
}

/* ── wiring ─────────────────────────────────────────────────────────────────────────────── */

/// Refuse to skip TLS verification for a public host.
///
/// On a LAN box with a self-signed certificate this setting is a defensible local choice; on a
/// name that resolves on the internet it is not a preference, it is the absence of TLS. The
/// same check runs again when the client is built, so a hand-edited `backends.json` cannot get
/// past it either.
fn reject_insecure(url: &str, allow: bool) -> Result<()> {
    if allow && !registry::is_private_host(url) {
        return Err(Error::BadRequest(
            "TLS verification can only be skipped for a loopback or private-network address".into(),
        ));
    }
    Ok(())
}

/// RFC 3339 in UTC, which is what every instant in this system is.
///
/// Written out by hand rather than by pulling in a date library for one call: the app records
/// exactly one timestamp of its own, when a backend was added.
fn now() -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    let secs = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0) as i64;
    let days = secs.div_euclid(86_400);
    let time = secs.rem_euclid(86_400);
    // Civil-from-days, Howard Hinnant's algorithm: exact, branch-free, and no dependency.
    let z = days + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z.rem_euclid(146_097);
    let yoe = (doe - doe / 1_460 + doe / 36_524 - doe / 146_096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let y = if m <= 2 { y + 1 } else { y };
    format!(
        "{y:04}-{m:02}-{d:02}T{:02}:{:02}:{:02}Z",
        time / 3_600,
        (time % 3_600) / 60,
        time % 60
    )
}

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    tauri::Builder::default()
        .plugin(tauri_plugin_opener::init())
        .setup(|app| {
            // The registry sits in the config directory and the secret fallback in the local
            // data directory — the platform's own answers to "configuration" and "state".
            let config = app.path().app_config_dir()?;
            let data = app.path().app_local_data_dir()?;
            app.manage(Core {
                registry: registry::Store::load(config.join("backends.json")),
                secrets: Secrets::new(data),
                clients: Clients::default(),
                streams: Streams::default(),
            });
            Ok(())
        })
        .invoke_handler(tauri::generate_handler![
            backends_list,
            backend_probe,
            backend_add,
            backend_update,
            backend_remove,
            backend_select,
            api_request,
            stream_start,
            stream_stop,
        ])
        .run(tauri::generate_context!())
        .expect("the app failed to start");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_instant_is_rfc_3339_in_utc() {
        assert!(now().ends_with('Z'));
        assert_eq!(now().len(), 20);
    }

    #[test]
    fn insecure_tls_is_refused_for_a_public_host() {
        assert!(reject_insecure("https://orchestra.example.com", true).is_err());
        assert!(reject_insecure("https://orchestra.example.com", false).is_ok());
        assert!(reject_insecure("https://192.168.1.9:8080", true).is_ok());
    }
}
