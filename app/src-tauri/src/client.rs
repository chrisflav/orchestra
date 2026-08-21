//! The HTTP half: one request, one answer.
//!
//! This is the only module that holds a secret and the only one that opens a socket. The
//! front-end names a backend by id and an API path; the base URL and the bearer token are
//! resolved here and never travel back up. A path is joined to the base — it is never allowed
//! to *be* a URL — so no bug in the front-end can address the token somewhere else.
//!
//! Authentication is the bearer half of the dashboard's scheme (see "Authentication" in
//! `Orchestra/Dashboard.lean`): `Authorization: Bearer <secret>` on every request, no cookie,
//! and `Content-Type: application/json` on every write, which the server requires.

use std::collections::HashMap;
use std::sync::Mutex;
use std::time::Duration;

use serde::{Deserialize, Serialize};

use crate::error::{Error, Result};
use crate::registry::Backend;

/// How long to wait on a single request. Generous enough for a task detail with a long log
/// tail over a slow link, short enough that a dead backend does not hang a screen.
const TIMEOUT: Duration = Duration::from_secs(30);

/// How long to wait for the connection itself. Separate from `TIMEOUT` so a stream, which has
/// no total timeout at all, still fails fast when there is nothing listening.
const CONNECT_TIMEOUT: Duration = Duration::from_secs(15);

/// A pool of clients, one per (origin, TLS policy) pair.
///
/// Per backend rather than one for all of them, because the TLS policy differs per backend and
/// because connection reuse is what makes a two-second SSE tick cheap. Keyed by id so that
/// editing a backend's address or its TLS setting drops the old client with the old policy.
#[derive(Default)]
pub struct Clients {
    inner: Mutex<HashMap<String, (String, bool, reqwest::Client)>>,
}

impl Clients {
    pub fn get(&self, backend: &Backend) -> Result<reqwest::Client> {
        let mut map = self.inner.lock().expect("client pool lock");
        if let Some((url, insecure, client)) = map.get(&backend.id) {
            if url == &backend.url && *insecure == backend.allow_insecure_tls {
                return Ok(client.clone());
            }
        }
        let client = build(backend)?;
        map.insert(
            backend.id.clone(),
            (
                backend.url.clone(),
                backend.allow_insecure_tls,
                client.clone(),
            ),
        );
        Ok(client)
    }

    /// Forget a backend's client. Called when it is removed, so its connections are not held
    /// open by a pool entry nothing can reach any more.
    pub fn forget(&self, id: &str) {
        self.inner.lock().expect("client pool lock").remove(id);
    }
}

/// Build a client for one backend.
///
/// `allow_insecure_tls` is honoured only for a private host. The registry refuses to store it
/// for anything else, and this is the second check: a hand-edited `backends.json` cannot turn
/// verification off for a public name.
pub fn build(backend: &Backend) -> Result<reqwest::Client> {
    build_with(backend, Some(TIMEOUT))
}

/// A client for a stream: the same TLS policy, and no total timeout.
///
/// A Server-Sent Events response is a body that never ends, so the request timeout every other
/// call wants would cut it off mid-stream on a schedule. The connect timeout stays — a backend
/// that cannot be reached should say so in seconds, whatever it was asked for.
pub fn build_streaming(backend: &Backend) -> Result<reqwest::Client> {
    build_with(backend, None)
}

fn build_with(backend: &Backend, timeout: Option<Duration>) -> Result<reqwest::Client> {
    let insecure = backend.allow_insecure_tls && crate::registry::is_private_host(&backend.url);
    let mut builder = reqwest::Client::builder()
        .user_agent(concat!("orchestra-app/", env!("CARGO_PKG_VERSION")))
        .danger_accept_invalid_certs(insecure)
        .connect_timeout(CONNECT_TIMEOUT);
    if let Some(timeout) = timeout {
        builder = builder.timeout(timeout);
    }
    builder
        .build()
        .map_err(|e| Error::Unreachable(format!("could not build an HTTP client: {e}")))
}

/// Join an API path to a backend's origin.
///
/// The path is what the front-end asked for, and the rules on it are strict on purpose: no
/// scheme, no leading slash, no `..`. Everything the app calls is under `/api/` or `/sse/`,
/// and `kind` says which — so a caller cannot reach any other part of the origin either.
pub fn url_for(backend: &Backend, kind: &str, path: &str) -> Result<String> {
    if path.contains("://") || path.starts_with('/') || path.starts_with("..") {
        return Err(Error::BadRequest(format!(
            "{path} is not an API path; give one relative to /{kind}/"
        )));
    }
    if path.split('/').any(|segment| segment == "..") {
        return Err(Error::BadRequest("an API path cannot climb".into()));
    }
    Ok(format!("{}/{}/{}", backend.url, kind, path))
}

/// What a command answers with.
///
/// The status is carried rather than folded into an error for everything but `401`, because
/// several routes answer with a status the UI reads as an outcome: a `409` from `cancel` means
/// "not running", a `404` from a detail means "gone", and both are answers to show.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ApiResponse {
    pub status: u16,
    /// The parsed body, or `null` when the response had none (a `204`) or was not JSON.
    pub body: serde_json::Value,
}

#[derive(Debug, Deserialize)]
struct ApiErrorBody {
    error: Option<String>,
}

/// Perform one request and classify the result.
///
/// A `401` is raised as `Unauthorized` rather than returned as a status: it is the one answer
/// that means the *stored credential* is wrong, and the front-end has to put that backend into
/// its needs-credential state wherever it came from. Every other non-2xx comes back as an
/// `Http` error carrying the server's own `error` field, which the API sets on every failure.
pub async fn request(
    client: &reqwest::Client,
    method: reqwest::Method,
    url: &str,
    secret: &str,
    body: Option<serde_json::Value>,
) -> Result<ApiResponse> {
    let mut req = client
        .request(method, url)
        .bearer_auth(secret)
        .header(reqwest::header::ACCEPT, "application/json");
    if let Some(body) = body {
        // Every write must be `application/json` or the server answers 415. `.json()` sets it.
        req = req.json(&body);
    }
    let response = req.send().await?;
    let status = response.status();
    let text = response.text().await.unwrap_or_default();

    if status == reqwest::StatusCode::UNAUTHORIZED {
        return Err(Error::Unauthorized);
    }
    let parsed: serde_json::Value = if text.trim().is_empty() {
        serde_json::Value::Null
    } else {
        serde_json::from_str(&text).unwrap_or(serde_json::Value::Null)
    };
    if !status.is_success() {
        let message = serde_json::from_str::<ApiErrorBody>(&text)
            .ok()
            .and_then(|b| b.error)
            .unwrap_or_else(|| format!("the backend answered {}", status.as_u16()));
        return Err(Error::Http {
            status: status.as_u16(),
            message,
        });
    }
    Ok(ApiResponse {
        status: status.as_u16(),
        body: parsed,
    })
}

/// What a probe found. Saving a backend is gated on this, so the four ways an address can be
/// wrong — bad password, wrong port, TLS name mismatch, nothing listening — are told apart
/// before anything is written rather than after every page comes back empty.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Probe {
    pub ok: bool,
    /// `authenticated`, `rejected`, `notOrchestra`, `unreachable`.
    pub outcome: &'static str,
    pub message: String,
    /// Running and pending counts, when the probe got far enough to read them. Shown in the
    /// add dialog as proof that the address reached the intended daemon.
    pub running: Option<u64>,
    pub pending: Option<u64>,
}

/// A plain-HTTP dashboard reached over `https` fails deep inside the TLS handshake, and what
/// rustls says about it ("received corrupt message of type InvalidContentType") is true and
/// useless. Since a bare host is taken as `https`, this is the single likeliest way an address
/// someone typed goes wrong — so say what it probably is.
fn scheme_hint(backend: &Backend, message: &str) -> String {
    if backend.url.starts_with("https://") && message.contains("could not connect") {
        format!(
            "{message}\n\nThat address was taken as https. If the dashboard is plain HTTP there, \
             write http:// in front of it."
        )
    } else {
        message.to_string()
    }
}

/// Probe an address and a secret without touching the registry.
///
/// Two requests, and the pair is what distinguishes the outcomes. `GET /api/session` needs a
/// credential and answers `{"authenticated":true}`; a `401` there means the secret is wrong
/// rather than that the host is. `GET /api/v1/overview` then proves it is an orchestra backend
/// and not merely something that answered — and gives the dialog two numbers to show.
pub async fn probe(backend: &Backend, secret: &str) -> Probe {
    let client = match build(backend) {
        Ok(c) => c,
        Err(e) => {
            return Probe {
                ok: false,
                outcome: "unreachable",
                message: e.to_string(),
                running: None,
                pending: None,
            }
        }
    };
    let session_url = match url_for(backend, "api", "session") {
        Ok(u) => u,
        Err(e) => {
            return Probe {
                ok: false,
                outcome: "unreachable",
                message: e.to_string(),
                running: None,
                pending: None,
            }
        }
    };
    match request(&client, reqwest::Method::GET, &session_url, secret, None).await {
        Ok(_) => {}
        Err(Error::Unauthorized) => {
            return Probe {
                ok: false,
                outcome: "rejected",
                message: "the backend is there, but it rejected that password".into(),
                running: None,
                pending: None,
            }
        }
        Err(Error::Http { status, .. }) => {
            return Probe {
                ok: false,
                outcome: "notOrchestra",
                message: format!(
                    "something answered on that address, but not an orchestra dashboard \
                     (/api/session said {status})"
                ),
                running: None,
                pending: None,
            }
        }
        Err(e) => {
            return Probe {
                ok: false,
                outcome: "unreachable",
                message: scheme_hint(backend, &e.to_string()),
                running: None,
                pending: None,
            }
        }
    }

    let overview_url = match url_for(backend, "api", "v1/overview") {
        Ok(u) => u,
        Err(e) => {
            return Probe {
                ok: false,
                outcome: "unreachable",
                message: e.to_string(),
                running: None,
                pending: None,
            }
        }
    };
    match request(&client, reqwest::Method::GET, &overview_url, secret, None).await {
        Ok(response) => {
            let counts = response.body.get("counts");
            Probe {
                ok: true,
                outcome: "authenticated",
                message: "reached it".into(),
                running: counts
                    .and_then(|c| c.get("running"))
                    .and_then(|v| v.as_u64()),
                pending: counts
                    .and_then(|c| c.get("pending"))
                    .and_then(|v| v.as_u64()),
            }
        }
        Err(e) => Probe {
            ok: false,
            outcome: "notOrchestra",
            message: format!("the password was accepted, but /api/v1/overview failed: {e}"),
            running: None,
            pending: None,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn backend(url: &str) -> Backend {
        Backend {
            id: "b1".into(),
            name: "test".into(),
            url: url.into(),
            color: "strings".into(),
            allow_insecure_tls: false,
            notify: false,
            added_at: "2026-08-21T00:00:00Z".into(),
        }
    }

    #[test]
    fn a_path_is_joined_under_its_kind() {
        let b = backend("https://host");
        assert_eq!(
            url_for(&b, "api", "v1/queue").unwrap(),
            "https://host/api/v1/queue"
        );
        assert_eq!(
            url_for(&b, "sse", "v1/overview").unwrap(),
            "https://host/sse/v1/overview"
        );
    }

    #[test]
    fn a_path_cannot_be_a_url_or_climb() {
        let b = backend("https://host");
        assert!(url_for(&b, "api", "https://elsewhere/steal").is_err());
        assert!(url_for(&b, "api", "/api/v1/queue").is_err());
        assert!(url_for(&b, "api", "../login").is_err());
        assert!(url_for(&b, "api", "v1/../../x").is_err());
    }

    /// A one-shot HTTP server: answers the first request with `response`, and hands back what
    /// it was sent. Enough to prove what actually goes on the wire, which no amount of unit
    /// testing around `reqwest` can.
    fn stub(status: &str, body: &str) -> (String, std::thread::JoinHandle<String>) {
        let response = format!(
            "HTTP/1.1 {status}\r\nContent-Type: application/json\r\nContent-Length: {}\r\n\r\n{body}",
            body.len()
        );
        use std::io::{Read, Write};
        let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("addr").port();
        let handle = std::thread::spawn(move || {
            let (mut socket, _) = listener.accept().expect("accept");
            let mut buffer = [0u8; 4096];
            let read = socket.read(&mut buffer).expect("read");
            socket.write_all(response.as_bytes()).expect("write");
            socket.flush().ok();
            String::from_utf8_lossy(&buffer[..read]).to_string()
        });
        (format!("http://127.0.0.1:{port}"), handle)
    }

    #[tokio::test]
    async fn a_request_carries_the_bearer_token_and_nothing_else() {
        let (url, server) = stub("200 OK", "{\"ok\":true}");
        let b = backend(&url);
        let client = build(&b).unwrap();
        let response = request(
            &client,
            reqwest::Method::GET,
            &url_for(&b, "api", "v1/overview").unwrap(),
            "s3cret",
            None,
        )
        .await
        .unwrap();
        assert_eq!(response.status, 200);
        assert_eq!(response.body["ok"], true);

        let sent = server.join().expect("server");
        assert!(sent.starts_with("GET /api/v1/overview HTTP/1.1"), "{sent}");
        assert!(sent.contains("authorization: Bearer s3cret"), "{sent}");
        // No cookie is ever sent: the bearer half of the scheme is the whole of what this uses.
        assert!(!sent.to_lowercase().contains("cookie"), "{sent}");
    }

    #[tokio::test]
    async fn a_write_is_json_because_the_server_requires_it() {
        let (url, server) = stub("202 Accepted", "{\"seq\":4}");
        let b = backend(&url);
        let client = build(&b).unwrap();
        let response = request(
            &client,
            reqwest::Method::POST,
            &url_for(&b, "api", "v1/interactive").unwrap(),
            "s3cret",
            Some(serde_json::json!({ "text": "hello" })),
        )
        .await
        .unwrap();
        assert_eq!(response.status, 202);

        let sent = server.join().expect("server");
        assert!(sent.contains("content-type: application/json"), "{sent}");
        assert!(sent.contains("{\"text\":\"hello\"}"), "{sent}");
    }

    #[tokio::test]
    async fn a_401_is_raised_as_unauthorized_rather_than_returned() {
        let (url, server) = stub("401 Unauthorized", "{\"error\":\"unauthorized\"}");
        let b = backend(&url);
        let client = build(&b).unwrap();
        let error = request(
            &client,
            reqwest::Method::GET,
            &url_for(&b, "api", "session").unwrap(),
            "wrong",
            None,
        )
        .await
        .unwrap_err();
        assert!(matches!(error, Error::Unauthorized));
        let _ = server.join();
    }

    #[tokio::test]
    async fn a_failure_carries_the_servers_own_words() {
        let (url, server) = stub("409 Conflict", "{\"error\":\"that task is not running\"}");
        let b = backend(&url);
        let client = build(&b).unwrap();
        let error = request(
            &client,
            reqwest::Method::POST,
            &url_for(&b, "api", "v1/queue/t1/cancel").unwrap(),
            "s3cret",
            Some(serde_json::json!({})),
        )
        .await
        .unwrap_err();
        match error {
            Error::Http { status, message } => {
                assert_eq!(status, 409);
                assert_eq!(message, "that task is not running");
            }
            other => panic!("expected an HTTP error, got {other:?}"),
        }
        let _ = server.join();
    }

    #[tokio::test]
    async fn a_probe_tells_a_wrong_password_from_a_dead_host() {
        // Nothing listening: the port is bound and dropped, so the connection is refused.
        let dead = {
            let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
            let port = listener.local_addr().expect("addr").port();
            drop(listener);
            format!("http://127.0.0.1:{port}")
        };
        let refused = probe(&backend(&dead), "s3cret").await;
        assert!(!refused.ok);
        assert_eq!(refused.outcome, "unreachable");

        let (url, server) = stub("401 Unauthorized", "{\"error\":\"unauthorized\"}");
        let rejected = probe(&backend(&url), "wrong").await;
        assert!(!rejected.ok);
        assert_eq!(rejected.outcome, "rejected");
        let _ = server.join();
    }

    #[test]
    fn insecure_tls_is_ignored_for_a_public_host() {
        let mut b = backend("https://orchestra.example.com");
        b.allow_insecure_tls = true;
        // Nothing to assert on the built client directly; what matters is that the decision is
        // taken from `is_private_host` and not from the flag alone.
        assert!(!crate::registry::is_private_host(&b.url));
        assert!(build(&b).is_ok());
    }
}
