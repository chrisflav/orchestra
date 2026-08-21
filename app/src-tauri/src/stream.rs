//! The live half: a Server-Sent Events reader per subscription.
//!
//! Every read in the dashboard API is also a stream at the same path under `/sse/v1/`, and
//! liveness is most of what this app is for. A browser would use `EventSource` — which takes no
//! headers, and so cannot carry a bearer token — so the stream is read here instead, over an
//! ordinary authenticated request, and each frame is emitted to the front-end as a Tauri event.
//! The token never leaves this process and never lands in a URL.
//!
//! Every reader emits on the *same* event name and puts its stream id in the payload. One name
//! rather than one per stream is what lets the front-end attach its listener once, before any
//! stream is started — with a name per stream there is a window between `stream_start`
//! answering with an id and the listener being attached, and a frame that lands in it is gone.
//!
//! A stream survives its connection. When one drops, the reader backs off and re-opens, and for
//! the transcript it re-opens *at its cursor*: the `/events` stream takes `?after=<seq>` for
//! exactly this, so a reconnect resumes rather than replays. The front-end is told when the
//! connection comes and goes and shows it as a live/stale dot, but is not asked to do anything
//! about it.

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use futures_util::StreamExt;
use serde::Serialize;
use tauri::async_runtime;
use tauri::{AppHandle, Emitter};

use crate::client;
use crate::error::{Error, Result};
use crate::registry::Backend;

/// How long to wait before re-opening a dropped stream, and the ceiling that backoff climbs to.
const RETRY_FLOOR: Duration = Duration::from_secs(1);
const RETRY_CEILING: Duration = Duration::from_secs(20);

/// The event every frame is emitted on. See the module comment for why there is only one.
const EVENT: &str = "stream-frame";

/// One frame handed up to the front-end.
///
/// `backend` rides on every frame so a view can drop anything that is not from the backend it
/// is showing. Switching tears every stream down first, but a frame already in flight when the
/// switch happened would otherwise land in the wrong screen.
#[derive(Clone, Serialize)]
#[serde(rename_all = "camelCase")]
struct Frame {
    stream: String,
    backend: String,
    /// `open` — connected; `data` — a payload; `closed` — the connection dropped, a retry is
    /// scheduled; `failed` — the stream gave up and will not retry.
    kind: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    data: Option<serde_json::Value>,
    /// The `id:` the frame carried, where it had one. The transcript stream sets it to the
    /// last seq in the frame; the others do not set it at all.
    #[serde(skip_serializing_if = "Option::is_none")]
    cursor: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    message: Option<String>,
}

/// Every running reader, by stream id.
#[derive(Default)]
pub struct Streams {
    inner: Mutex<HashMap<String, Handle>>,
}

struct Handle {
    backend: String,
    stop: Arc<AtomicBool>,
    task: async_runtime::JoinHandle<()>,
}

impl Streams {
    /// Open a stream and answer the id the front-end will listen on.
    ///
    /// `path` is the SSE path without a cursor (`v1/queue`, `v1/interactive/<id>/events`); the
    /// cursor is a parameter here so that a reconnect can move it without the front-end being
    /// involved.
    pub fn start(
        &self,
        app: AppHandle,
        backend: Backend,
        secret: String,
        path: String,
        cursor: Option<u64>,
    ) -> Result<String> {
        // Fail before spawning if the path is not one we would send.
        let _ = client::url_for(&backend, "sse", &path)?;
        let id = crate::registry::new_id();
        let stop = Arc::new(AtomicBool::new(false));
        // Tauri's runtime rather than `tokio::spawn`: this is called from a synchronous
        // command, which runs on the main thread, where there is no Tokio runtime in context
        // and `tokio::spawn` panics. `async_runtime` is the handle Tauri owns and is reachable
        // from anywhere.
        let task = async_runtime::spawn(read(
            app,
            id.clone(),
            backend.clone(),
            secret,
            path,
            cursor,
            stop.clone(),
        ));
        self.inner.lock().expect("stream lock").insert(
            id.clone(),
            Handle {
                backend: backend.id,
                stop,
                task,
            },
        );
        Ok(id)
    }

    /// Stop one stream. Unknown ids are ignored: a view unmounting twice is not an error.
    pub fn stop(&self, id: &str) {
        if let Some(handle) = self.inner.lock().expect("stream lock").remove(id) {
            handle.stop.store(true, Ordering::Relaxed);
            handle.task.abort();
        }
    }

    /// Stop every stream against one backend. This is what a switch calls, and what removing a
    /// backend calls: no frame from the backend you left can arrive in the one you moved to.
    pub fn stop_backend(&self, backend_id: &str) {
        let mut map = self.inner.lock().expect("stream lock");
        let ids: Vec<String> = map
            .iter()
            .filter(|(_, h)| h.backend == backend_id)
            .map(|(id, _)| id.clone())
            .collect();
        for id in ids {
            if let Some(handle) = map.remove(&id) {
                handle.stop.store(true, Ordering::Relaxed);
                handle.task.abort();
            }
        }
    }
}

/// The reader loop: connect, read frames, and on a drop back off and connect again.
#[allow(clippy::too_many_arguments)]
async fn read(
    app: AppHandle,
    id: String,
    backend: Backend,
    secret: String,
    path: String,
    mut cursor: Option<u64>,
    stop: Arc<AtomicBool>,
) {
    let client = match client::build_streaming(&backend) {
        Ok(c) => c,
        Err(e) => {
            emit(
                &app,
                frame(&id, &backend, "failed", None, None, Some(e.to_string())),
            );
            return;
        }
    };
    let mut delay = RETRY_FLOOR;

    while !stop.load(Ordering::Relaxed) {
        match connect(
            &app,
            &client,
            &backend,
            &secret,
            &path,
            &id,
            &mut cursor,
            &stop,
        )
        .await
        {
            // A stream that ends cleanly is still a stream that ended: the daemon holds it open
            // indefinitely, so reaching the end means the connection went, not that there is
            // nothing more to say. Retry either way; only the message differs.
            Ok(()) => {
                emit(&app, frame(&id, &backend, "closed", None, cursor, None));
                delay = RETRY_FLOOR;
            }
            Err(Error::Unauthorized) => {
                // The credential is wrong. Retrying would spin against a 401 forever, so this
                // is the one failure the reader gives up on — the front-end puts the backend
                // into its needs-credential state and the user fixes it.
                emit(
                    &app,
                    frame(
                        &id,
                        &backend,
                        "failed",
                        None,
                        cursor,
                        Some("the backend rejected the credential".into()),
                    ),
                );
                return;
            }
            Err(e) => {
                emit(
                    &app,
                    frame(&id, &backend, "closed", None, cursor, Some(e.to_string())),
                );
            }
        }
        if stop.load(Ordering::Relaxed) {
            return;
        }
        tokio::time::sleep(delay).await;
        delay = (delay * 2).min(RETRY_CEILING);
    }
}

/// One connection's worth of reading. Returns when the body ends or the connection fails.
#[allow(clippy::too_many_arguments)]
async fn connect(
    app: &AppHandle,
    client: &reqwest::Client,
    backend: &Backend,
    secret: &str,
    path: &str,
    id: &str,
    cursor: &mut Option<u64>,
    stop: &Arc<AtomicBool>,
) -> Result<()> {
    let url = with_cursor(&client::url_for(backend, "sse", path)?, *cursor);
    let response = client
        .get(&url)
        .bearer_auth(secret)
        .header(reqwest::header::ACCEPT, "text/event-stream")
        // A proxy that buffers turns a live stream into a stalled one. The daemon does not
        // buffer; this is for whatever sits between.
        .header("Cache-Control", "no-cache")
        .send()
        .await?;

    let status = response.status();
    if status == reqwest::StatusCode::UNAUTHORIZED {
        return Err(Error::Unauthorized);
    }
    if !status.is_success() {
        return Err(Error::Http {
            status: status.as_u16(),
            message: format!("the stream answered {}", status.as_u16()),
        });
    }
    emit(app, frame(id, backend, "open", None, *cursor, None));

    let mut body = response.bytes_stream();
    // Frames are separated by a blank line and can arrive split across any number of chunks,
    // so the buffer is what makes this correct rather than merely usually correct.
    let mut buffer = String::new();
    while let Some(chunk) = body.next().await {
        if stop.load(Ordering::Relaxed) {
            return Ok(());
        }
        let chunk = chunk?;
        // Lossy on purpose: a multi-byte character split across two chunks would otherwise
        // fail the whole stream. The daemon frames whole JSON documents, so a replacement
        // character can only appear inside a payload that is already being re-sent.
        buffer.push_str(&String::from_utf8_lossy(&chunk));
        while let Some(split) = frame_end(&buffer) {
            let raw = buffer[..split.0].to_string();
            buffer.drain(..split.1);
            if let Some((data, seq)) = parse(&raw) {
                if let Some(seq) = seq {
                    *cursor = Some(seq);
                }
                emit(app, frame(id, backend, "data", Some(data), *cursor, None));
            }
        }
        // A stream that only ever grows its buffer is a stream that is not being framed. Drop
        // it rather than grow without bound; the retry re-opens at the cursor.
        if buffer.len() > 8 * 1024 * 1024 {
            return Err(Error::Unreachable(
                "the stream sent 8 MB without a frame boundary".into(),
            ));
        }
    }
    Ok(())
}

/// Where the first frame ends: the offset of its last byte, and the offset to resume at.
/// Both `\n\n` and `\r\n\r\n` separate frames; a server is free to use either.
fn frame_end(buffer: &str) -> Option<(usize, usize)> {
    let lf = buffer.find("\n\n").map(|i| (i, i + 2));
    let crlf = buffer.find("\r\n\r\n").map(|i| (i, i + 4));
    match (lf, crlf) {
        (Some(a), Some(b)) => Some(if a.0 <= b.0 { a } else { b }),
        (Some(a), None) => Some(a),
        (None, Some(b)) => Some(b),
        (None, None) => None,
    }
}

/// Parse one frame into its payload and its `id:`.
///
/// Only `data:` and `id:` are read. A comment line (`:`) is a keep-alive and carries nothing;
/// `event:` is not used by this API, where the payload's own shape says what it is. A frame
/// whose data is not JSON is dropped rather than raised: the next frame supersedes it, which
/// is what the dashboard's own reader does.
fn parse(raw: &str) -> Option<(serde_json::Value, Option<u64>)> {
    let mut data = String::new();
    let mut seq = None;
    for line in raw.lines() {
        if let Some(rest) = line.strip_prefix("data:") {
            if !data.is_empty() {
                data.push('\n');
            }
            data.push_str(rest.strip_prefix(' ').unwrap_or(rest));
        } else if let Some(rest) = line.strip_prefix("id:") {
            seq = rest.trim().parse::<u64>().ok();
        }
    }
    if data.is_empty() {
        return None;
    }
    serde_json::from_str(&data).ok().map(|v| (v, seq))
}

/// Put the cursor on the URL, replacing one that is already there.
///
/// The front-end passes a path without `after`, and a reconnect has a cursor the first connect
/// did not — so this is the one place the parameter is written, and it must not be able to
/// accumulate across retries.
fn with_cursor(url: &str, cursor: Option<u64>) -> String {
    let Some(cursor) = cursor else {
        return url.to_string();
    };
    let (base, query) = match url.split_once('?') {
        Some((base, query)) => (base, query),
        None => (url, ""),
    };
    let mut parts: Vec<String> = query
        .split('&')
        .filter(|p| !p.is_empty() && !p.starts_with("after="))
        .map(|p| p.to_string())
        .collect();
    parts.push(format!("after={cursor}"));
    format!("{base}?{}", parts.join("&"))
}

fn frame(
    id: &str,
    backend: &Backend,
    kind: &'static str,
    data: Option<serde_json::Value>,
    cursor: Option<u64>,
    message: Option<String>,
) -> Frame {
    Frame {
        stream: id.to_string(),
        backend: backend.id.clone(),
        kind,
        data,
        cursor,
        message,
    }
}

/// A failure to emit means the window is gone, which the reader finds out about through its
/// stop flag a moment later. There is nothing useful to do with it here.
fn emit(app: &AppHandle, payload: Frame) {
    let _ = app.emit(EVENT, payload);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_frame_is_parsed_from_its_data_lines() {
        let (value, seq) = parse("data: {\"a\":1}").unwrap();
        assert_eq!(value["a"], 1);
        assert_eq!(seq, None);
    }

    #[test]
    fn an_id_becomes_the_cursor() {
        let (_, seq) = parse("id: 42\ndata: {\"a\":1}").unwrap();
        assert_eq!(seq, Some(42));
    }

    #[test]
    fn multi_line_data_is_joined_with_newlines() {
        let (value, _) = parse("data: {\"a\":\ndata: 1}").unwrap();
        assert_eq!(value["a"], 1);
    }

    #[test]
    fn a_keep_alive_comment_carries_nothing() {
        assert!(parse(": keep-alive").is_none());
    }

    #[test]
    fn frames_split_on_either_separator() {
        assert_eq!(frame_end("data: 1\n\ndata: 2"), Some((7, 9)));
        assert_eq!(frame_end("data: 1\r\n\r\ndata: 2"), Some((7, 11)));
        assert_eq!(frame_end("data: 1\n"), None);
    }

    #[test]
    fn the_cursor_replaces_rather_than_accumulates() {
        assert_eq!(
            with_cursor("http://h/sse/v1/x", Some(3)),
            "http://h/sse/v1/x?after=3"
        );
        assert_eq!(
            with_cursor("http://h/sse/v1/x?after=3", Some(9)),
            "http://h/sse/v1/x?after=9"
        );
        assert_eq!(
            with_cursor("http://h/sse/v1/x?limit=5&after=3", Some(9)),
            "http://h/sse/v1/x?limit=5&after=9"
        );
        assert_eq!(with_cursor("http://h/sse/v1/x", None), "http://h/sse/v1/x");
    }
}
