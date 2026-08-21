/// `has_keychain` is on exactly where `Cargo.toml` declares the `keyring` dependency — the
/// three desktop platforms. Deriving it here rather than repeating the `target_os` list in
/// `secrets.rs` keeps the two from drifting apart: a platform added to one is added to both.
fn main() {
    println!("cargo::rustc-check-cfg=cfg(has_keychain)");
    let os = std::env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    if matches!(os.as_str(), "macos" | "windows" | "linux") {
        println!("cargo::rustc-cfg=has_keychain");
    }
    tauri_build::build()
}
