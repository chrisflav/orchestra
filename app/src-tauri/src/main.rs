// The desktop entry point. Mobile targets link the library and call `run()` through
// `tauri::mobile_entry_point`, so both sides start the same way.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

fn main() {
    orchestra_app_lib::run()
}
