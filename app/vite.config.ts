import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

/**
 * The front-end is bundled into the app and served from `tauri://`; it is never hosted.
 * There is no dev proxy here and there must not be one — the app talks to a backend through
 * the Rust core (see `src/core/transport.ts`), so a proxy would only give the webview a second,
 * unauthenticated way to reach one.
 */
export default defineConfig({
  plugins: [react()],
  // Tauri picks the port up from `tauri.conf.json`; both sides have to agree.
  server: { port: 1420, strictPort: true },
  // `tauri dev` reads this so a build failure surfaces as one.
  clearScreen: false,
  envPrefix: ["VITE_", "TAURI_"],
  build: {
    outDir: "dist",
    emptyOutDir: true,
    assetsDir: "assets",
    // The system webviews the app runs in: Safari on macOS/iOS, WebView2 on Windows,
    // webkit2gtk on Linux. The oldest of those sets the floor.
    target: ["es2021", "safari14"],
    sourcemap: true,
  },
});
