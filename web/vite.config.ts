import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

// The backend to proxy `/api` and `/sse` to during `npm run dev`.
const backend = process.env.ORCHESTRA_DASHBOARD_URL ?? "http://127.0.0.1:8080";

export default defineConfig({
  plugins: [react()],
  build: {
    // Served by `orchestra dashboard --site web/dist`.
    outDir: "dist",
    emptyOutDir: true,
    // Vite fingerprints everything under `assets/`, which is what lets the Lean server cache
    // those immutably while never caching `index.html` (see `staticHead`).
    assetsDir: "assets",
    sourcemap: true,
  },
  server: {
    // Proxying rather than pointing the app at an absolute backend URL keeps the app
    // same-origin in development too. That matters: the session cookie is `SameSite=Strict`,
    // so a cross-origin dev setup would silently fail to authenticate in a way production
    // never would.
    proxy: {
      "/api": { target: backend, changeOrigin: false },
      "/sse": { target: backend, changeOrigin: false, ws: false },
    },
  },
});
