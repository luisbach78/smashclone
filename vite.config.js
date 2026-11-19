import { defineConfig } from "vite";

// Fable schreibt die kompilierten Module als src/*.fs.js neben die F#-Quellen.
// Vite bündelt sie und lädt den Browser bei jeder Neukompilierung automatisch neu.
export default defineConfig({
  base: "./",
  clearScreen: false,
  server: {
    port: 5173,
    open: true,
    watch: {
      ignored: ["**/*.fs", "**/*.fsproj", "**/bin/**", "**/obj/**"],
    },
  },
  build: {
    outDir: "dist",
    target: "es2020",
    sourcemap: false,
  },
});
