import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";
import { version } from "../package.json";

export default defineConfig({
  base: "travelm-agency",
  define: {
    __VERSION__: JSON.stringify(version),
  },
  plugins: [elmPlugin({ optimize: process.env.NODE_ENV === "production" })],
});
