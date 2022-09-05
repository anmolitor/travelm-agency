import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";
import { name, version } from "../package.json";

export default defineConfig({
  base: `/${name}/`,
  define: {
    __VERSION__: JSON.stringify(version),
    __BASE_PATH__: JSON.stringify(name),
  },
  plugins: [elmPlugin({ optimize: process.env.NODE_ENV === "production" })],
});
