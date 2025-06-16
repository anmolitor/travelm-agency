import fs from "fs";
import { minify } from "uglify-js";
import ElmCompiler from "node-elm-compiler";

fs.mkdirSync("lib", { recursive: true });

const elmDts = fs.readFileSync("src/elm.d.ts", { encoding: "utf-8" });

const fixedElmDts = elmDts
  .split("\n")
  .filter((line) => !(line.startsWith("declare") || line.startsWith("}")))
  .map((line) => line.slice(2, line.length))
  .join("\n");

fs.writeFileSync("lib/elm.min.d.ts", fixedElmDts);

const isRelease = process.env.NODE_ENV === "production";

let elmCode = ElmCompiler.compileToStringSync("src/Main.elm", {
  optimize: isRelease,
});
elmCode = toESModule(elmCode);

if (isRelease) {
  const minifiedResult = minify(elmCode, {
    compress: {
      pure_funcs: "F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",
      pure_getters: true,
      keep_fargs: false,
      unsafe_comps: true,
      unsafe: true,
    },
    mangle: true,
  });
  if (minifiedResult.error) {
    throw minifiedResult.error;
  }
  elmCode = minifiedResult.code;
}

fs.writeFileSync("lib/elm.min.js", elmCode);

function toESModule(js) {
  const elmExports = js.match(
    /^\s*_Platform_export\(([^]*)\);\n?}\(this\)\);/m
  )[1];
  return js
    .replace(/\(function\s*\(scope\)\s*\{$/m, "// -- $&")
    .replace(/['"]use strict['"];$/m, "// -- $&")
    .replace(/function _Platform_export([^]*?)\}\n/g, "/*\n$&\n*/")
    .replace(/function _Platform_mergeExports([^]*?)\}\n\s*}/g, "/*\n$&\n*/")
    .replace(/^\s*_Platform_export\(([^]*)\);\n?}\(this\)\);/m, "/*\n$&\n*/")
    .concat(`
export const Elm = ${elmExports};
  `);
}
