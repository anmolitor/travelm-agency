const fs = require("fs");
const { minify } = require("uglify-js");
const ElmCompiler = require("node-elm-compiler");

fs.mkdirSync("lib", { recursive: true });

const elmDts = fs.readFileSync("src/elm.d.ts", { encoding: "utf-8" });

const fixedElmDts = elmDts
  .split("\n")
  .filter((line) => !(line.startsWith("declare") || line.startsWith("}")))
  .map((line) => line.slice(2, line.length))
  .join("\n");

fs.writeFileSync("lib/elm.min.d.ts", fixedElmDts);

const elmCode = ElmCompiler.compileToStringSync("src/Main.elm", {
  // optimize: true,
});

const minifiedElmCode = minify(elmCode, {
  compress: {
    pure_funcs: "F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",
    pure_getters: true,
    keep_fargs: false,
    unsafe_comps: true,
    unsafe: true,
  },
  mangle: true,
});

if (minifiedElmCode.error) {
  throw minifiedElmCode.error;
}

fs.writeFileSync("lib/elm.min.js", minifiedElmCode.code);
