const fs = require("fs");

const elmDts = fs.readFileSync("src/elm.d.ts", { encoding: "utf-8" });

const fixedElmDts = elmDts
  .split("\n")
  .filter((line) => !(line.startsWith("declare") || line.startsWith("}")))
  .map((line) => line.slice(2, line.length))
  .join("\n");

fs.writeFileSync("lib/elm.min.d.ts", fixedElmDts);
