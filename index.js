#!/usr/bin/env node
"use strict";

const { run } = require("./lib/main.js");
const yargs = require("yargs");
const { hideBin } = require("yargs/helpers");

(async () => {
  const args = await yargs(hideBin(process.argv))
    .command(
      "$0 <translation_directory>",
      "Generate Elm code for your translation files",
      (builder) =>
        builder
          .option("elm_path", {
            description:
              "Where to put the generated Elm file. There needs to be an elm.json file in some parent folder for this script to work.",
            type: "string",
            default: "src/Translations.elm",
          })
          .option("json_path", {
            description:
              "The directory to generate the optimized translation files into. This will not be used if --inline is specified.",
            type: "string",
            default: "dist/i18n",
          })
          .option("inline", {
            description:
              "Generate an Elm module that contains all of the translations inline (no resource loading necessary at runtime).",
            type: "boolean",
            default: false,
          })
          .option("hash", {
            description:
              "Add content hashes to generated json files. This helps with caching. This will not be used if --inline is specified.",
            type: "boolean",
            default: false,
          })
          .positional("translation_directory", {
            description:
              "The directory containing translation files (.json/.properties).",
            type: "string",
            demandOption: true,
          })
    )
    .parse();

  run({
    translationDir: args.translation_directory,
    elmPath: args.elm_path,
    jsonPath: args.json_path,
    elmJson: args.elm_json,
    generatorMode: args.inline ? "inline" : "dynamic",
    addContentHash: args.hash,
  });
})();

process.on("unhandledRejection", (err) => {
  throw err;
});
