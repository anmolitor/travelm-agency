#!/usr/bin/env node
"use strict";

const { run } = require("./dist/main.js");
const { ArgumentParser } = require("argparse");
const { version } = require("./package.json");

const parser = new ArgumentParser({
  description: "Elm I18n",
});

parser.add_argument("-v", "--version", { action: "version", version });
parser.add_argument("--elm_path", {
  required: true,
  help: "Where to put the generated Elm file. There needs to be an elm.json file in some parent folder for this script to work.",
});
parser.add_argument("--json_path", {
  required: true,
  help: "In which directory to place the generated json files.",
});
parser.add_argument("--elm_json", {
  help: "Path to your elm.json file",
  default: "./elm.json",
});
parser.add_argument("translation_directory", {
  help: "Where to find the translation files (.json/.properties)",
});

const args = parser.parse_args();

run({
  translationDir: args.translation_directory,
  elmPath: args.elm_path,
  jsonPath: args.json_path,
  elmJson: args.elm_json,
});

process.on("unhandledRejection", (err) => {
  throw err;
});
