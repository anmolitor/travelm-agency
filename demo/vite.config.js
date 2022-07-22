"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var vite_1 = require("vite");
var vite_plugin_elm_1 = __importDefault(require("vite-plugin-elm"));
var package_json_1 = require("../package.json");
exports.default = (0, vite_1.defineConfig)({
    base: "travelm-agency",
    define: {
        __VERSION__: JSON.stringify(package_json_1.version),
    },
    plugins: [(0, vite_plugin_elm_1.default)({ optimize: process.env.NODE_ENV === "production" })],
});
