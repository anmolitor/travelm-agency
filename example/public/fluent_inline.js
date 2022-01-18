import intl_proxy from "intl-proxy";
import { Elm } from "../src/Fluent/Inline/Main.elm";
import "./main.css";

Elm.Fluent.Inline.Main.init({ flags: { language: "en", intl: intl_proxy } });
