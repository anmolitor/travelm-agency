import intl_proxy from "intl-proxy";
import "./main.css";
import { Elm } from "./src/Inline/Main.elm";

Elm.Inline.Main.init({ flags: { language: "en", intl: intl_proxy } });
