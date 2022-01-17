import intl_proxy from "intl-proxy";
import "./main.css";
import { Elm } from "./src/Dynamic/Main.elm";

Elm.Dynamic.Main.init({ flags: { language: "en", intl: intl_proxy } });
