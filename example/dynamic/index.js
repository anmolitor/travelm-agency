import intl_proxy from "intl-proxy";
import { Elm } from "./src/Main.elm";
window.p = intl_proxy;
Elm.Main.init({ flags: { language: "en", intl: intl_proxy } });
