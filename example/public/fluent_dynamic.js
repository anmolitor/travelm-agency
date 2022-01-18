import intl_proxy from "intl-proxy";
import { Elm } from "../src/Fluent/Dynamic/Main.elm";
import "./main.css";

Elm.Fluent.Dynamic.Main.init({ flags: { language: "en", intl: intl_proxy } });
