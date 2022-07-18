import { intl_proxy } from "intl-proxy";
import "./main.css";
import { registerCodeComponent } from "./src/code-component";
import { Elm } from "./src/DemoMain.elm";

registerCodeComponent();

Elm.DemoMain.init({
  flags: { language: "en", version: __VERSION__, intl: intl_proxy },
});
