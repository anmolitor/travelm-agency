const assert = require("assert");
// const ElmCompiler = require("node-elm-compiler");
const path = require("path");
const http = require("http");
const finalhandler = require("finalhandler");
const serveStatic = require("serve-static");
const fs = require("fs/promises");
const jsdomGlobal = require("jsdom-global");

describe("integration-test", () => {
  let server;
  let global_URL = global.URL;
  before(async () => {
    server = require("http-shutdown")(
      http.createServer(function onRequest(req, res) {
        serveStatic("./example/dist")(req, res, finalhandler(req, res));
      })
    );
    await new Promise((res) => server.listen(9000, res));
  });

  after((done) => {
    server.shutdown(done);
  });

  ["dynamic", "inline"].forEach((scenario) => {
    describe(`integration-test: ${scenario}`, () => {
      let cleanDom;
      before(async () => {
        cleanDom = await startScenario(scenario);
      });

      after(() => {
        document.body.innerHTML = "";
        cleanDom();
        // needed because for some reason this particular part is not restored by JSDOM global.
        global.URL = global_URL;
      });

      it("displays the expected info text", () => {
        assert.equal(
          getInfoText(),
          "You may switch languages from en to another one here."
        );
      });

      it("displays the expected greeting", () => {
        changeName("TestName");
        assert.match(getGreeting(), /Hello TestName/);
      });

      it("displays the expected order text", () => {
        changeName("TestName");

        assert.match(
          getOrderText(),
          /The order of the named placeholder keys stays consistent even when switching languages! Language: en, Name: TestName./
        );
      });

      it("works after switching languages to de", async () => {
        switchLanguage("de");
        await waitMs(100);
        assert.equal(
          getInfoText(),
          "Du kannst hier deine Sprache von de zu einer anderen ändern."
        );
        changeName("Welt");
        assert.match(getGreeting(), /Hallo Welt/);
        assert.match(
          getOrderText(),
          /Die Reihenfolge der benannten Platzhalter bleibt konsistent auch wenn die Sprachen sich ändern! Name: Welt, Sprache: de/
        );
      });

      it("works after switching languages to fr", async () => {
        switchLanguage("fr");
        await waitMs(100);
        assert.equal(
          getInfoText(),
          "Vous pouvez changer votre langue de fr à une autre ici"
        );
        changeName("Monde");
        assert.match(getGreeting(), /Bonjour Monde/);
        assert.match(
          getOrderText(),
          /L'ordre des espaces réservés nommés reste cohérent même si les langues changent! Name: Monde, Langue: fr/
        );
      });
    });
  });

  ["fluent_dynamic", "fluent_inline"].forEach((scenario) => {
    describe(`integration-test: ${scenario}`, () => {
      let cleanDom;
      before(async () => {
        cleanDom = await startScenario(scenario);
      });

      after(() => {
        document.body.innerHTML = "";
        cleanDom();
        // needed because for some reason this particular part is not restored by JSDOM global.
        global.URL = global_URL;
      });

      it("displays the expected static term text", () => {
        assert.match(
          getContentByClass("static_term"),
          /static terms are supported/
        );
      });

      it("displays the expected dynamic term text", () => {
        assert.match(
          getContentByClass("dynamic_term"),
          /dynamic, "interpolated" terms are supported/
        );
      });

      it("displays the expected nested term text", () => {
        assert.match(
          getContentByClass("nested_term"),
          /nested terms are supported/
        );
      });

      it("displays the expected text with attributes", () => {
        assert.match(
          getContentByClass("attribute_example"),
          /Attributes are supported: Attributes | Variable static term/
        );
      });

      it("displays the expected text using datetime", () => {
        assert.match(
          getContentByClass("datetime_example"),
          /DATETIME function is supported: Thursday, January 1, 1970 at 1:00:00 AM GMT\+1/
        );
      });

      it("displays the expected text using number", () => {
        assert.match(
          getContentByClass("number_example"),
          /NUMBER function is supported: 43%/
        );
      });

      it("displays the expected text using number", () => {
        assert.match(
          getContentByClass("compile_time_functions"),
          /DATETIME und NUMBER functions with known values are evaluated at compile time:\n1\/18\/2022\n1\/1\/1970\n500,000/
        );
      });
    });
  });
});

const startScenario = async (scenario) => {
  const html = await fs.readFile(
    path.join(__dirname, "..", "example", "dist", `${scenario}.html`),
    { encoding: "utf-8" }
  );

  const cleanDom = jsdomGlobal(html, {
    url: "http://localhost:9000",
    runScripts: "dangerously",
    resources: "usable",
  });
  await waitMs(500);
  return cleanDom;
};

const waitMs = (ms) => new Promise((res) => setTimeout(res, ms));

const changeName = (newName) => {
  const input = document.querySelector(".name_input");
  input.value = newName;
  input.dispatchEvent(new Event("input"));
};

const switchLanguage = (newLanguage) => {
  const langSelect = document.querySelector(".language_select");
  langSelect.value = newLanguage;
  langSelect.dispatchEvent(new Event("change"));
};

const getContentByClass = (className) =>
  document.querySelector(`.${className}`).textContent;

const getGreeting = () => getContentByClass("greeting");

const getInfoText = () => getContentByClass("info_text");

const getOrderText = () => getContentByClass("order_text");
