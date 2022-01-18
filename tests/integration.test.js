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
        const html = await fs.readFile(
          path.join(__dirname, "..", "example", "dist", `${scenario}.html`),
          { encoding: "utf-8" }
        );

        cleanDom = jsdomGlobal(html, {
          url: "http://localhost:9000",
          runScripts: "dangerously",
          resources: "usable",
        });
        await waitMs(500);
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
});

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

const getGreeting = () => document.querySelector(".greeting").textContent;

const getInfoText = () => document.querySelector(".info_text").textContent;

const getOrderText = () => document.querySelector(".order_text").textContent;
