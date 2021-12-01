const assert = require("assert");
const ElmCompiler = require("node-elm-compiler");
const path = require("path");
const http = require("http");
const finalhandler = require("finalhandler");
const serveStatic = require("serve-static");

["inline", "dynamic"].forEach((scenario) => {
  describe(`integration-test: ${scenario}`, () => {
    let server;
    let cleanDom;

    before(async () => {
      if (scenario === "dynamic") {
        server = require("http-shutdown")(
          http.createServer(function onRequest(req, res) {
            serveStatic("./example/dist")(req, res, finalhandler(req, res));
          })
        );
        await new Promise((res) => server.listen(9000, res));
      }
      cleanDom = require("jsdom-global")("", {
        url: "http://localhost:9000",
      });
      process.chdir(path.join(__dirname, "..", "example", scenario));
      const elmPath = `../dist/${scenario}.js`;
      ElmCompiler.compileSync("src/Main.elm", {
        output: elmPath,
        optimize: true,
      });
      const { Elm } = require(path.join(process.cwd(), elmPath));
      process.chdir(path.join(__dirname, ".."));
      Elm.Main.init({ flags: "en" });
      await waitMs(100);
    });

    after((done) => {
      document.body.innerHTML = "";
      cleanDom();
      if (server) {
        server.shutdown(done);
      } else {
        done();
      }
    });

    it("displays the expected info text", () => {
      assert.equal(
        getInfoText(),
        "You may switch languages from en to another one here."
      );
    });

    it("displays the expected greeting", () => {
      changeName("TestName");
      assert.equal(getGreeting(), "Hello TestName");
    });

    it("displays the expected order text", () => {
      changeName("TestName");

      assert.equal(
        getOrderText(),
        "The order of the named placeholder keys stays consistent even when switching languages! Language: en, Name: TestName."
      );
    });

    it("works after switching languages", async () => {
      switchLanguage("de");
      await waitMs(100);
      assert.equal(
        getInfoText(),
        "Du kannst hier deine Sprache von de zu einer anderen ändern."
      );
      changeName("Welt");
      assert.equal(getGreeting(), "Hallo Welt");
      assert.equal(
        getOrderText(),
        "Die Reihenfolge der benannten Platzhalter bleibt konsistent auch wenn die Sprachen sich ändern! Name: Welt, Sprache: de"
      );
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
