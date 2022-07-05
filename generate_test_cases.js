const { writeFile, mkdir, readdir } = require("fs/promises");
const elmCompiler = require("node-elm-compiler");
const { resolve, dirname, parse } = require("path");

const testCaseDir = resolve(__dirname, "gen_test_cases");

const generate = async (pathToTestCase) => {
  const testCaseName = parse(pathToTestCase).name;
  const name = testCaseName.replace("Case", "");
  const worker = await elmCompiler.compileWorker(
    __dirname,
    pathToTestCase,
    testCaseName,
    { flags: { name } }
  );
  worker.ports.sendFile.subscribe(async ({ path, content }) => {
    const targetPath = resolve(__dirname, path);
    await mkdir(dirname(targetPath), { recursive: true });
    await writeFile(resolve(__dirname, path), content);
    worker.ports.sendFile.unsubscribe();
  });
};

const generateTestCases = async () => {
  const fileNames = await readdir(testCaseDir);
  fileNames
    .filter((fileName) => fileName.endsWith("Case.elm"))
    .map((fileName) => generate(resolve(testCaseDir, fileName)));
};

generateTestCases();
