import fs from "fs";
import path from "path";
import { promisify } from "util";
import { Elm, Ports, ResponseContent, ResponseHandler } from "./elm.min.js";

const readFile = (filePath: string) =>
  promisify(fs.readFile)(filePath, { encoding: "utf-8" });

const readDir = promisify(fs.readdir);

const writeFile = async (filePath: string, data: string) => {
  await promisify(fs.mkdir)(path.parse(filePath).dir, { recursive: true });
  await promisify(fs.writeFile)(filePath, data, { encoding: "utf-8" });
};

const getPluginVersion = (): string => {
  const file = path.resolve(__dirname, "..", "package.json");
  const nodeJson = JSON.parse(fs.readFileSync(file, { encoding: "utf-8" }));
  return nodeJson.version;
};

let ports: Ports | undefined;

const withElmApp = <T>(consumer: (ports: Ports) => T): T => {
  if (!ports) {
    const version = getPluginVersion();
    ({ ports } = Elm.Main.init({ flags: { version } }));
    const throwOnError: ResponseHandler = (response) => {
      if (response.error) {
        throw new Error(response.error);
      }
    };
    ports.sendResponse.subscribe(throwOnError);
  }
  return consumer(ports);
};

interface Options {
  jsonPath: string;
  elmPath: string;
  translationDir: string;
}

const sendTranslations = (translationDir: string): Promise<string[]> =>
  withElmApp(async (ports) => {
    const translationFileNames = await readDir(translationDir);
    await Promise.all(
      translationFileNames.map(async (fileName) => {
        const fileContent = await readFile(path.join(translationDir, fileName));
        ports.receiveRequest.send({
          type: "translation",
          fileName,
          fileContent,
        });
      })
    );
    return translationFileNames;
  });

const finishModule = ({
  elmModuleName,
  identifier,
}: {
  elmModuleName: string;
  identifier: string;
}): Promise<ResponseContent> =>
  withElmApp(
    async (ports) =>
      new Promise<ResponseContent>((resolve, reject) => {
        const responseHandler: ResponseHandler = async (res) => {
          ports.sendResponse.unsubscribe(responseHandler);
          if (res.error) {
            reject(res.error);
          }
          if (!res.content) {
            reject(new Error("Received neither error nor content from Elm."));
          } else {
            resolve(res.content);
          }
        };
        ports.sendResponse.subscribe(responseHandler);
        ports.receiveRequest.send({
          type: "finish",
          elmModuleName,
          identifier,
        });
      })
  );

export const run = async ({ elmPath, translationDir, jsonPath }: Options) => {
  const elmModuleName = await elmPathToModuleName(elmPath);
  const [firstTranslationFileName] = await sendTranslations(translationDir);
  if (!firstTranslationFileName) {
    throw new Error("Given translation directory does not contain any files");
  }
  const [identifier] = firstTranslationFileName.split(".");
  const { elmFile, optimizedJson } = await finishModule({
    elmModuleName,
    identifier,
  });
  const elmPromise = writeFile(elmPath, elmFile);
  const jsonPromises = optimizedJson.map(([language, content]) =>
    writeFile(path.join(jsonPath, `${identifier}.${language}.json`), content)
  );
  await Promise.all([elmPromise, ...jsonPromises]);
};

let elmConfig: { "source-directories": string[] } | undefined;

const elmPathToModuleName = async (elmPath: string): Promise<string> => {
  if (!elmConfig) {
    elmConfig = await lookForElmJson();
  }
  const possibleSourceDirs = elmConfig!["source-directories"].filter(
    (sourceDir) => elmPath.startsWith(sourceDir)
  );
  if (possibleSourceDirs.length == 0) {
    throw new Error("Could not determine elm module name");
  }
  if (possibleSourceDirs.length > 1) {
    throw new Error(
      "Multiple matching source directories: " + possibleSourceDirs.join(",")
    );
  }
  return elmPath
    .replace(".elm", "")
    .replace(possibleSourceDirs[0], "")
    .split("/")
    .filter((s) => s)
    .join(".");
};

const lookForElmJson = () =>
  readFile(path.join(process.cwd(), "elm.json")).then(JSON.parse);
