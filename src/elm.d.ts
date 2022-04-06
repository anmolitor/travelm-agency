declare module "*elm.min.js" {
  export type Request = TranslationRequest | FinishRequest;

  export interface TranslationRequest {
    type: "translation";
    fileName: string;
    fileContent: string;
  }

  export type GeneratorMode = "dynamic" | "inline";

  export interface FinishRequest {
    type: "finish";
    elmModuleName: string;
    generatorMode: GeneratorMode | null;
    addContentHash: boolean | null;
    i18nArgPosition: "first" | "last";
  }

  export interface Response {
    error?: string;
    content?: ResponseContent;
  }

  export interface ResponseContent {
    elmFile: string;
    optimizedJson: { filename: string; content: string }[];
  }

  export type ResponseHandler = (res: Response) => void;

  export interface Ports {
    receiveRequest: {
      send: (req: Request) => void;
    };
    sendResponse: {
      subscribe: (handler: ResponseHandler) => void;
      unsubscribe: (handler: ResponseHandler) => void;
    };
  }

  export const Elm: {
    Main: {
      init(args: { flags: { version: string; intl: {}; devMode: boolean } }): {
        ports: Ports;
      };
    };
  };
}
