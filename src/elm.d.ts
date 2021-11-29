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
    identifier: string;
    generatorMode: GeneratorMode | null;
  }

  export interface Response {
    error?: string;
    content?: ResponseContent;
  }

  export interface ResponseContent {
    elmFile: string;
    optimizedJson: [string, string][];
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
      init(args: { flags: { version: string } }): {
        ports: Ports;
      };
    };
  };
}
