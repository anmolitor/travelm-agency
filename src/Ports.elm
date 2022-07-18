port module Ports exposing (FinishRequest, GeneratorMode(..), Request(..), ResponseContent, TranslationRequest, generatorModeToString, requestDecoder, respond, subToRequests, generatorModeFromString)

import Json.Decode as D
import Json.Decode.Pipeline as D
import State exposing (OptimizedJson)
import Types.Error as Error exposing (Failable)


port sendResponse : Response -> Cmd msg


type alias Response =
    { error : Maybe String, content : Maybe ResponseContent }


type alias ResponseContent =
    { elmFile : String, optimizedJson : List OptimizedJson }


respond : Failable ResponseContent -> Cmd msg
respond res =
    sendResponse <|
        case Error.formatFail res of
            Err err ->
                { error = Just err, content = Nothing }

            Ok ok ->
                { content = Just ok, error = Nothing }


port receiveRequest : (D.Value -> msg) -> Sub msg


type Request
    = FinishModule FinishRequest
    | AddTranslation TranslationRequest


type alias TranslationRequest =
    { content : String
    , extension : String
    , identifier : String
    , language : String
    }


type GeneratorMode
    = Inline
    | Dynamic


generatorModeToString : GeneratorMode -> String
generatorModeToString mode =
    case mode of
        Inline ->
            "inline"

        Dynamic ->
            "dynamic"


generatorModeFromString : String -> Maybe GeneratorMode
generatorModeFromString str =
    case String.toLower str of
        "inline" ->
            Just Inline

        "dynamic" ->
            Just Dynamic

        _ ->
            Nothing


generatorModeDecoder : D.Decoder GeneratorMode
generatorModeDecoder =
    D.string
        |> D.andThen
            (\str ->
                case generatorModeFromString str of
                    Just mode ->
                        D.succeed mode

                    Nothing ->
                        D.fail <| "Unsupported generator mode: " ++ str
            )


type alias FinishRequest =
    { elmModuleName : String, generatorMode : GeneratorMode, addContentHash : Bool, i18nArgLast : Bool }


subToRequests : (Result D.Error Request -> msg) -> Sub msg
subToRequests callback =
    receiveRequest (D.decodeValue requestDecoder >> callback)


requestDecoder : D.Decoder Request
requestDecoder =
    D.field "type" D.string
        |> D.andThen
            (\type_ ->
                case type_ of
                    "translation" ->
                        translationRequestDecoder |> D.map AddTranslation

                    "finish" ->
                        finishRequestDecoder
                            |> D.map FinishModule

                    _ ->
                        D.fail <| "Unknown type of request: '" ++ type_ ++ "'"
            )


translationRequestDecoder : D.Decoder TranslationRequest
translationRequestDecoder =
    internalRequestDecoder
        |> D.andThen
            (\{ fileContent, fileName } ->
                case String.split "." fileName of
                    [ identifier, language, extension ] ->
                        D.succeed TranslationRequest
                            |> D.hardcoded fileContent
                            |> D.hardcoded extension
                            |> D.hardcoded identifier
                            |> D.hardcoded language

                    [ single ] ->
                        D.fail <| "Cannot determine extension from file name '" ++ single ++ "'."

                    _ ->
                        D.fail "Please remove dots from identifier to follow the [identifier].[language].[extension] convention."
            )


finishRequestDecoder : D.Decoder FinishRequest
finishRequestDecoder =
    D.succeed FinishRequest
        |> D.required "elmModuleName" D.string
        |> D.optional "generatorMode" generatorModeDecoder Dynamic
        |> D.optional "addContentHash" D.bool False
        |> D.optional "i18nArgLast" D.bool False


type alias InternalRequest =
    { fileContent : String
    , fileName : String
    }


internalRequestDecoder : D.Decoder InternalRequest
internalRequestDecoder =
    D.succeed InternalRequest
        |> D.required "fileContent" D.string
        |> D.required "fileName" D.string
