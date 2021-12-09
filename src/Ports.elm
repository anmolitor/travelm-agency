port module Ports exposing (FinishRequest, GeneratorMode(..), Request(..), ResponseContent, TranslationRequest, respond, subToRequests, requestDecoder)

import ContentTypes.Fluent
import ContentTypes.Json
import ContentTypes.Properties
import Json.Decode as D
import Json.Decode.Pipeline as D
import State exposing (OptimizedJson)
import Types
import Util


port sendResponse : Response -> Cmd msg


type alias Response =
    { error : Maybe String, content : Maybe ResponseContent }


type alias ResponseContent =
    { elmFile : String, optimizedJson : List OptimizedJson }


respond : Result String ResponseContent -> Cmd msg
respond res =
    sendResponse <|
        case res of
            Err err ->
                { error = Just err, content = Nothing }

            Ok ok ->
                { content = Just ok, error = Nothing }


port receiveRequest : (D.Value -> msg) -> Sub msg


type Request
    = FinishModule FinishRequest
    | AddTranslation TranslationRequest


type alias TranslationRequest =
    { content : Types.Translations
    , identifier : String
    , language : String
    }


type GeneratorMode
    = Inline
    | Dynamic


generatorModeDecoder : D.Decoder GeneratorMode
generatorModeDecoder =
    D.string
        |> D.andThen
            (\str ->
                case String.toLower str of
                    "inline" ->
                        D.succeed Inline

                    "dynamic" ->
                        D.succeed Dynamic

                    _ ->
                        D.fail <| "Unsupported generator mode: " ++ str
            )


type alias FinishRequest =
    { elmModuleName : String, generatorMode : GeneratorMode, addContentHash : Bool }


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


contentDecoder : String -> String -> D.Decoder Types.Translations
contentDecoder extension =
    (case extension of
        "json" ->
            ContentTypes.Json.parseJson >> Result.andThen ContentTypes.Json.jsonToInternalRep

        "properties" ->
            ContentTypes.Properties.parseProperties >> Result.andThen ContentTypes.Properties.propertiesToInternalRep

        "ftl" ->
            ContentTypes.Fluent.runFluentParser >> Result.andThen ContentTypes.Fluent.fluentToInternalRep

        _ ->
            always <| Err <| "Unsupported content type '" ++ extension ++ "'"
    )
        >> Result.map (List.sortBy Tuple.first)
        >> Util.resultToDecoder


translationRequestDecoder : D.Decoder TranslationRequest
translationRequestDecoder =
    internalRequestDecoder
        |> D.andThen
            (\{ fileContent, fileName } ->
                case String.split "." fileName of
                    [ identifier, language, extension ] ->
                        D.succeed TranslationRequest
                            |> D.custom (contentDecoder extension fileContent)
                            |> D.hardcoded identifier
                            |> D.hardcoded language

                    [ _, _ ] ->
                        D.fail "Default syntax not supported yet."

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


type alias InternalRequest =
    { fileContent : String
    , fileName : String
    }


internalRequestDecoder : D.Decoder InternalRequest
internalRequestDecoder =
    D.succeed InternalRequest
        |> D.required "fileContent" D.string
        |> D.required "fileName" D.string
