module ContentTypes.Json exposing (parse)

import Json.Decode as D
import Parser exposing ((|.), (|=))
import Placeholder.Internal as Placeholder exposing (Template)
import Result.Extra
import Types exposing (I18nPairs)
import Util


type Json
    = Object (List ( String, Json ))
    | StringValue String


type alias FlattenedJson =
    List ( List String, String )


parse : String -> D.Decoder I18nPairs
parse =
    let
        addContext : ( c, Result x a ) -> Result ( c, x ) ( c, a )
        addContext ( c, rx ) =
            Result.map (Tuple.pair c) rx
                |> Result.mapError (Tuple.pair c)

        resultsToDecoder : List ( List String, Result String Template ) -> D.Decoder I18nPairs
        resultsToDecoder =
            List.map addContext
                >> Result.Extra.partition
                >> (\( parsedJson, errors ) ->
                        case errors of
                            [] ->
                                D.succeed <| List.map (Tuple.mapFirst <| Util.keyToName) parsedJson

                            _ ->
                                List.map (\( key, err ) -> String.join "." key ++ ": " ++ err) errors
                                    |> String.join "\n"
                                    |> D.fail
                   )
    in
    jsonDecoder
        >> D.map flattenJson
        >> D.andThen (resultsToDecoder << List.map (Tuple.mapSecond <| Placeholder.parseTemplate { startSymbol = "{{", endSymbol = "}}" }))
        >> D.map (List.sortBy Tuple.first)


jsonDecoder : String -> D.Decoder Json
jsonDecoder str =
    case D.decodeString decoder str of
        Ok json ->
            D.succeed json

        Err err ->
            D.fail <| D.errorToString err


flattenJson : Json -> FlattenedJson
flattenJson json =
    case json of
        StringValue _ ->
            []

        Object obj ->
            flattenJsonHelper obj


flattenJsonHelper : List ( String, Json ) -> FlattenedJson
flattenJsonHelper =
    List.concatMap <|
        \( key, value ) ->
            case value of
                StringValue str ->
                    [ ( [ key ], str ) ]

                Object innerObj ->
                    List.map (Tuple.mapFirst <| (::) key) <|
                        flattenJsonHelper innerObj


decoder : D.Decoder Json
decoder =
    let
        objectDecoder =
            D.keyValuePairs (D.lazy <| \_ -> decoder)
                |> D.map Object

        stringDecoder =
            D.string |> D.map StringValue
    in
    D.oneOf
        [ objectDecoder
        , stringDecoder
        ]
