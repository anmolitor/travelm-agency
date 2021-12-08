module ContentTypes.Json exposing (Json(..), jsonToInternalRep, parseJson)

import Json.Decode as D
import List.NonEmpty
import Parser as P exposing ((|.), (|=), Parser)
import Parser.DeadEnds
import Result.Extra
import Types
import Util


type Json
    = Object (List ( String, Json ))
    | StringValue String


type alias FlattenedJson =
    List ( List String, String )


jsonToInternalRep : Json -> Result String Types.Translations
jsonToInternalRep =
    flattenJson
        >> Result.Extra.combineMap
            (\( k, v ) ->
                case P.run parsePlaceholderString v of
                    Ok tValue ->
                        Ok ( Util.keyToName k, tValue )

                    Err err ->
                        Err <| Parser.DeadEnds.deadEndsToString err
            )


parsePlaceholderString : Parser Types.TValue
parsePlaceholderString =
    let
        untilEndOrNextPlaceholder =
            P.chompUntilEndOr "{" |> P.getChompedString
    in
    P.loop []
        (\revSegments ->
            untilEndOrNextPlaceholder
                |> P.andThen
                    (\text ->
                        P.oneOf
                            [ P.succeed
                                (P.Done <|
                                    List.reverse <|
                                        if String.isEmpty text then
                                            revSegments

                                        else
                                            Types.Text text :: revSegments
                                )
                                |. P.end
                            , P.succeed
                                (\var ->
                                    P.Loop <|
                                        Types.Interpolation var
                                            :: (if String.isEmpty text then
                                                    revSegments

                                                else
                                                    Types.Text text :: revSegments
                                               )
                                )
                                |. P.token "{"
                                |= (P.chompUntil "}" |> P.getChompedString)
                                |. P.token "}"
                            ]
                    )
        )
        |> P.andThen
            (\segments ->
                case List.NonEmpty.fromList segments of
                    Just nonEmpty ->
                        P.succeed nonEmpty

                    Nothing ->
                        P.problem "Encountered empty json string value. This should never happen."
            )


parseJson : String -> Result String Json
parseJson =
    D.decodeString decoder >> Result.mapError D.errorToString


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
