module ContentTypes.Json exposing (Json, NestedJson(..), jsonToInternalRep, parseJson)

import Dict
import Json.Decode as D
import List.NonEmpty
import Parser as P exposing ((|.), (|=), Parser)
import Parser.DeadEnds
import Result.Extra
import State exposing (Translation, fromTranslations)
import Types.Segment as Segment
import Util


type alias Json =
    List ( String, NestedJson )


type NestedJson
    = Object (List ( String, NestedJson ))
    | StringValue String


type alias FlattenedJson =
    List ( List String, String )


jsonToInternalRep : Json -> Result String (Translation ())
jsonToInternalRep =
    flattenJson
        >> Result.Extra.combineMap
            (\( k, v ) ->
                case ( k, P.run parsePlaceholderString v ) of
                    ( [ "--fallback-language" ], _ ) ->
                        Ok { pairs = Dict.empty, resources = (), fallback = Just v }

                    ( _, Ok tValue ) ->
                        Ok { pairs = Dict.singleton (Util.keyToName k) tValue, resources = (), fallback = Nothing }

                    ( _, Err err ) ->
                        Err <| Parser.DeadEnds.deadEndsToString err
            )
        >> Result.map State.foldTranslations


parsePlaceholderString : Parser Segment.TValue
parsePlaceholderString =
    let
        escapeChars =
            [ '\\', '{' ]

        escapeStrings =
            List.map String.fromChar escapeChars

        untilNextSpecialChar =
            P.chompWhile (\c -> not <| List.member c escapeChars) |> P.getChompedString
    in
    P.loop []
        (\revSegments ->
            untilNextSpecialChar
                |> P.andThen
                    (\text ->
                        P.oneOf
                            [ P.succeed
                                (P.Done <|
                                    List.reverse <|
                                        if String.isEmpty text then
                                            revSegments

                                        else
                                            Segment.Text text :: revSegments
                                )
                                |. P.end
                            , P.succeed
                                (\var ->
                                    P.Loop <|
                                        Segment.Interpolation var
                                            :: (if String.isEmpty text then
                                                    revSegments

                                                else
                                                    Segment.Text text :: revSegments
                                               )
                                )
                                |. P.token "{"
                                |= (P.chompUntil "}" |> P.getChompedString)
                                |. P.token "}"
                            , P.succeed (\escapedChar -> P.Loop <| Segment.Text (text ++ escapedChar) :: revSegments)
                                |. P.token "\\"
                                |= P.oneOf
                                    (P.problem "Invalid escaped char"
                                        :: List.map (\c -> P.token c |> P.map (\_ -> c)) escapeStrings
                                    )
                            ]
                    )
        )
        |> P.andThen
            (\segments ->
                case List.NonEmpty.fromList segments of
                    Just nonEmpty ->
                        P.succeed <| Segment.concatenateTextSegments nonEmpty

                    Nothing ->
                        P.succeed <| List.NonEmpty.singleton (Segment.Text "")
            )


parseJson : String -> Result String Json
parseJson =
    D.decodeString decoder >> Result.mapError D.errorToString


flattenJson : Json -> FlattenedJson
flattenJson =
    List.concatMap <|
        \( key, value ) ->
            case value of
                StringValue str ->
                    [ ( [ key ], str ) ]

                Object innerObj ->
                    List.map (Tuple.mapFirst <| (::) key) <|
                        flattenJson innerObj


decoder : D.Decoder Json
decoder =
    D.keyValuePairs
        (D.oneOf
            [ objectDecoder
            , stringDecoder
            ]
        )


nestedDecoder : D.Decoder NestedJson
nestedDecoder =
    D.oneOf
        [ objectDecoder
        , stringDecoder
        ]


objectDecoder : D.Decoder NestedJson
objectDecoder =
    D.keyValuePairs (D.lazy <| \_ -> nestedDecoder)
        |> D.map Object


stringDecoder : D.Decoder NestedJson
stringDecoder =
    D.string |> D.map StringValue
