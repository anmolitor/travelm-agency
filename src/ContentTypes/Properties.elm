module ContentTypes.Properties exposing (keyValueParser, parsePlaceholderString, parseProperties, propertiesToInternalRep, valueParser)

import List.NonEmpty
import Parser as P exposing ((|.), (|=), Parser)
import Parser.DeadEnds
import Result.Extra
import Types.Segment as Segment
import Util
import State exposing (Translations)


type alias Properties =
    List ( List String, String )


parseProperties : String -> Result String Properties
parseProperties =
    P.run propertiesParser >> Result.mapError Parser.DeadEnds.deadEndsToString


keyParser : Parser (List String)
keyParser =
    P.loop []
        (\kSegments ->
            P.succeed (\kSeg step -> step (kSeg :: kSegments))
                |= (P.getChompedString <| P.chompWhile Char.isAlphaNum)
                |= P.oneOf
                    [ P.succeed (List.reverse >> P.Done) |. P.spaces |. P.symbol "="
                    , P.succeed P.Loop |. P.symbol "."
                    ]
        )


valueParser : Parser String
valueParser =
    P.loop ""
        (\str ->
            P.chompWhile (\c -> c /= '\n' && c /= '\\')
                |> P.getChompedString
                |> P.andThen
                    (\line ->
                        if String.isEmpty line then
                            P.succeed <| P.Done str

                        else
                            P.oneOf
                                [ P.succeed (P.Loop <| str ++ String.trimLeft line)
                                    |. P.token "\\"
                                    |. P.token "\n"
                                , P.succeed (P.Done <| str ++ String.trimLeft line)
                                    |. P.oneOf [ P.end, P.token "\n" ]
                                ]
                    )
        )


parsePlaceholderString : Parser Segment.TValue
parsePlaceholderString =
    let
        specialChars =
            [ '"', '\'', '{' ]

        untilNextSpecialChar =
            P.chompWhile (\c -> not <| List.member c specialChars) |> P.getChompedString
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
                            , P.succeed (\var -> P.Loop <| Segment.Interpolation var :: Segment.Text text :: revSegments)
                                |. P.token "{"
                                |= (P.chompUntil "}" |> P.getChompedString)
                                |. P.token "}"
                            , P.succeed (\moreText -> P.Loop <| Segment.Text (text ++ moreText) :: revSegments)
                                |. P.token "\""
                                |= (P.chompUntil "\"" |> P.getChompedString)
                                |. P.token "\""
                            , P.succeed (\moreText -> P.Loop <| Segment.Text (text ++ moreText) :: revSegments)
                                |. P.token "'"
                                |= (P.chompUntil "'" |> P.getChompedString)
                                |. P.token "'"
                            ]
                    )
        )
        |> P.andThen
            (\segments ->
                case List.NonEmpty.fromList segments of
                    Just nonEmpty ->
                        P.succeed <| Segment.concatenateTextSegments nonEmpty

                    Nothing ->
                        P.succeed ( Segment.Text "", [] )
            )


keyValueParser : Parser ( List String, String )
keyValueParser =
    P.succeed Tuple.pair
        |= keyParser
        |= valueParser


propertiesParser : Parser Properties
propertiesParser =
    P.loop []
        (\st ->
            P.succeed identity
                |. P.spaces
                |= P.oneOf
                    [ P.succeed (P.Done <| List.reverse st) |. P.end
                    , P.succeed (P.Loop st) |. P.token "#" |. P.chompWhile ((/=) '\n')
                    , P.succeed (\kv -> P.Loop <| kv :: st)
                        |= keyValueParser
                    ]
        )


propertiesToInternalRep : Properties -> Result String Translations
propertiesToInternalRep =
    List.map
        (\( k, v ) ->
            case P.run parsePlaceholderString v of
                Err err ->
                    Err <| Parser.DeadEnds.deadEndsToString err

                Ok val ->
                    Ok ( Util.keyToName k, val )
        )
        >> Result.Extra.combine
