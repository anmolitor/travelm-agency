module ContentTypes.Properties exposing (keyValueParser, parseProperties, propertiesToInternalRep, valueParser)

import List.NonEmpty
import Parser as P exposing ((|.), (|=), Parser)
import Parser.DeadEnds
import Result.Extra
import Types
import Util


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
                            , P.succeed (\var -> P.Loop <| Types.Interpolation var :: Types.Text text :: revSegments)
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
                        P.succeed ( Types.Text "", [] )
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
                    , P.succeed (\kv -> P.Loop <| kv :: st)
                        |= keyValueParser
                    ]
        )


propertiesToInternalRep : Properties -> Result String Types.Translations
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
