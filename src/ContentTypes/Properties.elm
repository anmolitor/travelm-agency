module ContentTypes.Properties exposing (Comment(..), Resource(..), keyValueParser, parsePlaceholderString, parseProperties, propertiesToInternalRep, valueParser)

import Dict
import List.NonEmpty
import Parser as P exposing ((|.), (|=), Parser)
import Types.Error as Error exposing (Failable)
import Types.Segment as Segment
import Types.Translation exposing (Translation)
import Util


type alias Properties =
    List (Resource ( List String, String ))


type Resource a
    = PropertyResource a
    | CommentResource Comment


type Comment
    = FallbackDirective String
    | OtherComment String


parseProperties : String -> Failable Properties
parseProperties =
    Error.runParser propertiesParser


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


commentParser : Parser Comment
commentParser =
    P.succeed identity
        |. P.spaces
        |= P.oneOf
            [ P.succeed FallbackDirective
                |. P.keyword "fallback-language"
                |. P.spaces
                |. P.token ":"
                |. P.spaces
                |= (P.getChompedString <| P.chompWhile (not << (\c -> c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t')))
                |. P.chompUntilEndOr "\n"
            , P.succeed OtherComment
                |= (P.getChompedString <| P.chompUntilEndOr "\n")
            ]


propertiesParser : Parser Properties
propertiesParser =
    P.loop []
        (\st ->
            P.succeed identity
                |. P.spaces
                |= P.oneOf
                    [ P.succeed (P.Done <| List.reverse st) |. P.end
                    , P.succeed (\comment -> P.Loop <| CommentResource comment :: st) |. P.token "#" |= commentParser
                    , P.succeed (\kv -> P.Loop <| PropertyResource kv :: st)
                        |= keyValueParser
                    ]
        )


propertiesToInternalRep : Properties -> Failable (Translation ())
propertiesToInternalRep =
    List.map
        (\resource ->
            case resource of
                PropertyResource ( k, v ) ->
                    Error.runParser parsePlaceholderString v
                        |> Result.map
                            (\val ->
                                { pairs = Dict.singleton (Util.keyToName k) val
                                , resources = ()
                                , fallback = Nothing
                                }
                            )

                CommentResource (FallbackDirective fallback) ->
                    Ok { pairs = Dict.empty, resources = (), fallback = Just fallback }

                _ ->
                    Ok { pairs = Dict.empty, resources = (), fallback = Nothing }
        )
        >> Error.combineList
        >> Result.map Types.Translation.concat
