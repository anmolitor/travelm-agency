module ContentTypes.Properties exposing (Comment(..), Resource(..), keyValueParser, parsePlaceholderString, parseProperties, propertiesToInternalRep, valueParser, parse)

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


parse : String -> Failable (Translation ())
parse =
    parseProperties >> Result.andThen propertiesToInternalRep


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
        untilNextSpecialChar specialChars =
            P.chompWhile (\c -> not <| List.member c specialChars) |> P.getChompedString
    in
    P.loop { revSegments = [], htmlTagsOpenForContent = [], revSegmentsHtml = [], htmlTagOpenForAttrs = Nothing }
        (\({ revSegments, htmlTagsOpenForContent, revSegmentsHtml, htmlTagOpenForAttrs } as state) ->
            case ( htmlTagsOpenForContent, htmlTagOpenForAttrs ) of
                ( [], Nothing ) ->
                    untilNextSpecialChar [ '"', '\'', '{', '<' ]
                        |> P.andThen
                            (\text ->
                                P.oneOf
                                    [ P.succeed
                                        (P.Done <|
                                            List.reverse <|
                                                Segment.Text text
                                                    :: revSegments
                                        )
                                        |. P.end
                                    , P.succeed (\var -> P.Loop { state | revSegments = Segment.Interpolation var :: Segment.Text text :: revSegments })
                                        |. P.token "{"
                                        |= (P.chompUntil "}" |> P.getChompedString)
                                        |. P.token "}"
                                    , P.succeed (\moreText -> P.Loop { state | revSegments = Segment.Text (text ++ moreText) :: revSegments })
                                        |. P.token "\""
                                        |= (P.chompUntil "\"" |> P.getChompedString)
                                        |. P.token "\""
                                    , P.succeed (\moreText -> P.Loop { state | revSegments = Segment.Text (text ++ moreText) :: revSegments })
                                        |. P.token "'"
                                        |= (P.chompUntil "'" |> P.getChompedString)
                                        |. P.token "'"
                                    , P.succeed
                                        (\htmlTag ->
                                            P.Loop
                                                { state
                                                    | htmlTagOpenForAttrs = Just { tag = htmlTag, attrs = [], unfinishedAttrKey = Nothing }
                                                    , revSegments = Segment.Text text :: revSegments
                                                }
                                        )
                                        |. P.token "<"
                                        |= (P.chompWhile Char.isAlpha |> P.getChompedString)
                                        |. P.spaces
                                    ]
                            )

                ( _, Just openTag ) ->
                    P.oneOf
                        ((P.problem ("Found unfinished open html tag: " ++ openTag.tag)
                            |. P.end
                         )
                            :: (case openTag.unfinishedAttrKey of
                                    Just key ->
                                        [ P.succeed
                                            (\value ->
                                                P.Loop
                                                    { state
                                                        | htmlTagOpenForAttrs =
                                                            Just
                                                                { openTag
                                                                    | attrs =
                                                                        ( key, ( Segment.Text value, [] ) )
                                                                            :: openTag.attrs
                                                                    , unfinishedAttrKey = Nothing
                                                                }
                                                    }
                                            )
                                            |. P.token "\""
                                            |= (P.chompUntil "\"" |> P.getChompedString)
                                            |. P.token "\""
                                        ]

                                    Nothing ->
                                        [ P.succeed
                                            (P.Loop
                                                { state
                                                    | htmlTagsOpenForContent = { tag = openTag.tag, attrs = openTag.attrs } :: htmlTagsOpenForContent
                                                    , htmlTagOpenForAttrs = Nothing
                                                }
                                            )
                                            |. P.token ">"
                                        , P.succeed
                                            (\attrKey ->
                                                P.Loop
                                                    { state
                                                        | htmlTagOpenForAttrs = Just { openTag | unfinishedAttrKey = Just attrKey }
                                                    }
                                            )
                                            |= untilNextSpecialChar [ '=', ' ', '\n' ]
                                            |. P.spaces
                                            |. P.token "="
                                        ]
                               )
                        )
                        |. P.spaces

                ( firstOpenHtmlTag :: otherOpenTags, Nothing ) ->
                    untilNextSpecialChar [ '"', '\'', '{', '<' ]
                        |> P.andThen
                            (\text ->
                                P.oneOf
                                    [ P.problem ("Found unclosed html tag: " ++ firstOpenHtmlTag.tag)
                                        |. P.end
                                    , P.succeed
                                        (P.Loop
                                            { state
                                                | htmlTagsOpenForContent = otherOpenTags
                                                , revSegments =
                                                    Segment.Html
                                                        { tag = firstOpenHtmlTag.tag
                                                        , attrs = firstOpenHtmlTag.attrs
                                                        , content = ( Segment.Text text, [] )
                                                        }
                                                        :: revSegments
                                            }
                                        )
                                        |. P.token ("</" ++ firstOpenHtmlTag.tag ++ ">")
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
