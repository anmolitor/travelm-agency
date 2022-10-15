module ContentTypes.Properties exposing (parse, parser)

import ContentTypes.Shared exposing (HtmlTagState(..), ParsingState, initialParsingState)
import Dict
import Parser as P exposing ((|.), (|=), Parser)
import Types.Error as Error exposing (Failable)
import Types.Segment as Segment
import Types.Translation exposing (Translation)
import Util


type Comment
    = FallbackDirective String
    | OtherComment String


parse : String -> Failable (Translation ())
parse =
    Error.runParser parser


parser : Parser (Translation ())
parser =
    P.loop { pairs = [], fallback = Nothing }
        (\st ->
            P.succeed identity
                |. P.spaces
                |= P.oneOf
                    [ P.succeed
                        (P.Done
                            { pairs = Dict.fromList <| List.reverse st.pairs
                            , fallback = st.fallback
                            , resources = ()
                            }
                        )
                        |. P.end
                    , P.succeed
                        (\comment ->
                            case comment of
                                FallbackDirective fallback ->
                                    P.Loop { st | fallback = Just fallback }

                                OtherComment _ ->
                                    P.Loop st
                        )
                        |. P.token "#"
                        |= commentParser
                    , P.succeed (\k v -> P.Loop { st | pairs = ( k, v ) :: st.pairs })
                        |= keyParser
                        |. P.chompWhile (\char -> char == ' ')
                        |= valueParser
                    ]
        )


keyParser : Parser Segment.TKey
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
        |> P.map Util.keyToName


valueParser : Parser Segment.TValue
valueParser =
    ContentTypes.Shared.buildValueParser valueParserHelper


valueParserHelper : ParsingState -> Parser (P.Step ParsingState Segment.TValue)
valueParserHelper ({ htmlTagParsingState, revSegments, nesting } as state) =
    let
        addText : String -> P.Step ParsingState Segment.TValue
        addText text =
            case revSegments of
                (Segment.Text previousText) :: otherSegs ->
                    P.Loop { state | revSegments = Segment.Text (previousText ++ text) :: otherSegs }

                _ ->
                    P.Loop { state | revSegments = Segment.Text text :: revSegments }
    in
    case htmlTagParsingState of
        CollectingContent _ _ _ ->
            P.problem "Somehow ended up at 'CollectingContent' state although we should go to the inner most state. This should never happen."

        NoHtml ->
            P.oneOf
                ([ P.map P.Done <|
                    ContentTypes.Shared.onEnd state
                        |. P.end
                 , P.map P.Done <|
                    ContentTypes.Shared.onEnd state
                        |. P.token "\n"
                 , P.succeed (P.Loop state) |. P.token "\\\n" |. P.spaces
                 , P.map (\interp -> P.Loop { state | revSegments = interp :: revSegments })
                    interpolationParser
                 , P.map addText
                    (ContentTypes.Shared.bracket "\"" "\"")
                 , P.map addText
                    (ContentTypes.Shared.bracket "'" "'")
                 ]
                    ++ (case nesting of
                            firstOpenHtmlTag :: _ ->
                                [ P.succeed (P.Done <| ContentTypes.Shared.finalizeRevSegments revSegments)
                                    |. P.token ("</" ++ firstOpenHtmlTag ++ ">")
                                ]

                            [] ->
                                []
                       )
                    ++ [ P.succeed
                            (\htmlTag ->
                                P.Loop { state | htmlTagParsingState = CollectingAttrs htmlTag [] }
                            )
                            |. P.token "<"
                            |= (P.chompWhile Char.isAlpha |> P.getChompedString)
                       , P.map addText <|
                            ContentTypes.Shared.chompAllExcept [ '<', '"', '\'', '{', '\n', '\\' ]
                       ]
                )

        CollectingAttrs tag attrs ->
            P.succeed identity
                |. P.chompWhile ((==) ' ')
                |= P.oneOf
                    [ P.succeed
                        (P.Loop
                            { state
                                | htmlTagParsingState =
                                    CollectingContent tag
                                        attrs
                                        { initialParsingState | nesting = tag :: state.nesting }
                            }
                        )
                        |. P.token ">"
                    , P.succeed
                        (\key value ->
                            P.Loop
                                { state
                                    | htmlTagParsingState = CollectingAttrs tag (( key, value ) :: attrs)
                                }
                        )
                        |= (P.chompWhile (\char -> char /= ' ' && char /= '=') |> P.getChompedString)
                        |. P.chompWhile ((==) ' ')
                        |. P.token "="
                        |. P.chompWhile ((==) ' ')
                        |. P.token "\""
                        |= P.loop []
                            (\revAttrSegments ->
                                P.oneOf
                                    [ (P.succeed <| P.Done revAttrSegments)
                                        |. P.token "\""
                                    , P.map (ContentTypes.Shared.addText revAttrSegments >> P.Loop)
                                        (ContentTypes.Shared.bracket "'" "'")
                                    , P.map (\interp -> P.Loop <| interp :: revAttrSegments)
                                        interpolationParser
                                    , P.andThen
                                        (\text ->
                                            if String.isEmpty text then
                                                P.problem "Invalid Attribute. It seems like you did not finish your value with a '\"'."

                                            else
                                                P.succeed <| P.Loop <| ContentTypes.Shared.addText revAttrSegments text
                                        )
                                      <|
                                        ContentTypes.Shared.chompAllExcept [ '"', '{', '\\', '\'' ]
                                    ]
                            )
                    ]


interpolationParser : Parser Segment.TSegment
interpolationParser =
    ContentTypes.Shared.bracket "{" "}" |> P.map Segment.Interpolation


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
