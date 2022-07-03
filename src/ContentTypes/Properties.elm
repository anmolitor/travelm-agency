module ContentTypes.Properties exposing (parse, parser)

import Dict
import List.NonEmpty
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


type alias ParsingState =
    { revSegments : List Segment.TSegment
    , htmlTagParsingState : HtmlTagState
    , nesting : List String
    }


initialParsingState : ParsingState
initialParsingState =
    { revSegments = [], htmlTagParsingState = NoHtml, nesting = [] }


type alias HtmlAttrs =
    List ( String, List Segment.TSegment )


type HtmlTagState
    = CollectingAttrs String HtmlAttrs
    | CollectingContent String HtmlAttrs ParsingState
    | NoHtml


valueParser : Parser Segment.TValue
valueParser =
    P.loop initialParsingState (applyStepInnermost valueParserHelper)


applyStepInnermost : (ParsingState -> P.Parser (P.Step ParsingState Segment.TValue)) -> ParsingState -> P.Parser (P.Step ParsingState Segment.TValue)
applyStepInnermost step state =
    case state.htmlTagParsingState of
        CollectingContent tag attrs innerState ->
            P.map
                (\stepResult ->
                    case stepResult of
                        P.Done content ->
                            P.Loop
                                { state
                                    | htmlTagParsingState = NoHtml
                                    , revSegments =
                                        Segment.Html
                                            { tag = tag
                                            , attrs = List.reverse <| List.map (Tuple.mapSecond finalizeRevSegments) attrs
                                            , content = content
                                            }
                                            :: state.revSegments
                                }

                        P.Loop newInnerState ->
                            P.Loop { state | htmlTagParsingState = CollectingContent tag attrs newInnerState }
                )
                (applyStepInnermost step innerState)

        _ ->
            step state


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
                    onEnd state
                        |. P.end
                 , P.map P.Done <|
                    onEnd state
                        |. P.token "\n"
                 , P.succeed (P.Loop state) |. P.token "\\\n" |. P.spaces
                 , P.map (\interp -> P.Loop { state | revSegments = interp :: revSegments })
                    interpolationParser
                 , P.map addText
                    (bracket "\"" "\"")
                 , P.map addText
                    (bracket "'" "'")
                 ]
                    ++ (case nesting of
                            firstOpenHtmlTag :: _ ->
                                [ P.succeed (P.Done <| finalizeRevSegments revSegments)
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
                            chompAllExcept [ '<', '"', '\'', '{', '\n', '\\' ]
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
                                    | htmlTagParsingState = CollectingAttrs tag (( key, [ Segment.Text value ] ) :: attrs)
                                }
                        )
                        |= (P.chompWhile (\char -> char /= ' ' && char /= '=') |> P.getChompedString)
                        |. P.chompWhile ((==) ' ')
                        |. P.token "="
                        |. P.chompWhile ((==) ' ')
                        |. P.token "\""
                        |= (P.chompWhile ((/=) '"') |> P.getChompedString)
                        |. P.token "\""
                    ]


onEnd : ParsingState -> P.Parser Segment.TValue
onEnd state =
    case state.nesting of
        firstOpenHtmlTag :: _ ->
            P.problem <| "Found unclosed html tag: " ++ firstOpenHtmlTag

        [] ->
            P.succeed <| finalizeRevSegments state.revSegments


finalizeRevSegments : List Segment.TSegment -> Segment.TValue
finalizeRevSegments revSegs =
    case List.NonEmpty.fromList revSegs of
        Nothing ->
            ( Segment.Text "", [] )

        Just nonEmptySegs ->
            List.NonEmpty.reverse nonEmptySegs


chompAllExcept : List Char -> Parser String
chompAllExcept chars =
    P.getChompedString <|
        P.chompWhile (\char -> not <| List.member char chars)


bracket : String -> String -> Parser String
bracket start end =
    P.succeed identity
        |. P.token start
        |= (P.chompUntil end |> P.getChompedString)
        |. P.token end


interpolationParser : Parser Segment.TSegment
interpolationParser =
    bracket "{" "}" |> P.map Segment.Interpolation


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
