module ContentTypes.Json exposing (parse)

import ContentTypes.Shared exposing (HtmlTagState(..), ParsingState, initialParsingState)
import Dict
import List.NonEmpty exposing (NonEmpty)
import Maybe.Extra
import Parser as P exposing ((|.), (|=), Parser)
import Types.Error as Error exposing (Failable)
import Types.Segment as Segment
import Types.Translation exposing (Translation)
import Util


parse : String -> Failable (Translation ())
parse =
    Error.runParser parser


parser : Parser (Translation ())
parser =
    objectParser
        |> P.map
            (\revPairs ->
                let
                    pairs =
                        List.reverse revPairs

                    transforms =
                        List.filterMap isSpecialPair pairs
                            |> List.foldl (>>) identity
                in
                transforms
                    { fallback = Nothing
                    , resources = ()
                    , pairs =
                        pairs
                            |> List.filter (isSpecialPair >> Maybe.Extra.isNothing)
                            |> List.map (Tuple.mapFirst (List.NonEmpty.toList >> Util.keyToName))
                            |> Dict.fromList
                    }
            )


objectParser : Parser (List ( NonEmpty String, Segment.TValue ))
objectParser =
    P.succeed identity
        |. P.token "{"
        |. P.spaces
        |= P.loop []
            (\st ->
                P.oneOf
                    [ P.succeed (P.Done st)
                        |. P.token "}"
                    , keyParser
                        |> P.andThen
                            (\key ->
                                P.oneOf
                                    [ P.succeed
                                        (\value isDone ->
                                            (if isDone then
                                                P.Done

                                             else
                                                P.Loop
                                            )
                                                (( ( key, [] ), value )
                                                    :: st
                                                )
                                        )
                                        |. P.token "\""
                                        |= valueParser
                                        |. P.spaces
                                        |= P.oneOf
                                            [ P.succeed True |. P.token "}"
                                            , P.succeed False |. P.token ","
                                            ]
                                    , P.map
                                        (\innerState isDone ->
                                            (if isDone then
                                                P.Done

                                             else
                                                P.Loop
                                            )
                                            <|
                                                List.map (Tuple.mapFirst <| List.NonEmpty.cons key) innerState
                                                    ++ st
                                        )
                                        objectParser
                                        |. P.spaces
                                        |= P.oneOf
                                            [ P.succeed True |. P.token "}"
                                            , P.succeed False |. P.token "," |. P.spaces
                                            ]
                                    ]
                            )
                    ]
                    |. P.spaces
            )
        |. P.spaces


keyParser : Parser String
keyParser =
    P.succeed identity
        |. P.token "\""
        |= (P.chompUntil "\"" |> P.getChompedString)
        |. P.token "\""
        |. P.spaces
        |. P.token ":"
        |. P.spaces


valueParser : Parser Segment.TValue
valueParser =
    ContentTypes.Shared.buildValueParser valueParserHelper


validEscapeSequences : List ( String, String )
validEscapeSequences =
    [ ( "\\<", "<" ), ( "\\{", "{" ), ( "\\", "\\" ), ( "\"", "\"" ) ]


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
                        |. P.token "\""
                 , P.map (\interp -> P.Loop { state | revSegments = interp :: revSegments })
                    interpolationParser
                 , P.succeed addText
                    |. P.token "\\"
                    |= P.oneOf
                        (List.map (\( sequence, result ) -> P.token sequence |> P.map (\_ -> result)) validEscapeSequences
                            ++ [ P.problem "Invalid escaped char" ]
                        )
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
                            |= (P.chompWhile (\c -> c /= ' ' && c /= '>') |> P.getChompedString)
                       , P.andThen
                            (\text ->
                                if String.isEmpty text then
                                    P.problem "Invalid JSON. It seems like you did not finish your value with a '\"'."

                                else
                                    P.succeed <| addText text
                            )
                         <|
                            ContentTypes.Shared.chompAllExcept [ '<', '"', '{', '\n', '\\' ]
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


interpolationParser : Parser Segment.TSegment
interpolationParser =
    P.succeed Segment.Interpolation
        |. P.token "{"
        |. P.chompWhile ((==) ' ')
        |= (P.chompWhile (\c -> c /= ' ' && c /= '}') |> P.getChompedString)
        |. P.chompWhile ((==) ' ')
        |. P.token "}"


isSpecialPair : ( NonEmpty String, Segment.TValue ) -> Maybe (Translation () -> Translation ())
isSpecialPair pair =
    case pair of
        ( ( "--fallback-language", [] ), ( Segment.Text fallback, [] ) ) ->
            Just <| \translation -> { translation | fallback = Just fallback }

        _ ->
            Nothing
