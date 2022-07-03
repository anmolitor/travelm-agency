module ContentTypes.Shared exposing (..)

import List.NonEmpty
import Parser as P exposing ((|.), (|=))
import Types.Segment as Segment


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


buildValueParser : (ParsingState -> P.Parser (P.Step ParsingState Segment.TValue)) -> P.Parser Segment.TValue
buildValueParser =
    applyStepInnermost >> P.loop initialParsingState


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


onEnd : ParsingState -> P.Parser Segment.TValue
onEnd state =
    case state.nesting of
        firstOpenHtmlTag :: _ ->
            P.problem <| "Found unclosed html tag: " ++ firstOpenHtmlTag

        [] ->
            P.succeed <| finalizeRevSegments state.revSegments


bracket : String -> String -> P.Parser String
bracket start end =
    P.succeed identity
        |. P.token start
        |= (P.chompUntil end |> P.getChompedString)
        |. P.token end


chompAllExcept : List Char -> P.Parser String
chompAllExcept chars =
    P.getChompedString <|
        P.chompWhile (\char -> not <| List.member char chars)


finalizeRevSegments : List Segment.TSegment -> Segment.TValue
finalizeRevSegments revSegs =
    case List.NonEmpty.fromList revSegs of
        Nothing ->
            ( Segment.Text "", [] )

        Just nonEmptySegs ->
            List.NonEmpty.reverse nonEmptySegs
