module ContentTypes.Shared exposing (..)

import List.Extra
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


isSpecialAttribute : String -> Bool
isSpecialAttribute =
    (==) idAttributeName


idAttributeName : String
idAttributeName =
    "_id"


applyStepInnermost : (ParsingState -> P.Parser (P.Step ParsingState Segment.TValue)) -> ParsingState -> P.Parser (P.Step ParsingState Segment.TValue)
applyStepInnermost step state =
    case state.htmlTagParsingState of
        CollectingContent tag attrs innerState ->
            P.andThen
                (\stepResult ->
                    case stepResult of
                        P.Done content ->
                            let
                                finalizedAttrs =
                                    List.reverse <| List.map (Tuple.mapSecond finalizeRevSegments) attrs

                                idAttribute =
                                    List.Extra.find (Tuple.first >> (==) idAttributeName) finalizedAttrs
                                        |> Maybe.map Tuple.second
                            in
                            case idAttribute of
                                Just ( Segment.Text id, [] ) ->
                                    P.succeed <|
                                        P.Loop
                                            { state
                                                | htmlTagParsingState = NoHtml
                                                , revSegments =
                                                    Segment.Html
                                                        { tag = tag
                                                        , id = id
                                                        , attrs = List.filter (not << isSpecialAttribute << Tuple.first) finalizedAttrs
                                                        , content = content
                                                        }
                                                        :: state.revSegments
                                            }

                                Nothing ->
                                    P.succeed <|
                                        P.Loop
                                            { state
                                                | htmlTagParsingState = NoHtml
                                                , revSegments =
                                                    Segment.Html
                                                        { tag = tag
                                                        , id = tag
                                                        , attrs = List.filter (not << isSpecialAttribute << Tuple.first) finalizedAttrs
                                                        , content = content
                                                        }
                                                        :: state.revSegments
                                            }

                                _ ->
                                    P.problem <|
                                        """I found an '_id' attribute on an html element containing features other than plain text.
Since the _id attribute is resolved at compile time, this is not allowed.

Here is the html tag that misses the _id attribute: """
                                            ++ tag

                        P.Loop newInnerState ->
                            P.succeed <| P.Loop { state | htmlTagParsingState = CollectingContent tag attrs newInnerState }
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


addText : List Segment.TSegment -> String -> List Segment.TSegment
addText revSegments text =
    case revSegments of
        (Segment.Text previousText) :: otherSegs ->
            Segment.Text (previousText ++ text) :: otherSegs

        _ ->
            Segment.Text text :: revSegments


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
