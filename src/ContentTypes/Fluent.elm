module ContentTypes.Fluent exposing
    ( AST
    , Content(..)
    , Identifier(..)
    , Literal(..)
    , Message
    , Placeable(..)
    , Resource(..)
    , ast
    , fluentToInternalRep
    , identifier
    , message
    , messageLine
    , multilineHelper
    , noAttrs
    , runFluentParser
    , stringLit
    )

import Dict exposing (Dict)
import List.Extra as List
import List.NonEmpty exposing (NonEmpty)
import Parser exposing ((|.), (|=), Parser, Step(..), andThen, chompUntil, chompWhile, end, float, getChompedString, loop, map, oneOf, problem, spaces, succeed, token)
import Parser.DeadEnds
import Result.Extra
import String.Extra
import Types
import Util


type alias AST =
    List Resource


type Resource
    = MessageResource Message


noAttrs : { identifier : Identifier, content : NonEmpty Content } -> Resource
noAttrs msg =
    MessageResource { identifier = msg.identifier, content = msg.content, attrs = [] }


type alias Attribute =
    { identifier : String
    , content : NonEmpty Content
    }


type alias Message =
    { identifier : Identifier
    , content : NonEmpty Content
    , attrs : List Attribute
    }


type Content
    = TextContent String
    | PlaceableContent Placeable


fluentToInternalRep : AST -> Result String Types.Translations
fluentToInternalRep ast_ =
    let
        identifierToKey : Identifier -> Types.TKey
        identifierToKey id =
            case id of
                TermIdentifier t ->
                    t

                MessageIdentifier m ->
                    m

        contentsToValue : Content -> Result String (NonEmpty Types.TSegment)
        contentsToValue =
            contentsToValueHelper [] []

        -- Limits recursion by keeping track of terms
        -- Inlines arguments known at compile time
        contentsToValueHelper : List String -> List ( String, Literal ) -> Content -> Result String (NonEmpty Types.TSegment)
        contentsToValueHelper previousTerms accumulatedArgs cnt =
            case cnt of
                TextContent str ->
                    Ok <| List.NonEmpty.singleton <| Types.Text str

                PlaceableContent (VarRef var) ->
                    Ok <|
                        List.NonEmpty.singleton <|
                            case List.find (Tuple.first >> (==) var) accumulatedArgs of
                                Just ( _, StringLiteral str ) ->
                                    Types.Text str

                                Just ( _, NumberLiteral n ) ->
                                    Types.Text <| String.fromFloat n

                                Nothing ->
                                    Types.Interpolation var

                PlaceableContent (TermRef term args) ->
                    case ( List.member term previousTerms, Dict.get term termDict ) of
                        ( True, _ ) ->
                            Err <| "Recursive term reference " ++ (String.join " <- " <| term :: previousTerms)

                        ( False, Just contents ) ->
                            List.NonEmpty.map (contentsToValueHelper (term :: previousTerms) (args ++ accumulatedArgs)) contents
                                |> combineMapResultNonEmpty
                                |> Result.map List.NonEmpty.concat

                        ( False, Nothing ) ->
                            Err <| "Could not resolve term reference: '" ++ term ++ "'"

                PlaceableContent (StringLit lit) ->
                    Ok <| List.NonEmpty.singleton <| Types.Text lit

        termDict : Dict String (NonEmpty Content)
        termDict =
            List.filterMap
                (\(MessageResource msg) ->
                    case msg.identifier of
                        TermIdentifier str ->
                            Just ( str, msg.content )

                        MessageIdentifier _ ->
                            Nothing
                )
                ast_
                |> Dict.fromList

        termFilter : Resource -> Bool
        termFilter (MessageResource msg) =
            case msg.identifier of
                TermIdentifier _ ->
                    False

                MessageIdentifier _ ->
                    True

        attrToInternalRep : Attribute -> Result String ( Types.TKey, Types.TValue )
        attrToInternalRep attr =
            List.NonEmpty.map contentsToValue attr.content
                |> combineMapResultNonEmpty
                |> Result.map
                    (List.NonEmpty.concat
                        >> Types.concatenateTextSegments
                        >> Tuple.pair attr.identifier
                    )

        msgToAttrs : Message -> NonEmpty Attribute
        msgToAttrs msg =
            let
                key =
                    identifierToKey msg.identifier
            in
            List.NonEmpty.fromCons { identifier = key, content = msg.content } <|
                List.map
                    (\attr -> { attr | identifier = Util.keyToName [ key, attr.identifier ] })
                    msg.attrs

        resourceToInternalRep : Resource -> Result String (NonEmpty ( Types.TKey, Types.TValue ))
        resourceToInternalRep res =
            case res of
                MessageResource msg ->
                    msgToAttrs msg
                        |> List.NonEmpty.map attrToInternalRep
                        |> combineMapResultNonEmpty
    in
    List.filter termFilter ast_
        |> Result.Extra.combineMap resourceToInternalRep
        |> Result.map (List.concatMap List.NonEmpty.toList)


combineMapResultNonEmpty : NonEmpty (Result x a) -> Result x (NonEmpty a)
combineMapResultNonEmpty =
    List.NonEmpty.map (Result.map List.NonEmpty.singleton)
        >> List.NonEmpty.foldr1 (Result.map2 List.NonEmpty.append)


runFluentParser : String -> Result String AST
runFluentParser =
    Parser.run ast
        >> Result.mapError Parser.DeadEnds.deadEndsToString


ast : Parser AST
ast =
    loop []
        (\st ->
            chompWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}')
                |> andThen
                    (\_ ->
                        oneOf
                            [ succeed (\msg -> Loop <| msg :: st) |= map MessageResource message
                            , succeed (Done st) |. end
                            ]
                    )
        )
        |> map List.reverse


message : Parser Message
message =
    (succeed
        (\id firstLineCnt multilines ->
            addMultilines multilines firstLineCnt id
        )
        |= identifier
        |. spaces
        |= messageLine
        |= loop End multilineHelper
    )
        |> andThen
            (Result.fromMaybe "Content of message cannot be empty"
                >> resultToParser
            )


addMultilines : Multiline -> List Content -> Identifier -> Maybe Message
addMultilines multis firstLineCnts id =
    let
        extractContents : Multiline -> ( List (List Content), List Attribute )
        extractContents multi =
            case multi of
                Continuation lineCnts nextLine ->
                    let
                        ( recursiveCnts, recursiveAttrs ) =
                            extractContents nextLine
                    in
                    ( lineCnts :: recursiveCnts, recursiveAttrs )

                AttributeDef msg nextLine ->
                    let
                        ( recursiveCnts, recursiveAttrs ) =
                            extractContents nextLine
                    in
                    ( recursiveCnts
                    , { identifier =
                            case msg.identifier of
                                TermIdentifier term ->
                                    term

                                MessageIdentifier msgId ->
                                    msgId
                      , content = msg.content
                      }
                        :: recursiveAttrs
                        ++ msg.attrs
                    )

                End ->
                    ( [], [] )

        ( otherLineCnts, attrs ) =
            extractContents multis
    in
    List.NonEmpty.fromList (combineLines <| firstLineCnts :: removeCommonIndentCnt (List.reverse otherLineCnts))
        |> Maybe.map (\cnts -> { identifier = id, content = cnts, attrs = attrs })


type Multiline
    = Continuation (List Content) Multiline
    | AttributeDef Message Multiline
    | End


multilineHelper : Multiline -> Parser (Step Multiline Multiline)
multilineHelper multi =
    let
        prefixTextContent prefix step =
            case step of
                Loop (Continuation ((TextContent text) :: otherCnts) m) ->
                    Loop (Continuation (TextContent (prefix ++ text) :: otherCnts) m)

                Loop (Continuation cnts m) ->
                    Loop (Continuation (TextContent prefix :: cnts) m)

                _ ->
                    step

        addNewLines numberOfNewLines step =
            case step of
                Loop (Continuation c m) ->
                    Loop (Continuation c <| List.foldl Continuation m (List.repeat numberOfNewLines []))

                _ ->
                    step
    in
    succeed (\newLines step -> addNewLines (String.length newLines) step)
        |. oneOf [ end, Parser.token "\n" ]
        |= (chompWhile ((==) '\n') |> getChompedString)
        |= oneOf
            [ succeed prefixTextContent
                |. Parser.chompIf isSpace
                |= (spaces |> getChompedString)
                |= oneOf
                    [ Parser.token "." |> Parser.andThen (\_ -> message) |> Parser.map (\msg -> AttributeDef msg multi |> Done)
                    , messageLine |> Parser.map (\cnts -> Loop (Continuation cnts multi))
                    ]
            , succeed (Done multi)
            ]


messageLine : Parser (List Content)
messageLine =
    loop [] messageLineHelper


messageLineHelper : List Content -> Parser (Step (List Content) (List Content))
messageLineHelper revCnt =
    oneOf
        [ succeed (\cnt -> Loop (cnt :: revCnt))
            |= content
        , succeed (Done <| List.reverse revCnt)
        ]


isSpace : Char -> Bool
isSpace c =
    c == ' ' || c == '\t'


content : Parser Content
content =
    oneOf
        [ succeed PlaceableContent |. token "{" |= placeable
        , chompWhile (\c -> c /= '{' && c /= '\n')
            |> getChompedString
            |> andThen
                (\cnt ->
                    if cnt == "" then
                        problem "Expected more content."

                    else
                        succeed (TextContent cnt)
                )
        ]


combineLines : List (List Content) -> List Content
combineLines =
    List.foldl
        (\lineCnts acc ->
            case ( acc, lineCnts ) of
                ( (TextContent str) :: restAcc, (TextContent strNextLine) :: restLine ) ->
                    List.reverse restLine ++ TextContent (str ++ "\n" ++ strNextLine) :: restAcc

                ( (TextContent str) :: restAcc, _ ) ->
                    List.reverse lineCnts ++ TextContent (str ++ "\n") :: restAcc

                ( _ :: _, (TextContent strNextLine) :: restLine ) ->
                    List.reverse restLine ++ TextContent ("\n" ++ strNextLine) :: acc

                ( _ :: _, _ ) ->
                    List.reverse lineCnts ++ TextContent "\n" :: acc

                ( [], _ ) ->
                    List.reverse lineCnts ++ acc
        )
        []
        >> List.reverse


removeCommonIndentCnt : List (List Content) -> List (List Content)
removeCommonIndentCnt lines =
    let
        removeIndent =
            String.dropLeft <| commonIndentCnt lines
    in
    List.map
        (\cnts ->
            case cnts of
                (TextContent str) :: rest ->
                    (TextContent <| removeIndent str) :: rest

                _ ->
                    cnts
        )
        lines


commonIndentCnt : List (List Content) -> Int
commonIndentCnt =
    List.filterMap
        (\list ->
            case list of
                (TextContent str) :: rest ->
                    if List.isEmpty rest && String.Extra.isBlank str then
                        Nothing

                    else
                        Just str

                _ ->
                    Nothing
        )
        >> commonIndent


commonIndent : List String -> Int
commonIndent =
    List.filter (not << String.isEmpty)
        >> List.map
            (String.foldl
                (\c ( ind, encounteredNonWhitespace ) ->
                    if encounteredNonWhitespace || not (isSpace c) then
                        ( ind, True )

                    else
                        ( ind + 1, False )
                )
                ( 0, False )
            )
        >> List.map Tuple.first
        >> List.minimum
        >> Maybe.withDefault 0


type Identifier
    = TermIdentifier String
    | MessageIdentifier String


type Placeable
    = VarRef String
    | TermRef String (List ( String, Literal ))
    | StringLit String


type Literal
    = StringLiteral String
    | NumberLiteral Float


identifier : Parser Identifier
identifier =
    let
        parseName =
            chompWhile (not << (\c -> c == ' ' || c == '\n' || c == '\u{000D}' || c == '='))
                |> getChompedString
    in
    oneOf
        [ succeed TermIdentifier |. token "-" |= parseName
        , succeed MessageIdentifier |= parseName
        ]
        |. spaces
        |. token "="


endsVar : Char -> Bool
endsVar c =
    c == ' ' || c == '\n' || c == '\u{000D}' || c == '}'


placeable : Parser Placeable
placeable =
    succeed identity
        |. spaces
        |= oneOf
            [ token "$" |> andThen (\_ -> chompWhile (not << endsVar) |> getChompedString |> map VarRef)
            , token "-" |> andThen (\_ -> termRefParser)
            , stringLit |> map StringLit
            ]
        |. spaces
        |. token "}"


termRefParser : Parser Placeable
termRefParser =
    let
        argParser : Parser ( String, Literal )
        argParser =
            succeed (\argName literal -> ( String.trim argName, literal ))
                |. spaces
                |= (chompUntil ":" |> getChompedString)
                |. token ":"
                |. spaces
                |= oneOf [ stringLit |> map StringLiteral, float |> map NumberLiteral ]

        argsParser : Parser (List ( String, Literal ))
        argsParser =
            loop [] <|
                \args ->
                    (argParser |. spaces)
                        |> andThen
                            (\arg ->
                                oneOf
                                    [ succeed (Loop <| arg :: args) |. token ","
                                    , succeed (Done <| arg :: args) |. token ")"
                                    ]
                            )
    in
    succeed TermRef
        |= (chompWhile (\c -> not (endsVar c) && c /= '(') |> getChompedString)
        |= oneOf
            [ succeed identity
                |. token "("
                |= argsParser
            , succeed []
            ]


stringLit : Parser String
stringLit =
    (succeed identity
        |. token "\""
        |= loop [] stringHelp
    )
        |> andThen resultToParser


resultToParser : Result String a -> Parser a
resultToParser res =
    case res of
        Ok a ->
            succeed a

        Err err ->
            problem err


stringHelp : List String -> Parser (Step (List String) (Result String String))
stringHelp revChunks =
    oneOf
        [ succeed (\chunk -> Loop (chunk :: revChunks))
            |. token "\\"
            |= oneOf
                [ map (\_ -> "\n") (token "n")
                , map (\_ -> "\t") (token "t")
                , map (\_ -> "\"") (token "\"")
                , map (\_ -> "\\") (token "\\")
                , succeed String.fromChar
                    |. token "u"
                    |= unicode
                ]
        , token "\""
            |> map (\_ -> Done (Ok <| String.join "" (List.reverse revChunks)))
        , end |> map (\_ -> Done (Err "String ended without closing '\"'."))
        , chompWhile isUninteresting
            |> getChompedString
            |> map (\chunk -> Loop (chunk :: revChunks))
        ]


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '\\' && char /= '"'



-- UNICODE


unicode : Parser Char
unicode =
    getChompedString (chompWhile Char.isHexDigit)
        |> andThen codeToChar


codeToChar : String -> Parser Char
codeToChar str =
    let
        length =
            String.length str

        code =
            String.foldl addHex 0 str
    in
    if 4 >= length && length >= 6 then
        problem "code point must have between 4 and 6 digits"

    else if 0 <= code && code <= 0x0010FFFF then
        succeed (Char.fromCode code)

    else
        problem "code point must be between 0 and 0x10FFFF"


addHex : Char -> Int -> Int
addHex char total =
    let
        code =
            Char.toCode char
    in
    if 0x30 <= code && code <= 0x39 then
        16 * total + (code - 0x30)

    else if 0x41 <= code && code <= 0x46 then
        16 * total + (10 + code - 0x41)

    else
        16 * total + (10 + code - 0x61)
