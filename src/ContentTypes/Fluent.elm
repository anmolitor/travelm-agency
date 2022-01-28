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
    , matchCaseParser
    , message
    , messageLine
    , multilineHelper
    , noAttrs
    , runFluentParser
    , stringLit
    )

import Char exposing (isAlphaNum)
import Dict exposing (Dict)
import Intl exposing (Intl)
import Iso8601
import List.Extra as List
import List.NonEmpty exposing (NonEmpty)
import Parser exposing ((|.), (|=), Parser, Step(..), andThen, chompUntil, chompUntilEndOr, chompWhile, end, float, getChompedString, loop, map, oneOf, problem, spaces, succeed, token)
import Parser.DeadEnds
import Result.Extra
import State exposing (Translations)
import String.Extra
import Time
import Types.ArgValue as ArgValue exposing (ArgValue)
import Types.Segment as Segment exposing (TKey, TSegment, TValue)
import Util


type alias AST =
    List Resource


type Resource
    = MessageResource Message
    | CommentResource String


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


fluentToInternalRep : Intl -> String -> AST -> Result String Translations
fluentToInternalRep intl language ast_ =
    let
        identifierToKey : Identifier -> TKey
        identifierToKey id =
            case id of
                TermIdentifier t ->
                    t

                MessageIdentifier m ->
                    m

        contentsToValue : Content -> Result String (NonEmpty TSegment)
        contentsToValue =
            contentsToValueHelper [] []

        formatNumberLit : Literal -> ExtraArguments -> Result String String
        formatNumberLit lit args =
            case lit of
                StringLiteral str ->
                    Err <| "Cannot format the string literal '" ++ str ++ "' as a number"

                NumberLiteral n ->
                    Ok <|
                        Intl.formatFloat intl
                            { number = n
                            , language = language
                            , args = List.map (Tuple.mapSecond ArgValue.encode) args
                            }

        formatDateLit : Literal -> ExtraArguments -> Result String String
        formatDateLit lit args =
            case lit of
                StringLiteral str ->
                    Result.map
                        (\time ->
                            Intl.formatDateTime intl
                                { time = time
                                , language = language
                                , args = List.map (Tuple.mapSecond ArgValue.encode) args
                                }
                        )
                        (Iso8601.toTime str |> Result.mapError (\err -> "Error while parsing the iso8601 date literal: '" ++ str ++ "': " ++ Parser.DeadEnds.deadEndsToString err))

                NumberLiteral n ->
                    Ok <| Intl.formatDateTime intl { time = Time.millisToPosix <| floor n, language = language, args = [] }

        getPluralRule : String -> Result String String
        getPluralRule str =
            case String.toFloat str of
                Just num ->
                    Intl.determinePluralRuleFloat intl { language = language, number = num, type_ = Intl.Cardinal }
                        |> Intl.pluralRuleToString
                        |> Ok

                Nothing ->
                    Err <| "Cannot determine plural rule for '" ++ str ++ "'"

        -- Limits recursion by keeping track of terms
        -- Inlines arguments known at compile time
        contentsToValueHelper : List String -> List ( String, Literal ) -> Content -> Result String (NonEmpty TSegment)
        contentsToValueHelper previousTerms accumulatedArgs cnt =
            let
                recurseOnContentList : NonEmpty Content -> Result String (NonEmpty TSegment)
                recurseOnContentList =
                    List.NonEmpty.map (contentsToValueHelper previousTerms accumulatedArgs)
                        >> combineMapResultNonEmpty
                        >> Result.map List.NonEmpty.concat
            in
            case cnt of
                TextContent str ->
                    Ok <| List.NonEmpty.singleton <| Segment.Text str

                PlaceableContent (VarRef var) ->
                    Ok <|
                        List.NonEmpty.singleton <|
                            case List.find (Tuple.first >> (==) var) accumulatedArgs of
                                Just ( _, StringLiteral str ) ->
                                    Segment.Text str

                                Just ( _, NumberLiteral n ) ->
                                    Segment.Text <| String.fromFloat n

                                Nothing ->
                                    Segment.Interpolation var

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
                    Ok <| List.NonEmpty.singleton <| Segment.Text lit

                PlaceableContent (FunctionCall NUMBER (VarArgument var) extraArgs) ->
                    Result.map List.NonEmpty.singleton <|
                        case List.find (Tuple.first >> (==) var) accumulatedArgs of
                            Just ( _, lit ) ->
                                formatNumberLit lit extraArgs |> Result.map Segment.Text

                            Nothing ->
                                Ok <| Segment.FormatNumber var extraArgs

                PlaceableContent (FunctionCall DATETIME (VarArgument var) extraArgs) ->
                    Result.map List.NonEmpty.singleton <|
                        case List.find (Tuple.first >> (==) var) accumulatedArgs of
                            Just ( _, lit ) ->
                                formatDateLit lit extraArgs |> Result.map Segment.Text

                            Nothing ->
                                Ok <| Segment.FormatDate var extraArgs

                PlaceableContent (FunctionCall NUMBER (LiteralArgument lit) extraArgs) ->
                    formatNumberLit lit extraArgs |> Result.map (Segment.Text >> List.NonEmpty.singleton)

                PlaceableContent (FunctionCall DATETIME (LiteralArgument lit) extraArgs) ->
                    formatDateLit lit extraArgs |> Result.map (Segment.Text >> List.NonEmpty.singleton)

                PlaceableContent (NumberMatchStatement (LiteralArgument lit) extraArgs options) ->
                    formatNumberLit lit extraArgs
                        |> Result.andThen getPluralRule
                        |> Result.map (chooseMatch options)
                        |> Result.andThen recurseOnContentList

                PlaceableContent (NumberMatchStatement (VarArgument var) extraArgs options) ->
                    Result.map2 (Segment.PluralCase var extraArgs)
                        (recurseOnContentList options.default)
                        (Dict.toList options.otherOptions
                            |> Result.Extra.combineMap (Result.Extra.combineMapSecond recurseOnContentList)
                            |> Result.map Dict.fromList
                        )
                        |> Result.map List.NonEmpty.singleton

                PlaceableContent (StringMatchStatement var options) ->
                    Result.map2 (Segment.InterpolationCase var)
                        (recurseOnContentList options.default)
                        (Dict.toList options.otherOptions
                            |> Result.Extra.combineMap (Result.Extra.combineMapSecond recurseOnContentList)
                            |> Result.map Dict.fromList
                        )
                        |> Result.map List.NonEmpty.singleton

        termDict : Dict String (NonEmpty Content)
        termDict =
            List.filterMap
                (\res ->
                    case res of
                        MessageResource msg ->
                            case msg.identifier of
                                TermIdentifier str ->
                                    Just ( str, msg.content )

                                MessageIdentifier _ ->
                                    Nothing

                        CommentResource _ ->
                            Nothing
                )
                ast_
                |> Dict.fromList

        termFilter : Resource -> Bool
        termFilter res =
            case res of
                MessageResource msg ->
                    case msg.identifier of
                        TermIdentifier _ ->
                            False

                        MessageIdentifier _ ->
                            True

                CommentResource _ ->
                    False

        attrToInternalRep : Attribute -> Result String ( TKey, TValue )
        attrToInternalRep attr =
            List.NonEmpty.map contentsToValue attr.content
                |> combineMapResultNonEmpty
                |> Result.map
                    (List.NonEmpty.concat
                        >> Segment.concatenateTextSegments
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

        resourceToInternalRep : Resource -> Result String (List ( TKey, TValue ))
        resourceToInternalRep res =
            case res of
                MessageResource msg ->
                    msgToAttrs msg
                        |> List.NonEmpty.map attrToInternalRep
                        |> combineMapResultNonEmpty
                        |> Result.map List.NonEmpty.toList

                CommentResource _ ->
                    Ok []
    in
    List.filter termFilter ast_
        |> Result.Extra.combineMap resourceToInternalRep
        |> Result.map List.concat


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
                            [ succeed (\comment -> Loop <| comment :: st) |. token "#" |= map CommentResource (chompUntilEndOr "\n" |> getChompedString)
                            , succeed (\msg -> Loop <| msg :: st) |= map MessageResource message
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
    | FunctionCall BuiltInFunction FunctionArgument ExtraArguments
    | NumberMatchStatement FunctionArgument ExtraArguments MatchOptions
    | StringMatchStatement String MatchOptions


type alias MatchOptions =
    { default : NonEmpty Content
    , otherOptions : Dict String (NonEmpty Content)
    }


chooseMatch : MatchOptions -> String -> NonEmpty Content
chooseMatch { default, otherOptions } str =
    Dict.get str otherOptions |> Maybe.withDefault default


type alias ExtraArguments =
    List ( String, ArgValue )


type FunctionArgument
    = VarArgument String
    | LiteralArgument Literal


type BuiltInFunction
    = NUMBER
    | DATETIME


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
    c == ' ' || c == '\n' || c == '\u{000D}' || c == '}' || c == ')'


varParser : Parser String
varParser =
    token "$" |> andThen (\_ -> chompWhile isAlphaNum |> getChompedString)


argumentParser : Parser FunctionArgument
argumentParser =
    oneOf
        [ varParser |> map VarArgument
        , stringLit |> map (StringLiteral >> LiteralArgument)
        , float |> map (NumberLiteral >> LiteralArgument)
        ]


argValueParser : Parser ArgValue
argValueParser =
    oneOf
        [ stringLit |> map ArgValue.StringArg
        , float |> map ArgValue.NumberArg
        , succeed (ArgValue.BoolArg True) |. token "true"
        , succeed (ArgValue.BoolArg False) |. token "false"
        ]


otherArgumentsParser : Parser (List ( String, ArgValue ))
otherArgumentsParser =
    loop [] <|
        \state ->
            oneOf
                [ succeed (\k v -> Loop <| ( k, v ) :: state)
                    |. token ","
                    |. spaces
                    |= (chompWhile (\c -> c /= ' ' && c /= ':') |> getChompedString)
                    |. spaces
                    |. token ":"
                    |. spaces
                    |= argValueParser
                    |. spaces
                , succeed (Done <| List.reverse state) |. token ")"
                ]


type alias IntermediateMatchCase =
    ( Maybe (NonEmpty Content), Dict String (NonEmpty Content) )


matchCaseParser : Parser ( NonEmpty Content, Dict String (NonEmpty Content) )
matchCaseParser =
    let
        matchCaseHelper : IntermediateMatchCase -> Parser (Step IntermediateMatchCase IntermediateMatchCase)
        matchCaseHelper state =
            oneOf
                [ (succeed
                    (\isDefault key value ->
                        case List.NonEmpty.fromList value of
                            Just nonEmptyContent ->
                                succeed <|
                                    Loop <|
                                        if isDefault then
                                            ( Just nonEmptyContent, Tuple.second state )

                                        else
                                            state |> Tuple.mapSecond (Dict.insert key nonEmptyContent)

                            Nothing ->
                                problem <| "Failed to read content for case match on '" ++ key ++ "'"
                    )
                    |= oneOf [ succeed True |. token "*[", succeed False |. token "[" ]
                    |. spaces
                    |= (chompWhile isAlphaNum |> getChompedString)
                    |. spaces
                    |. token "]"
                    |. spaces
                    |= messageLine
                    |. token "\n"
                    |. spaces
                  )
                    |> andThen identity
                , succeed (Done state) |. token "}"
                ]
    in
    loop ( Nothing, Dict.empty ) matchCaseHelper
        |> andThen
            (\( mayDefault, otherOptions ) ->
                case mayDefault of
                    Just default ->
                        succeed ( default, otherOptions )

                    Nothing ->
                        problem "Could not find any default case. Make sure to mark one of the cases in a match statement with '*'."
            )


placeable : Parser Placeable
placeable =
    succeed identity
        |. spaces
        |= oneOf
            [ token "NUMBER("
                |> andThen
                    (\_ ->
                        succeed
                            (\arg otherArgs matchCase ->
                                case matchCase of
                                    Just ( defMay, matchOptions ) ->
                                        NumberMatchStatement arg otherArgs { default = defMay, otherOptions = matchOptions }

                                    Nothing ->
                                        FunctionCall NUMBER arg otherArgs
                            )
                            |. spaces
                            |= argumentParser
                            |. spaces
                            |= otherArgumentsParser
                            |. spaces
                            |= oneOf
                                [ succeed Nothing |. token "}"
                                , succeed Just
                                    |. token "->"
                                    |. spaces
                                    |= matchCaseParser
                                ]
                    )
            , token "DATETIME("
                |> andThen
                    (\_ ->
                        succeed (FunctionCall DATETIME)
                            |. spaces
                            |= argumentParser
                            |. spaces
                            |= otherArgumentsParser
                            |. spaces
                            |. token "}"
                    )
            , succeed
                (\var matchCase ->
                    case matchCase of
                        Just ( defMay, matchOptions ) ->
                            StringMatchStatement var { default = defMay, otherOptions = matchOptions }

                        Nothing ->
                            VarRef var
                )
                |= varParser
                |. spaces
                |= oneOf
                    [ succeed Nothing |. token "}"
                    , succeed Just
                        |. token "->"
                        |. spaces
                        |= matchCaseParser
                    ]
            , succeed identity |. token "-" |= termRefParser |. spaces |. token "}"
            , succeed StringLit
                |= stringLit
                |. spaces
                |. token "}"
            ]


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
