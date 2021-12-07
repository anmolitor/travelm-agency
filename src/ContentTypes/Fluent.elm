module ContentTypes.Fluent exposing (AST, Content(..), Identifier(..), Message, Placeable(..), Resource(..), ast, message)

import List.NonEmpty exposing (NonEmpty)
import Parser exposing ((|.), (|=), Parser, Step(..), andThen, chompWhile, end, getChompedString, loop, map, oneOf, problem, spaces, succeed, token)
import String.Extra
import Types


type alias AST =
    List Resource


type Resource
    = MessageResource Message


type alias Message =
    { identifier : Identifier
    , content : NonEmpty Content
    }


type Content
    = TextContent String
    | PlaceableContent Placeable


astToInternalRep : AST -> Types.Translations
astToInternalRep ast_ =
    let
        identifierToKey : Identifier -> Types.TKey
        identifierToKey id =
            case id of
                TermIdentifier t ->
                    Types.HiddenKey t

                MessageIdentifier m ->
                    Types.ExposedKey m

        contentsToValue : Content -> Types.TSegment
        contentsToValue cnt =
            case cnt of
                TextContent str ->
                    Types.Text str

                PlaceableContent (VarRef var) ->
                    Types.Interpolation var

                PlaceableContent (TermRef term) ->
                    Types.Reference term

                PlaceableContent (StringLit lit) ->
                    Types.Text lit

        resourceToInternalRep : Resource -> ( Types.TKey, Types.TValue )
        resourceToInternalRep res =
            case res of
                MessageResource msg ->
                    ( identifierToKey msg.identifier, List.NonEmpty.map contentsToValue msg.content )
    in
    List.map resourceToInternalRep ast_


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
    (succeed (\id firstLineCnt otherLines -> { identifier = id, content = combineLines <| firstLineCnt :: removeCommonIndentCnt otherLines })
        |= identifier
        |. spaces
        |= messageLine
        |= loop [] multilineHelper
    )
        |> andThen
            (\msg ->
                List.NonEmpty.fromList msg.content
                    |> Result.fromMaybe "Content of message cannot be empty"
                    |> resultToParser
                    |> map (Message msg.identifier)
            )


multilineHelper : List (List Content) -> Parser (Step (List (List Content)) (List (List Content)))
multilineHelper revLines =
    succeed identity
        |. oneOf [ end, Parser.token "\n" ]
        |= oneOf
            [ succeed (\cnts -> Loop <| cnts :: revLines)
                |. Parser.chompIf isSpace
                |= messageLine
            , succeed (Done <| List.reverse revLines)
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
    | TermRef String
    | StringLit String


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


placeable : Parser Placeable
placeable =
    let
        endsVar c =
            c == ' ' || c == '\n' || c == '\u{000D}' || c == '}'
    in
    succeed identity
        |. spaces
        |= oneOf
            [ token "$" |> andThen (\_ -> chompWhile (not << endsVar) |> getChompedString |> map VarRef)
            , token "-" |> andThen (\_ -> chompWhile (not << endsVar) |> getChompedString |> map TermRef)
            , stringLit |> map StringLit
            ]
        |. spaces
        |. token "}"


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
