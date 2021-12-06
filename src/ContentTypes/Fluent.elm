module ContentTypes.Fluent exposing (..)

import Parser exposing ((|.), (|=), Parser, Step(..), andThen, chompUntilEndOr, chompWhile, end, getChompedString, loop, map, oneOf, problem, spaces, succeed, token)


type alias AST =
    List Resource


type Resource
    = EntryResource Entry
    | BlankBlock
    | Junk String


type Entry
    = MessageEntry Message
    | TermEntry Message


type alias Message =
    { identifier : Identifier
    , content : List Content
    }


type Content
    = TextContent String
    | PlaceableContent Placeable


message : Parser Message
message =
    succeed Message
        |= identifier
        |. spaces
        |= loop []
            (\revCnt ->
                oneOf
                    [ succeed (\cnt -> Loop (cnt :: revCnt))
                        |= content
                    , succeed () |> map (\_ -> Done <| List.reverse revCnt)
                    ]
            )


content : Parser Content
content =
    oneOf
        [ succeed PlaceableContent |. token "{" |= placeable
        , chompUntilEndOr "{"
            |> getChompedString
            |> andThen
                (\cnt ->
                    if cnt == "" then
                        problem "Expected more content."

                    else
                        succeed (TextContent cnt)
                )
        ]


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


text : Parser String
text =
    chompUntilEndOr "{" |> getChompedString


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
