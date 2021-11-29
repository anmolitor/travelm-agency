module ContentTypes.Properties exposing (parse)

import Json.Decode as D
import Parser exposing ((|.), (|=), Parser)
import Placeholder.Internal as Placeholder
import Result.Extra
import Types exposing (I18nPairs)
import Util


parse : String -> D.Decoder I18nPairs
parse =
    parseProperties >> Util.resultToDecoder


keyParser : Parser (List String)
keyParser =
    Parser.loop []
        (\kSegments ->
            Parser.succeed (\kSeg step -> step (kSeg :: kSegments))
                |= (Parser.getChompedString <| Parser.chompWhile Char.isAlphaNum)
                |= Parser.oneOf
                    [ Parser.succeed (List.reverse >> Parser.Done) |. whitespace |. Parser.symbol "="
                    , Parser.succeed Parser.Loop |. Parser.symbol "."
                    ]
        )


valueParser : Parser String
valueParser =
    Parser.succeed identity
        |. whitespace
        |= (Parser.getChompedString <| Parser.chompUntilEndOr "\n")


syntax : Placeholder.Syntax
syntax =
    { startSymbol = "{{", endSymbol = "}}" }


keyValueParser : Parser ( List String, String )
keyValueParser =
    Parser.succeed Tuple.pair
        |= keyParser
        |= valueParser


parseProperties : String -> Result String I18nPairs
parseProperties =
    String.split "\n"
        >> List.filter (\str -> not <| String.isEmpty str || String.startsWith "#" str)
        >> List.map
            (Parser.run keyValueParser
                >> Result.mapError Parser.deadEndsToString
                >> Result.map (Tuple.mapFirst Util.keyToName)
                >> Result.andThen (Result.Extra.combineMapSecond <| Placeholder.parseTemplate syntax)
            )
        >> Result.Extra.combine


whitespace : Parser ()
whitespace =
    Parser.chompWhile (\c -> c == ' ')
