module Routes exposing (..)

import InputType exposing (InputType)
import Ports
import Url
import Url.Builder exposing (absolute, string)
import Url.Parser exposing ((<?>), Parser, map, oneOf, parse, s, top)
import Url.Parser.Query as Query


type Route
    = Intro (Maybe Ports.GeneratorMode) (Maybe InputType)
    | NotFound Url.Url


parser : Parser (Route -> a) a
parser =
    let
        modeParser : Query.Parser (Maybe Ports.GeneratorMode)
        modeParser =
            Query.string "mode"
                |> Query.map (Maybe.andThen Ports.generatorModeFromString)

        inputParser : Query.Parser (Maybe InputType)
        inputParser =
            Query.string "input"
                |> Query.map (Maybe.andThen InputType.fromString)
    in
    oneOf
        [ map Intro (s "intro" <?> modeParser <?> inputParser)
        ]


fromUrl : Url.Url -> Route
fromUrl url =
    parse parser url |> Maybe.withDefault (NotFound url)


toUrl : Route -> String
toUrl route =
    case route of
        Intro mode inputType ->
            absolute [ "intro" ] <|
                List.filterMap identity
                    [ Maybe.map (string "mode" << Ports.generatorModeToString) mode
                    , Maybe.map (string "input" << InputType.toString) inputType
                    ]

        NotFound url ->
            Url.toString url


setInputType : InputType -> Route -> Route
setInputType inputType route =
    case route of
        Intro mode _ ->
            Intro mode (Just inputType)

        NotFound _ ->
            route


setGeneratorMode : Ports.GeneratorMode -> Route -> Route
setGeneratorMode mode route =
    case route of
        Intro _ inputType ->
            Intro (Just mode) inputType

        NotFound _ ->
            route
