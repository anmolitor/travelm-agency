module Routes exposing (..)

import InputType exposing (InputType)
import Ports
import Url
import Url.Builder exposing (absolute, string)
import Url.Parser exposing ((<?>), Parser, map, oneOf, parse, s, top)
import Url.Parser.Query as Query


type Route
    = Intro (Maybe Ports.GeneratorMode) (Maybe InputType)
    | Interpolation (Maybe Ports.GeneratorMode) (Maybe InputType)
    | NotFound Url.Url


order : List (Maybe Ports.GeneratorMode -> Maybe InputType -> Route)
order =
    [ Intro, Interpolation ]


next : Route -> Maybe Route
next route =
    let
        inputType =
            getInputType route

        generatorMode =
            getGeneratorMode route

        appliedOrder =
            List.map (\r -> r generatorMode inputType) order
    in
    List.drop 1 appliedOrder
        |> List.map2 Tuple.pair appliedOrder
        |> List.filter (\( curr, _ ) -> curr == route)
        |> List.map Tuple.second
        |> List.head


previous : Route -> Maybe Route
previous route =
    let
        inputType =
            getInputType route

        generatorMode =
            getGeneratorMode route

        appliedOrder =
            List.map (\r -> r generatorMode inputType) order
    in
    List.drop 1 appliedOrder
        |> List.map2 Tuple.pair appliedOrder
        |> List.filter (\( _, curr ) -> curr == route)
        |> List.map Tuple.first
        |> List.head


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
        , map Interpolation (s "interpolation" <?> modeParser <?> inputParser)
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

        Interpolation mode inputType ->
            absolute [ "interpolation" ] <|
                List.filterMap identity
                    [ Maybe.map (string "mode" << Ports.generatorModeToString) mode
                    , Maybe.map (string "input" << InputType.toString) inputType
                    ]

        NotFound url ->
            Url.toString url


getInputType : Route -> Maybe InputType
getInputType route =
    case route of
        Intro _ inputType ->
            inputType

        Interpolation _ inputType ->
            inputType

        NotFound _ ->
            Nothing


getGeneratorMode : Route -> Maybe Ports.GeneratorMode
getGeneratorMode route =
    case route of
        Intro mode _ ->
            mode

        Interpolation mode _ ->
            mode

        NotFound _ ->
            Nothing


setInputType : InputType -> Route -> Route
setInputType inputType route =
    case route of
        Intro mode _ ->
            Intro mode (Just inputType)

        Interpolation mode _ ->
            Interpolation mode (Just inputType)

        NotFound _ ->
            route


setGeneratorMode : Ports.GeneratorMode -> Route -> Route
setGeneratorMode mode route =
    case route of
        Intro _ inputType ->
            Intro (Just mode) inputType

        Interpolation _ inputType ->
            Interpolation (Just mode) inputType

        NotFound _ ->
            route
