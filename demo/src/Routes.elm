module Routes exposing (..)

import InputType exposing (InputType)
import Ports
import Url
import Url.Builder exposing (absolute, string)
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, parse, s)
import Url.Parser.Query as Query


type Route
    = Intro (Maybe Ports.GeneratorMode) (Maybe InputType)
    | Interpolation (Maybe Ports.GeneratorMode) (Maybe InputType)
    | Consistency (Maybe Ports.GeneratorMode) (Maybe InputType)
    | Bundles (Maybe Ports.GeneratorMode) (Maybe InputType)
    | NotFound Url.Url


order : List (Maybe Ports.GeneratorMode -> Maybe InputType -> Route)
order =
    [ Intro, Interpolation, Consistency, Bundles ]


next : Route -> Maybe Route
next route =
    let
        ( inputType, generatorMode ) =
            getParams route

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
        ( inputType, generatorMode ) =
            getParams route

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
        , map Consistency (s "consistency" <?> modeParser <?> inputParser)
        , map Bundles (s "bundles" <?> modeParser <?> inputParser)
        ]


fromUrl : String -> Url.Url -> Route
fromUrl basePath url =
    parse (s basePath </> parser) url |> Maybe.withDefault (NotFound url)


toUrl : String -> Route -> String
toUrl basePath route =
    let
        default path mode inputType =
            absolute [ basePath, path ] <|
                List.filterMap identity
                    [ Maybe.map (string "mode" << Ports.generatorModeToString) mode
                    , Maybe.map (string "input" << InputType.toString) inputType
                    ]
    in
    case route of
        Intro mode inputType ->
            default "intro" mode inputType

        Interpolation mode inputType ->
            default "interpolation" mode inputType

        Consistency mode inputType ->
            default "consistency" mode inputType

        Bundles mode inputType ->
            default "bundles" mode inputType

        NotFound url ->
            Url.toString url


getParams : Route -> ( Maybe InputType, Maybe Ports.GeneratorMode )
getParams route =
    case route of
        Intro mode inputType ->
            ( inputType, mode )

        Interpolation mode inputType ->
            ( inputType, mode )

        Consistency mode inputType ->
            ( inputType, mode )

        Bundles mode inputType ->
            ( inputType, mode )

        NotFound _ ->
            ( Nothing, Nothing )


setInputType : InputType -> Route -> Route
setInputType inputType route =
    case route of
        Intro mode _ ->
            Intro mode (Just inputType)

        Interpolation mode _ ->
            Interpolation mode (Just inputType)

        Consistency mode _ ->
            Consistency mode (Just inputType)

        Bundles mode _ ->
            Bundles mode (Just inputType)

        NotFound _ ->
            route


setGeneratorMode : Ports.GeneratorMode -> Route -> Route
setGeneratorMode mode route =
    case route of
        Intro _ inputType ->
            Intro (Just mode) inputType

        Interpolation _ inputType ->
            Interpolation (Just mode) inputType

        Consistency _ inputType ->
            Consistency (Just mode) inputType

        Bundles _ inputType ->
            Bundles (Just mode) inputType

        NotFound _ ->
            route
