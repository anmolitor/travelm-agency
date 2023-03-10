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
    | Language (Maybe Ports.GeneratorMode) (Maybe InputType)
    | Bundles (Maybe Ports.GeneratorMode) (Maybe InputType)
    | Html (Maybe Ports.GeneratorMode) (Maybe InputType)
    | Terms (Maybe Ports.GeneratorMode)
    | CaseInterpolation (Maybe Ports.GeneratorMode)
    | NumberFormat (Maybe Ports.GeneratorMode)
    | DateFormat (Maybe Ports.GeneratorMode)
    | PluralRules (Maybe Ports.GeneratorMode)
    | NotFound Url.Url


fixedInputType : (Maybe Ports.GeneratorMode -> Route) -> Maybe Ports.GeneratorMode -> Maybe InputType -> Route
fixedInputType route mode _ =
    route mode


order : List (Maybe Ports.GeneratorMode -> Maybe InputType -> Route)
order =
    [ Intro
    , Interpolation
    , Consistency
    , Language
    , Bundles
    , Html
    , fixedInputType Terms
    , fixedInputType CaseInterpolation
    , fixedInputType NumberFormat
    , fixedInputType DateFormat
    , fixedInputType PluralRules
    ]


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
        , map Language (s "language" <?> modeParser <?> inputParser)
        , map Bundles (s "bundles" <?> modeParser <?> inputParser)
        , map Html (s "html" <?> modeParser <?> inputParser)
        , map Terms (s "terms" <?> modeParser)
        , map CaseInterpolation (s "case-interpolation" <?> modeParser)
        , map NumberFormat (s "number-format" <?> modeParser)
        , map DateFormat (s "date-format" <?> modeParser)
        , map PluralRules (s "plural-rules" <?> modeParser)
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

        fluentOnly path mode =
            default path mode Nothing
    in
    case route of
        Intro mode inputType ->
            default "intro" mode inputType

        Interpolation mode inputType ->
            default "interpolation" mode inputType

        Consistency mode inputType ->
            default "consistency" mode inputType

        Language mode inputType ->
            default "language" mode inputType

        Bundles mode inputType ->
            default "bundles" mode inputType

        Html mode inputType ->
            default "html" mode inputType

        Terms mode ->
            fluentOnly "terms" mode

        CaseInterpolation mode ->
            fluentOnly "case-interpolation" mode

        NumberFormat mode ->
            fluentOnly "number-format" mode

        DateFormat mode ->
            fluentOnly "date-format" mode

        PluralRules mode ->
            fluentOnly "plural-rules" mode

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

        Language mode inputType ->
            ( inputType, mode )

        Bundles mode inputType ->
            ( inputType, mode )

        Html mode inputType ->
            ( inputType, mode )

        Terms mode ->
            ( Nothing, mode )

        CaseInterpolation mode ->
            ( Nothing, mode )

        NumberFormat mode ->
            ( Nothing, mode )

        DateFormat mode ->
            ( Nothing, mode )

        PluralRules mode ->
            ( Nothing, mode )

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

        Language mode _ ->
            Language mode (Just inputType)

        Bundles mode _ ->
            Bundles mode (Just inputType)

        Html mode _ ->
            Html mode (Just inputType)

        Terms _ ->
            route

        CaseInterpolation _ ->
            route

        NumberFormat _ ->
            route

        DateFormat _ ->
            route

        PluralRules _ ->
            route

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

        Language _ inputType ->
            Language (Just mode) inputType

        Bundles _ inputType ->
            Bundles (Just mode) inputType

        Html _ inputType ->
            Html (Just mode) inputType

        Terms _ ->
            Terms (Just mode)

        CaseInterpolation _ ->
            CaseInterpolation (Just mode)

        NumberFormat _ ->
            NumberFormat (Just mode)

        DateFormat _ ->
            DateFormat (Just mode)

        PluralRules _ ->
            PluralRules (Just mode)

        NotFound _ ->
            route
