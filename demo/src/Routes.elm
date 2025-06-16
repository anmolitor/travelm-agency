module Routes exposing (..)

import InputType exposing (InputType)
import Ports
import Url
import Url.Builder exposing (absolute, string)
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, parse, s)
import Url.Parser.Query as Query


type Route
    = Intro (Maybe Ports.GeneratorMode) (Maybe InputType) (Maybe String)
    | Interpolation (Maybe Ports.GeneratorMode) (Maybe InputType) (Maybe String)
    | Consistency (Maybe Ports.GeneratorMode) (Maybe InputType) (Maybe String)
    | Language (Maybe Ports.GeneratorMode) (Maybe InputType) (Maybe String)
    | Bundles (Maybe Ports.GeneratorMode) (Maybe InputType) (Maybe String)
    | Html (Maybe Ports.GeneratorMode) (Maybe InputType) (Maybe String)
    | Terms (Maybe Ports.GeneratorMode) (Maybe String)
    | CaseInterpolation (Maybe Ports.GeneratorMode) (Maybe String)
    | NumberFormat (Maybe Ports.GeneratorMode) (Maybe String)
    | DateFormat (Maybe Ports.GeneratorMode) (Maybe String)
    | PluralRules (Maybe Ports.GeneratorMode) (Maybe String)
    | NotFound Url.Url


fixedInputType : (Maybe Ports.GeneratorMode -> Maybe String -> Route) -> Maybe Ports.GeneratorMode -> Maybe InputType -> Maybe String -> Route
fixedInputType route mode _ customHtmlModule =
    route mode customHtmlModule


order : List (Maybe Ports.GeneratorMode -> Maybe InputType -> Maybe String -> Route)
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
        { inputType, generatorMode, customHtmlModule } =
            getParams route

        appliedOrder =
            List.map (\r -> r generatorMode inputType customHtmlModule) order
    in
    List.drop 1 appliedOrder
        |> List.map2 Tuple.pair appliedOrder
        |> List.filter (\( curr, _ ) -> curr == route)
        |> List.map Tuple.second
        |> List.head


previous : Route -> Maybe Route
previous route =
    let
        { inputType, generatorMode, customHtmlModule } =
            getParams route

        appliedOrder =
            List.map (\r -> r generatorMode inputType customHtmlModule) order
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

        customHtmlParser : Query.Parser (Maybe String)
        customHtmlParser =
            Query.string "customHtmlModule"
    in
    oneOf
        [ map Intro (s "intro" <?> modeParser <?> inputParser <?> customHtmlParser)
        , map Interpolation (s "interpolation" <?> modeParser <?> inputParser <?> customHtmlParser)
        , map Consistency (s "consistency" <?> modeParser <?> inputParser <?> customHtmlParser)
        , map Language (s "language" <?> modeParser <?> inputParser <?> customHtmlParser)
        , map Bundles (s "bundles" <?> modeParser <?> inputParser <?> customHtmlParser)
        , map Html (s "html" <?> modeParser <?> inputParser <?> customHtmlParser)
        , map Terms (s "terms" <?> modeParser <?> customHtmlParser)
        , map CaseInterpolation (s "case-interpolation" <?> modeParser <?> customHtmlParser)
        , map NumberFormat (s "number-format" <?> modeParser <?> customHtmlParser)
        , map DateFormat (s "date-format" <?> modeParser <?> customHtmlParser)
        , map PluralRules (s "plural-rules" <?> modeParser <?> customHtmlParser)
        ]


fromUrl : String -> Url.Url -> Route
fromUrl basePath url =
    parse (s basePath </> parser) url |> Maybe.withDefault (NotFound url)


toUrl : String -> Route -> String
toUrl basePath route =
    let
        default path mode inputType customHtmlModule =
            absolute [ basePath, path ] <|
                List.filterMap identity
                    [ Maybe.map (string "mode" << Ports.generatorModeToString) mode
                    , Maybe.map (string "input" << InputType.toString) inputType
                    , Maybe.map (string "customHtmlModule") customHtmlModule
                    ]

        fluentOnly path mode =
            default path mode Nothing
    in
    case route of
        Intro mode inputType customHtmlModule ->
            default "intro" mode inputType customHtmlModule

        Interpolation mode inputType customHtmlModule ->
            default "interpolation" mode inputType customHtmlModule

        Consistency mode inputType customHtmlModule ->
            default "consistency" mode inputType customHtmlModule

        Language mode inputType customHtmlModule ->
            default "language" mode inputType customHtmlModule

        Bundles mode inputType customHtmlModule ->
            default "bundles" mode inputType customHtmlModule

        Html mode inputType customHtmlModule ->
            default "html" mode inputType customHtmlModule

        Terms mode customHtmlModule ->
            fluentOnly "terms" mode customHtmlModule

        CaseInterpolation mode customHtmlModule ->
            fluentOnly "case-interpolation" mode customHtmlModule

        NumberFormat mode customHtmlModule ->
            fluentOnly "number-format" mode customHtmlModule

        DateFormat mode customHtmlModule ->
            fluentOnly "date-format" mode customHtmlModule

        PluralRules mode customHtmlModule ->
            fluentOnly "plural-rules" mode customHtmlModule

        NotFound url ->
            Url.toString url


getParams : Route -> { inputType : Maybe InputType, generatorMode : Maybe Ports.GeneratorMode, customHtmlModule : Maybe String }
getParams route =
    let
        default =
            { inputType = Nothing, generatorMode = Nothing, customHtmlModule = Nothing }
    in
    case route of
        Intro mode inputType customHtmlModule ->
            { default | inputType = inputType, generatorMode = mode, customHtmlModule = customHtmlModule }

        Interpolation mode inputType customHtmlModule ->
            { default | inputType = inputType, generatorMode = mode, customHtmlModule = customHtmlModule }

        Consistency mode inputType customHtmlModule ->
            { default | inputType = inputType, generatorMode = mode, customHtmlModule = customHtmlModule }

        Language mode inputType customHtmlModule ->
            { default | inputType = inputType, generatorMode = mode, customHtmlModule = customHtmlModule }

        Bundles mode inputType customHtmlModule ->
            { default | inputType = inputType, generatorMode = mode, customHtmlModule = customHtmlModule }

        Html mode inputType customHtmlModule ->
            { default | inputType = inputType, generatorMode = mode, customHtmlModule = customHtmlModule }

        Terms mode customHtmlModule ->
            { default | generatorMode = mode, customHtmlModule = customHtmlModule }

        CaseInterpolation mode customHtmlModule ->
            { default | generatorMode = mode, customHtmlModule = customHtmlModule }

        NumberFormat mode customHtmlModule ->
            { default | generatorMode = mode, customHtmlModule = customHtmlModule }

        DateFormat mode customHtmlModule ->
            { default | generatorMode = mode, customHtmlModule = customHtmlModule }

        PluralRules mode customHtmlModule ->
            { default | generatorMode = mode, customHtmlModule = customHtmlModule }

        NotFound _ ->
            default


setInputType : InputType -> Route -> Route
setInputType inputType route =
    case route of
        Intro mode _ customHtmlModule ->
            Intro mode (Just inputType) customHtmlModule

        Interpolation mode _ customHtmlModule ->
            Interpolation mode (Just inputType) customHtmlModule

        Consistency mode _ customHtmlModule ->
            Consistency mode (Just inputType) customHtmlModule

        Language mode _ customHtmlModule ->
            Language mode (Just inputType) customHtmlModule

        Bundles mode _ customHtmlModule ->
            Bundles mode (Just inputType) customHtmlModule

        Html mode _ customHtmlModule ->
            Html mode (Just inputType) customHtmlModule

        Terms _ _ ->
            route

        CaseInterpolation _ _ ->
            route

        NumberFormat _ _ ->
            route

        DateFormat _ _ ->
            route

        PluralRules _ _ ->
            route

        NotFound _ ->
            route


setGeneratorMode : Ports.GeneratorMode -> Route -> Route
setGeneratorMode mode route =
    case route of
        Intro _ inputType customHtmlModule ->
            Intro (Just mode) inputType customHtmlModule

        Interpolation _ inputType customHtmlModule ->
            Interpolation (Just mode) inputType customHtmlModule

        Consistency _ inputType customHtmlModule ->
            Consistency (Just mode) inputType customHtmlModule

        Language _ inputType customHtmlModule ->
            Language (Just mode) inputType customHtmlModule

        Bundles _ inputType customHtmlModule ->
            Bundles (Just mode) inputType customHtmlModule

        Html _ inputType customHtmlModule ->
            Html (Just mode) inputType customHtmlModule

        Terms _ customHtmlModule ->
            Terms (Just mode) customHtmlModule

        CaseInterpolation _ customHtmlModule ->
            CaseInterpolation (Just mode) customHtmlModule

        NumberFormat _ customHtmlModule ->
            NumberFormat (Just mode) customHtmlModule

        DateFormat _ customHtmlModule ->
            DateFormat (Just mode) customHtmlModule

        PluralRules _ customHtmlModule ->
            PluralRules (Just mode) customHtmlModule

        NotFound _ ->
            route


setHtmlModule : String -> Route -> Route
setHtmlModule customHtmlModule route =
    case route of
        Intro mode inputType _ ->
            Intro mode inputType (Just customHtmlModule)

        Interpolation mode inputType _ ->
            Interpolation mode inputType (Just customHtmlModule)

        Consistency mode inputType _ ->
            Consistency mode inputType (Just customHtmlModule)

        Language mode inputType _ ->
            Language mode inputType (Just customHtmlModule)

        Bundles mode inputType _ ->
            Bundles mode inputType (Just customHtmlModule)

        Html mode inputType _ ->
            Html mode inputType (Just customHtmlModule)

        Terms mode _ ->
            Terms mode (Just customHtmlModule)

        CaseInterpolation mode _ ->
            CaseInterpolation mode (Just customHtmlModule)

        NumberFormat mode _ ->
            NumberFormat mode (Just customHtmlModule)

        DateFormat mode _ ->
            DateFormat mode (Just customHtmlModule)

        PluralRules mode _ ->
            PluralRules mode (Just customHtmlModule)

        NotFound _ ->
            route
