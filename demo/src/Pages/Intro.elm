module Pages.Intro exposing (init, viewExplanation)

import Accordion
import Dict exposing (Dict)
import File exposing (InputFile)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Http
import InputType exposing (InputType)
import Material.Icons.Types exposing (Coloring(..))
import Ports exposing (GeneratorMode)
import Translations exposing (I18n, Language)


type alias Model model =
    { model
        | generatorMode : GeneratorMode
        , inputType : InputType
        , language : Language
        , activeInputFilePath : String
        , basePath : String
    }


init :
    { onInputLoad : Result Http.Error InputFile -> msg, onTranslationLoad : Result Http.Error (I18n -> I18n) -> msg }
    -> Model model
    -> Maybe GeneratorMode
    -> Maybe InputType
    -> ( Model model, Cmd msg )
init events model mayMode mayInputType =
    let
        generatorMode =
            mayMode |> Maybe.withDefault Ports.Inline

        inputType =
            mayInputType |> Maybe.withDefault InputType.Json

        filePathToLoad =
            "example.en." ++ InputType.toString inputType
    in
    ( { model | generatorMode = generatorMode, inputType = inputType, activeInputFilePath = filePathToLoad }
    , Cmd.batch
        [ Translations.loadIntro { language = model.language, path = "/i18n", onLoad = events.onTranslationLoad }
        , Http.get
            { url = "/intro/" ++ filePathToLoad
            , expect =
                Http.expectString
                    (Result.map
                        (\content ->
                            { name = "example"
                            , language = "en"
                            , extension = InputType.toString inputType
                            , content = content
                            }
                        )
                        >> events.onInputLoad
                    )
            }
        ]
    )


viewExplanation :
    { model | i18n : I18n, openAccordionElements : Dict String Int }
    -> { onToggleAccordionEl : String -> Int -> msg }
    -> List (Html msg)
viewExplanation { i18n, openAccordionElements } events =
    [ Html.p [] [ Html.text <| Translations.introPreamble i18n ]
    , Html.h2 [] [ Html.text <| Translations.introExplanationHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.introExplanationBody i18n ]
    , Html.h2 [] [ Html.text <| Translations.advantagesHeadline i18n ]
    , Accordion.view
        { headline = Translations.advantageReadabilityHeadline i18n
        , content = List.map (Html.map never) <| Translations.advantageReadabilityBody i18n [ class "highlighted" ]
        , onToggle = events.onToggleAccordionEl "readability"
        , height = Dict.get "readability" openAccordionElements |> Maybe.withDefault 0
        }
    , Accordion.view
        { headline = Translations.advantageTypeSafetyHeadline i18n
        , content =
            List.map (Html.map never) <|
                Translations.advantageTypeSafetyBody i18n
                    { code = [ class "highlighted" ], list = [], item = [] }
        , onToggle = events.onToggleAccordionEl "type_safety"
        , height = Dict.get "type_safety" openAccordionElements |> Maybe.withDefault 0
        }
    , Accordion.view
        { headline = Translations.advantagePerformanceHeadline i18n
        , content =
            List.map (Html.map never) <|
                Translations.advantagePerformanceBody i18n [ class "highlighted" ]
        , onToggle = events.onToggleAccordionEl "performance"
        , height = Dict.get "performance" openAccordionElements |> Maybe.withDefault 0
        }
    , Html.h2 [] [ Html.text <| Translations.disadvantagesHeadline i18n ]
    , Accordion.view
        { headline = Translations.disadvantageProgrammabilityHeadline i18n
        , content = [ Html.text <| Translations.disadvantageProgrammabilityBody i18n ]
        , onToggle = events.onToggleAccordionEl "programmability"
        , height = Dict.get "programmability" openAccordionElements |> Maybe.withDefault 0
        }
    , Accordion.view
        { headline = Translations.disadvantageToolchainHeadline i18n
        , content = [ Html.text <| Translations.disadvantageToolchainBody i18n ]
        , onToggle = events.onToggleAccordionEl "toolchain"
        , height = Dict.get "toolchain" openAccordionElements |> Maybe.withDefault 0
        }
    , Html.h2 [] [ Html.text <| Translations.tutorialHowtoHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.tutorialHowtoBody i18n ]
    , Html.h2 [] [ Html.text <| Translations.textsFeatureHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.textsFeatureBody i18n ]
    ]
