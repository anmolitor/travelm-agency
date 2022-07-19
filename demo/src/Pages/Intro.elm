module Pages.Intro exposing (init, viewExplanation)

import Dict exposing (Dict)
import File exposing (InputFile)
import Html exposing (Html)
import Http
import InputType exposing (InputType)
import Ports exposing (GeneratorMode)
import Translations exposing (I18n, Language)


type alias Model model =
    { model
        | generatorMode : GeneratorMode
        , inputType : InputType
        , language : Language
        , activeInputFilePath : String
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
        [ Translations.loadIntro { language = model.language, path = "dist/i18n", onLoad = events.onTranslationLoad }
        , Http.get
            { url = "intro/" ++ filePathToLoad
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


viewExplanation : { model | i18n : I18n } -> List (Html Never)
viewExplanation { i18n } =
    [ Html.h1 [] [ Html.text <| Translations.introHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.introPreamble i18n ]
    , Html.h2 [] [ Html.text <| Translations.introExplanationHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.introExplanationBody i18n ]
    , Html.h2 [] [ Html.text <| Translations.advantagesHeadline i18n ]
    , Html.h3 [] [ Html.text <| Translations.advantageReadabilityHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.advantageReadabilityBody i18n ]
    , Html.h3 [] [ Html.text <| Translations.advantageTypeSafetyHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.advantageTypeSafetyBody i18n ]
    , Html.h3 [] [ Html.text <| Translations.advantagePerformanceHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.advantagePerformanceBody i18n ]
    , Html.h2 [] [ Html.text <| Translations.disadvantagesHeadline i18n ]
    , Html.h3 [] [ Html.text <| Translations.disadvantageProgrammabilityHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.disadvantageProgrammabilityBody i18n ]
    , Html.h3 [] [ Html.text <| Translations.disadvantageToolchainHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.disadvantageToolchainBody i18n ]
    , Html.h2 [] [ Html.text <| Translations.tutorialHowtoHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.tutorialHowtoBody i18n ]
    , Html.h2 [] [ Html.text <| Translations.textsFeatureHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.textsFeatureBody i18n ]
    ]
