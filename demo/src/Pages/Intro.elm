module Pages.Intro exposing (init, viewExplanation)

import Accordion
import Dict exposing (Dict)
import File exposing (InputFile)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Http
import InputType exposing (InputType)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Page
import Ports exposing (GeneratorMode)
import Translations exposing (I18n, Language)


init : Model -> ( Model, Cmd Msg )
init model =
    model
        |> Model.setInputTypeAndModeDefaults ( InputType.Json, Ports.Inline )
        |> Page.loadInputFiles { directory = "intro", files = ( { name = "example", language = "en" }, [] ) }
        |> Page.withTranslations Translations.loadIntro


viewExplanation : Model -> List (Html Msg)
viewExplanation ({ i18n } as model) =
    [ Html.p [] [ Html.text <| Translations.introPreamble i18n ]
    , Html.h2 [] [ Html.text <| Translations.introExplanationHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.introExplanationBody i18n ]
    , Html.h2 [] [ Html.text <| Translations.advantagesHeadline i18n ]
    , Accordion.view
        { headline = Translations.advantageReadabilityHeadline i18n
        , content = List.map (Html.map never) <| Translations.advantageReadabilityBody [ class "highlighted" ] i18n
        , id = "readability"
        }
        model
    , Accordion.view
        { headline = Translations.advantageTypeSafetyHeadline i18n
        , content =
            List.map (Html.map never) <|
                Translations.advantageTypeSafetyBody
                    { code = [ class "highlighted" ], list = [], item = [] }
                    i18n
        , id = "type_safety"
        }
        model
    , Accordion.view
        { headline = Translations.advantagePerformanceHeadline i18n
        , content =
            List.map (Html.map never) <|
                Translations.advantagePerformanceBody [ class "highlighted" ] i18n
        , id = "performance"
        }
        model
    , Html.h2 [] [ Html.text <| Translations.disadvantagesHeadline i18n ]
    , Accordion.view
        { headline = Translations.disadvantageProgrammabilityHeadline i18n
        , content = [ Html.text <| Translations.disadvantageProgrammabilityBody i18n ]
        , id = "programmability"
        }
        model
    , Accordion.view
        { headline = Translations.disadvantageToolchainHeadline i18n
        , content = [ Html.text <| Translations.disadvantageToolchainBody i18n ]
        , id = "toolchain"
        }
        model
    , Html.h2 [] [ Html.text <| Translations.tutorialHowtoHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.tutorialHowtoBody i18n ]
    , Html.h2 [] [ Html.text <| Translations.textsFeatureHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.textsFeatureBody i18n ]
    ]
