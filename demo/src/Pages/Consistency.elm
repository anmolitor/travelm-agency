module Pages.Consistency exposing (init, viewExplanation)

import Accordion
import Dict exposing (Dict)
import File exposing (InputFile)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Http
import InputType exposing (InputType)
import Model exposing (Model)
import Msg exposing (Msg)
import Page
import Ports exposing (GeneratorMode)
import Translations exposing (I18n, Language)


init : Model -> ( Model, Cmd Msg )
init model =
    model
        |> Model.setInputTypeAndModeDefaults ( InputType.Json, Ports.Inline )
        |> Page.loadInputFiles
            { directory = "consistency"
            , files =
                ( { name = "example", language = "en" }
                , [ { name = "example", language = "de" }
                  ]
                )
            }
        |> Page.withTranslations Translations.loadConsistency


viewExplanation : Model -> List (Html Msg)
viewExplanation ({ i18n } as model) =
    [ Html.p [] [ Html.text <| Translations.consistencyPreamble i18n ]
    , Html.h2 [] [ Html.text <| Translations.consistencyMissingKeysHeadline i18n ]
    , Html.p [] <| Translations.consistencyMissingKeysBody [ class "highlighted" ] i18n
    , Html.h2 [] [ Html.text <| Translations.consistencyFallbackHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.consistencyFallbackBody i18n ]
    , Accordion.view
        { headline = Translations.sharedJsonHeadline i18n
        , content = Translations.consistencyFallbackSyntaxJson [ class "highlighted" ] i18n
        , id = "json_syntax"
        }
        model
    , Accordion.view
        { headline = Translations.sharedPropertiesHeadline i18n
        , content = Translations.consistencyFallbackSyntaxProperties [ class "highlighted" ] i18n
        , id = "properties_syntax"
        }
        model
    , Accordion.view
        { headline = Translations.sharedFluentHeadline i18n
        , content = Translations.consistencyFallbackSyntaxFluent [ class "highlighted" ] i18n
        , id = "fluent_syntax"
        }
        model
    ]
