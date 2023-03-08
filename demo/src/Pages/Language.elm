module Pages.Language exposing (init, viewExplanation)

import Accordion
import Html exposing (Html)
import Html.Attributes exposing (class)
import InputType
import Model exposing (Model)
import Msg exposing (Msg(..))
import Page
import Ports
import Translations


init : Model -> ( Model, Cmd Msg )
init model =
    model
        |> Model.setInputTypeAndModeDefaults ( InputType.Json, Ports.Inline )
        |> Page.loadInputFiles
            { directory = "language"
            , files =
                ( { name = "example", language = "en" }
                , [ { name = "example", language = "de" }
                  , { name = "example", language = "fr" }
                  , { name = "example", language = "ja" }
                  ]
                )
            }
        |> Page.withTranslations Translations.loadLanguage


viewExplanation : Model -> List (Html Msg)
viewExplanation { i18n } =
    [ Html.p [] [ Html.text <| Translations.languagePreamble i18n ]
    , Html.h2 [] [ Html.text <| Translations.languageConversionsHeadline i18n ]
    , Html.p [] <| Translations.languageConversionsBody [ class "highlighted" ] i18n
    , Html.h2 [] [ Html.text <| Translations.languageActiveLanguageHeadline i18n ]
    , Html.p [] <| Translations.languageActiveLanguageBody [ class "highlighted" ] i18n
    ]
