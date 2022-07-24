module Pages.Html exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class)
import InputType
import Model exposing (Model)
import Msg exposing (Msg)
import Page
import Ports
import Translations


init : Model -> ( Model, Cmd Msg )
init model =
    model
        |> Model.setInputTypeAndModeDefaults ( InputType.Json, Ports.Inline )
        |> Page.loadInputFiles { directory = "html", files = ( { name = "example", language = "en" }, [] ) }
        |> Page.withTranslations Translations.loadHtml


viewExplanation : Model -> List (Html Msg)
viewExplanation { i18n } =
    [ Html.map never <| Html.p [] <| Translations.htmlPreamble i18n [ class "highlighted" ]
    , Html.h2 [] [ Html.text <| Translations.htmlBasicsHeadline i18n ]
    , Html.map never <| Html.p [] <| Translations.htmlBasicsBody i18n [ class "highlighted" ]
    , Html.h2 [] [ Html.text <| Translations.htmlIdHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.htmlIdBody i18n ]
    , Html.h2 [] [ Html.text <| Translations.htmlSecurityHeadline i18n ]
    , Html.map never <| Html.p [] <| Translations.htmlSecurityBody i18n []
    ]
