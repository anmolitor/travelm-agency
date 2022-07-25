module Pages.NumberFormat exposing (..)

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
        |> Model.setInputTypeAndModeDefaults ( InputType.Fluent, Ports.Inline )
        |> Page.loadInputFiles { directory = "number-format", files = ( { name = "example", language = "en" }, [] ) }
        |> Page.withTranslations Translations.loadNumberFormat


viewExplanation : Model -> List (Html Msg)
viewExplanation { i18n } =
    [ Html.map never <| Html.p [] <| Translations.numberFormatPreamble i18n [ class "highlighted" ]
    , Html.h2 [] [ Html.text <| Translations.numberFormatIntlHeadline i18n ]
    , Html.map never <| Html.p [] <| Translations.numberFormatIntlBody i18n { a = [], code = [ class "highlighted" ] }
    , Html.h2 [] [ Html.text <| Translations.numberFormatOptionsHeadline i18n ]
    , Html.map never <| Html.p [] <| Translations.numberFormatOptionsBody i18n { a = [], code = [ class "highlighted" ] }
    ]
