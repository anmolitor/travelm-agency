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
    [ Html.p [] <| Translations.numberFormatPreamble [ class "highlighted" ] i18n
    , Html.h2 [] [ Html.text <| Translations.numberFormatIntlHeadline i18n ]
    , Html.p [] <| Translations.numberFormatIntlBody { a = [], code = [ class "highlighted" ] } i18n
    , Html.h2 [] [ Html.text <| Translations.numberFormatOptionsHeadline i18n ]
    , Html.p [] <| Translations.numberFormatOptionsBody { a = [], code = [ class "highlighted" ] } i18n
    ]
