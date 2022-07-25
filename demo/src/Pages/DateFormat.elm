module Pages.DateFormat exposing (..)

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
        |> Page.loadInputFiles { directory = "date-format", files = ( { name = "example", language = "en" }, [] ) }
        |> Page.withTranslations Translations.loadDateFormat


viewExplanation : Model -> List (Html Msg)
viewExplanation { i18n } =
    [ Html.p [] [ Html.text <| Translations.dateFormatPreamble i18n ]
    , Html.h2 [] [ Html.text <| Translations.dateFormatIntlHeadline i18n ]
    , Html.map never <| Html.p [] <| Translations.dateFormatIntlBody { a = [], code = [ class "highlighted" ] } i18n
    , Html.h2 [] [ Html.text <| Translations.dateFormatCompileTimeHeadline i18n ]
    , Html.map never <| Html.p [] [ Html.text <| Translations.dateFormatCompileTimeBody i18n ]
    ]
