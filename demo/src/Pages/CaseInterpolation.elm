module Pages.CaseInterpolation exposing (..)

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
        |> Page.loadInputFiles { directory = "case-interpolation", files = ( { name = "example", language = "en" }, [] ) }
        |> Page.withTranslations Translations.loadCaseInterpolation


viewExplanation : Model -> List (Html Msg)
viewExplanation { i18n } =
    [ Html.p [] <| Translations.caseInterpolationPreamble [ class "highlighted" ] i18n
    , Html.h2 [] [ Html.text <| Translations.caseInterpolationSyntaxHeadline i18n ]
    , Html.p [] <| Translations.caseInterpolationSyntaxBody [ class "highlighted" ] i18n
    , Html.h2 [] [ Html.text <| Translations.caseInterpolationAdviceHeadline i18n ]
    , Html.p [] <| Translations.caseInterpolationAdviceBody [ class "highlighted" ] i18n
    ]
