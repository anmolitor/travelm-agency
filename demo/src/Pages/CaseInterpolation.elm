module Pages.CaseInterpolation exposing (..)

import InputType
import Model exposing (Model)
import Msg exposing (Msg)
import Page
import Ports
import Translations
import Html exposing (Html)


init : Model -> ( Model, Cmd Msg )
init model =
    model
        |> Model.setInputTypeAndModeDefaults ( InputType.Fluent, Ports.Inline )
        |> Page.loadInputFiles { directory = "case-interpolation", files = ( { name = "example", language = "en" }, [] ) }
        |> Page.withTranslations Translations.loadCaseInterpolation

viewExplanation : Model -> List (Html Msg)
viewExplanation { i18n } =
    []