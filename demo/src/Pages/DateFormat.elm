module Pages.DateFormat exposing (..)

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
        |> Page.loadInputFiles { directory = "date-format", files = ( { name = "example", language = "en" }, [] ) }
        |> Page.withTranslations Translations.loadDateFormat


viewExplanation : Model -> List (Html Msg)
viewExplanation { i18n } =
    []