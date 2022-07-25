module Pages.Terms exposing (..)

import Html exposing (Html)
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
        |> Page.loadInputFiles { directory = "terms", files = ( { name = "example", language = "en" }, [] ) }
        |> Page.withTranslations Translations.loadTerms


viewExplanation : Model -> List (Html Msg)
viewExplanation { i18n } =
    []
