module Pages.Terms exposing (..)

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
        |> Page.loadInputFiles { directory = "terms", files = ( { name = "example", language = "en" }, [] ) }
        |> Page.withTranslations Translations.loadTerms


viewExplanation : Model -> List (Html Msg)
viewExplanation { i18n } =
    [ Html.p [] [ Html.text <| Translations.termsPreamble i18n ]
    , Html.h2 [] [ Html.text <| Translations.termsSyntaxHeadline i18n ]
    , Html.p [] <| Translations.termsSyntaxBody [ class "highlighted" ] i18n
    , Html.h2 [] [ Html.text <| Translations.termsFluentOnlyHeadline i18n ]
    , Html.p [] <| Translations.termsFluentOnlyBody [ class "highlighted" ] i18n
    ]
