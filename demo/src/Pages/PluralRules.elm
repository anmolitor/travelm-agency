module Pages.PluralRules exposing (..)

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
        |> Page.loadInputFiles { directory = "plural-rules", files = ( { name = "example", language = "en" }, [] ) }
        |> Page.withTranslations Translations.loadPluralRules


viewExplanation : Model -> List (Html Msg)
viewExplanation { i18n } =
    [ Html.map never <| Html.p [] <| Translations.pluralRulesPreamble i18n []
    , Html.h2 [] [ Html.text <| Translations.pluralRulesSyntaxHeadline i18n ]
    , Html.map never <| Html.p [] <| Translations.pluralRulesSyntaxBody i18n { a = [], code = [ class "highlighted" ] }
    , Html.h2 [] [ Html.text <| Translations.pluralRulesIntlHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.pluralRulesIntlBody i18n ]
    ]
