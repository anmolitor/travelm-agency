module Pages.Interpolation exposing (init, viewExplanation)

import Accordion
import Html exposing (Html)
import Html.Attributes exposing (class)
import InputType
import Model exposing (Model)
import Msg exposing (Msg(..))
import Page
import Ports
import Translations


init : Model -> ( Model, Cmd Msg )
init model =
    model
        |> Model.setInputTypeAndModeDefaults ( InputType.Json, Ports.Inline )
        |> Page.loadInputFiles { directory = "interpolation", files = ( { name = "example", language = "en" }, [] ) }
        |> Page.withTranslations Translations.loadInterpolation


viewExplanation : Model -> List (Html Msg)
viewExplanation ({ i18n } as model) =
    [ Html.p [] [ Html.text <| Translations.interpolationPreamble i18n ]
    , Html.h2 [] [ Html.text <| Translations.syntaxHeadline i18n ]
    , Accordion.view
        { headline = Translations.jsonHeadline i18n
        , content = List.map (Html.map never) <| Translations.interpolationJsonSyntaxBody [ class "highlighted" ] i18n
        , id = "json_syntax"
        }
        model
    , Accordion.view
        { headline = Translations.propertiesHeadline i18n
        , content = List.map (Html.map never) <| Translations.interpolationPropertiesSyntaxBody [ class "highlighted" ] i18n
        , id = "properties_syntax"
        }
        model
    , Accordion.view
        { headline = Translations.fluentHeadline i18n
        , content = List.map (Html.map never) <| Translations.interpolationFluentSyntaxBody [ class "highlighted" ] i18n
        , id = "fluent_syntax"
        }
        model
    , Html.h2 [] [ Html.text <| Translations.interpolationGeneratedCodeHeadline i18n ]
    , Html.map never <| Html.p [] <| Translations.interpolationGeneratedCodeBody [ class "highlighted" ] i18n
    , Html.h2 [] [ Html.text <| Translations.interpolationInconsistentKeysHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.interpolationInconsistentKeysBody i18n ]
    ]
