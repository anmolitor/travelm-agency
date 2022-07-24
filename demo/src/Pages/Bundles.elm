module Pages.Bundles exposing (..)

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
        |> Model.setInputTypeAndModeDefaults ( InputType.Json, Ports.Dynamic )
        |> Page.loadInputFiles
            { directory = "bundles"
            , files =
                ( { name = "foo", language = "en" }
                , [ { name = "bar", language = "en" }
                  ]
                )
            }
        |> Page.withTranslations Translations.loadBundles


viewExplanation : Model -> List (Html msg)
viewExplanation { i18n } =
    [ Html.map never <| Html.p [] <| Translations.bundlesPreamble i18n [ class "highlighted" ]
    , Html.h2 [] [ Html.text <| Translations.bundlesConsiderationsHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.bundlesConsiderationsBody i18n ]
    , Html.h2 [] [ Html.text <| Translations.bundlesExploreHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.bundlesExploreBody i18n ]
    ]
