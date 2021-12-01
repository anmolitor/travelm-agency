module Main exposing (main)

import Browser exposing (Document)
import Html exposing (div, input, option, p, select, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onInput)
import Html.Events.Extra exposing (onChange)
import Http
import I18n exposing (I18n)


type Msg
    = GotTranslations (Result Http.Error (I18n -> I18n))
    | ChangedName String
    | ChangeLanguage String


type alias Model =
    { i18n : I18n
    , name : String
    , language : String
    }


init : String -> ( Model, Cmd Msg )
init language =
    ( { i18n = I18n.init, name = "", language = language }, loadTranslations language )


loadTranslations : String -> Cmd Msg
loadTranslations input =
    case I18n.languageFromString input of
        Just language ->
            I18n.loadMessages { language = language, path = "/i18n", onLoad = GotTranslations }

        Nothing ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTranslations (Ok addTranslations) ->
            ( { model | i18n = addTranslations model.i18n }, Cmd.none )

        GotTranslations (Err _) ->
            ( model, Cmd.none )

        ChangedName name ->
            ( { model | name = name }, Cmd.none )

        ChangeLanguage language ->
            ( { model | language = language }, loadTranslations language )


view : Model -> Document Msg
view model =
    { title = "Example: " ++ model.language
    , body =
        [ div
            []
            [ input [ value model.name, onInput ChangedName, class "name_input" ] []
            , p [ class "info_text" ] [ text <| I18n.languageSwitchInfo model.i18n model.language ]
            , select [ onChange ChangeLanguage, class "language_select" ] <|
                List.map
                    (\language -> option [ selected <| language == model.language ] [ text language ])
                    (List.map I18n.languageToString I18n.languages)
            , p [ class "greeting" ] [ text <| I18n.greeting model.i18n model.name ]
            , p [ class "order_text" ] [ text <| I18n.order model.i18n { language = model.language, name = model.name } ]
            ]
        ]
    }


main : Program String Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
