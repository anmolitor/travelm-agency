module Main exposing (main)

import Browser exposing (Document)
import Html exposing (div, input, option, p, select, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onInput)
import Html.Events.Extra exposing (onChange)
import I18n exposing (I18n)


type Msg
    = ChangedName String
    | ChangeLanguage String


type alias Model =
    { i18n : I18n
    , name : String
    , language : String
    }


init : String -> ( Model, Cmd Msg )
init language =
    ( { i18n = I18n.en, name = "", language = language }, Cmd.none )


i18nFromString : String -> I18n
i18nFromString input =
    case input of
        "de" ->
            I18n.de

        "en" ->
            I18n.en

        "fr" ->
            I18n.fr

        _ ->
            I18n.en


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedName name ->
            ( { model | name = name }, Cmd.none )

        ChangeLanguage language ->
            ( { model | language = language, i18n = i18nFromString language }, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Example: " ++ model.language
    , body =
        [ div
            []
            [ input [ value model.name, onInput ChangedName, class "name_input" ] []
            , p [ class "info_text" ] [ text <| model.i18n.languageSwitchInfo model.language ]
            , select [ onChange ChangeLanguage, class "language_select" ] <|
                List.map
                    (\language -> option [ selected <| language == model.language, class language ] [ text language ])
                    [ "de", "en", "fr" ]
            , p [ class "greeting" ] [ text <| model.i18n.greeting model.name ]
            , p [ class "order_text" ] [ text <| model.i18n.order { language = model.language, name = model.name } ]
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
