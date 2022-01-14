module Main exposing (main)

import Browser exposing (Document)
import Html exposing (div, input, option, p, select, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onInput)
import Html.Events.Extra exposing (onChange)
import I18n exposing (I18n)
import Intl exposing (Intl)
import Time


type Msg
    = ChangedName String
    | ChangeLanguage String


type alias Model =
    { i18n : I18n
    , name : String
    , language : I18n.Language
    }


init : { intl : Intl, language : String } -> ( Model, Cmd Msg )
init { intl, language } =
    ( { i18n = I18n.init intl I18n.En
      , name = ""
      , language = I18n.languageFromString language |> Maybe.withDefault I18n.En
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedName name ->
            ( { model | name = name }, Cmd.none )

        ChangeLanguage langString ->
            case I18n.languageFromString langString of
                Just language ->
                    ( { model | language = language, i18n = I18n.load language model.i18n }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    let
        currentLangString =
            I18n.languageToString model.language
    in
    { title = "Example: " ++ currentLangString
    , body =
        [ div
            []
            [ input [ value model.name, onInput ChangedName, class "name_input" ] []
            , p [ class "info_text" ] [ text <| I18n.languageSwitchInfo model.i18n currentLangString ]
            , select [ onChange ChangeLanguage, class "language_select" ] <|
                List.map
                    (\language ->
                        option
                            [ selected <| language == model.language
                            , class <| I18n.languageToString language
                            ]
                            [ text <| I18n.languageToString language ]
                    )
                    I18n.languages
            , p [ class "greeting" ] [ text <| I18n.greeting model.i18n model.name ]
            , p [ class "order_text" ] [ text <| I18n.order model.i18n { language = currentLangString, name = model.name } ]
            , p [ class "sent_on" ] [ text <| I18n.sentOn model.i18n <| Time.millisToPosix 0 ]
            ]
        ]
    }


main : Program { intl : Intl, language : String } Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
