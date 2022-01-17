module Main exposing (main)

import Browser exposing (Document)
import Html exposing (div, input, option, p, select, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onInput)
import Html.Events.Extra exposing (onChange)
import Http
import I18n exposing (I18n, Language)
import Intl exposing (Intl)


type Msg
    = GotTranslations (Result Http.Error (I18n -> I18n))
    | ChangedName String
    | ChangeLanguage String


type alias Model =
    { i18n : I18n
    , intl : Intl
    , name : String
    , language : Language
    }


init : Flags -> ( Model, Cmd Msg )
init { intl, language } =
    ( { i18n = I18n.init, name = "", language = I18n.languageFromString language |> Maybe.withDefault I18n.En, intl = intl }, loadTranslations language )


loadTranslations : String -> Cmd Msg
loadTranslations input =
    case I18n.languageFromString input of
        Just language ->
            I18n.loadDemo { language = language, path = "/i18n", onLoad = GotTranslations }

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

        ChangeLanguage input ->
            case I18n.languageFromString input of
                Just language ->
                    ( { model | language = language }, I18n.loadDemo { language = language, path = "/i18n", onLoad = GotTranslations } )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Example: " ++ I18n.languageToString model.language
    , body =
        [ div
            []
            [ input [ value model.name, onInput ChangedName, class "name_input" ] []
            , p [ class "info_text" ] [ text <| I18n.languageSwitchInfo model.i18n <| I18n.languageToString model.language ]
            , select [ onChange ChangeLanguage, class "language_select" ] <|
                List.map
                    (\language -> option [ selected <| language == I18n.languageToString model.language ] [ text language ])
                    (List.map I18n.languageToString I18n.languages)
            , p [ class "greeting" ] [ text <| I18n.greeting model.i18n model.name ]
            , p [ class "order_text" ] [ text <| I18n.order model.i18n { language = I18n.languageToString model.language, name = model.name } ]
            , p [] [ text <| I18n.sentOn model.intl model.language model.i18n "0" ]
            ]
        ]
    }


type alias Flags =
    { language : String
    , intl : Intl
    }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
