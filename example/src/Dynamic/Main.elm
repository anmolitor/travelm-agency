module Dynamic.Main exposing (main)

import Browser exposing (Document)
import Dynamic.I18n as I18n exposing (I18n, Language)
import Html exposing (div, input, option, p, select, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onInput)
import Html.Events.Extra exposing (onChange)
import Http
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
    let
        lang =
            I18n.languageFromString language |> Maybe.withDefault I18n.En
    in
    ( { i18n = I18n.init, name = "", language = lang, intl = intl }
    , I18n.loadMessages { language = lang, path = "/i18n", onLoad = GotTranslations }
    )


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
                    ( { model | language = language }, I18n.loadMessages { language = language, path = "/i18n", onLoad = GotTranslations } )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> Document Msg
view ({ i18n } as model) =
    let
        currentLangString =
            I18n.languageToString model.language
    in
    { title = "Example: " ++ currentLangString
    , body =
        [ div [ class "row" ]
            [ p [ class "info_text" ] [ text <| I18n.languageSwitchInfo model.i18n currentLangString ]
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
            ]
        , div [ class "row" ] [ text <| I18n.staticText i18n ]
        , div [ class "row" ] [ p [ class "greeting" ] [ text <| I18n.greeting model.i18n model.name ], input [ value model.name, onInput ChangedName, class "name_input" ] [] ]
        , div [ class "row" ] [ text <| I18n.specialCharacters i18n ]
        , div [ class "row" ] [ text <| I18n.orderDemo i18n { language = currentLangString, name = model.name } ]
        , div [ class "row" ] [ text <| I18n.differentVars i18n { elmEn = "Elm", unionGer = "Vereinigung" } ]
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
