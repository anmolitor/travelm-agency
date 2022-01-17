module Inline.Main exposing (main)

import Browser exposing (Document)
import Html exposing (div, input, option, p, select, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onInput)
import Html.Events.Extra exposing (onChange)
import Inline.I18n as I18n exposing (I18n)


type Msg
    = ChangedName String
    | ChangeLanguage String


type alias Model =
    { i18n : I18n
    , name : String
    , language : I18n.Language
    }


init : { language : String } -> ( Model, Cmd Msg )
init { language } =
    ( { i18n = I18n.init I18n.En
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
            , div [ class "row" ] [ text <| I18n.staticText i18n ]
            , div [ class "row" ] [ p [ class "greeting" ] [ text <| I18n.greeting model.i18n model.name ], input [ value model.name, onInput ChangedName, class "name_input" ] [] ]
            , div [ class "row" ] [ text <| I18n.specialCharacters i18n ]
            , div [ class "row" ] [ text <| I18n.orderDemo i18n { language = currentLangString, name = model.name } ]
            , div [ class "row" ] [ text <| I18n.differentVars i18n { elmEn = "Elm", unionGer = "Vereinigung" } ]
            ]
        ]
    }


main : Program { language : String } Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
