module Fluent.Inline.Main exposing (main)

import Browser exposing (Document)
import Fluent.Inline.I18n as I18n exposing (I18n)
import Html exposing (button, div, input, option, p, select, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onChange)
import Intl exposing (Intl)
import Time


type Msg
    = ChangedName String
    | ChangeLanguage I18n.Language


type alias Model =
    { i18n : I18n
    , name : String
    , language : I18n.Language
    }


init : { language : String, intl : Intl } -> ( Model, Cmd Msg )
init { language, intl } =
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

        ChangeLanguage language ->
            ( { model | language = language, i18n = I18n.load language model.i18n }, Cmd.none )


view : Model -> Document Msg
view ({ i18n } as model) =
    let
        currentLangString =
            I18n.languageToString model.language
    in
    { title = "Example: " ++ currentLangString
    , body =
        [ div [ class "row" ] (List.map (\lang -> button [ onClick <| ChangeLanguage lang ] [ text <| I18n.languageToString lang ]) I18n.languages)
        , div [ class "row" ] [ text <| I18n.staticTermKey i18n ]
        , div [ class "row" ] [ text <| I18n.dynamicTermKey i18n "\"interpolated\"" ]
        , div [ class "row" ] [ text <| I18n.nestedTermKey i18n ]
        , div [ class "row" ]
            [ text <|
                I18n.attributes i18n
                    ++ ": "
                    ++ I18n.attributesTitle i18n
                    ++ " | "
                    ++ I18n.attributesWithVarAndTerm i18n "Variable"
            ]
        , div [ class "row" ] [ text <| I18n.dateTimeFun i18n (Time.millisToPosix 0) ]
        , div [ class "row" ] [ text <| I18n.numberFun i18n 0.42526 ]
        , div [ class "row" ] [ text <| I18n.compileTimeDatesAndNumbers i18n ]
        ]
    }


main : Program { language : String, intl : Intl } Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
