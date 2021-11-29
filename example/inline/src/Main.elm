module Main exposing (main)

import Browser exposing (Document)
import Html exposing (div, input, option, p, select, text)
import Html.Attributes exposing (selected, value)
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
            [ input [ value model.name, onInput ChangedName ] []
            , p [] [ text <| I18n.languageSwitchInfo model.i18n model.language ]
            , select [ onChange ChangeLanguage ] <|
                List.map
                    (\language -> option [ selected <| language == model.language ] [ text language ])
                    [ "de", "en" ]
            , p [] [ text <| I18n.greeting model.i18n model.name ]
            , p [] [ text <| I18n.order model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order1 model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order2 model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order3 model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order4 model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order5 model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order6 model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order7 model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order8 model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order9 model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order10 model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order11 model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order12 model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order13 model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order14 model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order15 model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order16 model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order17 model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order18 model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order19 model.i18n { language = model.language, name = model.name } ]
            , p [] [ text <| I18n.order20 model.i18n { language = model.language, name = model.name } ]
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
