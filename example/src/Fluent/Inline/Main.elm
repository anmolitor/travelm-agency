module Fluent.Inline.Main exposing (main)

import Browser exposing (Document)
import Fluent.Inline.I18n as I18n exposing (I18n)
import Html exposing (Html, button, div, input, option, p, select, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onChange)
import Intl exposing (Intl)
import Time


type Msg
    = ChangedName String
    | ChangeLanguage I18n.Language
    | ChangedGender String
    | ChangedNumber Int


type alias Model =
    { i18n : I18n
    , name : String
    , gender : String
    , number : Int
    , language : I18n.Language
    }


init : { language : String, intl : Intl } -> ( Model, Cmd Msg )
init { language, intl } =
    ( { i18n = I18n.init intl I18n.En
      , name = ""
      , gender = "Unknown"
      , number = 0
      , language = I18n.languageFromString language |> Maybe.withDefault I18n.En
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedName name ->
            ( { model | name = name }, Cmd.none )

        ChangedGender gender ->
            ( { model | gender = gender }, Cmd.none )

        ChangedNumber number ->
            ( { model | number = number }, Cmd.none )

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
        , div [ class "row static_term" ] [ text <| I18n.staticTermKey i18n ]
        , div [ class "row dynamic_term" ] [ text <| I18n.dynamicTermKey i18n "\"interpolated\"" ]
        , div [ class "row nested_term" ] [ text <| I18n.nestedTermKey i18n ]
        , div [ class "row attribute_example" ]
            [ text <|
                I18n.attributes i18n
                    ++ ": "
                    ++ I18n.attributesTitle i18n
                    ++ " | "
                    ++ I18n.attributesWithVarAndTerm i18n "Variable"
            ]
        , div [ class "row datetime_example" ] [ text <| I18n.dateTimeFun i18n (Time.millisToPosix 0) ]
        , div [ class "row number_example" ] [ text <| I18n.numberFun i18n 0.42526 ]
        , div [ class "row compile_time_functions" ] [ text <| I18n.compileTimeDatesAndNumbers i18n ]
        , div [ class "row string_match" ] (text (I18n.matchOnGender i18n model.gender) :: List.map (switchGenderButton i18n) [ "male", "female" ])
        , div [ class "row number_match" ]
            [ text <| I18n.matchOnNumbers i18n <| toFloat model.number
            , button [ class <| "add_number", onClick <| ChangedNumber (model.number + 1) ] [ text "+1" ]
            ]
        ]
    }


switchGenderButton : I18n -> String -> Html Msg
switchGenderButton i18n gender =
    button [ class <| "gender_" ++ gender, onClick <| ChangedGender gender ] [ text <| I18n.displayGender i18n gender ]


main : Program { language : String, intl : Intl } Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
