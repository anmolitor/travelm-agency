module DemoMain exposing (..)

import Browser
import Browser.Navigation
import Dict
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import InputType exposing (InputType)
import Intl exposing (Intl)
import Json.Decode
import Main
import Maybe.Extra
import Ports
import Routes exposing (Route)
import Translations exposing (I18n, Language)
import Types.Error exposing (Failable)
import Url


type alias Flags =
    { language : String, version : String, intl : Intl }


type alias Model =
    { key : Browser.Navigation.Key
    , route : Route
    , language : Language
    , i18n : I18n
    , version : String
    , inputType : InputType
    , input : String
    , caretPos : Int
    , generatorMode : Ports.GeneratorMode
    , output : String
    , intl : Intl
    }


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | LoadedTranslations (Result Http.Error (I18n -> I18n))
    | LoadedInput (Result Http.Error String)
    | EditedInput String Int
    | ChangeInputType InputType


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        language =
            Translations.languageFromString flags.language |> Maybe.withDefault Translations.En

        route =
            Routes.fromUrl url

        model =
            { key = key
            , language = language
            , i18n = Translations.init
            , input = ""
            , caretPos = 0
            , inputType = InputType.Json
            , generatorMode = Ports.Inline
            , output = ""
            , version = flags.version
            , intl = flags.intl
            , route = route
            }
    in
    initPage route model


getInput : String -> String -> InputType -> Cmd Msg
getInput folder fileName inputType =
    Http.get { url = folder ++ "/" ++ fileName ++ "." ++ InputType.toString inputType, expect = Http.expectString LoadedInput }


initPage : Route -> Model -> ( Model, Cmd Msg )
initPage route model =
    case route of
        Routes.Intro mayMode mayInputType ->
            let
                generatorMode =
                    mayMode |> Maybe.withDefault Ports.Inline

                inputType =
                    mayInputType |> Maybe.withDefault InputType.Json
            in
            ( { model | generatorMode = generatorMode, inputType = inputType, route = route }
            , Cmd.batch
                [ Translations.loadIntro { language = model.language, path = "dist/i18n", onLoad = LoadedTranslations }
                , getInput "intro" "example" inputType
                ]
            )

        Routes.NotFound _ ->
            ( model, Browser.Navigation.replaceUrl model.key <| Routes.toUrl <| Routes.Intro Nothing Nothing )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            initPage (Routes.fromUrl url) model

        UrlRequested _ ->
            ( model, Cmd.none )

        LoadedTranslations (Ok addTranslations) ->
            ( { model | i18n = addTranslations model.i18n }, Cmd.none )

        LoadedTranslations (Err _) ->
            ( model, Cmd.none )

        LoadedInput (Ok input) ->
            ( { model | input = input } |> runTravelmAgencyAndUpdateModel, Cmd.none )

        LoadedInput (Err _) ->
            ( model, Cmd.none )

        EditedInput newInput caretPos ->
            ( { model | caretPos = caretPos, input = newInput } |> runTravelmAgencyAndUpdateModel, Cmd.none )

        ChangeInputType inputType ->
            ( model
            , model.route
                |> Routes.setInputType inputType
                |> Routes.toUrl
                |> Browser.Navigation.pushUrl model.key
            )


runTravelmAgencyAndUpdateModel : Model -> Model
runTravelmAgencyAndUpdateModel model =
    case runTravelmAgency model of
        Err _ ->
            model

        Ok responseContent ->
            { model | output = responseContent.elmFile }


runTravelmAgency : Model -> Failable Ports.ResponseContent
runTravelmAgency model =
    let
        mainModel =
            { version = model.version
            , state = Dict.empty
            , devMode = False
            , intl = model.intl
            }

        translationRequest =
            { content = model.input
            , extension = InputType.toString model.inputType
            , identifier = "messages"
            , language = "en"
            }

        finishRequest =
            { i18nArgLast = False
            , generatorMode = model.generatorMode
            , addContentHash = False
            , elmModuleName = "Translations"
            }
    in
    Main.tryAddTranslation translationRequest mainModel |> Result.andThen (Main.tryFinishModule finishRequest)


view : Model -> Browser.Document Msg
view { i18n, input, inputType, caretPos, output } =
    { title = "Demo"
    , body =
        [ --Html.h1 [] [ Html.text <| Translations.headline i18n ]
          --, Html.h2 [] [ Html.text <| Translations.stepOne i18n ]
          Html.button [ Html.Events.onClick <| ChangeInputType InputType.Properties ]
            [ Html.text <| Translations.inputTypeProperties i18n ]
        , highlightedCode { lang = InputType.toString inputType, code = input, caretPos = Just caretPos }
        , highlightedCode { lang = "elm", code = output, caretPos = Nothing }
        ]
    }


highlightedCode : { lang : String, code : String, caretPos : Maybe Int } -> Html Msg
highlightedCode { lang, code, caretPos } =
    Html.node "highlighted-code"
        ([ Html.Attributes.attribute "lang" lang
         , Html.Attributes.attribute "code" code
         , Html.Events.on "edit"
            (Json.Decode.map2 EditedInput
                (Json.Decode.at [ "detail", "content" ] Json.Decode.string)
                (Json.Decode.at [ "detail", "caretPos" ] Json.Decode.int)
            )
         ]
            ++ Maybe.Extra.toList (Maybe.map (Html.Attributes.attribute "pos" << String.fromInt) caretPos)
        )
        []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
