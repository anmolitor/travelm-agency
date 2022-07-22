module DemoMain exposing (..)

import Browser
import Browser.Events
import Browser.Navigation
import Dict exposing (Dict)
import File exposing (InputFile, OutputFile)
import Html exposing (Html)
import Http
import InputType exposing (InputType)
import Intl exposing (Intl)
import Main
import Pages.Interpolation
import Pages.Intro
import Ports
import Routes exposing (Route)
import Set exposing (Set)
import State exposing (OptimizedJson)
import Translations exposing (I18n, Language)
import TutorialView
import Types.Error exposing (Failable)
import Url


type alias Flags =
    { language : String, version : String, intl : Intl, height : Int, width : Int }


type alias Model =
    { -- static data
      key : Browser.Navigation.Key
    , version : String

    -- internationization
    , i18n : I18n
    , intl : Intl
    , language : Language

    -- routing
    , route : Route
    , generatorMode : Ports.GeneratorMode
    , inputType : InputType

    -- viewport
    , height : Int
    , width : Int

    -- explanation text
    , openAccordionElements : Dict String Int

    -- code editor
    , caretPosition : Int
    , inputFiles : Dict String InputFile
    , activeInputFilePath : String
    , outputFiles : Dict String OutputFile
    , activeOutputFilePath : String
    }


type Msg
    = -- Navigation
      UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
      -- Get static resource files
    | LoadedTranslations (Result Http.Error (I18n -> I18n))
    | LoadedInputFile (Result Http.Error InputFile)
      -- Explanation text
    | ToggleAccordionElement String Int
      -- Code editor
    | EditedInput { filePath : String, newContent : String, caretPosition : Int }
    | ChangeInputType InputType
    | ChangeGeneratorMode Ports.GeneratorMode
    | ChangeActiveInputFile String
    | ChangeActiveOutputFile String
      -- Browser-related
    | Resize Int Int


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        language =
            Translations.languageFromString flags.language |> Maybe.withDefault Translations.En

        route =
            Routes.fromUrl url

        model =
            { -- static data
              key = key
            , version = flags.version

            -- internationalzation
            , i18n = Translations.init
            , intl = flags.intl
            , language = language

            -- routing
            , route = route
            , generatorMode = Ports.Inline
            , inputType = InputType.Json

            -- viewport
            , height = flags.height
            , width = flags.width

            -- explanation text
            , openAccordionElements = Dict.empty

            -- code editor
            , inputFiles = Dict.empty
            , activeInputFilePath = ""
            , outputFiles = Dict.empty
            , activeOutputFilePath = "Translations.elm"
            , caretPosition = 0
            }
    in
    initPage model


initPage : Model -> ( Model, Cmd Msg )
initPage model =
    let
        events =
            { onInputLoad = LoadedInputFile, onTranslationLoad = LoadedTranslations }
    in
    case model.route of
        Routes.Intro mayMode mayInputType ->
            Pages.Intro.init events model mayMode mayInputType

        Routes.Interpolation mayMode mayInputType ->
            Pages.Interpolation.init events model mayMode mayInputType

        Routes.NotFound _ ->
            ( model, Browser.Navigation.replaceUrl model.key <| Routes.toUrl <| Routes.Intro Nothing Nothing )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            initPage
                { model
                    | route = Routes.fromUrl url
                    , inputFiles = Dict.empty
                    , outputFiles = Dict.empty
                    , activeOutputFilePath = "Translations.elm"
                }

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

                Browser.External url ->
                    ( model, Browser.Navigation.load url )

        LoadedTranslations (Ok addTranslations) ->
            ( { model | i18n = addTranslations model.i18n }, Cmd.none )

        LoadedTranslations (Err _) ->
            ( model, Cmd.none )

        LoadedInputFile (Ok file) ->
            let
                path =
                    File.inputFileToPath file

                runTravelmAgencyIfActive =
                    if path == model.activeInputFilePath then
                        runTravelmAgencyAndUpdateModel

                    else
                        identity
            in
            ( { model | inputFiles = Dict.insert path file model.inputFiles } |> runTravelmAgencyIfActive, Cmd.none )

        LoadedInputFile (Err _) ->
            ( model, Cmd.none )

        EditedInput { caretPosition, newContent, filePath } ->
            ( { model
                | caretPosition = caretPosition
                , inputFiles = Dict.update filePath (Maybe.map <| \file -> { file | content = newContent }) model.inputFiles
              }
                |> runTravelmAgencyAndUpdateModel
            , Cmd.none
            )

        ChangeInputType inputType ->
            ( model
            , model.route
                |> Routes.setInputType inputType
                |> Routes.toUrl
                |> Browser.Navigation.pushUrl model.key
            )

        ChangeGeneratorMode mode ->
            ( model
            , model.route
                |> Routes.setGeneratorMode mode
                |> Routes.toUrl
                |> Browser.Navigation.pushUrl model.key
            )

        Resize width height ->
            ( { model | width = width, height = height }, Cmd.none )

        ChangeActiveInputFile fileName ->
            ( { model | activeInputFilePath = fileName }, Cmd.none )

        ChangeActiveOutputFile fileName ->
            ( { model | activeOutputFilePath = fileName }, Cmd.none )

        ToggleAccordionElement key elHeight ->
            ( { model | openAccordionElements = toggleDict key elHeight model.openAccordionElements }, Cmd.none )


runTravelmAgencyAndUpdateModel : Model -> Model
runTravelmAgencyAndUpdateModel model =
    case runTravelmAgency model of
        Err _ ->
            model

        Ok responseContent ->
            let
                generatedElmFile : OutputFile
                generatedElmFile =
                    { name = "Translations"
                    , extension = "elm"
                    , language = Nothing
                    , content = responseContent.elmFile
                    }

                optimizedJsonToFile : OptimizedJson -> Maybe ( String, OutputFile )
                optimizedJsonToFile json =
                    case String.split "." json.filename of
                        [ name, language, extension ] ->
                            Just
                                ( json.filename
                                , { name = name
                                  , language = Just language
                                  , extension = extension
                                  , content = json.content
                                  }
                                )

                        _ ->
                            Nothing
            in
            { model
                | outputFiles =
                    Dict.fromList <|
                        ( "Translations.elm", generatedElmFile )
                            :: List.filterMap optimizedJsonToFile responseContent.optimizedJson
            }


runTravelmAgency : Model -> Failable Ports.ResponseContent
runTravelmAgency model =
    let
        mainModel =
            { version = model.version
            , state = Dict.empty
            , devMode = False
            , intl = model.intl
            }

        translationRequest inputFile =
            { content = inputFile.content
            , extension = InputType.toString model.inputType
            , identifier = inputFile.name
            , language = inputFile.language
            }

        finishRequest =
            { i18nArgLast = False
            , generatorMode = model.generatorMode
            , addContentHash = False
            , elmModuleName = "Translations"
            }

        addInputFile inputFile =
            Result.andThen (Main.tryAddTranslation <| translationRequest inputFile)
    in
    List.foldl addInputFile (Ok mainModel) (Dict.values model.inputFiles)
        |> Result.andThen (Main.tryFinishModule 80 finishRequest)


toggleDict : comparable -> v -> Dict comparable v -> Dict comparable v
toggleDict key val dict =
    if Dict.member key dict then
        Dict.remove key dict

    else
        Dict.insert key val dict


view : Model -> Browser.Document Msg
view ({ inputFiles, activeInputFilePath, outputFiles, activeOutputFilePath, caretPosition, route, inputType } as model) =
    { title = "Tutorial"
    , body =
        TutorialView.view
            { headline = viewHeadline model
            , activeInputType = inputType
            , inputTypes = inputTypesForRoute route
            , route = route
            , inputFiles = inputFiles
            , activeInputFilePath = activeInputFilePath
            , outputFiles = outputFiles
            , activeOutputFilePath = activeOutputFilePath
            , caretPosition = caretPosition
            }
            { onEditInput = EditedInput
            , onSwitchInput = ChangeActiveInputFile
            , onSwitchOutput = ChangeActiveOutputFile
            , onSwitchInputType = ChangeInputType
            }
        <|
            viewExplanation model
    }


inputTypesForRoute : Route -> List InputType
inputTypesForRoute _ =
    [ InputType.Json, InputType.Properties, InputType.Fluent ]


viewHeadline : Model -> String
viewHeadline model =
    case model.route of
        Routes.Intro _ _ ->
            Translations.introHeadline model.i18n

        Routes.Interpolation _ _ ->
            Translations.interpolationHeadline model.i18n

        _ ->
            ""


viewExplanation : Model -> List (Html Msg)
viewExplanation model =
    case model.route of
        Routes.Intro _ _ ->
            Pages.Intro.viewExplanation model { onToggleAccordionEl = ToggleAccordionElement }

        Routes.Interpolation _ _ ->
            Pages.Interpolation.viewExplanation model

        _ ->
            []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize Resize


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
