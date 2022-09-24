module DemoMain exposing (..)

import Browser
import Browser.Events
import Browser.Navigation
import Dict exposing (Dict)
import File exposing (OutputFile)
import Html exposing (Html)
import InputType exposing (InputType)
import Intl exposing (Intl)
import Main
import Model exposing (Model)
import Msg exposing (Msg(..))
import Pages.Bundles
import Pages.CaseInterpolation
import Pages.Consistency
import Pages.DateFormat
import Pages.Html
import Pages.Interpolation
import Pages.Intro
import Pages.NumberFormat
import Pages.PluralRules
import Pages.Terms
import Ports
import Routes exposing (Route)
import State exposing (OptimizedJson)
import Translations
import TutorialView
import Types.Error exposing (Failable)
import Url


type alias Flags =
    { language : String, version : String, intl : Intl, height : Int, width : Int, basePath : String }


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        language =
            Translations.languageFromString flags.language |> Maybe.withDefault Translations.En

        route =
            Routes.fromUrl flags.basePath url

        model =
            { -- static data
              key = key
            , basePath = flags.basePath
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
            , errorMessage = Nothing
            }
    in
    initPage model
        |> Tuple.mapSecond
            (\cmds ->
                Cmd.batch
                    [ Translations.loadShared
                        { language = model.language
                        , path = model.basePath ++ "/i18n"
                        , onLoad = LoadedTranslations
                        }
                    , cmds
                    ]
            )


initPage : Model -> ( Model, Cmd Msg )
initPage model =
    case model.route of
        Routes.Intro _ _ ->
            Pages.Intro.init model

        Routes.Interpolation _ _ ->
            Pages.Interpolation.init model

        Routes.Consistency _ _ ->
            Pages.Consistency.init model

        Routes.Bundles _ _ ->
            Pages.Bundles.init model

        Routes.Html _ _ ->
            Pages.Html.init model

        Routes.Terms _ ->
            Pages.Terms.init model

        Routes.CaseInterpolation _ ->
            Pages.CaseInterpolation.init model

        Routes.NumberFormat _ ->
            Pages.NumberFormat.init model

        Routes.DateFormat _ ->
            Pages.DateFormat.init model

        Routes.PluralRules _ ->
            Pages.PluralRules.init model

        Routes.NotFound _ ->
            ( model, Browser.Navigation.replaceUrl model.key <| Routes.toUrl model.basePath <| Routes.Intro Nothing Nothing )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            initPage
                { model
                    | route = Routes.fromUrl model.basePath url
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
                |> Routes.toUrl model.basePath
                |> Browser.Navigation.pushUrl model.key
            )

        ChangeGeneratorMode mode ->
            ( model
            , model.route
                |> Routes.setGeneratorMode mode
                |> Routes.toUrl model.basePath
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

        AddFile file ->
            ( { model | inputFiles = Dict.insert (File.inputFileToPath file) file model.inputFiles }, Cmd.none )

        EditFileName oldFilePath newFile ->
            ( { model
                | inputFiles =
                    (if Dict.member (File.inputFileToPath newFile) model.inputFiles then
                        identity

                     else
                        Dict.remove oldFilePath
                            >> Dict.insert (File.inputFileToPath newFile) newFile
                    )
                        model.inputFiles
              }
            , Cmd.none
            )


runTravelmAgencyAndUpdateModel : Model -> Model
runTravelmAgencyAndUpdateModel model =
    case runTravelmAgency model |> Types.Error.formatFail of
        Err err ->
            { model | errorMessage = Just err }

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
                , errorMessage = Nothing
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
            { i18nArgFirst = False
            , generatorMode = model.generatorMode
            , addContentHash = False
            , prefixFileIdentifier = False
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
view ({ inputFiles, activeInputFilePath, outputFiles, activeOutputFilePath, caretPosition, route, inputType, basePath } as model) =
    { title = "Tutorial"
    , body =
        TutorialView.view
            { headline = viewHeadline model
            , activeInputType = inputType
            , generatorMode = model.generatorMode
            , inputTypes = inputTypesForRoute route
            , route = route
            , inputFiles = inputFiles
            , activeInputFilePath = activeInputFilePath
            , outputFiles = outputFiles
            , activeOutputFilePath = activeOutputFilePath
            , caretPosition = caretPosition
            , basePath = basePath
            , errorMessage = model.errorMessage
            }
        <|
            viewExplanation model
    }


inputTypesForRoute : Route -> List InputType
inputTypesForRoute route =
    case route of
        Routes.Terms _ ->
            [ InputType.Fluent ]

        Routes.CaseInterpolation _ ->
            [ InputType.Fluent ]

        Routes.NumberFormat _ ->
            [ InputType.Fluent ]

        Routes.DateFormat _ ->
            [ InputType.Fluent ]

        Routes.PluralRules _ ->
            [ InputType.Fluent ]

        _ ->
            [ InputType.Json, InputType.Properties, InputType.Fluent ]


viewHeadline : Model -> String
viewHeadline model =
    case model.route of
        Routes.Intro _ _ ->
            Translations.introHeadline model.i18n

        Routes.Interpolation _ _ ->
            Translations.interpolationHeadline model.i18n

        Routes.Consistency _ _ ->
            Translations.consistencyHeadline model.i18n

        Routes.Bundles _ _ ->
            Translations.bundlesHeadline model.i18n

        Routes.Html _ _ ->
            Translations.htmlHeadline model.i18n

        Routes.Terms _ ->
            Translations.termsHeadline model.i18n

        Routes.CaseInterpolation _ ->
            Translations.caseInterpolationHeadline model.i18n

        Routes.NumberFormat _ ->
            Translations.numberFormatHeadline model.i18n

        Routes.DateFormat _ ->
            Translations.dateFormatHeadline model.i18n

        Routes.PluralRules _ ->
            Translations.pluralRulesHeadline model.i18n

        Routes.NotFound _ ->
            ""


viewExplanation : Model -> List (Html Msg)
viewExplanation model =
    case model.route of
        Routes.Intro _ _ ->
            Pages.Intro.viewExplanation model

        Routes.Interpolation _ _ ->
            Pages.Interpolation.viewExplanation model

        Routes.Consistency _ _ ->
            Pages.Consistency.viewExplanation model

        Routes.Bundles _ _ ->
            Pages.Bundles.viewExplanation model

        Routes.Html _ _ ->
            Pages.Html.viewExplanation model

        Routes.Terms _ ->
            Pages.Terms.viewExplanation model

        Routes.CaseInterpolation _ ->
            Pages.CaseInterpolation.viewExplanation model

        Routes.NumberFormat _ ->
            Pages.NumberFormat.viewExplanation model

        Routes.DateFormat _ ->
            Pages.DateFormat.viewExplanation model

        Routes.PluralRules _ ->
            Pages.PluralRules.viewExplanation model

        Routes.NotFound _ ->
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
