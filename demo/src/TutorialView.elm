module TutorialView exposing (TutorialModel, view)

import Color
import Dict exposing (Dict)
import File exposing (InputFile, OutputFile)
import Html exposing (Html)
import Html.Attributes exposing (class, href)
import Html.Events
import InputType exposing (InputType)
import Json.Decode
import Material.Icons
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra
import Msg exposing (Msg(..))
import Ports exposing (GeneratorMode)
import Routes
import Translations exposing (Language)


type alias TutorialModel =
    { headline : String
    , route : Routes.Route
    , inputTypes : List InputType
    , activeInputType : InputType
    , generatorMode : GeneratorMode
    , currentLanguage : Language
    , arrivedLanguage : Language
    , inputFiles : Dict String InputFile
    , activeInputFilePath : String
    , caretPosition : Int
    , outputFiles : Dict String OutputFile
    , activeOutputFilePath : String
    , basePath : String
    , errorMessage : Maybe String
    }


generateFreeFileName : TutorialModel -> InputFile
generateFreeFileName { activeInputType, inputFiles } =
    let
        go num =
            let
                attempt =
                    { name = "new" ++ String.fromInt num
                    , language = "en"
                    , extension = InputType.toString activeInputType
                    , content = ""
                    }
            in
            if Dict.member (File.inputFileToPath attempt) inputFiles then
                go (num + 1)

            else
                attempt
    in
    go 0


view : TutorialModel -> List (Html Msg) -> List (Html Msg)
view model explanationText =
    let
        activeInputFile =
            Dict.get model.activeInputFilePath model.inputFiles

        activeOutputFile =
            Dict.get model.activeOutputFilePath model.outputFiles

        navigation =
            Html.div [ class "nav" ]
                [ case Routes.previous model.route of
                    Just previous ->
                        Html.a [ href <| Routes.toUrl model.basePath previous, class "arrow" ] [ Material.Icons.arrow_back 50 Inherit ]

                    Nothing ->
                        Material.Icons.arrow_back 50 (Color <| Color.rgba 0.5 0.5 0.5 0.5)
                , Html.text model.headline
                , case Routes.next model.route of
                    Just next ->
                        Html.a [ href <| Routes.toUrl model.basePath next, class "arrow" ] [ Material.Icons.arrow_forward 50 Inherit ]

                    Nothing ->
                        Material.Icons.arrow_forward 50 (Color <| Color.rgba 0.5 0.5 0.5 0.5)
                ]

        renderLanguage lang =
            Html.img
                [ Html.Events.onClick <| ChangeLanguage lang
                , Html.Attributes.src <| "/flag_" ++ Translations.languageToString lang ++ ".svg"
                , Html.Attributes.height 20
                , class "language-flag"
                , class <|
                    if lang == model.arrivedLanguage then
                        "arrived"

                    else if lang == model.currentLanguage then
                        "current"

                    else
                        ""
                ]
                []

        languageSelect =
            Html.div [ class "language-select" ] <|
                List.map
                    renderLanguage
                    Translations.languages

        inputTypeSelect =
            if List.length model.inputTypes > 1 then
                Just <|
                    Html.select
                        [ Html.Events.onInput
                            (InputType.fromString
                                >> Maybe.withDefault model.activeInputType
                                >> ChangeInputType
                            )
                        ]
                    <|
                        List.map
                            (\inputType ->
                                Html.option
                                    [ Html.Attributes.selected <| inputType == model.activeInputType ]
                                    [ Html.text <| InputType.toString inputType ]
                            )
                            model.inputTypes

            else
                Nothing

        outputModeSelect =
            Html.select
                [ Html.Events.onInput
                    (Ports.generatorModeFromString
                        >> Maybe.withDefault model.generatorMode
                        >> ChangeGeneratorMode
                    )
                ]
            <|
                List.map
                    (\mode ->
                        Html.option
                            [ Html.Attributes.selected <| mode == model.generatorMode ]
                            [ Html.text <| Ports.generatorModeToString mode ]
                    )
                    [ Ports.Inline, Ports.Dynamic ]

        inputHeader =
            Html.div [ class "file-header-container" ] <|
                List.filterMap identity
                    [ Just <|
                        Html.div [ class "flex" ] <|
                            List.map
                                (\( path, file ) ->
                                    viewFileHeader
                                        { fileName = File.inputFileToPath file
                                        , isActive = path == model.activeInputFilePath
                                        , onClick = ChangeActiveInputFile <| File.inputFileToPath file
                                        }
                                )
                                (Dict.toList model.inputFiles)
                                ++ [ Html.div
                                        [ class "file-header"
                                        , Html.Events.onClick <| AddFile <| generateFreeFileName model
                                        ]
                                        [ Html.text "+" ]
                                   ]
                    , inputTypeSelect
                    ]

        outputHeader =
            Html.div [ class "file-header-container" ]
                [ Html.div [ class "flex" ] <|
                    List.map
                        (\( path, file ) ->
                            viewFileHeader
                                { fileName = File.outputFileToPath file
                                , isActive = path == model.activeOutputFilePath
                                , onClick = ChangeActiveOutputFile <| File.outputFileToPath file
                                }
                        )
                    <|
                        Dict.toList
                            model.outputFiles
                , outputModeSelect
                ]

        inputCode =
            case activeInputFile of
                Just file ->
                    highlightedCode
                        { language = file.extension
                        , code = file.content
                        , caretPosition = Just model.caretPosition
                        , onEdit =
                            Just <|
                                \newContent caretPosition ->
                                    EditedInput
                                        { filePath = File.inputFileToPath file
                                        , newContent = newContent
                                        , caretPosition = caretPosition
                                        }
                        }

                Nothing ->
                    Html.div [] []

        outputCode =
            case ( activeOutputFile, model.errorMessage ) of
                ( _, Just err ) ->
                    Html.p [ class "editor error-message" ] [ Material.Icons.error 50 Inherit, Html.text err ]

                ( Just file, Nothing ) ->
                    highlightedCode
                        { language = file.extension
                        , code = file.content
                        , caretPosition = Nothing
                        , onEdit = Nothing
                        }

                ( Nothing, Nothing ) ->
                    Html.div [] []
    in
    [ Html.div [ class "content" ]
        [ Html.div [ class "left-sidebar" ]
            [ navigation, Html.div [ class "explanation" ] explanationText ]
        , Html.div [ class "playground" ]
            [ languageSelect
            , inputHeader
            , inputCode
            , outputHeader
            , outputCode
            ]
        ]
    ]


viewFileHeader : { fileName : String, isActive : Bool, onClick : msg } -> Html msg
viewFileHeader { fileName, isActive, onClick } =
    let
        classList =
            Html.Attributes.classList [ ( "file-header", True ), ( "active", isActive ) ]
    in
    Html.div [ Html.Events.onClick onClick, classList ] [ Html.text fileName ]


highlightedCode :
    { language : String
    , code : String
    , caretPosition : Maybe Int
    , onEdit : Maybe (String -> Int -> msg)
    }
    -> Html msg
highlightedCode { language, code, caretPosition, onEdit } =
    Html.node "highlighted-code"
        ([ Html.Attributes.attribute "lang" language
         , Html.Attributes.attribute "code" code
         , class "editor"
         ]
            ++ Maybe.Extra.toList (Maybe.map (Html.Attributes.attribute "pos" << String.fromInt) caretPosition)
            ++ Maybe.withDefault []
                (Maybe.map
                    (\callback ->
                        [ Html.Events.on "edit"
                            (Json.Decode.map2 callback
                                (Json.Decode.at [ "detail", "content" ] Json.Decode.string)
                                (Json.Decode.at [ "detail", "caretPos" ] Json.Decode.int)
                            )
                        , Html.Attributes.attribute "editable" "true"
                        ]
                    )
                    onEdit
                )
        )
        []
