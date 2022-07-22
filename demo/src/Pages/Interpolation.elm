module Pages.Interpolation exposing (init, viewExplanation)

import Accordion
import Dict exposing (Dict)
import File exposing (InputFile)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Http
import InputType exposing (InputType)
import Ports exposing (GeneratorMode)
import Translations exposing (I18n, Language)


type alias Model model =
    { model
        | generatorMode : GeneratorMode
        , inputType : InputType
        , language : Language
        , activeInputFilePath : String
        , basePath : String
    }


init :
    { onInputLoad : Result Http.Error InputFile -> msg, onTranslationLoad : Result Http.Error (I18n -> I18n) -> msg }
    -> Model model
    -> Maybe GeneratorMode
    -> Maybe InputType
    -> ( Model model, Cmd msg )
init events model mayMode mayInputType =
    let
        generatorMode =
            mayMode |> Maybe.withDefault Ports.Inline

        inputType =
            mayInputType |> Maybe.withDefault InputType.Json

        filePathToLoad =
            "example.en." ++ InputType.toString inputType
    in
    ( { model | generatorMode = generatorMode, inputType = inputType, activeInputFilePath = filePathToLoad }
    , Cmd.batch
        [ Translations.loadInterpolation { language = model.language, path = model.basePath ++ "/i18n", onLoad = events.onTranslationLoad }
        , Http.get
            { url = model.basePath ++ "/interpolation/" ++ filePathToLoad
            , expect =
                Http.expectString
                    (Result.map
                        (\content ->
                            { name = "example"
                            , language = "en"
                            , extension = InputType.toString inputType
                            , content = content
                            }
                        )
                        >> events.onInputLoad
                    )
            }
        ]
    )


viewExplanation : { model | i18n : I18n, openAccordionElements : Dict String Int } -> { onToggleAccordionEl : String -> Int -> msg } -> List (Html msg)
viewExplanation { i18n, openAccordionElements } events =
    [ Html.p [] [ Html.text <| Translations.interpolationPreamble i18n ]
    , Html.h2 [] [ Html.text <| Translations.syntaxHeadline i18n ]
    , Accordion.view
        { headline = Translations.jsonHeadline i18n
        , content = List.map (Html.map never) <| Translations.jsonSyntaxBody i18n [ class "highlighted" ]
        , onToggle = events.onToggleAccordionEl "json_syntax"
        , height = Dict.get "json_syntax" openAccordionElements |> Maybe.withDefault 0
        }
    , Accordion.view
        { headline = Translations.propertiesHeadline i18n
        , content = List.map (Html.map never) <| Translations.propertiesSyntaxBody i18n [ class "highlighted" ]
        , onToggle = events.onToggleAccordionEl "properties_syntax"
        , height = Dict.get "properties_syntax" openAccordionElements |> Maybe.withDefault 0
        }
    , Accordion.view
        { headline = Translations.fluentHeadline i18n
        , content = List.map (Html.map never) <| Translations.fluentSyntaxBody i18n [ class "highlighted" ]
        , onToggle = events.onToggleAccordionEl "fluent_syntax"
        , height = Dict.get "fluent_syntax" openAccordionElements |> Maybe.withDefault 0
        }
    ]
