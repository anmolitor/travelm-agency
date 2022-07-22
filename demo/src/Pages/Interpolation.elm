module Pages.Interpolation exposing (init, viewExplanation)

import File exposing (InputFile)
import Html exposing (Html)
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
        [ Translations.loadInterpolation { language = model.language, path = "/i18n", onLoad = events.onTranslationLoad }
        , Http.get
            { url = "/interpolation/" ++ filePathToLoad
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


viewExplanation : { model | i18n : I18n } -> List (Html msg)
viewExplanation { i18n } =
    [ Html.p [] [ Html.text <| Translations.interpolationPreamble i18n ]
    , Html.h2 [] [ Html.text <| Translations.syntaxHeadline i18n ]
    , Html.h3 [] [ Html.text <| Translations.jsonHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.jsonSyntaxBody i18n ]
    , Html.h3 [] [ Html.text <| Translations.propertiesHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.propertiesSyntaxBody i18n ]
    , Html.h3 [] [ Html.text <| Translations.fluentHeadline i18n ]
    , Html.p [] [ Html.text <| Translations.fluentSyntaxBody i18n ]
    ]
