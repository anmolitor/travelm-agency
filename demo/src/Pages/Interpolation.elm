module Pages.Interpolation exposing (init)

import File exposing (InputFile)
import Http
import InputType exposing (InputType)
import Ports exposing (GeneratorMode)
import Translations exposing (I18n, Language)


type alias Model model =
    { model
        | generatorMode : GeneratorMode
        , inputType : InputType
        , language : Language
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
    in
    ( { model | generatorMode = generatorMode, inputType = inputType }
    , Cmd.batch
        [ Translations.loadInterpolation { language = model.language, path = "dist/i18n", onLoad = events.onTranslationLoad }
        , Http.get
            { url = "interpolation/example.en." ++ InputType.toString inputType
            , expect =
                Http.expectString
                    (Result.map (\content -> { name = "example", language = "en", extension = "json", content = content })
                        >> events.onInputLoad
                    )
            }
        ]
    )
