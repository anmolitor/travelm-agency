module Page exposing (..)

import Http
import InputType
import List.NonEmpty exposing (NonEmpty)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Translations exposing (I18n, Language)


loadInputFiles : { files : NonEmpty { name : String, language : String }, directory : String } -> Model -> ( Model, Cmd Msg )
loadInputFiles { files, directory } model =
    let
        fileExtension =
            InputType.toString model.inputType

        toFileName { name, language } =
            String.join "." [ name, language, fileExtension ]

        loadOne file =
            Http.get
                { url = String.join "/" [ model.basePath, directory, toFileName file ]
                , expect =
                    Http.expectString
                        (Result.map
                            (\content ->
                                { name = file.name
                                , language = file.language
                                , extension = fileExtension
                                , content = content
                                }
                            )
                            >> LoadedInputFile
                        )
                }
    in
    ( { model | activeInputFilePath = List.NonEmpty.head files |> toFileName }
    , List.NonEmpty.toList files |> List.map loadOne |> Cmd.batch
    )


withTranslations :
    ((Result Http.Error (I18n -> I18n) -> Msg) -> I18n -> Cmd Msg)
    -> ( Model, Cmd Msg )
    -> ( Model, Cmd Msg )
withTranslations load ( model, otherCmds ) =
    ( model, Cmd.batch [ load LoadedTranslations model.i18n, otherCmds ] )
