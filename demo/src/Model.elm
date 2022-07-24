module Model exposing (..)

import Browser.Navigation
import Dict exposing (Dict)
import File exposing (InputFile, OutputFile)
import InputType exposing (InputType)
import Intl exposing (Intl)
import Ports
import Routes exposing (Route)
import Translations exposing (I18n, Language)


type alias Model =
    { -- static data
      key : Browser.Navigation.Key
    , basePath : String
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
    , errorMessage : Maybe String
    }


setInputTypeAndModeDefaults : ( InputType, Ports.GeneratorMode ) -> Model -> Model
setInputTypeAndModeDefaults ( defaultInputType, defaultMode ) model =
    let
        ( inputType, mode ) =
            Routes.getParams model.route
    in
    { model
        | inputType = inputType |> Maybe.withDefault defaultInputType
        , generatorMode = mode |> Maybe.withDefault defaultMode
    }
