module Msg exposing (..)

import Browser
import File exposing (InputFile)
import Http
import InputType exposing (InputType)
import Ports
import Translations exposing (I18n)
import Url


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
    | AddFile InputFile
    | EditFileName String InputFile
      -- Browser-related
    | Resize Int Int
