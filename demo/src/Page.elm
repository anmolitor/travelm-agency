module Page exposing (..)

import Translations exposing (I18n, Language)
import Http


type alias Page msg =
    { loadTranslations : { language : Language, path : String, onLoad : Result Http.Error (I18n -> I18n) -> msg } -> Cmd msg
    }
