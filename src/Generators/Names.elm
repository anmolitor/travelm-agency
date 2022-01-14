module Generators.Names exposing (Names, defaultNames)

import String.Extra


type alias Names =
    { languageTypeName : String
    , languagesName : String
    , i18nTypeName : String
    , initFunName : String
    , loadName : String -> String
    , languageFromStringFunName : String
    , languageToStringFunName : String
    , decoderName : String -> String
    }


defaultNames : Names
defaultNames =
    { languageTypeName = "Language"
    , languagesName = "languages"
    , i18nTypeName = "I18n"
    , initFunName = "init"
    , loadName = \identifier -> "load" ++ String.Extra.classify identifier
    , languageFromStringFunName = "languageFromString"
    , languageToStringFunName = "languageToString"
    , decoderName = \identifier -> "decode" ++ String.Extra.classify identifier
    }
