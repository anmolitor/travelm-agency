module Generators.Names exposing (..)

import State exposing (Identifier)
import String.Extra


type alias Names =
    { languageTypeName : String
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
    , i18nTypeName = "I18n"
    , initFunName = "init"
    , loadName = \identifier -> "load" ++ String.Extra.classify identifier
    , languageFromStringFunName = "languageFromString"
    , languageToStringFunName = "languageToString"
    , decoderName = \identifier -> "decode" ++ String.Extra.classify identifier
    }


type alias IdentifierScopedNames =
    { loadName : String, decoderName : String }


applyIdentifier : Identifier -> Names -> IdentifierScopedNames
applyIdentifier identifier { loadName, decoderName } =
    { loadName = loadName identifier, decoderName = decoderName identifier }
