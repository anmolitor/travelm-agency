module Generators.Names exposing (Names, defaultNames, withUniqueNames)

import Set
import State exposing (Identifier)
import String.Extra
import Types.UniqueName as Unique


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


withUniqueNames : List Identifier -> Names -> (Names -> a -> b) -> Unique.UniqueNameContext a -> Unique.UniqueNameContext b
withUniqueNames identifiers names doWithNames =
    let
        newNameList =
            List.map ((<|) names.loadName) identifiers
                ++ List.map ((<|) names.loadName) identifiers
                ++ [ names.languageTypeName
                   , names.languagesName
                   , names.i18nTypeName
                   , names.initFunName
                   , names.languageFromStringFunName
                   , names.languageToStringFunName
                   ]
    in
    Unique.combineAndThen (\_ -> Set.fromList newNameList) <|
        \_ a lookup ->
            doWithNames
                { languageTypeName = lookup names.languageTypeName
                , languagesName = lookup names.languagesName
                , i18nTypeName = lookup names.i18nTypeName
                , initFunName = lookup names.initFunName
                , languageFromStringFunName = lookup names.languageFromStringFunName
                , languageToStringFunName = lookup names.languageToStringFunName
                , loadName = names.loadName >> lookup
                , decoderName = names.decoderName >> lookup
                }
                a
