module Generators.Names exposing (Names, defaultNames, withUniqueNames)

import Elm.CodeGen exposing (ModuleName)
import Set
import String.Extra
import Types.Segment exposing (TKey)
import Types.UniqueName as Unique


type alias Names =
    { languageTypeName : String
    , languagesName : String
    , i18nTypeName : String
    , initFunName : String
    , loadName : String -> String
    , loadDataName : String -> String
    , languageFromStringFunName : String
    , languageToStringFunName : String
    , decoderName : String -> String
    , decoderDataName : String -> String
    , htmlModuleName : ModuleName
    , htmlAttributesModuleName : ModuleName
    }


defaultNames : Names
defaultNames =
    { languageTypeName = "Language"
    , languagesName = "languages"
    , i18nTypeName = "I18n"
    , initFunName = "init"
    , loadName = \identifier -> "load" ++ String.Extra.classify identifier
    , loadDataName = \identifier -> "load" ++ String.Extra.classify identifier ++ "Data"
    , languageFromStringFunName = "languageFromString"
    , languageToStringFunName = "languageToString"
    , decoderName = \identifier -> "decode" ++ String.Extra.classify identifier
    , decoderDataName = \identifier -> "decode" ++ String.Extra.classify identifier ++ "Data"
    , htmlModuleName = [ "Html" ]
    , htmlAttributesModuleName = [ "Html", "Attributes" ]
    }


withUniqueNames : List TKey -> Names -> (Names -> a -> b) -> Unique.UniqueNameContext a -> Unique.UniqueNameContext b
withUniqueNames identifiers names doWithNames =
    let
        newNameList =
            List.map ((<|) names.loadName) identifiers
                ++ List.map ((<|) names.loadDataName) identifiers
                ++ List.map ((<|) names.decoderName) identifiers
                ++ List.map ((<|) names.decoderDataName) identifiers
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
                , loadDataName = names.loadDataName >> lookup
                , decoderName = names.decoderName >> lookup
                , decoderDataName = names.decoderDataName >> lookup
                , htmlModuleName = names.htmlModuleName
                , htmlAttributesModuleName = names.htmlAttributesModuleName
                }
                a
