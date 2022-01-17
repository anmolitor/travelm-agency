module Generators.Inline exposing (..)

import CodeGen.Imports
import CodeGen.Shared exposing (Context, endoAnn, intlAnn, languageRelatedDecls)
import Dict
import Dict.NonEmpty
import Elm.CodeGen as CG
import List.NonEmpty
import State exposing (NonEmptyState, Translation)
import String.Extra
import Types
import Util


toFile : Context -> NonEmptyState () -> CG.File
toFile { moduleName, names, version } state =
    let
        pairs : Types.Translations
        pairs =
            translationSet
                |> Dict.NonEmpty.getFirstEntry
                |> Tuple.second
                |> .pairs
                |> List.sortBy Tuple.first

        needsIntl =
            State.stateNeedsIntl state

        initDecl : CG.Declaration
        initDecl =
            let
                langToI18nAnn =
                    CG.funAnn (CG.typed names.languageTypeName []) (CG.typed names.i18nTypeName [])

                initBody exprFromLang =
                    CG.caseExpr (CG.val "lang_")
                        (languages
                            |> List.map
                                (\lang ->
                                    ( CG.namedPattern (String.Extra.classify lang) []
                                    , exprFromLang lang
                                    )
                                )
                        )
            in
            if needsIntl then
                CG.funDecl (Just (CG.emptyDocComment |> CG.markdown "Initialize an i18n instance based on a language and access to the Intl API"))
                    (Just <| CG.funAnn intlAnn langToI18nAnn)
                    names.initFunName
                    [ CG.varPattern "intl_", CG.varPattern "lang_" ]
                    (initBody <| \lang -> CG.tuple [ CG.val lang, CG.val "intl_" ])

            else
                CG.funDecl (Just (CG.emptyDocComment |> CG.markdown "Initialize an i18n instance based on a language"))
                    (Just langToI18nAnn)
                    names.initFunName
                    [ CG.varPattern "lang_" ]
                    (initBody <| \lang -> CG.val lang)

        loadFunName =
            names.loadName ""

        loadLanguageDecl : CG.Declaration
        loadLanguageDecl =
            let
                loadLanguageAnn =
                    Just <| CG.funAnn (CG.typed names.languageTypeName []) (endoAnn <| CG.typed names.i18nTypeName [])
            in
            if needsIntl then
                CG.funDecl (Just (CG.emptyDocComment |> CG.markdown "Switch to another i18n instance based on a language"))
                    loadLanguageAnn
                    loadFunName
                    [ CG.varPattern "lang_", CG.tuplePattern [ CG.allPattern, CG.varPattern "intl_" ] ]
                    (CG.apply [ CG.fun names.initFunName, CG.val "intl_", CG.val "lang_" ])

            else
                CG.funDecl (Just (CG.emptyDocComment |> CG.markdown "Switch to another i18n instance based on a language"))
                    loadLanguageAnn
                    loadFunName
                    [ CG.varPattern "lang_", CG.allPattern ]
                    (CG.apply [ CG.fun names.initFunName, CG.val "lang_" ])

        languages =
            State.getLanguages state

        interpolationMap =
            State.interpolationMap translationSet

        translationSet =
            State.collectiveTranslationSet state

        isIntlNeededForKey key =
            Dict.get key interpolationMap
                |> Maybe.withDefault Dict.empty
                |> Dict.toList
                |> List.any (Tuple.second >> Types.isIntlInterpolation)

        translationToRecordTypeAnn : Types.TKey -> CG.TypeAnnotation
        translationToRecordTypeAnn key =
            let
                placeholders =
                    Dict.get key interpolationMap
                        |> Maybe.withDefault Dict.empty
                        |> Dict.toList
                        |> List.sortBy Tuple.first
            in
            case placeholders of
                [] ->
                    CG.stringAnn

                [ ( _, kind ) ] ->
                    CG.funAnn (Types.interpolationKindToTypeAnn kind) CG.stringAnn

                many ->
                    many
                        |> List.map (Tuple.mapSecond Types.interpolationKindToTypeAnn)
                        |> (\fields -> CG.funAnn (CG.recordAnn fields) CG.stringAnn)

        i18nTypeDecls : List CG.Declaration
        i18nTypeDecls =
            if needsIntl then
                let
                    addIntlIfNeeded key =
                        if isIntlNeededForKey key then
                            CG.funAnn intlAnn

                        else
                            identity
                in
                [ CG.aliasDecl Nothing names.i18nTypeName [] (CG.tupleAnn [ CG.typed (Util.safeName names.i18nTypeName) [], intlAnn ])
                , CG.aliasDecl Nothing
                    (Util.safeName names.i18nTypeName)
                    []
                    (CG.recordAnn <|
                        List.map (\( k, _ ) -> ( Util.safeName k, addIntlIfNeeded k <| translationToRecordTypeAnn k )) pairs
                    )
                ]

            else
                [ CG.aliasDecl Nothing
                    names.i18nTypeName
                    []
                    (CG.recordAnn <|
                        List.map (\( k, _ ) -> ( Util.safeName k, translationToRecordTypeAnn k )) pairs
                    )
                ]

        i18nDeclForLang : String -> Translation () -> CG.Declaration
        i18nDeclForLang lang translation =
            let
                typeDecl =
                    if needsIntl then
                        CG.typed (Util.safeName names.i18nTypeName) []

                    else
                        CG.typed names.i18nTypeName []
            in
            CG.funDecl (Just (CG.emptyDocComment |> CG.markdown ("`I18n` instance containing all values for the language " ++ String.Extra.classify lang)))
                (Just typeDecl)
                lang
                []
            <|
                CG.record (List.map (\( k, v ) -> ( Util.safeName k, inlineTemplate lang k v )) <| List.sortBy Tuple.first translation.pairs)

        accessorDeclForKey : Types.TKey -> CG.Declaration
        accessorDeclForKey key =
            CG.funDecl Nothing
                (Just <| CG.funAnn (CG.typed names.i18nTypeName []) (translationToRecordTypeAnn key))
                key
                [ if needsIntl then
                    CG.tuplePattern [ CG.varPattern "i18n_", CG.varPattern "intl_" ]

                  else
                    CG.varPattern "i18n_"
                ]
                (if isIntlNeededForKey key then
                    CG.apply [ CG.access (CG.val "i18n_") (Util.safeName key), CG.val "intl_" ]

                 else
                    CG.access (CG.val "i18n_") (Util.safeName key)
                )

        accessorDecls =
            interpolationMap
                |> Dict.keys
                |> List.map accessorDeclForKey

        inlineTemplate : String -> Types.TKey -> Types.TValue -> CG.Expression
        inlineTemplate lang key value =
            let
                placeholders =
                    Dict.get key interpolationMap |> Maybe.withDefault Dict.empty

                specificPlaceholdersForThisLanguage =
                    Types.getInterpolationVarNames value

                accessParam =
                    if Dict.size placeholders == 1 then
                        CG.val << Util.safeName

                    else
                        CG.access (CG.val "data_")

                addIntlIfNeeded =
                    if State.valNeedsIntl value then
                        (::) (CG.varPattern "intl_")

                    else
                        identity

                segmentToExpression : Types.TSegment -> CG.Expression
                segmentToExpression segm =
                    case segm of
                        Types.Interpolation var ->
                            accessParam var

                        Types.InterpolationCase var _ ->
                            accessParam var

                        Types.FormatDate var args ->
                            CG.apply
                                [ CG.fqFun [ "Intl" ] "formatDateTime"
                                , CG.val "intl_"
                                , CG.record
                                    [ ( "time", accessParam var )
                                    , ( "language", CG.string lang )
                                    , ( "args", args |> List.map (\( k, v ) -> CG.tuple [ CG.string k, Types.genArgValue v ]) |> CG.list )
                                    ]
                                ]

                        Types.FormatNumber var args ->
                            CG.apply
                                [ CG.fqFun [ "Intl" ] "formatFloat"
                                , CG.val "intl_"
                                , CG.record
                                    [ ( "number", accessParam var )
                                    , ( "language", CG.string lang )
                                    , ( "args", args |> List.map (\( k, v ) -> CG.tuple [ CG.string k, Types.genArgValue v ]) |> CG.list )
                                    ]
                                ]

                        Types.Text text ->
                            CG.string text

                concatenateExpressions e1 e2 =
                    CG.applyBinOp e2 CG.append e1
            in
            List.NonEmpty.map segmentToExpression value
                |> List.NonEmpty.foldl1 concatenateExpressions
                |> (case Dict.toList placeholders of
                        [] ->
                            identity

                        [ ( single, _ ) ] ->
                            CG.lambda <| addIntlIfNeeded [ CG.varPattern <| Util.safeName single ]

                        _ ->
                            CG.lambda <|
                                addIntlIfNeeded
                                    [ if Dict.isEmpty specificPlaceholdersForThisLanguage then
                                        CG.allPattern

                                      else
                                        CG.varPattern "data_"
                                    ]
                   )

        i18nDecls : List CG.Declaration
        i18nDecls =
            Dict.NonEmpty.map i18nDeclForLang translationSet
                |> Dict.NonEmpty.toList
                |> List.map Tuple.second

        ( languageDecls, languageExposes ) =
            languageRelatedDecls names languages

        declarations =
            i18nTypeDecls ++ initDecl :: loadLanguageDecl :: i18nDecls ++ languageDecls ++ accessorDecls

        exposed =
            CG.typeOrAliasExpose names.i18nTypeName
                :: CG.funExpose names.initFunName
                :: CG.funExpose loadFunName
                :: List.map CG.funExpose languages
                ++ languageExposes
                ++ List.map CG.funExpose (Dict.keys interpolationMap)

        fileComment =
            CG.emptyFileComment |> CG.markdown ("This file was generated by elm-i18n version " ++ version ++ ".")
    in
    CG.file (CG.normalModule moduleName exposed)
        (CodeGen.Imports.extractImports declarations |> CodeGen.Imports.dictToImports)
        declarations
        (Just fileComment)
