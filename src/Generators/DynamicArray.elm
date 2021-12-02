module Generators.DynamicArray exposing (toFile)

import CodeGen.BasicM as BasicM
import CodeGen.DecodeM as DecodeM
import CodeGen.Imports
import CodeGen.References as Refs
import CodeGen.Shared exposing (Context, templateTypeAnn, templateTypeAnnRecord)
import Dict exposing (Dict)
import Dict.NonEmpty exposing (NonEmpty)
import Elm.CodeGen as CG
import Generators.Names exposing (IdentifierScopedNames, Names)
import Placeholder.Internal as Placeholder exposing (Template)
import Set
import State exposing (Identifier, NonEmptyState, OptimizedJson, State, TranslationSet)
import String.Extra
import Types exposing (I18nPairs)
import Util


toFile : Context -> NonEmptyState OptimizedJson -> CG.File
toFile { moduleName, version, names, languages } state =
    let
        languagesName =
            "languages"

        identifiers =
            Dict.NonEmpty.keys state

        i18nTypeDecl =
            CG.customTypeDecl Nothing
                names.i18nTypeName
                []
                [ ( names.i18nTypeName, [ CG.recordAnn <| List.map (\id -> ( id, CG.fqTyped [ "Array" ] "Array" [ CG.stringAnn ] )) identifiers ] ) ]

        initDecl =
            CG.valDecl (Just (CG.emptyDocComment |> CG.markdown "Initialize an (empty) `I18n` instance. This is useful on startup when no JSON was `load`ed yet."))
                (Just <| CG.typed names.i18nTypeName [])
                names.initFunName
                (CG.apply [ CG.fun names.i18nTypeName, CG.record (List.map (\id -> ( id, CG.fqFun [ "Array" ] "empty" )) identifiers) ])

        fallbackValDecl =
            CG.valDecl Nothing (Just CG.stringAnn) "fallbackValue_" (CG.string "...")

        languageTypeDecl : CG.Declaration
        languageTypeDecl =
            CG.customTypeDecl (Just (CG.emptyDocComment |> CG.markdown "Enumeration of the supported languages")) names.languageTypeName [] <|
                List.map (String.Extra.classify >> (\lang -> ( lang, [] ))) languages

        languageToStringDecl : CG.Declaration
        languageToStringDecl =
            CG.funDecl (Just (CG.emptyDocComment |> CG.markdown "Convert a `Language` to its `String` representation."))
                (Just <| CG.funAnn (CG.typed names.languageTypeName []) CG.stringAnn)
                names.languageToStringFunName
                [ CG.varPattern "lang_" ]
                (CG.caseExpr (CG.val "lang_") <|
                    List.map (\lang -> ( CG.namedPattern (String.Extra.classify lang) [], CG.string lang )) languages
                )

        languageFromStringDecl : CG.Declaration
        languageFromStringDecl =
            CG.funDecl (Just (CG.emptyDocComment |> CG.markdown """Maybe parse a `Language` from a `String`. 
This only considers the keys given during compile time, if you need something like 'en-US' to map to the correct `Language`,
you should write your own parsing function."""))
                (Just <| CG.funAnn CG.stringAnn (CG.maybeAnn <| CG.typed names.languageTypeName []))
                names.languageFromStringFunName
                [ CG.varPattern "lang_" ]
                (CG.caseExpr (CG.val "lang_") <|
                    List.map (\lang -> ( CG.stringPattern lang, CG.apply [ CG.fun "Just", CG.val <| String.Extra.classify lang ] )) languages
                        ++ [ ( CG.allPattern, CG.val "Nothing" ) ]
                )

        accessorDeclaration : Identifier -> Int -> ( String, Template ) -> CG.Declaration
        accessorDeclaration identifier index ( key, template ) =
            let
                placeholders =
                    Placeholder.getAlphabeticalPlaceholderNames template

                placeholderPatterns =
                    case placeholders of
                        [] ->
                            []

                        [ single ] ->
                            [ CG.varPattern <| Util.safeName single ]

                        _ ->
                            [ CG.varPattern "placeholders_" ]

                placeholderFunctionArguments =
                    case placeholders of
                        [] ->
                            []

                        [ single ] ->
                            [ CG.val <| Util.safeName single ]

                        many ->
                            List.map (\name -> CG.access (CG.val "placeholders_") name) many
            in
            CG.funDecl Nothing
                (Just <| CG.funAnn (CG.typed names.i18nTypeName []) (templateTypeAnnRecord template))
                key
                (CG.namedPattern names.i18nTypeName [ CG.recordPattern [ identifier ] ] :: placeholderPatterns)
                (CG.caseExpr (CG.apply [ CG.fqFun [ "Array" ] "get", CG.int index, CG.val identifier ])
                    [ ( CG.namedPattern "Just" [ CG.varPattern "translation_" ]
                      , if List.isEmpty placeholderFunctionArguments then
                            CG.val "translation_"

                        else
                            CG.apply [ CG.fun "replacePlaceholders", CG.list placeholderFunctionArguments, CG.val "translation_" ]
                      )
                    , ( CG.namedPattern "Nothing" []
                      , CG.val "fallbackValue_"
                      )
                    ]
                )

        accessorDecls : List CG.Declaration
        accessorDecls =
            Dict.NonEmpty.toList state
                |> List.concatMap
                    (\( identifier, translationSet ) ->
                        List.indexedMap (accessorDeclaration identifier)
                            (Dict.NonEmpty.getFirstEntry translationSet |> Tuple.second).pairs
                    )

        languagesDecl : CG.Declaration
        languagesDecl =
            CG.valDecl (Just (CG.emptyDocComment |> CG.markdown "A list containing all `Language`s"))
                (Just <| CG.listAnn <| CG.typed names.languageTypeName [])
                languagesName
                (CG.list <| List.map (String.Extra.classify >> CG.val) languages)

        decodeAndLoadDecls : List CG.Declaration
        decodeAndLoadDecls =
            state
                |> Dict.NonEmpty.map (generateDeclarationsForIdentifier names)
                |> Dict.NonEmpty.values
                |> List.concat

        declarations =
            [ i18nTypeDecl
            , initDecl
            , languageTypeDecl
            , languagesDecl
            , languageToStringDecl
            , languageFromStringDecl
            , fallbackValDecl
            , replacePlaceholdersDecl
            ]
                ++ accessorDecls
                ++ decodeAndLoadDecls

        exposed =
            CG.closedTypeExpose names.i18nTypeName
                :: CG.openTypeExpose names.languageTypeName
                :: List.map CG.funExpose
                    [ names.initFunName
                    , languagesName
                    , names.languageToStringFunName
                    , names.languageFromStringFunName
                    ]
                ++ List.filterMap (Refs.declName >> Maybe.map CG.funExpose) accessorDecls
                ++ List.filterMap (Refs.declName >> Maybe.map CG.funExpose) decodeAndLoadDecls

        fileComment =
            CG.emptyFileComment |> CG.markdown ("This file was generated by elm-i18n version " ++ version ++ ".")
    in
    CG.file (CG.normalModule moduleName exposed)
        (CodeGen.Imports.extractImports declarations |> Set.toList |> List.map CodeGen.Imports.basicImport)
        declarations
        (Just fileComment)


generateDeclarationsForIdentifier : Names -> Identifier -> TranslationSet OptimizedJson -> List CG.Declaration
generateDeclarationsForIdentifier { languageTypeName, i18nTypeName, loadName, decoderName, languageToStringFunName } identifier translations =
    let
        ( someLanguage, someTranslation ) =
            Dict.NonEmpty.getFirstEntry translations

        endoAnn : CG.TypeAnnotation -> CG.TypeAnnotation
        endoAnn t =
            CG.funAnn t t

        decoderDecl : CG.Declaration
        decoderDecl =
            CG.funDecl (Just (CG.emptyDocComment |> CG.markdown "Decode an `I18n` from Json. Make sure this is *only* used on the files generated by this package."))
                (Just <| DecodeM.decoder <| endoAnn <| CG.typed i18nTypeName [])
                (decoderName identifier)
                []
                (CG.applyBinOp
                    (CG.apply [ DecodeM.array, DecodeM.string ])
                    CG.piper
                    (CG.apply
                        [ DecodeM.map
                        , CG.lambda
                            [ CG.varPattern "arr_", CG.namedPattern i18nTypeName [ CG.varPattern "i18n_" ] ]
                            (CG.apply [ CG.fun i18nTypeName, CG.update "i18n_" [ ( identifier, CG.val "arr_" ) ] ])
                        ]
                    )
                )

        languageToFileNameName =
            "languageToFileName_" ++ identifier

        languageToFileNameDecl : CG.Declaration
        languageToFileNameDecl =
            CG.funDecl Nothing
                (Just <| CG.funAnn (CG.typed languageTypeName []) CG.stringAnn)
                languageToFileNameName
                [ CG.varPattern "lang_" ]
                (CG.caseExpr (CG.val "lang_") <|
                    List.map
                        (\( language, { pairs, resources } ) ->
                            ( CG.namedPattern (String.Extra.classify language) []
                            , CG.string resources.filename
                            )
                        )
                    <|
                        (List.sortBy Tuple.first << Dict.NonEmpty.toList) translations
                )

        loadDecl : CG.Declaration
        loadDecl =
            CG.funDecl (Just (CG.emptyDocComment |> CG.markdown ("""
Load translations for identifier '""" ++ identifier ++ """' and a `Language` from the server. This is a simple `Http.get`, if you need more customization,
you can use the `decoder` instead. Pass the path and a callback to your `update` function, for example

    load { language = """ ++ String.Extra.classify someLanguage ++ """, path = "/i18n", onLoad = GotTranslations }

will make a `GET` request to /i18n/""" ++ someTranslation.resources.filename ++ """ and will call GotTranslations with the decoded response.""")))
                (Just <|
                    CG.funAnn
                        (CG.recordAnn
                            [ ( "language", CG.typed languageTypeName [] )
                            , ( "path", CG.stringAnn )
                            , ( "onLoad"
                              , CG.funAnn
                                    (BasicM.result (CG.fqTyped [ "Http" ] "Error" [])
                                        (endoAnn <| CG.typed i18nTypeName [])
                                    )
                                    (CG.typeVar "msg")
                              )
                            ]
                        )
                        (CG.typed "Cmd" [ CG.typeVar "msg" ])
                )
                (loadName identifier)
                [ CG.varPattern "opts_" ]
                (let
                    opts =
                        CG.val "opts_"
                 in
                 CG.apply
                    [ CG.fqFun [ "Http" ] "get"
                    , CG.record
                        [ ( "expect", CG.apply [ CG.fqFun [ "Http" ] "expectJson", CG.access opts "onLoad", CG.fun (decoderName identifier) ] )
                        , ( "url"
                          , appendAll (CG.access opts "path")
                                [ CG.string "/"
                                , CG.apply [ CG.fun languageToFileNameName, CG.access opts "language" ]
                                ]
                          )
                        ]
                    ]
                )
    in
    [ decoderDecl, loadDecl, languageToFileNameDecl ]


{-| Generates the following definition

    replacePlaceholders : List String -> String -> String
    replacePlaceholders list str =
        List.foldl (\\val ( index, acc ) -> ( index + 1, String.replace ("{{" ++ String.fromInt index ++ "}}") val acc )) ( 0, str ) list
            |> Tuple.second

-}
replacePlaceholdersDecl : CG.Declaration
replacePlaceholdersDecl =
    CG.funDecl (Just (CG.emptyDocComment |> CG.markdown "Replaces all placeholder expressions in a string in order with the given values"))
        (Just <| CG.funAnn (CG.listAnn CG.stringAnn) (CG.funAnn CG.stringAnn CG.stringAnn))
        "replacePlaceholders"
        [ CG.varPattern "list_", CG.varPattern "str_" ]
    <|
        CG.applyBinOp
            (CG.apply
                [ CG.fqFun [ "List" ] "foldl"
                , CG.lambda [ CG.varPattern "val_", CG.tuplePattern [ CG.varPattern "i_", CG.varPattern "acc_" ] ]
                    (CG.tuple
                        [ CG.applyBinOp (CG.val "i_") CG.plus (CG.int 1)
                        , CG.apply
                            [ CG.fqFun [ "String" ] "replace"
                            , CG.parens <| appendAll (CG.string "{{") [ CG.apply [ CG.fqFun [ "String" ] "fromInt", CG.val "i_" ], CG.string "}}" ]
                            , CG.val "val_"
                            , CG.val "acc_"
                            ]
                        ]
                    )
                , CG.tuple [ CG.int 0, CG.val "str_" ]
                , CG.val "list_"
                ]
            )
            CG.piper
            (CG.fqFun [ "Tuple" ] "second")


appendAll : CG.Expression -> List CG.Expression -> CG.Expression
appendAll =
    List.foldl (\before new -> CG.applyBinOp new CG.append before)
