module Generators.DynamicArray exposing (toFile)

import CodeGen.BasicM as BasicM
import CodeGen.DecodeM as DecodeM
import CodeGen.Imports
import CodeGen.Shared exposing (templateTypeAnn, templateTypeAnnRecord)
import Dict.NonEmpty exposing (NonEmpty)
import Elm.CodeGen as CG
import Placeholder.Internal as Placeholder exposing (Template)
import Set
import String.Extra
import Types exposing (I18nPairs)
import Util


type alias Context =
    { identifier : String
    , moduleName : CG.ModuleName
    }


toFile : Context -> NonEmpty String I18nPairs -> CG.File
toFile { moduleName, identifier } state =
    let
        ( _, pairs ) =
            Dict.NonEmpty.getSomeEntry state

        languages =
            Dict.NonEmpty.keys state

        i18nName =
            "I18n"

        decoderName =
            "decoder"

        initName =
            "init"

        loadName =
            "load"

        languageName =
            "Language"

        languagesName =
            "languages"

        languageToStringName =
            "languageToString"

        languageFromStringName =
            "languageFromString"

        i18nTypeAnn =
            CG.typed i18nName []

        i18nTypeDecl =
            CG.customTypeDecl Nothing
                i18nName
                []
                [ ( i18nName, [ CG.fqTyped [ "Array" ] "Array" [ CG.stringAnn ] ] ) ]

        initDecl =
            CG.valDecl Nothing
                (Just <| i18nTypeAnn)
                initName
                (CG.apply [ CG.fun i18nName, CG.fqFun [ "Array" ] "empty" ])

        fallbackValDecl =
            CG.valDecl Nothing (Just CG.stringAnn) "fallbackValue_" (CG.string "...")

        languageTypeDecl : CG.Declaration
        languageTypeDecl =
            CG.customTypeDecl Nothing languageName [] <|
                List.map (String.Extra.classify >> (\lang -> ( lang, [] ))) languages

        languageToStringDecl : CG.Declaration
        languageToStringDecl =
            CG.funDecl Nothing
                (Just <| CG.funAnn (CG.typed languageName []) CG.stringAnn)
                languageToStringName
                [ CG.varPattern "lang_" ]
                (CG.caseExpr (CG.val "lang_") <|
                    List.map (\lang -> ( CG.namedPattern (String.Extra.classify lang) [], CG.string lang )) languages
                )

        languageFromStringDecl : CG.Declaration
        languageFromStringDecl =
            CG.funDecl Nothing
                (Just <| CG.funAnn CG.stringAnn (CG.maybeAnn <| CG.typed languageName []))
                languageFromStringName
                [ CG.varPattern "lang_" ]
                (CG.caseExpr (CG.val "lang_") <|
                    List.map (\lang -> ( CG.stringPattern lang, CG.apply [ CG.fun "Just", CG.val <| String.Extra.classify lang ] )) languages
                        ++ [ ( CG.allPattern, CG.val "Nothing" ) ]
                )

        accessorDeclaration : Int -> ( String, Template ) -> CG.Declaration
        accessorDeclaration index ( key, template ) =
            let
                i18nVar =
                    Util.safeName "i18n"

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
                (Just <| CG.funAnn i18nTypeAnn (templateTypeAnnRecord template))
                key
                (CG.namedPattern i18nName [ CG.varPattern i18nVar ] :: placeholderPatterns)
                (CG.caseExpr (CG.apply [ CG.fqFun [ "Array" ] "get", CG.int index, CG.val i18nVar ])
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

        decoderDecl : CG.Declaration
        decoderDecl =
            CG.funDecl Nothing
                (Just <| DecodeM.decoder i18nTypeAnn)
                "decoder"
                []
                (CG.applyBinOp
                    (CG.apply [ DecodeM.array, DecodeM.string ])
                    CG.piper
                    (CG.apply [ DecodeM.map, CG.fun i18nName ])
                )

        loadDecl : CG.Declaration
        loadDecl =
            CG.funDecl Nothing
                (Just <|
                    CG.funAnn
                        (CG.recordAnn
                            [ ( "language", CG.typed languageName [] )
                            , ( "path", CG.stringAnn )
                            , ( "onLoad", CG.funAnn (BasicM.result (CG.fqTyped [ "Http" ] "Error" []) i18nTypeAnn) (CG.typeVar "msg") )
                            ]
                        )
                        (CG.typed "Cmd" [ CG.typeVar "msg" ])
                )
                loadName
                [ CG.varPattern "opts_" ]
                (let
                    opts =
                        CG.val "opts_"
                 in
                 CG.apply
                    [ CG.fqFun [ "Http" ] "get"
                    , CG.record
                        [ ( "expect", CG.apply [ CG.fqFun [ "Http" ] "expectJson", CG.access opts "onLoad", CG.fun decoderName ] )
                        , ( "url"
                          , appendAll (CG.access opts "path")
                                [ CG.string <| "/" ++ identifier ++ "."
                                , CG.apply [ CG.fun languageToStringName, CG.access opts "language" ]
                                , CG.string ".json"
                                ]
                          )
                        ]
                    ]
                )

        languagesDecl : CG.Declaration
        languagesDecl =
            CG.valDecl Nothing
                (Just <| CG.listAnn <| CG.typed languageName [])
                languagesName
                (CG.list <| List.map (String.Extra.classify >> CG.val) languages)

        declarations =
            [ i18nTypeDecl
            , initDecl
            , languageTypeDecl
            , languagesDecl
            , languageToStringDecl
            , languageFromStringDecl
            , fallbackValDecl
            , decoderDecl
            , loadDecl
            , replacePlaceholdersDecl
            ]
                ++ List.indexedMap accessorDeclaration pairs

        exposed =
            [ CG.closedTypeExpose i18nName
            , CG.openTypeExpose languageName
            , CG.funExpose decoderName
            , CG.funExpose initName
            , CG.funExpose loadName
            , CG.funExpose languagesName
            , CG.funExpose languageToStringName
            , CG.funExpose languageFromStringName
            ]
                ++ List.map (Tuple.first >> CG.funExpose) pairs

        fileComment =
            CG.emptyFileComment |> CG.markdown "This file was generated by elm-i18n."
    in
    CG.file (CG.normalModule moduleName exposed)
        (CodeGen.Imports.extractImports declarations |> Set.toList |> List.map CodeGen.Imports.basicImport)
        declarations
        (Just fileComment)


{-| Generates the following definition

    replacePlaceholders : List String -> String -> String
    replacePlaceholders list str =
        List.foldl (\\val ( index, acc ) -> ( index + 1, String.replace ("{{" ++ String.fromInt index ++ "}}") val acc )) ( 0, str ) list
            |> Tuple.second

-}
replacePlaceholdersDecl : CG.Declaration
replacePlaceholdersDecl =
    CG.funDecl Nothing
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
