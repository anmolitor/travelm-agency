module Generators.DynamicElmPH exposing (toFile)

import CodeGen.BasicM as BasicM
import CodeGen.DecodeM as DecodeM
import CodeGen.Imports
import Dict.NonEmpty exposing (NonEmpty)
import Elm.CodeGen as CG
import Placeholder.Internal as Placeholder exposing (Template)
import Set
import String.Extra
import Types exposing (I18nPairs)
import Util
import CodeGen.Shared exposing (templateTypeAnnRecord)
import CodeGen.Shared exposing (templateTypeAnn)


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

        i18nInstanceName =
            "I18nInstance"

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
                [ ( "Loaded", [ CG.typed i18nInstanceName [] ] )
                , ( "NotLoaded", [] )
                ]

        i18nInstanceTypeDecl =
            CG.aliasDecl Nothing
                i18nInstanceName
                []
                (CG.recordAnn <| List.map (Tuple.mapBoth Util.safeName templateTypeAnn) pairs)

        initDecl =
            CG.valDecl Nothing
                (Just <| i18nTypeAnn)
                initName
                (CG.fun "NotLoaded")

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

        accessorDeclaration : ( String, Template ) -> CG.Declaration
        accessorDeclaration ( key, template ) =
            let
                i18nVar =
                    Util.safeName "i18n"

                instanceVar =
                    Util.safeName "instance"

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
                (CG.varPattern i18nVar :: placeholderPatterns)
                (CG.caseExpr (CG.val i18nVar)
                    [ ( CG.namedPattern "Loaded" [ CG.varPattern instanceVar ]
                      , CG.apply <|
                            CG.access (CG.val instanceVar) (Util.safeName key)
                                :: placeholderFunctionArguments
                      )
                    , ( CG.namedPattern "NotLoaded" []
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
                    (List.foldl
                        (\( key, template ) pipeline ->
                            CG.applyBinOp pipeline
                                CG.piper
                                (CG.apply
                                    [ DecodeM.required
                                    , CG.string key
                                    , chooseDecoder template
                                    ]
                                )
                        )
                        (CG.apply [ DecodeM.succeed, CG.fun i18nInstanceName ])
                        (List.indexedMap (\index -> Tuple.mapFirst (always <| String.fromInt index)) pairs)
                    )
                    CG.piper
                    (CG.apply [ DecodeM.map, CG.fun "Loaded" ])
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
            [ i18nInstanceTypeDecl
            , i18nTypeDecl
            , initDecl
            , languageTypeDecl
            , languagesDecl
            , languageToStringDecl
            , languageFromStringDecl
            , fallbackValDecl
            , decoderDecl
            , loadDecl
            , stringParserToDecoder
            ]
                ++ List.map accessorDeclaration pairs

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






chooseDecoder : Template -> CG.Expression
chooseDecoder template =
    let
        numberOfPlaceholders =
            Placeholder.getAlphabeticalPlaceholderNames template |> List.length
    in
    if numberOfPlaceholders == 0 then
        DecodeM.string

    else
        CG.parens <|
            CG.apply
                [ CG.fun "stringParserToDecoder"
                , CG.fqFun [ "Placeholder", "DoubleCurly" ] ("parsePlaceholderAlph" ++ String.fromInt numberOfPlaceholders)
                ]


{-|

    Generates the following function:

    stringParserToDecoder : (String -> Result String a) -> D.Decoder a
    stringParserToDecoder f =
        D.string
            |> D.andThen
                (\str ->
                    case f str of
                        Ok ok ->
                            D.succeed ok

                        Err err ->
                            D.fail err
                )

-}
stringParserToDecoder : CG.Declaration
stringParserToDecoder =
    CG.funDecl Nothing
        (Just <|
            CG.funAnn
                (CG.funAnn CG.stringAnn
                    (CG.typed "Result" [ CG.stringAnn, CG.typeVar "a" ])
                )
                (DecodeM.decoder <| CG.typeVar "a")
        )
        "stringParserToDecoder"
        [ CG.varPattern "f" ]
        (CG.applyBinOp DecodeM.string
            CG.piper
            (CG.apply
                [ DecodeM.andThen
                , CG.parens <|
                    CG.lambda [ CG.varPattern "str" ] <|
                        CG.caseExpr (CG.apply [ CG.val "f", CG.val "str" ])
                            [ ( CG.namedPattern "Ok" [ CG.varPattern "ok" ], CG.apply [ DecodeM.succeed, CG.val "ok" ] )
                            , ( CG.namedPattern "Err" [ CG.varPattern "err" ], CG.apply [ DecodeM.fail, CG.val "err" ] )
                            ]
                ]
            )
        )


appendAll : CG.Expression -> List CG.Expression -> CG.Expression
appendAll =
    List.foldl (\before new -> CG.applyBinOp new CG.append before)
