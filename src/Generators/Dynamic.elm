module Generators.Dynamic exposing (optimizeJsonAllLanguages, toFile)

import Array
import CodeGen.BasicM as BasicM
import CodeGen.DecodeM as DecodeM
import CodeGen.DynamicParser exposing (replacePlaceholdersIntlDecl)
import CodeGen.Shared exposing (Context, appendAll, intlAnn, languageRelatedDecls)
import CodeGen.Utils
import Dict exposing (Dict)
import Dict.NonEmpty
import Elm.CodeGen as CG
import FNV1a
import Generators.Names exposing (Names)
import Json.Encode as E
import List.NonEmpty
import State exposing (Identifier, NonEmptyState, OptimizedJson, TranslationSet, Translations)
import String.Extra
import Types.ArgValue as ArgValue exposing (ArgValue)
import Types.Features as Features
import Types.InterpolationKind as InterpolationKind
import Types.Segment as Segment exposing (TKey, TSegment, TValue)
import Util


toFile : Context -> NonEmptyState OptimizedJson -> CG.File
toFile { moduleName, version, names } state =
    let
        identifiers =
            Dict.NonEmpty.keys state

        needsIntl =
            State.inferFeatures state |> Features.isActive Features.Intl

        languages =
            State.getLanguages state

        interpolationMap =
            state
                |> Dict.NonEmpty.map (\_ -> State.interpolationMap)
                |> Dict.NonEmpty.foldl1 Dict.union

        simpleI18nType =
            CG.recordAnn <| List.map (\id -> ( id, CG.fqTyped [ "Array" ] "Array" [ CG.stringAnn ] )) identifiers

        i18nTypeDecl =
            CG.customTypeDecl Nothing
                names.i18nTypeName
                []
                [ ( names.i18nTypeName
                  , if needsIntl then
                        [ simpleI18nType, intlAnn, CG.typed names.languageTypeName [] ]

                    else
                        [ simpleI18nType ]
                  )
                ]

        emptyI18nRecord =
            CG.record (List.map (\id -> ( id, CG.fqFun [ "Array" ] "empty" )) identifiers)

        initDecl =
            CG.funDecl (Just (CG.emptyDocComment |> CG.markdown "Initialize an (empty) `I18n` instance. This is useful on startup when no JSON was `load`ed yet."))
                (Just <|
                    if needsIntl then
                        CG.funAnn intlAnn (CG.funAnn (CG.typed names.languageTypeName []) (CG.typed names.i18nTypeName []))

                    else
                        CG.typed names.i18nTypeName []
                )
                names.initFunName
                (if needsIntl then
                    [ CG.varPattern "intl_", CG.varPattern "lang_" ]

                 else
                    []
                )
                (CG.apply
                    (CG.fun names.i18nTypeName
                        :: (if needsIntl then
                                [ emptyI18nRecord, CG.val "intl_", CG.val "lang_" ]

                            else
                                [ emptyI18nRecord ]
                           )
                    )
                )

        fallbackValDecl =
            CG.valDecl Nothing (Just CG.stringAnn) "fallbackValue_" (CG.string "...")

        accessorDeclaration : Identifier -> Int -> ( TKey, TValue ) -> CG.Declaration
        accessorDeclaration identifier index ( key, value ) =
            let
                placeholders =
                    Dict.get key interpolationMap
                        |> Maybe.withDefault Dict.empty
                        |> Dict.toList
                        |> List.sortBy Tuple.first

                placeholderPatterns =
                    case placeholders of
                        [] ->
                            []

                        [ ( single, _ ) ] ->
                            [ CG.varPattern <| Util.safeName single ]

                        _ ->
                            [ CG.varPattern "placeholders_" ]

                placeholderFunctionArguments =
                    case placeholders of
                        [] ->
                            []

                        [ ( single, kind ) ] ->
                            [ InterpolationKind.interpolatedValueToString kind <| CG.val <| Util.safeName single ]

                        many ->
                            List.map (\( name, _ ) -> CG.access (CG.val "placeholders_") name) many

                typeAnn =
                    case placeholders of
                        [] ->
                            CG.stringAnn

                        [ ( _, kind ) ] ->
                            CG.funAnn (InterpolationKind.toTypeAnn kind) CG.stringAnn

                        many ->
                            many
                                |> List.map (Tuple.mapSecond InterpolationKind.toTypeAnn)
                                |> (\fields -> CG.funAnn (CG.extRecordAnn "a" fields) CG.stringAnn)

                aliasPatternIfNotEmptyPlaceholders =
                    if List.isEmpty placeholders then
                        identity

                    else
                        \pat -> CG.asPattern pat "i18n_"
            in
            CG.funDecl Nothing
                (Just <| CG.funAnn (CG.typed names.i18nTypeName []) typeAnn)
                key
                ((if needsIntl then
                    aliasPatternIfNotEmptyPlaceholders (CG.namedPattern names.i18nTypeName [ CG.recordPattern [ identifier ], CG.allPattern, CG.allPattern ])

                  else
                    CG.namedPattern names.i18nTypeName [ CG.recordPattern [ identifier ] ]
                 )
                    :: placeholderPatterns
                )
                (CG.caseExpr (CG.apply [ CG.fqFun [ "Array" ] "get", CG.int index, CG.val identifier ])
                    [ ( CG.namedPattern "Just" [ CG.varPattern "translation_" ]
                      , if List.isEmpty placeholderFunctionArguments then
                            CG.val "translation_"

                        else if needsIntl then
                            CG.apply [ CG.fun "replacePlaceholders", CG.val "i18n_", CG.list placeholderFunctionArguments, CG.val "translation_" ]

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

        decodeAndLoadDecls : List CG.Declaration
        decodeAndLoadDecls =
            state
                |> Dict.NonEmpty.map (generateDeclarationsForIdentifier names needsIntl)
                |> Dict.NonEmpty.values
                |> List.concat

        ( languageDecls, languageExposes ) =
            languageRelatedDecls names languages

        declarations =
            [ i18nTypeDecl
            , initDecl
            , fallbackValDecl
            , if needsIntl then
                replacePlaceholdersIntlDecl names

              else
                replacePlaceholdersDecl
            ]
                ++ languageDecls
                ++ accessorDecls
                ++ decodeAndLoadDecls

        exposed =
            CG.closedTypeExpose names.i18nTypeName
                :: CG.openTypeExpose names.languageTypeName
                :: CG.funExpose names.initFunName
                :: languageExposes
                ++ List.filterMap (CodeGen.Utils.declName >> Maybe.map CG.funExpose) accessorDecls
                ++ List.filterMap (CodeGen.Utils.declName >> Maybe.map CG.funExpose) decodeAndLoadDecls

        fileComment =
            CG.emptyFileComment |> CG.markdown ("This file was generated by elm-i18n version " ++ version ++ ".")
    in
    CG.file (CG.normalModule moduleName exposed)
        (CodeGen.Utils.extractImports declarations |> CodeGen.Utils.dictToImports)
        declarations
        (Just fileComment)


generateDeclarationsForIdentifier : Names -> Bool -> Identifier -> TranslationSet OptimizedJson -> List CG.Declaration
generateDeclarationsForIdentifier { languageTypeName, i18nTypeName, loadName, decoderName } needsIntl identifier translations =
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
                            [ CG.varPattern "arr_"
                            , CG.namedPattern i18nTypeName
                                (if needsIntl then
                                    [ CG.varPattern "i18n_", CG.varPattern "intl_", CG.varPattern "lang_" ]

                                 else
                                    [ CG.varPattern "i18n_" ]
                                )
                            ]
                            (CG.apply <|
                                [ CG.fun i18nTypeName, CG.update "i18n_" [ ( identifier, CG.val "arr_" ) ] ]
                                    ++ (if needsIntl then
                                            [ CG.val "intl_", CG.val "lang_" ]

                                        else
                                            []
                                       )
                            )
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
                        (\( language, { resources } ) ->
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
        List.foldl (\\val ( index, acc ) -> ( index + 1, String.replace ("{" ++ String.fromInt index ++ "}") val acc )) ( 0, str ) list
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
                            , CG.parens <| appendAll (CG.string "{") [ CG.apply [ CG.fqFun [ "String" ] "fromInt", CG.val "i_" ], CG.string "}" ]
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



{- Associated JSON generation -}


optimizeJsonAllLanguages : Bool -> Identifier -> TranslationSet resources -> TranslationSet OptimizedJson
optimizeJsonAllLanguages addContentHash identifier =
    Dict.NonEmpty.map <|
        \language { pairs } ->
            { pairs = pairs
            , resources =
                let
                    content =
                        optimizeJson pairs |> E.encode 0
                in
                { content = content
                , filename =
                    String.join "." <|
                        List.filter (not << String.isEmpty)
                            [ identifier
                            , language
                            , if addContentHash then
                                FNV1a.hash content |> String.fromInt

                              else
                                ""
                            , "json"
                            ]
                }
            }


optimizeJson : Translations -> E.Value
optimizeJson translations =
    let
        optimizeSegments : TValue -> String
        optimizeSegments =
            indicifyInterpolations >> encodeSegments
    in
    translations
        |> List.map (Tuple.second >> optimizeSegments)
        |> Array.fromList
        |> E.array E.string


{-| Replaces all interpolations with numbers starting from 0.
Interpolations are assigned numbers in alphabetical order.
Multiple interpolations with the same key get the same number.
-}
indicifyInterpolations : TValue -> TValue
indicifyInterpolations tval =
    let
        interpolationSet =
            tval |> Segment.interpolationVars |> Dict.keys

        positionDict =
            interpolationSet
                |> List.sort
                |> List.indexedMap (\i var -> ( var, String.fromInt i ))
                |> Dict.fromList

        indicify var =
            Dict.get var positionDict |> Maybe.withDefault ""
    in
    Segment.modifyVars indicify tval


wrapVar : String -> String
wrapVar var =
    "{" ++ var ++ "}"


encodeSegments : TValue -> String
encodeSegments =
    List.NonEmpty.map encodeSegment
        >> List.NonEmpty.toList
        >> String.join ""


encodeCaseOptions : Dict String TValue -> String
encodeCaseOptions =
    Dict.toList
        >> List.map
            (\( k, v ) ->
                (E.string k |> E.encode 0)
                    ++ ":"
                    ++ (E.string (encodeSegments v) |> E.encode 0)
            )
        >> String.join ","


encodeSegment : TSegment -> String
encodeSegment segment =
    case segment of
        Segment.Text str ->
            str

        Segment.Interpolation var ->
            wrapVar var

        Segment.InterpolationCase var default otherOptions ->
            wrapVar <|
                "S"
                    ++ var
                    ++ "|"
                    ++ encodeSegments default
                    ++ "|"
                    ++ encodeCaseOptions otherOptions

        Segment.PluralCase var opts default otherOptions ->
            wrapVar <|
                "P"
                    ++ var
                    ++ "|"
                    ++ encodeSegments default
                    ++ "|"
                    ++ encodeCaseOptions otherOptions

        Segment.FormatNumber var args ->
            wrapVar <| "N" ++ var ++ encodeArgs args

        Segment.FormatDate var args ->
            wrapVar <| "D" ++ var ++ encodeArgs args


encodeArgs : List ( String, ArgValue ) -> String
encodeArgs =
    List.map (\( k, v ) -> (E.string k |> E.encode 0) ++ ":" ++ (ArgValue.encode v |> E.encode 0))
        >> String.join ","
