module Generators.Dynamic exposing (optimizeJsonAllLanguages, toFile)

import Array
import CodeGen.BasicM as BasicM
import CodeGen.DecodeM as DecodeM
import CodeGen.DynamicParser exposing (addReplacePlaceholderDeclaration)
import CodeGen.Shared as Shared exposing (Context, appendAll, intlAnn)
import CodeGen.Utils
import Dict exposing (Dict)
import Dict.NonEmpty
import Elm.CodeGen as CG
import FNV1a
import Generators.Names as Names exposing (Names)
import Intl exposing (Intl)
import Json.Encode as E
import List.NonEmpty
import Set exposing (Set)
import State exposing (NonEmptyState, OptimizedJson, TranslationSet)
import String.Extra
import Types.ArgValue as ArgValue exposing (ArgValue)
import Types.Basic exposing (Identifier)
import Types.Features as Features
import Types.InterpolationKind as InterpolationKind exposing (InterpolationKind)
import Types.Segment as Segment exposing (TKey, TSegment, TValue)
import Types.Translation
import Types.UniqueName as Unique


type alias WithCtx ctx =
    { ctx
        | names : Names
        , intl : Intl
        , state : NonEmptyState OptimizedJson
        , file : CG.File
        , i18nArgLast : Bool
    }


type alias WithAccessors ctx =
    { ctx | lookupAccessor : String -> String }


type alias WithHelperFunctions ctx =
    { ctx
        | replacePlaceholdersName : String
        , replaceHtmlPlaceholdersName : String
        , lookupLanguageToFileName : String -> String
        , lookupAccessor : String -> String
    }


toFileUnique : Unique.UniqueNameContext (WithCtx ctx) -> CG.File
toFileUnique =
    let
        getKeySet ctx =
            State.allTranslationKeys ctx.state |> Set.fromList

        getIdentifierSet ctx =
            Dict.NonEmpty.keys ctx.state |> Set.fromList
    in
    Unique.combineAndThen getKeySet
        (\_ ctx lookup ->
            { names = ctx.names
            , intl = ctx.intl
            , state = ctx.state
            , i18nArgLast = ctx.i18nArgLast
            , file = ctx.file
            , lookupAccessor = lookup
            }
        )
        >> Unique.combineAndThen (getIdentifierSet >> Set.map ((++) "languageToFileName_"))
            (\_ ctx lookup ->
                { names = ctx.names
                , intl = ctx.intl
                , state = ctx.state
                , file = ctx.file
                , i18nArgLast = ctx.i18nArgLast
                , lookupAccessor = ctx.lookupAccessor
                , lookupLanguageToFileName = (++) "languageToFileName_" >> lookup
                }
            )
        >> Unique.andThen5
            "replacePlaceholders"
            "replaceHtmlPlaceholders"
            "parser"
            "htmlParser"
            "dictParser"
            (\_ ctx replacePlaceholdersName replaceHtmlPlaceholdersName parserName htmlParserName dictParserName ->
                { names = ctx.names
                , intl = ctx.intl
                , state = ctx.state
                , file = ctx.file
                , i18nArgLast = ctx.i18nArgLast
                , lookupAccessor = ctx.lookupAccessor
                , lookupLanguageToFileName = ctx.lookupLanguageToFileName
                , replacePlaceholdersName = replacePlaceholdersName
                , replaceHtmlPlaceholdersName = replaceHtmlPlaceholdersName
                , parserName = parserName
                , htmlParserName = htmlParserName
                , dictParserName = dictParserName
                }
            )
        >> Unique.andThen "intParser"
            (\_ ctx intParserName ->
                { names = ctx.names
                , intl = ctx.intl
                , state = ctx.state
                , file = ctx.file
                , i18nArgLast = ctx.i18nArgLast
                , lookupAccessor = ctx.lookupAccessor
                , lookupLanguageToFileName = ctx.lookupLanguageToFileName
                , replacePlaceholdersName = ctx.replacePlaceholdersName
                , replaceHtmlPlaceholdersName = ctx.replaceHtmlPlaceholdersName
                , parserName = ctx.parserName
                , htmlParserName = ctx.htmlParserName
                , dictParserName = ctx.dictParserName
                , intParserName = intParserName
                }
            )
        >> addI18nTypeDeclaration
        >> addInitDeclaration
        >> addCurrentLanguageDeclaration
        >> addArrivedLanguageDeclaration
        >> Shared.addLanguageRelatedDeclsUnique
        >> addLoadDeclarations
        >> addAccessorDeclarations
        >> addDecodeDeclarations
        >> addReplacePlaceholderDeclaration
        >> addLanguageToFileNameDecls
        >> Unique.unwrap
        >> .file
        >> Shared.finishFile


addI18nTypeDeclaration : Unique.UniqueNameContext (WithCtx ctx) -> Unique.UniqueNameContext (WithCtx ctx)
addI18nTypeDeclaration =
    Unique.map <|
        \ctx ->
            let
                bundleType =
                    CG.tupleAnn [ CG.fqTyped [ "Array" ] "Array" [ CG.stringAnn ], CG.typed ctx.names.languageTypeName [] ]

                simpleI18nType =
                    CG.recordAnn <| List.map (\id -> ( id, bundleType )) (Dict.NonEmpty.keys ctx.state)

                i18nOptsType =
                    CG.recordAnn <|
                        if State.inferFeatures ctx.state |> Features.needsIntl then
                            [ ( "lang", CG.typed ctx.names.languageTypeName [] )
                            , ( "path", CG.stringAnn )
                            , ( "intl", intlAnn )
                            ]

                        else
                            [ ( "lang", CG.typed ctx.names.languageTypeName [] )
                            , ( "path", CG.stringAnn )
                            ]

                i18nTypeDecl =
                    CG.customTypeDecl Nothing
                        ctx.names.i18nTypeName
                        []
                        [ ( ctx.names.i18nTypeName
                          , [ i18nOptsType, simpleI18nType ]
                          )
                        ]
            in
            { ctx | file = ctx.file |> Shared.addDeclaration i18nTypeDecl |> Shared.addExposing (CG.closedTypeExpose ctx.names.i18nTypeName) }


addInitDeclaration : Unique.UniqueNameContext (WithCtx ctx) -> Unique.UniqueNameContext (WithCtx ctx)
addInitDeclaration =
    Unique.scoped <|
        Unique.andThen "opts" <|
            \_ ctx optsName ->
                let
                    emptyBundle =
                        CG.tuple [ CG.fqFun [ "Array" ] "empty", CG.access (CG.val "opts") "lang" ]

                    emptyI18nRecord =
                        CG.record (List.map (\id -> ( id, emptyBundle )) (Dict.NonEmpty.keys ctx.state))

                    needsIntl =
                        State.inferFeatures ctx.state |> Features.needsIntl

                    initDecl =
                        CG.funDecl (Just (CG.emptyDocComment |> CG.markdown "Initialize an (empty) `I18n` instance. This is useful on startup when no JSON was `load`ed yet."))
                            (Just <|
                                CG.funAnn
                                    (CG.recordAnn <|
                                        if needsIntl then
                                            [ ( "lang", CG.typed ctx.names.languageTypeName [] )
                                            , ( "path", CG.stringAnn )
                                            , ( "intl", intlAnn )
                                            ]

                                        else
                                            [ ( "lang", CG.typed ctx.names.languageTypeName [] )
                                            , ( "path", CG.stringAnn )
                                            ]
                                    )
                                <|
                                    CG.typed ctx.names.i18nTypeName []
                            )
                            ctx.names.initFunName
                            [ CG.varPattern optsName ]
                            (CG.apply
                                [ CG.fun ctx.names.i18nTypeName
                                , CG.record <|
                                    if needsIntl then
                                        [ ( "lang", CG.access (CG.val optsName) "lang" )
                                        , ( "path", CG.access (CG.val optsName) "path" )
                                        , ( "intl", CG.access (CG.val optsName) "intl" )
                                        ]

                                    else
                                        [ ( "lang", CG.access (CG.val optsName) "lang" )
                                        , ( "path", CG.access (CG.val optsName) "path" )
                                        ]
                                , emptyI18nRecord
                                ]
                            )
                in
                { ctx | file = ctx.file |> Shared.addDeclaration initDecl |> Shared.addExposing (CG.closedTypeExpose ctx.names.initFunName) }


addAccessorDeclarations :
    Unique.UniqueNameContext (WithHelperFunctions (WithAccessors (WithCtx ctx)))
    -> Unique.UniqueNameContext (WithHelperFunctions (WithAccessors (WithCtx ctx)))
addAccessorDeclarations =
    Unique.scoped <|
        Unique.andThen3 "data" "translation" "i18n" <|
            \lookup ctx dataName translationName i18nName ->
                let
                    interpolationMap =
                        State.interpolationMap <| State.collectiveTranslationSet ctx.state

                    needsIntl =
                        State.inferFeatures ctx.state |> Features.needsIntl

                    htmlMap =
                        State.getHtmlIds ctx.state

                    accessorDeclaration : Identifier -> Int -> ( TKey, TValue ) -> CG.Declaration
                    accessorDeclaration identifier index ( key, value ) =
                        let
                            placeholders =
                                Dict.get key interpolationMap
                                    |> Maybe.withDefault Dict.empty
                                    |> Dict.toList
                                    |> List.sortBy Tuple.first

                            htmlIds =
                                Dict.get key htmlMap |> Maybe.withDefault Set.empty

                            placeholderPatterns =
                                case placeholders of
                                    [] ->
                                        []

                                    [ ( single, _ ) ] ->
                                        [ CG.varPattern <| lookup single ]

                                    _ ->
                                        [ CG.varPattern dataName ]

                            placeholderFunctionArguments =
                                case placeholders of
                                    [] ->
                                        []

                                    [ ( single, kind ) ] ->
                                        [ InterpolationKind.interpolatedValueToString kind <| CG.val <| lookup single ]

                                    many ->
                                        List.map (\( name, _ ) -> CG.access (CG.val dataName) name) many

                            returnType =
                                case List.NonEmpty.fromList <| Set.toList htmlIds of
                                    Nothing ->
                                        CG.stringAnn

                                    Just nonEmptyIds ->
                                        CG.funAnn (Shared.htmlRecordTypeAnn nonEmptyIds)
                                            (CG.listAnn <| CG.fqTyped [ "Html" ] "Html" [ CG.typeVar "msg" ])

                            orderTypeSignature signatureWithoutI18n =
                                (if ctx.i18nArgLast then
                                    CodeGen.Utils.funAnnArgLast

                                 else
                                    CG.funAnn
                                )
                                    (CG.typed ctx.names.i18nTypeName [])
                                <|
                                    signatureWithoutI18n

                            typeAnn =
                                orderTypeSignature <|
                                    case placeholders of
                                        [] ->
                                            returnType

                                        [ ( _, kind ) ] ->
                                            CG.funAnn (InterpolationKind.toTypeAnn kind) returnType

                                        many ->
                                            CG.funAnn
                                                (many
                                                    |> List.map (Tuple.mapSecond InterpolationKind.toTypeAnn)
                                                    |> CG.extRecordAnn "a"
                                                )
                                                returnType

                            aliasPatternIfNotEmptyPlaceholders =
                                if List.isEmpty placeholders && Set.isEmpty htmlIds then
                                    identity

                                else
                                    \pat -> CG.asPattern pat i18nName

                            htmlPatterns =
                                case List.NonEmpty.fromList <| Set.toList htmlIds of
                                    Nothing ->
                                        []

                                    Just ( single, [] ) ->
                                        [ CG.varPattern <| lookup <| single ++ "Attrs" ]

                                    _ ->
                                        [ CG.varPattern <| lookup "extraHtmlAttrs" ]

                            htmlRecordToList nonEmptyIds =
                                if List.NonEmpty.isSingleton nonEmptyIds then
                                    CG.list [ CG.val <| lookup <| List.NonEmpty.head nonEmptyIds ++ "Attrs" ]

                                else
                                    CG.list <|
                                        List.map (CG.access <| CG.val <| lookup "extraHtmlAttrs") <|
                                            List.NonEmpty.toList nonEmptyIds

                            i18nPattern =
                                aliasPatternIfNotEmptyPlaceholders <|
                                    CG.namedPattern ctx.names.i18nTypeName
                                        [ CG.allPattern
                                        , CG.recordPattern [ identifier ]
                                        ]

                            patterns =
                                if ctx.i18nArgLast then
                                    placeholderPatterns ++ htmlPatterns ++ [ i18nPattern ]

                                else
                                    i18nPattern :: placeholderPatterns ++ htmlPatterns

                            intlVal =
                                if needsIntl then
                                    Just <| CG.val i18nName

                                else
                                    Nothing

                            body =
                                case ( List.NonEmpty.fromList <| Set.toList htmlIds, placeholderFunctionArguments ) of
                                    ( Nothing, [] ) ->
                                        CG.val translationName

                                    ( Nothing, _ :: _ ) ->
                                        CG.apply <|
                                            List.filterMap identity
                                                [ Just <| CG.fun ctx.replacePlaceholdersName
                                                , intlVal
                                                , Just <| CG.list placeholderFunctionArguments
                                                , Just <| CG.val translationName
                                                ]

                                    ( Just nonEmptyIds, _ ) ->
                                        CG.apply <|
                                            List.filterMap identity
                                                [ Just <| CG.fun ctx.replaceHtmlPlaceholdersName
                                                , intlVal
                                                , Just <| CG.list placeholderFunctionArguments
                                                , Just <| htmlRecordToList nonEmptyIds
                                                , Just <| CG.val translationName
                                                ]
                        in
                        CG.funDecl Nothing
                            (Just typeAnn)
                            (ctx.lookupAccessor key)
                            patterns
                            (CG.caseExpr
                                (CG.apply
                                    [ CG.fqFun [ "Array" ] "get"
                                    , CG.int index
                                    , CG.parens <| CG.apply [ CG.fqFun [ "Tuple" ] "first", CG.val identifier ]
                                    ]
                                )
                                [ ( CG.namedPattern "Just" [ CG.varPattern translationName ]
                                  , body
                                  )
                                , ( CG.namedPattern "Nothing" []
                                  , if Set.isEmpty htmlIds then
                                        CG.string ""

                                    else
                                        CG.list []
                                  )
                                ]
                            )

                    accessorDecls : List CG.Declaration
                    accessorDecls =
                        Dict.NonEmpty.toList ctx.state
                            |> List.concatMap
                                (\( identifier, translationSet ) ->
                                    (Dict.NonEmpty.foldl1 Types.Translation.append translationSet).pairs
                                        |> Dict.toList
                                        |> List.sortBy Tuple.first
                                        |> List.indexedMap (accessorDeclaration identifier)
                                )
                in
                { ctx
                    | file =
                        ctx.file
                            |> Shared.addDeclarations accessorDecls
                            |> Shared.addExposings (List.filterMap (CodeGen.Utils.declName >> Maybe.map CG.funExpose) accessorDecls)
                }


addCurrentLanguageDeclaration : Unique.UniqueNameContext (WithCtx ctx) -> Unique.UniqueNameContext (WithCtx ctx)
addCurrentLanguageDeclaration =
    Unique.andThen "currentLanguage" <|
        \lookup ctx currentLanguageName ->
            let
                currentLanguageAnn =
                    Just <|
                        CG.funAnn (CG.typed ctx.names.i18nTypeName [])
                            (CG.typed ctx.names.languageTypeName [])

                pattern =
                    CG.namedPattern ctx.names.i18nTypeName [ CG.varPattern <| lookup "opts", CG.allPattern ]

                currentLanguageDecl =
                    CG.funDecl
                        (Just (CG.emptyDocComment |> CG.markdown """Get the currently active language.
Note: This might not be what the user sees in this moment, since the translations might still be loading and instead
another language is visible. If you need the other semantic, use `arrivedLanguage` instead."""))
                        currentLanguageAnn
                        currentLanguageName
                        [ pattern ]
                        (CG.access (CG.val <| lookup "opts") "lang")
            in
            { ctx | file = ctx.file |> Shared.addDeclaration currentLanguageDecl |> Shared.addExposing (CG.funExpose currentLanguageName) }


addArrivedLanguageDeclaration : Unique.UniqueNameContext (WithCtx ctx) -> Unique.UniqueNameContext (WithCtx ctx)
addArrivedLanguageDeclaration =
    Unique.andThen "arrivedLanguage" <|
        \lookup ctx arrivedLanguageName ->
            let
                identifiers =
                    Dict.NonEmpty.keys ctx.state

                arrivedLanguageAnn =
                    Just <|
                        CG.funAnn (CG.typed ctx.names.i18nTypeName [])
                            (CG.typed ctx.names.languageTypeName [])

                pattern =
                    CG.namedPattern ctx.names.i18nTypeName [ CG.varPattern <| lookup "opts", CG.varPattern <| lookup "bundles" ]

                arrivedLanguageDecl =
                    CG.funDecl
                        (Just (CG.emptyDocComment |> CG.markdown """Get the language currently on screen.
This might be different to the `currentLanguage` which is the language which we *like* to have on screen.
Due to loading the translations asynchronously from a server, there will be times where these are different.
This function will equal `currentLanguage` if all pending translations have settled."""))
                        arrivedLanguageAnn
                        arrivedLanguageName
                        [ pattern ]
                    <|
                        CG.pipe (CG.list <| List.map (\identifier -> CG.access (CG.val <| lookup "bundles") identifier) identifiers)
                            [ CG.apply [ CG.fqFun [ "List" ] "filter", CG.parens <| CG.binOpChain (CG.fun "not") CG.composel [ CG.fqFun [ "Array" ] "isEmpty", CG.fqFun [ "Tuple" ] "first" ] ]
                            , CG.apply [ CG.fqFun [ "List" ] "map", CG.fqFun [ "Tuple" ] "second" ]
                            , CG.apply [ CG.fqFun [ "List" ] "filter", CG.parens <| CG.apply [ CG.val "(/=)", CG.access (CG.val <| lookup "opts") "lang" ] ]
                            , CG.fqFun [ "List" ] "head"
                            , CG.apply [ CG.fqFun [ "Maybe" ] "withDefault", CG.access (CG.val <| lookup "opts") "lang" ]
                            ]
            in
            { ctx | file = ctx.file |> Shared.addDeclaration arrivedLanguageDecl |> Shared.addExposing (CG.funExpose arrivedLanguageName) }


addDecodeDeclarations : Unique.UniqueNameContext (WithCtx ctx) -> Unique.UniqueNameContext (WithCtx ctx)
addDecodeDeclarations =
    Unique.scoped <|
        Unique.andThen4 "arr" "bundles" "opts" "lang" <|
            \_ ctx arrName bundlesName optsName langName ->
                let
                    decoderDecl : Identifier -> CG.Declaration
                    decoderDecl identifier =
                        CG.funDecl (Just (CG.emptyDocComment |> CG.markdown """Decode an `I18n` from Json. Make sure this is *only* used on the files generated by this package.
You need to pass the language of the loaded translations (for bookkeeping)."""))
                            (Just <| CG.funAnn (CG.typed ctx.names.languageTypeName []) <| DecodeM.decoder <| Shared.endoAnn <| CG.typed ctx.names.i18nTypeName [])
                            (ctx.names.decoderName identifier)
                            [ CG.varPattern langName ]
                            (CG.applyBinOp
                                (CG.apply [ DecodeM.array, DecodeM.string ])
                                CG.piper
                                (CG.apply
                                    [ DecodeM.map
                                    , CG.lambda
                                        [ CG.varPattern arrName
                                        , CG.namedPattern ctx.names.i18nTypeName
                                            [ CG.varPattern optsName, CG.varPattern bundlesName ]
                                        ]
                                        (CG.apply <|
                                            [ CG.fun ctx.names.i18nTypeName
                                            , CG.val optsName
                                            , CG.update bundlesName
                                                [ ( identifier, CG.tuple [ CG.val arrName, CG.val langName ] ) ]
                                            ]
                                        )
                                    ]
                                )
                            )

                    decoderDecls =
                        ctx.state |> Dict.NonEmpty.keys |> List.map decoderDecl
                in
                { ctx
                    | file =
                        ctx.file
                            |> Shared.addDeclarations decoderDecls
                            |> Shared.addExposings (List.filterMap (CodeGen.Utils.declName >> Maybe.map CG.funExpose) decoderDecls)
                }


addLoadDeclarations : Unique.UniqueNameContext (WithHelperFunctions (WithCtx ctx)) -> Unique.UniqueNameContext (WithHelperFunctions (WithCtx ctx))
addLoadDeclarations =
    Unique.scoped <|
        Unique.andThen2 "onLoad" "opts" <|
            \_ ctx onLoadName optsName ->
                let
                    loadDecl : Identifier -> TranslationSet OptimizedJson -> CG.Declaration
                    loadDecl identifier translations =
                        let
                            ( someLanguage, someTranslation ) =
                                Dict.NonEmpty.getFirstEntry translations
                        in
                        CG.funDecl (Just (CG.emptyDocComment |> CG.markdown ("""
Load translations for identifier '""" ++ identifier ++ """' and a `Language` from the server. This is a simple `Http.get`, if you need more customization,
you can use the `decoder` instead. Pass the path and a callback to your `update` function, for example

    load { language = """ ++ String.Extra.classify someLanguage ++ """, path = "/i18n", onLoad = GotTranslations }

will make a `GET` request to /i18n/""" ++ someTranslation.resources.filename ++ """ and will call GotTranslations with the decoded response.""")))
                            (Just <|
                                CG.funAnn
                                    (CG.funAnn
                                        (BasicM.result (CG.fqTyped [ "Http" ] "Error" [])
                                            (Shared.endoAnn <| CG.typed ctx.names.i18nTypeName [])
                                        )
                                        (CG.typeVar "msg")
                                    )
                                    (CG.funAnn (CG.typed ctx.names.i18nTypeName []) (CG.typed "Cmd" [ CG.typeVar "msg" ]))
                            )
                            (ctx.names.loadName identifier)
                            [ CG.varPattern onLoadName
                            , CG.namedPattern ctx.names.i18nTypeName
                                [ CG.varPattern optsName
                                , CG.allPattern
                                ]
                            ]
                            (CG.apply
                                [ CG.fqFun [ "Http" ] "get"
                                , CG.record
                                    [ ( "expect"
                                      , CG.apply
                                            [ CG.fqFun [ "Http" ] "expectJson"
                                            , CG.val onLoadName
                                            , CG.parens <| CG.apply [ CG.fun (ctx.names.decoderName identifier), CG.access (CG.val optsName) "lang" ]
                                            ]
                                      )
                                    , ( "url"
                                      , appendAll (CG.access (CG.val optsName) "path")
                                            [ CG.string "/"
                                            , CG.apply [ CG.fun <| ctx.lookupLanguageToFileName identifier, CG.access (CG.val optsName) "lang" ]
                                            ]
                                      )
                                    ]
                                ]
                            )

                    loadDecls =
                        ctx.state |> Dict.NonEmpty.map loadDecl |> Dict.NonEmpty.values
                in
                { ctx
                    | file =
                        ctx.file
                            |> Shared.addDeclarations loadDecls
                            |> Shared.addExposings (List.filterMap (CodeGen.Utils.declName >> Maybe.map CG.funExpose) loadDecls)
                }


addLanguageToFileNameDecls : Unique.UniqueNameContext (WithHelperFunctions (WithCtx ctx)) -> Unique.UniqueNameContext (WithHelperFunctions (WithCtx ctx))
addLanguageToFileNameDecls =
    Unique.scoped <|
        Unique.andThen "lang" <|
            \_ ctx langName ->
                let
                    languageToFileNameDecl : Identifier -> TranslationSet OptimizedJson -> CG.Declaration
                    languageToFileNameDecl identifier translations =
                        CG.funDecl Nothing
                            (Just <| CG.funAnn (CG.typed ctx.names.languageTypeName []) CG.stringAnn)
                            (ctx.lookupLanguageToFileName identifier)
                            [ CG.varPattern langName ]
                            (CG.caseExpr (CG.val langName) <|
                                List.map
                                    (\( language, { resources } ) ->
                                        ( CG.namedPattern (String.Extra.classify language) []
                                        , CG.string resources.filename
                                        )
                                    )
                                <|
                                    (List.sortBy Tuple.first << Dict.NonEmpty.toList) translations
                            )

                    languageToFileNameDecls =
                        ctx.state |> Dict.NonEmpty.map languageToFileNameDecl |> Dict.NonEmpty.values
                in
                { ctx
                    | file =
                        ctx.file
                            |> Shared.addDeclarations languageToFileNameDecls
                            |> Shared.addExposings (List.filterMap (CodeGen.Utils.declName >> Maybe.map CG.funExpose) languageToFileNameDecls)
                }


toFile : Context -> NonEmptyState OptimizedJson -> CG.File
toFile context state =
    Unique.new ()
        |> Names.withUniqueNames (Dict.NonEmpty.keys state)
            context.names
            (\names _ ->
                { intl = context.intl
                , state = state
                , file = Shared.emptyFile context
                , names = names
                , i18nArgLast = context.i18nArgLast
                }
            )
        |> toFileUnique



{- Associated JSON generation -}


optimizeJsonAllLanguages : Bool -> Identifier -> TranslationSet resources -> TranslationSet OptimizedJson
optimizeJsonAllLanguages addContentHash identifier translationSet =
    Dict.NonEmpty.map
        (\language translation ->
            Types.Translation.map
                (\_ ->
                    let
                        interpolationMap =
                            State.interpolationMap translationSet

                        htmlMap =
                            State.getHtmlIdsForTranslationSet translationSet

                        content =
                            translation.pairs
                                |> optimizeJson interpolationMap htmlMap
                                |> E.encode 0
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
                )
                translation
        )
        translationSet


optimizeJson : Dict TKey (Dict String InterpolationKind) -> Dict TKey (Set String) -> Dict TKey TValue -> E.Value
optimizeJson interpolationMap htmlMap translations =
    let
        optimizeSegments : ( TKey, TValue ) -> String
        optimizeSegments ( key, val ) =
            let
                interpolations =
                    Dict.get key interpolationMap
                        |> Maybe.map Dict.keys
                        |> Maybe.withDefault []

                htmlIds =
                    Dict.get key htmlMap |> Maybe.withDefault Set.empty
            in
            val
                |> indicifyInterpolations interpolations
                |> indicifyHtmlIds htmlIds
                |> encodeSegments
    in
    translations
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.map optimizeSegments
        |> Array.fromList
        |> E.array E.string


{-| Replaces all interpolations with numbers starting from 0.
Interpolations are assigned numbers in alphabetical order.
Multiple interpolations with the same key get the same number.
-}
indicifyInterpolations : List String -> TValue -> TValue
indicifyInterpolations interpolationSet tval =
    let
        positionDict =
            interpolationSet
                |> List.sort
                |> List.indexedMap (\i var -> ( var, String.fromInt i ))
                |> Dict.fromList

        indicify var =
            Dict.get var positionDict |> Maybe.withDefault ""
    in
    Segment.modifyVars indicify tval


indicifyHtmlIds : Set String -> TValue -> TValue
indicifyHtmlIds htmlIds tval =
    let
        htmlDict =
            htmlIds
                |> Set.toList
                |> List.sort
                |> List.indexedMap (\i var -> ( var, String.fromInt i ))
                |> Dict.fromList

        indicify var =
            Dict.get var htmlDict |> Maybe.withDefault ""
    in
    Segment.modifyHtmlIds indicify tval


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
                k ++ "|" ++ encodeSegments v
            )
        >> String.join "|"


encodeSegment : TSegment -> String
encodeSegment segment =
    case segment of
        Segment.Text str ->
            escapeCurlyBrackets str

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

        Segment.PluralCase var _ default otherOptions ->
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

        Segment.Html html ->
            wrapVar <|
                "H"
                    ++ html.id
                    ++ html.tag
                    ++ "|"
                    ++ encodeSegments html.content
                    ++ "|"
                    ++ (List.concatMap (\( key, val ) -> [ key, encodeSegments val ]) html.attrs
                            |> String.join "|"
                       )


escapeCurlyBrackets : String -> String
escapeCurlyBrackets =
    String.foldl
        (\char result ->
            if List.member char [ '{', '}', '\\', '|' ] then
                result ++ "\\" ++ String.fromChar char

            else
                result ++ String.fromChar char
        )
        ""


encodeArgs : List ( String, ArgValue ) -> String
encodeArgs =
    List.map (\( k, v ) -> (E.string k |> E.encode 0) ++ ":" ++ (ArgValue.encode v |> E.encode 0))
        >> String.join ","
