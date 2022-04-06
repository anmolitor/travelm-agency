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
import Generators.Inline exposing (WithCtx)
import Generators.Names as Names exposing (Names)
import Intl exposing (Intl)
import Json.Encode as E
import List.NonEmpty
import Set
import State exposing (Identifier, NonEmptyState, OptimizedJson, TranslationSet, Translations)
import String.Extra
import Types.ArgValue as ArgValue exposing (ArgValue)
import Types.Features as Features
import Types.InterpolationKind as InterpolationKind
import Types.Segment as Segment exposing (TKey, TSegment, TValue)
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
    { ctx | fallbackValueName : String, replacePlaceholdersName : String, lookupLanguageToFileName : String -> String }


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
        >> Unique.andThen2 "fallbackValue"
            "replacePlaceholders"
            (\_ ctx fallbackValueName replacePlaceholdersName ->
                { names = ctx.names
                , intl = ctx.intl
                , state = ctx.state
                , file = ctx.file
                , i18nArgLast = ctx.i18nArgLast
                , lookupAccessor = ctx.lookupAccessor
                , lookupLanguageToFileName = ctx.lookupLanguageToFileName
                , fallbackValueName = fallbackValueName
                , replacePlaceholdersName = replacePlaceholdersName
                }
            )
        >> addI18nTypeDeclaration
        >> addInitDeclaration
        >> Shared.addLanguageRelatedDeclsUnique
        >> addLoadDeclarations
        >> addAccessorDeclarations
        >> addDecodeDeclarations
        >> addFallbackDeclaration
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
                simpleI18nType =
                    CG.recordAnn <| List.map (\id -> ( id, CG.fqTyped [ "Array" ] "Array" [ CG.stringAnn ] )) (Dict.NonEmpty.keys ctx.state)

                i18nTypeDecl =
                    CG.customTypeDecl Nothing
                        ctx.names.i18nTypeName
                        []
                        [ ( ctx.names.i18nTypeName
                          , if State.inferFeatures ctx.state |> Features.isActive Features.Intl then
                                [ simpleI18nType, intlAnn, CG.typed ctx.names.languageTypeName [] ]

                            else
                                [ simpleI18nType ]
                          )
                        ]
            in
            { ctx | file = ctx.file |> Shared.addDeclaration i18nTypeDecl |> Shared.addExposing (CG.closedTypeExpose ctx.names.i18nTypeName) }


addInitDeclaration : Unique.UniqueNameContext (WithCtx ctx) -> Unique.UniqueNameContext (WithCtx ctx)
addInitDeclaration =
    Unique.scoped <|
        Unique.andThen2 "intl" "lang" <|
            \_ ctx intlName langName ->
                let
                    emptyI18nRecord =
                        CG.record (List.map (\id -> ( id, CG.fqFun [ "Array" ] "empty" )) (Dict.NonEmpty.keys ctx.state))

                    needsIntl =
                        State.inferFeatures ctx.state |> Features.isActive Features.Intl

                    initDecl =
                        CG.funDecl (Just (CG.emptyDocComment |> CG.markdown "Initialize an (empty) `I18n` instance. This is useful on startup when no JSON was `load`ed yet."))
                            (Just <|
                                if needsIntl then
                                    CG.funAnn intlAnn (CG.funAnn (CG.typed ctx.names.languageTypeName []) (CG.typed ctx.names.i18nTypeName []))

                                else
                                    CG.typed ctx.names.i18nTypeName []
                            )
                            ctx.names.initFunName
                            (if needsIntl then
                                [ CG.varPattern intlName, CG.varPattern langName ]

                             else
                                []
                            )
                            (CG.apply
                                (CG.fun ctx.names.i18nTypeName
                                    :: (if needsIntl then
                                            [ emptyI18nRecord, CG.val intlName, CG.val langName ]

                                        else
                                            [ emptyI18nRecord ]
                                       )
                                )
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
                        State.inferFeatures ctx.state |> Features.isActive Features.Intl

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

                            orderTypeSignature templateArgsType =
                                if ctx.i18nArgLast then
                                    CG.stringAnn
                                        |> CG.funAnn (CG.typed ctx.names.i18nTypeName [])
                                        |> CG.funAnn templateArgsType

                                else
                                    CG.stringAnn
                                        |> CG.funAnn templateArgsType
                                        |> CG.funAnn (CG.typed ctx.names.i18nTypeName [])

                            typeAnn =
                                case placeholders of
                                    [] ->
                                        CG.funAnn (CG.typed ctx.names.i18nTypeName []) CG.stringAnn

                                    [ ( _, kind ) ] ->
                                        orderTypeSignature (InterpolationKind.toTypeAnn kind)

                                    many ->
                                        many
                                            |> List.map (Tuple.mapSecond InterpolationKind.toTypeAnn)
                                            |> CG.extRecordAnn "a"
                                            |> orderTypeSignature

                            aliasPatternIfNotEmptyPlaceholders =
                                if List.isEmpty placeholders then
                                    identity

                                else
                                    \pat -> CG.asPattern pat i18nName

                            i18nPattern =
                                if needsIntl then
                                    aliasPatternIfNotEmptyPlaceholders (CG.namedPattern ctx.names.i18nTypeName [ CG.recordPattern [ identifier ], CG.allPattern, CG.allPattern ])

                                else
                                    CG.namedPattern ctx.names.i18nTypeName [ CG.recordPattern [ identifier ] ]

                            patterns =
                                if ctx.i18nArgLast then
                                    placeholderPatterns ++ [ i18nPattern ]

                                else
                                    i18nPattern :: placeholderPatterns
                        in
                        CG.funDecl Nothing
                            (Just typeAnn)
                            key
                            patterns
                            (CG.caseExpr (CG.apply [ CG.fqFun [ "Array" ] "get", CG.int index, CG.val identifier ])
                                [ ( CG.namedPattern "Just" [ CG.varPattern translationName ]
                                  , if List.isEmpty placeholderFunctionArguments then
                                        CG.val translationName

                                    else if needsIntl then
                                        CG.apply [ CG.fun ctx.replacePlaceholdersName, CG.val i18nName, CG.list placeholderFunctionArguments, CG.val translationName ]

                                    else
                                        CG.apply [ CG.fun ctx.replacePlaceholdersName, CG.list placeholderFunctionArguments, CG.val translationName ]
                                  )
                                , ( CG.namedPattern "Nothing" []
                                  , CG.val ctx.fallbackValueName
                                  )
                                ]
                            )

                    accessorDecls : List CG.Declaration
                    accessorDecls =
                        Dict.NonEmpty.toList ctx.state
                            |> List.concatMap
                                (\( identifier, translationSet ) ->
                                    (Dict.NonEmpty.getFirstEntry translationSet |> Tuple.second).pairs
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


addFallbackDeclaration : Unique.UniqueNameContext (WithHelperFunctions (WithCtx ctx)) -> Unique.UniqueNameContext (WithHelperFunctions (WithCtx ctx))
addFallbackDeclaration =
    Unique.map <|
        \ctx ->
            let
                fallbackValDecl =
                    CG.valDecl Nothing (Just CG.stringAnn) ctx.fallbackValueName (CG.string "...")
            in
            { ctx | file = ctx.file |> Shared.addDeclaration fallbackValDecl }


addDecodeDeclarations : Unique.UniqueNameContext (WithCtx ctx) -> Unique.UniqueNameContext (WithCtx ctx)
addDecodeDeclarations =
    Unique.scoped <|
        Unique.andThen4 "arr" "i18n" "intl" "lang" <|
            \_ ctx arrName i18nName intlName langName ->
                let
                    needsIntl =
                        State.inferFeatures ctx.state |> Features.isActive Features.Intl

                    decoderDecl : Identifier -> CG.Declaration
                    decoderDecl identifier =
                        CG.funDecl (Just (CG.emptyDocComment |> CG.markdown "Decode an `I18n` from Json. Make sure this is *only* used on the files generated by this package."))
                            (Just <| DecodeM.decoder <| Shared.endoAnn <| CG.typed ctx.names.i18nTypeName [])
                            (ctx.names.decoderName identifier)
                            []
                            (CG.applyBinOp
                                (CG.apply [ DecodeM.array, DecodeM.string ])
                                CG.piper
                                (CG.apply
                                    [ DecodeM.map
                                    , CG.lambda
                                        [ CG.varPattern arrName
                                        , CG.namedPattern ctx.names.i18nTypeName
                                            (if needsIntl then
                                                [ CG.varPattern i18nName, CG.varPattern intlName, CG.varPattern langName ]

                                             else
                                                [ CG.varPattern i18nName ]
                                            )
                                        ]
                                        (CG.apply <|
                                            [ CG.fun ctx.names.i18nTypeName, CG.update i18nName [ ( identifier, CG.val arrName ) ] ]
                                                ++ (if needsIntl then
                                                        [ CG.val intlName, CG.val langName ]

                                                    else
                                                        []
                                                   )
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
        Unique.andThen "opts" <|
            \_ ctx optsName ->
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
                                    (CG.recordAnn
                                        [ ( "language", CG.typed ctx.names.languageTypeName [] )
                                        , ( "path", CG.stringAnn )
                                        , ( "onLoad"
                                          , CG.funAnn
                                                (BasicM.result (CG.fqTyped [ "Http" ] "Error" [])
                                                    (Shared.endoAnn <| CG.typed ctx.names.i18nTypeName [])
                                                )
                                                (CG.typeVar "msg")
                                          )
                                        ]
                                    )
                                    (CG.typed "Cmd" [ CG.typeVar "msg" ])
                            )
                            (ctx.names.loadName identifier)
                            [ CG.varPattern optsName ]
                            (let
                                opts =
                                    CG.val optsName
                             in
                             CG.apply
                                [ CG.fqFun [ "Http" ] "get"
                                , CG.record
                                    [ ( "expect", CG.apply [ CG.fqFun [ "Http" ] "expectJson", CG.access opts "onLoad", CG.fun (ctx.names.decoderName identifier) ] )
                                    , ( "url"
                                      , appendAll (CG.access opts "path")
                                            [ CG.string "/"
                                            , CG.apply [ CG.fun <| ctx.lookupLanguageToFileName identifier, CG.access opts "language" ]
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
        |> Dict.toList
        |> List.sortBy Tuple.first
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


encodeArgs : List ( String, ArgValue ) -> String
encodeArgs =
    List.map (\( k, v ) -> (E.string k |> E.encode 0) ++ ":" ++ (ArgValue.encode v |> E.encode 0))
        >> String.join ","
