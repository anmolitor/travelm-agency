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
                simpleI18nType =
                    CG.recordAnn <| List.map (\id -> ( id, CG.fqTyped [ "Array" ] "Array" [ CG.stringAnn ] )) (Dict.NonEmpty.keys ctx.state)

                i18nTypeDecl =
                    CG.customTypeDecl Nothing
                        ctx.names.i18nTypeName
                        []
                        [ ( ctx.names.i18nTypeName
                          , if State.inferFeatures ctx.state |> Features.needsIntl then
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
                        State.inferFeatures ctx.state |> Features.needsIntl

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
                                            (CG.listAnn <| CG.fqTyped [ "Html" ] "Html" [ CG.typed "Never" [] ])

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
                                if List.isEmpty placeholders then
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
                                if needsIntl then
                                    aliasPatternIfNotEmptyPlaceholders (CG.namedPattern ctx.names.i18nTypeName [ CG.recordPattern [ identifier ], CG.allPattern, CG.allPattern ])

                                else
                                    CG.namedPattern ctx.names.i18nTypeName [ CG.recordPattern [ identifier ] ]

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
                            (CG.caseExpr (CG.apply [ CG.fqFun [ "Array" ] "get", CG.int index, CG.val identifier ])
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


addDecodeDeclarations : Unique.UniqueNameContext (WithCtx ctx) -> Unique.UniqueNameContext (WithCtx ctx)
addDecodeDeclarations =
    Unique.scoped <|
        Unique.andThen4 "arr" "i18n" "intl" "lang" <|
            \_ ctx arrName i18nName intlName langName ->
                let
                    needsIntl =
                        State.inferFeatures ctx.state |> Features.needsIntl

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
optimizeJsonAllLanguages addContentHash identifier translationSet =
    Dict.NonEmpty.map
        (\language translation ->
            Types.Translation.map
                (\_ ->
                    let
                        getTranslationForLang lang =
                            Dict.NonEmpty.get lang translationSet

                        interpolationMap =
                            State.interpolationMap translationSet

                        htmlMap =
                            State.getHtmlIdsForTranslationSet translationSet

                        content =
                            Types.Translation.completeFallback getTranslationForLang language translation
                                |> Result.withDefault translation
                                |> .pairs
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
        (\char ( needsEscape, result ) ->
            if char == '{' && needsEscape then
                ( True, result ++ "\\{" )

            else if char == '}' && needsEscape then
                ( True, result ++ "\\}" )

            else if char == '\\' && needsEscape then
                ( True, result ++ "\\\\" )

            else if char == '\\' && not needsEscape then
                ( False, result )

            else
                ( True, result ++ String.fromChar char )
        )
        ( True, "" )
        >> Tuple.second


encodeArgs : List ( String, ArgValue ) -> String
encodeArgs =
    List.map (\( k, v ) -> (E.string k |> E.encode 0) ++ ":" ++ (ArgValue.encode v |> E.encode 0))
        >> String.join ","
