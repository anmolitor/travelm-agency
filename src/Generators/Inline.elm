module Generators.Inline exposing (..)

import CodeGen.Shared as Shared exposing (Context, endoAnn, intlAnn)
import CodeGen.Utils
import Dict
import Dict.NonEmpty
import Elm.CodeGen as CG
import Generators.Names as Names exposing (Names)
import Intl exposing (Intl)
import List.Extra
import List.NonEmpty
import Set
import State exposing (NonEmptyState)
import String.Extra
import Types.ArgValue as ArgValue
import Types.Basic exposing (Language)
import Types.Features as Features
import Types.InterpolationKind as InterpolationKind
import Types.Segment as Segment exposing (TKey, TSegment, TValue)
import Types.Translation exposing (Translation)
import Types.UniqueName as Unique
import Util


type alias WithCtx ctx =
    { ctx
        | names : Names
        , intl : Intl
        , state : NonEmptyState ()
        , file : CG.File
        , i18nArgLast : Bool
    }


type alias WithAccessors ctx =
    { ctx
        | lookupAccessor :
            String
            -> String -- the exposed accessor function
        , lookupAccessorProxy :
            String
            -> String -- the matching key in the inline record
    }


type alias WithI18nProxy ctx =
    { ctx | i18nProxyName : String }


toFileUnique : Unique.UniqueNameContext (WithCtx ctx) -> CG.File
toFileUnique =
    let
        addAccessorsToContext : ( WithCtx ctx, String -> String ) -> (String -> String) -> WithAccessors (WithCtx {})
        addAccessorsToContext ( ctx, lookupAccessor ) lookupAccessorProxy =
            { names = ctx.names
            , intl = ctx.intl
            , state = ctx.state
            , file = ctx.file
            , i18nArgLast = ctx.i18nArgLast
            , lookupAccessor = lookupAccessor
            , lookupAccessorProxy = lookupAccessorProxy
            }

        getKeySet ctx =
            State.allTranslationKeys ctx.state |> Set.fromList
    in
    Unique.combineAndThen getKeySet (\_ -> Tuple.pair)
        >> Unique.combineAndThen (Tuple.first >> getKeySet) (\_ -> addAccessorsToContext)
        >> addInitDeclaration
        >> addLoadLanguageDeclaration
        >> addI18nTypeDeclarations
        >> addAccessorDeclarations
        >> Shared.addLanguageRelatedDeclsUnique
        >> Unique.unwrap
        >> .file
        >> Shared.finishFile


addInitDeclaration : Unique.UniqueNameContext (WithCtx ctx) -> Unique.UniqueNameContext (WithCtx ctx)
addInitDeclaration =
    Unique.map <|
        \ctx ->
            let
                langToI18nAnn =
                    CG.funAnn (CG.typed ctx.names.languageTypeName []) (CG.typed ctx.names.i18nTypeName [])

                initDecl : CG.Declaration
                initDecl =
                    if State.inferFeatures ctx.state |> Features.needsIntl then
                        CG.valDecl (Just (CG.emptyDocComment |> CG.markdown "Initialize an i18n instance based on a language and access to the Intl API"))
                            (Just <| CG.funAnn intlAnn langToI18nAnn)
                            ctx.names.initFunName
                            (CG.val ctx.names.i18nTypeName)

                    else
                        CG.valDecl (Just (CG.emptyDocComment |> CG.markdown "Initialize an i18n instance based on a language"))
                            (Just langToI18nAnn)
                            ctx.names.initFunName
                            (CG.val ctx.names.i18nTypeName)
            in
            { ctx | file = ctx.file |> Shared.addDeclaration initDecl |> Shared.addExposing (CG.funExpose ctx.names.initFunName) }


addLoadLanguageDeclaration : Unique.UniqueNameContext (WithCtx ctx) -> Unique.UniqueNameContext (WithCtx ctx)
addLoadLanguageDeclaration =
    Unique.scoped <|
        Unique.andThen3 "load" "lang" "intl" <|
            \_ ctx loadName langName intlName ->
                let
                    loadLanguageAnn =
                        Just <| CG.funAnn (CG.typed ctx.names.languageTypeName []) (endoAnn <| CG.typed ctx.names.i18nTypeName [])

                    loadLanguageDecl =
                        if State.inferFeatures ctx.state |> Features.needsIntl then
                            CG.funDecl (Just (CG.emptyDocComment |> CG.markdown "Switch to another i18n instance based on a language"))
                                loadLanguageAnn
                                loadName
                                [ CG.varPattern langName, CG.namedPattern ctx.names.i18nTypeName [ CG.varPattern intlName, CG.allPattern ] ]
                                (CG.apply [ CG.fun ctx.names.initFunName, CG.val intlName, CG.val langName ])

                        else
                            CG.funDecl (Just (CG.emptyDocComment |> CG.markdown "Switch to another i18n instance based on a language"))
                                loadLanguageAnn
                                loadName
                                [ CG.varPattern langName, CG.allPattern ]
                                (CG.apply [ CG.fun ctx.names.initFunName, CG.val langName ])
                in
                { ctx | file = ctx.file |> Shared.addDeclaration loadLanguageDecl |> Shared.addExposing (CG.funExpose loadName) }


addI18nTypeDeclarations : Unique.UniqueNameContext (WithAccessors (WithCtx ctx)) -> Unique.UniqueNameContext (WithI18nProxy (WithAccessors (WithCtx {})))
addI18nTypeDeclarations unCtx =
    Unique.andThen (Unique.unwrap unCtx).names.i18nTypeName
        (\_ ctx wrappedI18nTypeName ->
            let
                i18nTypeDecls =
                    if State.inferFeatures ctx.state |> Features.needsIntl then
                        [ CG.customTypeDecl Nothing
                            ctx.names.i18nTypeName
                            []
                            [ ( ctx.names.i18nTypeName, [ intlAnn, CG.typed ctx.names.languageTypeName [] ] ) ]
                        ]

                    else
                        [ CG.customTypeDecl Nothing
                            ctx.names.i18nTypeName
                            []
                            [ ( ctx.names.i18nTypeName, [ CG.typed ctx.names.languageTypeName [] ] ) ]
                        ]
            in
            { file = ctx.file |> Shared.addDeclarations i18nTypeDecls |> Shared.addExposing (CG.closedTypeExpose ctx.names.i18nTypeName)
            , intl = ctx.intl
            , names = ctx.names
            , state = ctx.state
            , i18nArgLast = ctx.i18nArgLast
            , lookupAccessor = ctx.lookupAccessor
            , lookupAccessorProxy = ctx.lookupAccessorProxy
            , i18nProxyName = wrappedI18nTypeName
            }
        )
        unCtx


addAccessorDeclarations : Unique.UniqueNameContext (WithAccessors (WithCtx ctx)) -> Unique.UniqueNameContext (WithAccessors (WithCtx ctx))
addAccessorDeclarations =
    Unique.scoped <|
        Unique.andThen4 "intl" "lang" "data" "extraAttrs" <|
            \lookup ctx intlName langName dataName extraAttrsName ->
                let
                    interpolationMap =
                        State.interpolationMap <| State.collectiveTranslationSet ctx.state

                    htmlMap =
                        State.getHtmlIds ctx.state

                    languages =
                        State.getLanguages ctx.state

                    translationSet =
                        State.collectiveTranslationSet ctx.state

                    accessorDeclForKey : TKey -> CG.Declaration
                    accessorDeclForKey key =
                        let
                            placeholderKeys =
                                Dict.get key interpolationMap
                                    |> Maybe.map Dict.keys
                                    |> Maybe.withDefault []

                            htmlIds =
                                Dict.get key htmlMap
                                    |> Maybe.map Set.toList
                                    |> Maybe.withDefault []

                            hasHtml =
                                not <| List.isEmpty htmlIds

                            i18nPattern =
                                if State.inferFeatures ctx.state |> Features.needsIntl then
                                    CG.namedPattern ctx.names.i18nTypeName [ CG.varPattern intlName, CG.varPattern langName ]

                                else
                                    CG.namedPattern ctx.names.i18nTypeName [ CG.varPattern langName ]

                            addHtmlAttrsIfNeeded =
                                case htmlIds of
                                    [] ->
                                        identity

                                    [ single ] ->
                                        (::) (CG.varPattern <| lookup <| single ++ "Attrs")

                                    _ ->
                                        (::) (CG.varPattern extraAttrsName)

                            addPlaceholdersIfNeeded =
                                case placeholderKeys of
                                    [] ->
                                        identity

                                    [ single ] ->
                                        (::) (CG.varPattern <| lookup single)

                                    _ ->
                                        (::) (CG.varPattern dataName)

                            patterns =
                                if ctx.i18nArgLast then
                                    [ i18nPattern ]
                                        |> addHtmlAttrsIfNeeded
                                        |> addPlaceholdersIfNeeded

                                else
                                    i18nPattern :: ([] |> addHtmlAttrsIfNeeded |> addPlaceholdersIfNeeded)
                        in
                        CG.funDecl Nothing
                            (Just <|
                                (if ctx.i18nArgLast then
                                    CodeGen.Utils.funAnnArgLast

                                 else
                                    CG.funAnn
                                )
                                    (CG.typed ctx.names.i18nTypeName [])
                                    (translationToRecordTypeAnn ctx.state key)
                            )
                            (ctx.lookupAccessor key)
                            patterns
                            (List.map (\lang -> ( CG.namedPattern (String.Extra.classify lang) [], inlineTemplate lang key <| lookupTValue lang key )) languages |> CG.caseExpr (CG.val langName))

                    lookupTValue : Language -> TKey -> TValue
                    lookupTValue lang key =
                        translationSet
                            |> Dict.NonEmpty.get lang
                            |> Maybe.andThen (.pairs >> Dict.get key)
                            |> Maybe.withDefault ( Segment.Text "THIS SHOULD NOT HAPPEN.", [] )

                    inlineTemplate : String -> TKey -> TValue -> CG.Expression
                    inlineTemplate lang key value =
                        let
                            placeholders =
                                Dict.get key interpolationMap |> Maybe.withDefault Dict.empty

                            htmlIds =
                                Dict.get key htmlMap |> Maybe.withDefault Set.empty

                            toHtml =
                                Shared.applyWithParensIfNecessary (CG.fqFun [ "Html" ] "text")

                            needsHtmlConversion seg =
                                Set.isEmpty (Segment.htmlIdsForSegment seg) && not (Set.isEmpty htmlIds)

                            specificPlaceholdersForThisLanguage =
                                Segment.interpolationVars value

                            accessParam =
                                if Dict.size placeholders == 1 then
                                    CG.val << lookup

                                else
                                    CG.access (CG.val dataName)

                            refHtmlAttr id =
                                if Set.size htmlIds > 1 then
                                    CG.access (CG.val extraAttrsName) id

                                else
                                    CG.val <| lookup <| id ++ "Attrs"

                            matchStatement default otherOptions =
                                (Dict.toList otherOptions
                                    |> List.map
                                        (\( tkey, tval ) ->
                                            ( CG.stringPattern tkey
                                            , List.NonEmpty.map associateHtmlKindWithSegment tval
                                                |> concatHtmlKindSegments
                                            )
                                        )
                                )
                                    ++ [ ( CG.allPattern
                                         , List.NonEmpty.map associateHtmlKindWithSegment default
                                            |> concatHtmlKindSegments
                                         )
                                       ]

                            associateHtmlKindWithSegment : TSegment -> ( HtmlKind, TSegment )
                            associateHtmlKindWithSegment seg =
                                ( case seg of
                                    Segment.Html _ ->
                                        IsHtml

                                    Segment.PluralCase _ _ _ _ ->
                                        if Set.isEmpty htmlIds then
                                            NoHtml

                                        else
                                            ContainsHtml

                                    Segment.InterpolationCase _ _ _ ->
                                        if Set.isEmpty htmlIds then
                                            NoHtml

                                        else
                                            ContainsHtml

                                    _ ->
                                        if needsHtmlConversion seg then
                                            NoHtml

                                        else
                                            ContainsHtml
                                , seg
                                )

                            segmentToExpression : TSegment -> CG.Expression
                            segmentToExpression segm =
                                case segm of
                                    Segment.Interpolation var ->
                                        (Dict.get var placeholders |> Maybe.map InterpolationKind.interpolatedValueToString |> Maybe.withDefault identity) (accessParam var)

                                    Segment.InterpolationCase var default otherOptions ->
                                        CG.caseExpr (accessParam var)
                                            (matchStatement default otherOptions)

                                    Segment.PluralCase var numArgs default otherOptions ->
                                        CG.caseExpr
                                            (CG.pipe
                                                (CG.apply
                                                    [ CG.fqFun [ "Intl" ] "formatFloat"
                                                    , CG.val intlName
                                                    , CG.record
                                                        [ ( "number", accessParam var )
                                                        , ( "language", CG.string lang )
                                                        , ( "args", numArgs |> List.map (\( k, v ) -> CG.tuple [ CG.string k, ArgValue.generateEncoded v ]) |> CG.list )
                                                        ]
                                                    ]
                                                )
                                                [ CG.fqFun [ "String" ] "toFloat"
                                                , CG.apply
                                                    [ CG.fqFun [ "Maybe" ] "map"
                                                    , CG.parens <|
                                                        CG.lambda [ CG.varPattern <| lookup "n" ]
                                                            (CG.apply
                                                                [ CG.fqFun [ "Intl" ] "determinePluralRuleFloat"
                                                                , CG.val intlName
                                                                , CG.record
                                                                    [ ( "language", CG.string lang )
                                                                    , ( "number", CG.val <| lookup "n" )
                                                                    , ( "type_", CG.fqVal [ "Intl" ] "Cardinal" )
                                                                    ]
                                                                ]
                                                            )
                                                    ]
                                                , CG.apply [ CG.fqFun [ "Maybe" ] "withDefault", CG.fqVal [ "Intl" ] "Other" ]
                                                , CG.fqFun [ "Intl" ] "pluralRuleToString"
                                                ]
                                            )
                                            (matchStatement default otherOptions)

                                    Segment.FormatDate var args ->
                                        CG.apply
                                            [ CG.fqFun [ "Intl" ] "formatDateTime"
                                            , CG.val intlName
                                            , CG.record
                                                [ ( "time", accessParam var )
                                                , ( "language", CG.string lang )
                                                , ( "args", args |> List.map (\( k, v ) -> CG.tuple [ CG.string k, ArgValue.generateEncoded v ]) |> CG.list )
                                                ]
                                            ]

                                    Segment.FormatNumber var args ->
                                        CG.apply
                                            [ CG.fqFun [ "Intl" ] "formatFloat"
                                            , CG.val intlName
                                            , CG.record
                                                [ ( "number", accessParam var )
                                                , ( "language", CG.string lang )
                                                , ( "args", args |> List.map (\( k, v ) -> CG.tuple [ CG.string k, ArgValue.generateEncoded v ]) |> CG.list )
                                                ]
                                            ]

                                    Segment.Text text ->
                                        CG.string text

                                    Segment.Html html ->
                                        CG.apply
                                            [ CG.fqFun [ "Html" ] "node"
                                            , CG.string html.tag
                                            , CG.parens <|
                                                Shared.concatenateLists (refHtmlAttr html.id) (generateHtmlAttrs html.attrs)
                                            , generateHtmlContent html.content
                                            ]

                            generateHtmlAttrs attrs =
                                List.map
                                    (\( attrKey, attrVal ) ->
                                        CG.apply
                                            [ CG.fqFun [ "Html", "Attributes" ] "attribute"
                                            , CG.string attrKey
                                            , List.NonEmpty.map segmentToExpression attrVal
                                                |> List.NonEmpty.foldl1 Shared.concatenateLists
                                            ]
                                    )
                                    attrs
                                    |> CG.list

                            generateHtmlContent content =
                                List.NonEmpty.map associateHtmlKindWithSegment content
                                    |> concatHtmlKindSegments

                            concatHtmlKindSegments segmentsWithHtmlKinds =
                                if Set.isEmpty htmlIds then
                                    List.NonEmpty.map (Tuple.second >> segmentToExpression) segmentsWithHtmlKinds |> List.NonEmpty.foldl1 Shared.concatenateLists

                                else
                                    -- concat any groups of non-html expressions into lists
                                    -- append the lists
                                    List.NonEmpty.toList segmentsWithHtmlKinds
                                        |> List.Extra.groupWhile (\( k1, _ ) ( k2, _ ) -> k1 == k2)
                                        |> List.map
                                            (\(( ( htmlKind, _ ), _ ) as group) ->
                                                let
                                                    expressions =
                                                        List.NonEmpty.map (Tuple.second >> segmentToExpression) group
                                                in
                                                case htmlKind of
                                                    NoHtml ->
                                                        List.NonEmpty.foldl1 Shared.concatenateLists expressions
                                                            |> (if List.NonEmpty.isSingleton group then
                                                                    identity

                                                                else
                                                                    CG.parens
                                                               )
                                                            |> toHtml
                                                            |> List.NonEmpty.singleton
                                                            |> Tuple.pair htmlKind

                                                    _ ->
                                                        ( htmlKind, expressions )
                                            )
                                        |> List.Extra.groupWhile (\( k1, _ ) ( k2, _ ) -> k1 /= ContainsHtml && k2 /= ContainsHtml)
                                        |> List.map
                                            (\(( ( htmlKind, _ ), _ ) as group) ->
                                                case htmlKind of
                                                    ContainsHtml ->
                                                        List.NonEmpty.concatMap Tuple.second group
                                                            |> List.NonEmpty.foldl1 Shared.concatenateLists

                                                    _ ->
                                                        List.NonEmpty.concatMap Tuple.second group
                                                            |> List.NonEmpty.toList
                                                            |> CG.list
                                            )
                                        |> List.NonEmpty.fromList
                                        -- this is always safe since we started with a non-empty list
                                        |> Maybe.withDefault ( CG.val "This should never happen", [] )
                                        |> List.NonEmpty.foldl1 Shared.concatenateLists
                        in
                        List.NonEmpty.map associateHtmlKindWithSegment value
                            |> concatHtmlKindSegments

                    -- |> (case Dict.toList placeholders of
                    --         [] ->
                    --             case addHtmlAttrsIfNeeded [] of
                    --                 [] ->
                    --                     identity
                    --                 nonEmpty ->
                    --                     CG.lambda nonEmpty
                    --         [ ( single, _ ) ] ->
                    --             CG.lambda <| addIntlIfNeeded <| [ CG.varPattern <| lookup single ] ++ addHtmlAttrsIfNeeded []
                    --         _ ->
                    --             CG.lambda <|
                    --                 addIntlIfNeeded <|
                    --                     (if Dict.isEmpty specificPlaceholdersForThisLanguage then
                    --                         CG.allPattern
                    --                      else
                    --                         CG.varPattern dataName
                    --                     )
                    --                         :: addHtmlAttrsIfNeeded []
                    --    )
                    accessorDecls =
                        State.allTranslationKeys ctx.state
                            |> List.map accessorDeclForKey

                    exposings =
                        State.allTranslationKeys ctx.state
                            |> List.map (ctx.lookupAccessor >> CG.funExpose)
                in
                { ctx | file = ctx.file |> Shared.addDeclarations accessorDecls |> Shared.addExposings exposings }


toFile : Context -> NonEmptyState () -> CG.File
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


translationToRecordTypeAnn : NonEmptyState () -> TKey -> CG.TypeAnnotation
translationToRecordTypeAnn state key =
    let
        placeholders =
            State.collectiveTranslationSet state
                |> State.interpolationMap
                |> Dict.get key
                |> Maybe.withDefault Dict.empty
                |> Dict.toList
                |> List.sortBy Tuple.first

        htmlIds =
            State.getHtmlIds state
                |> Dict.get key
                |> Maybe.withDefault Set.empty

        htmlReturnType nonEmptyIds =
            CG.funAnn (Shared.htmlRecordTypeAnn nonEmptyIds)
                (CG.listAnn <| CG.fqTyped [ "Html" ] "Html" [ CG.typeVar "msg" ])
    in
    case ( placeholders, List.NonEmpty.fromList <| Set.toList htmlIds ) of
        ( [], Nothing ) ->
            CG.stringAnn

        ( [], Just nonEmptyIds ) ->
            htmlReturnType nonEmptyIds

        ( [ ( _, kind ) ], Nothing ) ->
            CG.funAnn (InterpolationKind.toTypeAnn kind) CG.stringAnn

        ( [ ( _, kind ) ], Just nonEmptyIds ) ->
            CG.funAnn (InterpolationKind.toTypeAnn kind) (htmlReturnType nonEmptyIds)

        ( many, Nothing ) ->
            many
                |> List.map (Tuple.mapSecond InterpolationKind.toTypeAnn)
                |> (\fields -> CG.funAnn (CG.recordAnn fields) CG.stringAnn)

        ( many, Just nonEmptyIds ) ->
            many
                |> List.map (Tuple.mapSecond InterpolationKind.toTypeAnn)
                |> (\fields -> CG.funAnn (CG.recordAnn fields) (htmlReturnType nonEmptyIds))


type HtmlKind
    = NoHtml
    | ContainsHtml
    | IsHtml
