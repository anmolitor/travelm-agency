module CodeGen.DynamicParser exposing (addReplacePlaceholderDeclaration)

import CodeGen.Shared as Shared exposing (appendAll, endoAnn)
import Elm.CodeGen as CG
import Elm.Syntax.Expression as Expr
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (emptyRange)
import Generators.Names exposing (Names)
import Parser exposing (..)
import State
import Types.Features as Features
import Types.UniqueName as Unique


addReplacePlaceholderDeclaration :
    Unique.UniqueNameContext
        { ctx
            | names : Names
            , file : CG.File
            , state : State.NonEmptyState any
            , replacePlaceholdersName : String
            , replaceHtmlPlaceholdersName : String
            , parserName : String
            , htmlParserName : String
            , dictParserName : String
            , intParserName : String
        }
    ->
        Unique.UniqueNameContext
            { ctx
                | names : Names
                , file : CG.File
                , state : State.NonEmptyState any
                , replacePlaceholdersName : String
                , replaceHtmlPlaceholdersName : String
                , parserName : String
                , htmlParserName : String
                , dictParserName : String
                , intParserName : String
            }
addReplacePlaceholderDeclaration =
    addDictParserDeclaration
        >> addIntParserDeclaration
        >> Unique.mapWithScope
            (\lookup ctx ->
                let
                    features =
                        ctx.state |> State.inferFeatures

                    filterByFeature feature =
                        if Features.isActive feature features then
                            Just

                        else
                            always Nothing

                    filterIntl =
                        if Features.needsIntl features then
                            Just

                        else
                            always Nothing

                    cfgParserType : CG.TypeAnnotation
                    cfgParserType =
                        (if Features.needsIntl features then
                            CG.funAnn (CG.typed ctx.names.i18nTypeName [])

                         else
                            identity
                        )
                        <|
                            CG.funAnn (CG.listAnn CG.charAnn) <|
                                CG.funAnn (CG.fqTyped [ "Array" ] "Array" [ CG.stringAnn ])
                                    (CG.fqTyped [ "Parser" ] "Parser" [ CG.tupleAnn [ CG.stringAnn, CG.maybeAnn CG.charAnn ] ])

                    cfgHtmlParserType : CG.TypeAnnotation
                    cfgHtmlParserType =
                        (if Features.needsIntl features then
                            CG.funAnn (CG.typed ctx.names.i18nTypeName [])

                         else
                            identity
                        )
                        <|
                            CG.funAnn (CG.listAnn CG.charAnn) <|
                                CG.funAnn (CG.fqTyped [ "Array" ] "Array" [ CG.stringAnn ]) <|
                                    CG.funAnn (CG.fqTyped [ "Array" ] "Array" [ CG.listAnn <| CG.fqTyped [ "Html" ] "Attribute" [ CG.typed "Never" [] ] ]) <|
                                        CG.fqTyped [ "Parser" ]
                                            "Parser"
                                            [ CG.tupleAnn
                                                [ CG.listAnn <| CG.fqTyped [ "Html" ] "Html" [ CG.typed "Never" [] ]
                                                , CG.maybeAnn CG.charAnn
                                                ]
                                            ]

                    cfgParserArgs =
                        List.filterMap identity
                            [ CG.namedPattern ctx.names.i18nTypeName [ CG.allPattern, CG.varPattern <| lookup "intl", CG.varPattern <| lookup "lang" ]
                                |> (\pat -> CG.asPattern pat (lookup "i18n"))
                                |> filterIntl
                            , Just <| CG.varPattern <| lookup "endSymbols"
                            , Just <| CG.varPattern <| lookup "args"
                            ]

                    cfgHtmlParserArgs =
                        List.filterMap identity
                            [ CG.namedPattern ctx.names.i18nTypeName [ CG.allPattern, CG.varPattern <| lookup "intl", CG.varPattern <| lookup "lang" ]
                                |> (\pat -> CG.asPattern pat (lookup "i18n"))
                                |> filterIntl
                            , Just <| CG.varPattern <| lookup "endSymbols"
                            , Just <| CG.varPattern <| lookup "args"
                            , Just <| CG.varPattern <| lookup "extraHtmlAttrs"
                            ]

                    recursiveParserCall : List Char -> CG.Expression
                    recursiveParserCall endSymbols =
                        p_lazy <|
                            CG.apply <|
                                List.filterMap identity
                                    [ Just <| CG.val ctx.parserName
                                    , filterIntl <| CG.val (lookup "i18n")
                                    , Just <| CG.list <| List.map CG.char endSymbols
                                    , Just <| CG.val <| lookup "args"
                                    ]

                    recursiveHtmlParserCall : List Char -> CG.Expression
                    recursiveHtmlParserCall endSymbols =
                        p_lazy <|
                            CG.apply <|
                                List.filterMap identity
                                    [ Just <| CG.val ctx.htmlParserName
                                    , filterIntl <| CG.val (lookup "i18n")
                                    , Just <| CG.list <| List.map CG.char endSymbols
                                    , Just <| CG.val <| lookup "args"
                                    , Just <| CG.val <| lookup "extraHtmlAttrs"
                                    ]

                    cfgParserDecl : CG.Declaration
                    cfgParserDecl =
                        CG.funDecl Nothing
                            (Just cfgParserType)
                            ctx.parserName
                            cfgParserArgs
                            (CG.letExpr
                                (List.filterMap identity
                                    [ Just <|
                                        CG.letFunction (lookup "getArg")
                                            [ CG.varPattern <| lookup "n" ]
                                            (CG.applyBinOp (CG.apply [ CG.fqFun [ "Array" ] "get", CG.val <| lookup "n", CG.val <| lookup "args" ])
                                                CG.piper
                                                defaultMaybeToEmptyString
                                            )
                                    , filterIntl <|
                                        CG.letVal (lookup "wrappedLang")
                                            (CG.binOpChain (CG.string "\"")
                                                CG.append
                                                [ CG.apply [ CG.val ctx.names.languageToStringFunName, CG.val <| lookup "lang" ]
                                                , CG.string "\""
                                                ]
                                            )
                                    , filterIntl <|
                                        CG.letVal (lookup "argParser") <|
                                            generateArgParser { lookup = lookup }
                                    , Just <|
                                        CG.letFunction (lookup "endParsers")
                                            [ CG.varPattern <| lookup "state" ]
                                        <|
                                            CG.applyBinOp
                                                (p_succeed
                                                    (CG.parens <|
                                                        CG.apply
                                                            [ p_Done
                                                            , CG.tuple
                                                                [ CG.val <| lookup "state"
                                                                , CG.fqVal [ "Maybe" ] "Nothing"
                                                                ]
                                                            ]
                                                    )
                                                    |> p_drop_infix p_end
                                                    |> CG.parens
                                                )
                                                CG.cons
                                                (CG.apply
                                                    [ CG.fqFun [ "List" ] "map"
                                                    , CG.lambda [ CG.varPattern <| lookup "symbol" ]
                                                        (p_succeed
                                                            (CG.parens <|
                                                                CG.apply
                                                                    [ p_Done
                                                                    , CG.tuple
                                                                        [ CG.val <| lookup "state"
                                                                        , CG.apply [ CG.fqVal [ "Maybe" ] "Just", CG.val <| lookup "symbol" ]
                                                                        ]
                                                                    ]
                                                            )
                                                            |> p_drop_infix
                                                                (CG.apply
                                                                    [ CG.fqFun [ "Parser" ] "token"
                                                                    , CG.parens <|
                                                                        CG.apply
                                                                            [ CG.fqFun [ "String" ] "fromChar"
                                                                            , CG.val <| lookup "symbol"
                                                                            ]
                                                                    ]
                                                                )
                                                        )
                                                    , CG.parens <| CG.applyBinOp (CG.char '}') CG.cons (CG.val <| lookup "endSymbols")
                                                    ]
                                                )
                                    , Just <|
                                        CG.letFunction
                                            (lookup "escapeParsers")
                                            [ CG.varPattern <| lookup "state" ]
                                        <|
                                            CG.apply
                                                [ CG.fqFun [ "List" ] "map"
                                                , CG.lambda [ CG.varPattern <| lookup "symbol" ]
                                                    (p_succeed
                                                        (CG.parens <|
                                                            CG.apply
                                                                [ p_Loop
                                                                , CG.parens <|
                                                                    CG.applyBinOp
                                                                        (CG.val <| lookup "state")
                                                                        CG.append
                                                                        (CG.val <| lookup "symbol")
                                                                ]
                                                        )
                                                        |> p_drop_infix
                                                            (CG.apply
                                                                [ CG.fqFun [ "Parser" ] "token"
                                                                , CG.parens <|
                                                                    CG.applyBinOp
                                                                        (CG.string "\\")
                                                                        CG.append
                                                                        (CG.val <| lookup "symbol")
                                                                ]
                                                            )
                                                    )
                                                , CG.list <| List.map CG.string [ "{", "}", "\\" ]
                                                ]
                                    , filterIntl <|
                                        CG.letFunction (lookup "numberFormatUnsafe")
                                            [ CG.varPattern <| lookup "n", CG.varPattern <| lookup "parsedArgString" ]
                                            (CG.applyBinOp defaultMaybeToEmptyString CG.pipel <|
                                                CG.applyBinOp
                                                    (CG.apply [ CG.fqFun [ "Intl" ] "unsafeAccess", CG.val <| lookup "intl" ])
                                                    CG.pipel
                                                <|
                                                    appendAll (CG.string "[\"NumberFormat\",[")
                                                        [ CG.val <| lookup "parsedArgString"
                                                        , CG.string "],\"format\",["
                                                        , CG.apply [ CG.val <| lookup "getArg", CG.val <| lookup "n" ]
                                                        , CG.string "]]"
                                                        ]
                                            )
                                    , filterIntl <|
                                        CG.letFunction (lookup "dateFormatUnsafe")
                                            [ CG.varPattern <| lookup "n", CG.varPattern <| lookup "parsedArgString" ]
                                            (CG.applyBinOp defaultMaybeToEmptyString CG.pipel <|
                                                CG.applyBinOp
                                                    (CG.apply [ CG.fqFun [ "Intl" ] "unsafeAccess", CG.val <| lookup "intl" ])
                                                    CG.pipel
                                                <|
                                                    CG.binOpChain
                                                        (CG.string "[\"DateTimeFormat\",[")
                                                        CG.append
                                                        [ CG.val <| lookup "parsedArgString"
                                                        , CG.string "],\"format\",["
                                                        , CG.apply [ CG.val <| lookup "getArg", CG.val <| lookup "n" ]
                                                        , CG.string "]]"
                                                        ]
                                            )
                                    , filterByFeature Features.CaseInterpolation <|
                                        CG.letFunction (lookup "matchStrings") [ CG.varPattern <| lookup "n", CG.varPattern <| lookup "default", CG.varPattern <| lookup "cases" ] <|
                                            CG.pipe (CG.apply [ CG.fqFun [ "Dict" ] "get", CG.parens <| CG.apply [ CG.fun <| lookup "getArg", CG.val <| lookup "n" ], CG.val <| lookup "cases" ])
                                                [ CG.apply [ CG.fqFun [ "Maybe" ] "withDefault", CG.val <| lookup "default" ]
                                                ]
                                    , filterIntl <|
                                        CG.letFunction (lookup "matchNumbers")
                                            [ CG.varPattern <| lookup "n"
                                            , CG.varPattern <| lookup "default"
                                            , CG.varPattern <| lookup "cases"
                                            ]
                                        <|
                                            CG.pipe (CG.apply [ CG.val <| lookup "getArg", CG.val <| lookup "n" ])
                                                [ CG.fqFun [ "String" ] "toFloat"
                                                , CG.apply
                                                    [ CG.fqFun [ "Maybe" ] "andThen"
                                                    , CG.parens <|
                                                        CG.lambda [ CG.varPattern <| lookup "i" ] <|
                                                            CG.pipe
                                                                (CG.apply
                                                                    [ CG.fqFun [ "Intl" ] "determinePluralRuleFloat"
                                                                    , CG.val <| lookup "intl"
                                                                    , CG.record
                                                                        [ ( "language", CG.apply [ CG.val ctx.names.languageToStringFunName, CG.val <| lookup "lang" ] )
                                                                        , ( "number", CG.val <| lookup "i" )
                                                                        , ( "type_", CG.fqVal [ "Intl" ] "Cardinal" )
                                                                        ]
                                                                    ]
                                                                )
                                                                [ CG.fqFun [ "Intl" ] "pluralRuleToString"
                                                                , CG.lambda [ CG.varPattern <| lookup "pluralRule" ] <| CG.apply [ CG.fqFun [ "Dict" ] "get", CG.val <| lookup "pluralRule", CG.val <| lookup "cases" ]
                                                                ]
                                                    ]
                                                , CG.apply [ CG.fqFun [ "Maybe" ] "withDefault", CG.val <| lookup "default" ]
                                                ]
                                    ]
                                )
                             <|
                                CG.applyBinOp (CG.apply [ p_loop, CG.string "" ]) CG.pipel <|
                                    CG.lambda [ CG.varPattern <| lookup "state" ] <|
                                        CG.apply
                                            [ CG.fqFun [ "Parser" ] "oneOf"
                                            , CG.parens <|
                                                CG.applyBinOp (CG.apply [ CG.val <| lookup "endParsers", CG.val <| lookup "state" ])
                                                    CG.append
                                                <|
                                                    CG.applyBinOp (CG.apply [ CG.val <| lookup "escapeParsers", CG.val <| lookup "state" ])
                                                        CG.append
                                                    <|
                                                        CG.list
                                                            [ p_succeed
                                                                (CG.parens <|
                                                                    CG.applyBinOp p_Loop CG.pipel <|
                                                                        CG.applyBinOp (CG.val <| lookup "state") CG.append (CG.string "{")
                                                                )
                                                                |> p_drop_infix (p_token "\\{")
                                                            , p_succeed
                                                                (CG.parens <|
                                                                    CG.applyBinOp p_Loop CG.pipel <|
                                                                        CG.applyBinOp (CG.val <| lookup "state") CG.append (CG.string "}")
                                                                )
                                                                |> p_drop_infix (p_token "\\}")
                                                            , p_succeed
                                                                (CG.parens <| CG.applyBinOp (CG.apply [ CG.fun "(++)", CG.val <| lookup "state" ]) CG.composer p_Loop)
                                                                |> p_drop_infix
                                                                    (p_token "{"
                                                                        |> p_keep_infix
                                                                            (p_oneOf <|
                                                                                List.filterMap identity
                                                                                    [ Just (p_succeed (CG.val <| lookup "getArg") |> p_keep_infix (CG.val ctx.intParserName |> p_drop_infix (p_token "}")))
                                                                                    , filterIntl
                                                                                        (p_succeed (CG.val <| lookup "numberFormatUnsafe")
                                                                                            |> p_drop_infix
                                                                                                (p_token "N"
                                                                                                    |> p_keep_infix (CG.val ctx.intParserName |> p_keep_infix (CG.val <| lookup "argParser"))
                                                                                                )
                                                                                        )
                                                                                    , filterIntl
                                                                                        (p_succeed (CG.val <| lookup "dateFormatUnsafe")
                                                                                            |> p_drop_infix
                                                                                                (p_token "D"
                                                                                                    |> p_keep_infix (CG.val ctx.intParserName |> p_keep_infix (CG.val <| lookup "argParser"))
                                                                                                )
                                                                                        )
                                                                                    , filterByFeature Features.CaseInterpolation
                                                                                        (p_succeed (CG.val <| lookup "matchStrings")
                                                                                            |> p_drop_infix
                                                                                                (p_token "S"
                                                                                                    |> p_keep_infix (CG.val ctx.intParserName)
                                                                                                    |> p_drop_infix (p_token "|")
                                                                                                    |> p_keep_infix (CG.parens <| CG.applyBinOp (p_map (CG.fqFun [ "Tuple" ] "first")) CG.pipel <| CG.parens <| recursiveParserCall [ '|' ])
                                                                                                    |> p_keep_infix (CG.apply [ CG.val ctx.dictParserName, CG.parens <| recursiveParserCall [ '|' ] ])
                                                                                                )
                                                                                        )
                                                                                    , filterIntl
                                                                                        (p_succeed (CG.val <| lookup "matchNumbers")
                                                                                            |> p_drop_infix
                                                                                                (p_token "P"
                                                                                                    |> p_keep_infix (CG.val ctx.intParserName)
                                                                                                    |> p_drop_infix (p_token "|")
                                                                                                    |> p_keep_infix (CG.parens <| CG.applyBinOp (p_map (CG.fqFun [ "Tuple" ] "first")) CG.pipel <| recursiveParserCall [ '|' ])
                                                                                                    |> p_keep_infix (CG.apply [ CG.val ctx.dictParserName, CG.parens <| recursiveParserCall [ '|' ] ])
                                                                                                )
                                                                                        )
                                                                                    ]
                                                                            )
                                                                    )
                                                            , CG.pipe
                                                                (CG.apply
                                                                    [ CG.fqFun [ "Parser" ] "chompWhile"
                                                                    , CG.parens <|
                                                                        CG.lambda [ CG.varPattern <| lookup "c" ] <|
                                                                            CG.pipe
                                                                                (CG.binOpChain (CG.char '{')
                                                                                    CG.cons
                                                                                    [ CG.char '}', CG.char '\\', CG.val <| lookup "endSymbols" ]
                                                                                )
                                                                                [ CG.apply [ CG.fqFun [ "List" ] "member", CG.val <| lookup "c" ]
                                                                                , CG.fun "not"
                                                                                ]
                                                                    ]
                                                                )
                                                                [ p_getChompedString
                                                                , p_map <|
                                                                    CG.parens <|
                                                                        CG.chain (CG.apply [ CG.fun "(++)", CG.val <| lookup "state" ])
                                                                            [ p_Loop ]
                                                                ]
                                                            ]
                                            ]
                            )

                    cfgHtmlParserDecl : CG.Declaration
                    cfgHtmlParserDecl =
                        CG.funDecl Nothing
                            (Just cfgHtmlParserType)
                            ctx.htmlParserName
                            cfgHtmlParserArgs
                            (CG.letExpr
                                (List.filterMap identity
                                    [ Just <|
                                        CG.letFunction (lookup "stringToHtml")
                                            []
                                        <|
                                            CG.chain (CG.fqFun [ "Html" ] "text") [ CG.fqFun [ "List" ] "singleton" ]
                                    , Just <|
                                        CG.letFunction (lookup "getArg")
                                            [ CG.varPattern <| lookup "n" ]
                                            (CG.applyBinOp (CG.apply [ CG.fqFun [ "Array" ] "get", CG.val <| lookup "n", CG.val <| lookup "args" ])
                                                CG.piper
                                                defaultMaybeToEmptyString
                                            )
                                    , Just <|
                                        CG.letFunction (lookup "getHtmlAttrs")
                                            [ CG.varPattern <| lookup "n" ]
                                            (CG.applyBinOp (CG.apply [ CG.fqFun [ "Array" ] "get", CG.val <| lookup "n", CG.val <| lookup "extraHtmlAttrs" ])
                                                CG.piper
                                                (CG.apply [ CG.fqFun [ "Maybe" ] "withDefault", CG.list [] ])
                                            )
                                    , filterIntl <|
                                        CG.letVal (lookup "wrappedLang")
                                            (CG.binOpChain (CG.string "\"")
                                                CG.append
                                                [ CG.apply [ CG.val ctx.names.languageToStringFunName, CG.val <| lookup "lang" ]
                                                , CG.string "\""
                                                ]
                                            )
                                    , filterIntl <|
                                        CG.letVal (lookup "argParser") <|
                                            generateArgParser { lookup = lookup }
                                    , Just <|
                                        CG.letFunction
                                            (lookup "endParsers")
                                            [ CG.varPattern <| lookup "state" ]
                                        <|
                                            CG.applyBinOp
                                                (p_succeed
                                                    (CG.parens <|
                                                        CG.apply
                                                            [ p_Done
                                                            , CG.tuple
                                                                [ CG.val <| lookup "state"
                                                                , CG.fqVal [ "Maybe" ] "Nothing"
                                                                ]
                                                            ]
                                                    )
                                                    |> p_drop_infix p_end
                                                    |> CG.parens
                                                )
                                                CG.cons
                                                (CG.apply
                                                    [ CG.fqFun [ "List" ] "map"
                                                    , CG.lambda [ CG.varPattern <| lookup "symbol" ]
                                                        (p_succeed
                                                            (CG.parens <|
                                                                CG.apply
                                                                    [ p_Done
                                                                    , CG.tuple
                                                                        [ CG.val <| lookup "state"
                                                                        , CG.apply [ CG.fqVal [ "Maybe" ] "Just", CG.val <| lookup "symbol" ]
                                                                        ]
                                                                    ]
                                                            )
                                                            |> p_drop_infix
                                                                (CG.apply
                                                                    [ CG.fqFun [ "Parser" ] "token"
                                                                    , CG.parens <|
                                                                        CG.apply
                                                                            [ CG.fqFun [ "String" ] "fromChar"
                                                                            , CG.val <| lookup "symbol"
                                                                            ]
                                                                    ]
                                                                )
                                                        )
                                                    , CG.parens <| CG.applyBinOp (CG.char '}') CG.cons (CG.val <| lookup "endSymbols")
                                                    ]
                                                )
                                    , Just <|
                                        CG.letFunction
                                            (lookup "escapeParsers")
                                            [ CG.varPattern <| lookup "state" ]
                                        <|
                                            CG.apply
                                                [ CG.fqFun [ "List" ] "map"
                                                , CG.lambda [ CG.varPattern <| lookup "symbol" ]
                                                    (p_succeed
                                                        (CG.parens <|
                                                            CG.apply
                                                                [ p_Loop
                                                                , CG.parens <|
                                                                    CG.applyBinOp
                                                                        (CG.val <| lookup "state")
                                                                        CG.append
                                                                        (CG.apply [ CG.fun <| lookup "stringToHtml", CG.val <| lookup "symbol" ])
                                                                ]
                                                        )
                                                        |> p_drop_infix
                                                            (CG.apply
                                                                [ CG.fqFun [ "Parser" ] "token"
                                                                , CG.parens <|
                                                                    CG.applyBinOp
                                                                        (CG.string "\\")
                                                                        CG.append
                                                                        (CG.val <| lookup "symbol")
                                                                ]
                                                            )
                                                    )
                                                , CG.list <| List.map CG.string [ "{", "}", "\\" ]
                                                ]
                                    , filterIntl <|
                                        CG.letFunction (lookup "numberFormatUnsafe")
                                            [ CG.varPattern <| lookup "n", CG.varPattern <| lookup "parsedArgString" ]
                                            (CG.applyBinOp defaultMaybeToEmptyString CG.pipel <|
                                                CG.applyBinOp
                                                    (CG.apply [ CG.fqFun [ "Intl" ] "unsafeAccess", CG.val <| lookup "intl" ])
                                                    CG.pipel
                                                <|
                                                    appendAll (CG.string "[\"NumberFormat\",[")
                                                        [ CG.val <| lookup "parsedArgString"
                                                        , CG.string "],\"format\",["
                                                        , CG.apply [ CG.val <| lookup "getArg", CG.val <| lookup "n" ]
                                                        , CG.string "]]"
                                                        ]
                                            )
                                    , filterIntl <|
                                        CG.letFunction (lookup "dateFormatUnsafe")
                                            [ CG.varPattern <| lookup "n", CG.varPattern <| lookup "parsedArgString" ]
                                            (CG.applyBinOp defaultMaybeToEmptyString CG.pipel <|
                                                CG.applyBinOp
                                                    (CG.apply [ CG.fqFun [ "Intl" ] "unsafeAccess", CG.val <| lookup "intl" ])
                                                    CG.pipel
                                                <|
                                                    CG.binOpChain
                                                        (CG.string "[\"DateTimeFormat\",[")
                                                        CG.append
                                                        [ CG.val <| lookup "parsedArgString"
                                                        , CG.string "],\"format\",["
                                                        , CG.apply [ CG.val <| lookup "getArg", CG.val <| lookup "n" ]
                                                        , CG.string "]]"
                                                        ]
                                            )
                                    , filterByFeature Features.CaseInterpolation <|
                                        CG.letFunction (lookup "matchStrings") [ CG.varPattern <| lookup "n", CG.varPattern <| lookup "default", CG.varPattern <| lookup "cases" ] <|
                                            CG.pipe (CG.apply [ CG.fqFun [ "Dict" ] "get", CG.parens <| CG.apply [ CG.fun <| lookup "getArg", CG.val <| lookup "n" ], CG.val <| lookup "cases" ])
                                                [ CG.apply [ CG.fqFun [ "Maybe" ] "withDefault", CG.val <| lookup "default" ]
                                                ]
                                    , filterIntl <|
                                        CG.letFunction (lookup "matchNumbers")
                                            [ CG.varPattern <| lookup "n"
                                            , CG.varPattern <| lookup "default"
                                            , CG.varPattern <| lookup "cases"
                                            ]
                                        <|
                                            CG.pipe (CG.apply [ CG.val <| lookup "getArg", CG.val <| lookup "n" ])
                                                [ CG.fqFun [ "String" ] "toFloat"
                                                , CG.apply
                                                    [ CG.fqFun [ "Maybe" ] "andThen"
                                                    , CG.parens <|
                                                        CG.lambda [ CG.varPattern <| lookup "i" ] <|
                                                            CG.pipe
                                                                (CG.apply
                                                                    [ CG.fqFun [ "Intl" ] "determinePluralRuleFloat"
                                                                    , CG.val <| lookup "intl"
                                                                    , CG.record
                                                                        [ ( "language", CG.apply [ CG.val ctx.names.languageToStringFunName, CG.val <| lookup "lang" ] )
                                                                        , ( "number", CG.val <| lookup "i" )
                                                                        , ( "type_", CG.fqVal [ "Intl" ] "Cardinal" )
                                                                        ]
                                                                    ]
                                                                )
                                                                [ CG.fqFun [ "Intl" ] "pluralRuleToString"
                                                                , CG.lambda [ CG.varPattern <| lookup "pluralRule" ] <| CG.apply [ CG.fqFun [ "Dict" ] "get", CG.val <| lookup "pluralRule", CG.val <| lookup "cases" ]
                                                                ]
                                                    ]
                                                , CG.apply [ CG.fqFun [ "Maybe" ] "withDefault", CG.val <| lookup "default" ]
                                                ]
                                    ]
                                )
                             <|
                                CG.applyBinOp (CG.apply [ p_loop, CG.list [] ]) CG.pipel <|
                                    CG.lambda [ CG.varPattern <| lookup "state" ] <|
                                        CG.apply
                                            [ CG.fqFun [ "Parser" ] "oneOf"
                                            , CG.parens <|
                                                CG.applyBinOp (CG.apply [ CG.val <| lookup "endParsers", CG.val <| lookup "state" ])
                                                    CG.append
                                                <|
                                                    CG.applyBinOp (CG.apply [ CG.val <| lookup "escapeParsers", CG.val <| lookup "state" ]) CG.append <|
                                                        CG.list
                                                            [ p_succeed
                                                                (CG.parens <| CG.applyBinOp (CG.apply [ CG.fun "(++)", CG.val <| lookup "state" ]) CG.composer p_Loop)
                                                                |> p_drop_infix
                                                                    (p_token "{"
                                                                        |> p_keep_infix
                                                                            (p_oneOf <|
                                                                                List.filterMap identity
                                                                                    [ Just
                                                                                        (p_succeed
                                                                                            (CG.parens <| CG.chain (CG.val <| lookup "getArg") [ CG.fun <| lookup "stringToHtml" ])
                                                                                            |> p_keep_infix (CG.val ctx.intParserName |> p_drop_infix (p_token "}"))
                                                                                        )
                                                                                    , Just
                                                                                        (p_succeed
                                                                                            (CG.lambda
                                                                                                [ CG.varPattern <| lookup "i"
                                                                                                , CG.varPattern <| lookup "tag"
                                                                                                , CG.varPattern <| lookup "content"
                                                                                                , CG.varPattern <| lookup "attrs"
                                                                                                ]
                                                                                                (CG.list
                                                                                                    [ CG.apply
                                                                                                        [ CG.fqFun [ "Html" ] "node"
                                                                                                        , CG.val <| lookup "tag"
                                                                                                        , CG.parens <|
                                                                                                            CG.applyBinOp
                                                                                                                (CG.parens <|
                                                                                                                    CG.pipe
                                                                                                                        (CG.apply
                                                                                                                            [ CG.fqFun [ "Dict" ] "map"
                                                                                                                            , CG.fqFun [ "Html", "Attributes" ] "attribute"
                                                                                                                            , CG.val <| lookup "attrs"
                                                                                                                            ]
                                                                                                                        )
                                                                                                                        [ CG.fqFun [ "Dict" ] "values" ]
                                                                                                                )
                                                                                                                CG.append
                                                                                                                (CG.apply [ CG.val <| lookup "getHtmlAttrs", CG.val <| lookup "i" ])
                                                                                                        , CG.val <| lookup "content"
                                                                                                        ]
                                                                                                    ]
                                                                                                )
                                                                                            )
                                                                                            |> p_drop_infix (p_token "H")
                                                                                            |> p_keep_infix (CG.val ctx.intParserName)
                                                                                            |> p_keep_infix (CG.parens <| CG.pipe (p_chompUntil "|") [ p_getChompedString ])
                                                                                            |> p_drop_infix (p_token "|")
                                                                                            |> p_keep_infix (CG.parens <| CG.applyBinOp (p_map (CG.fqFun [ "Tuple" ] "first")) CG.pipel <| recursiveHtmlParserCall [ '|' ])
                                                                                            |> p_keep_infix (CG.apply [ CG.val ctx.dictParserName, CG.parens <| recursiveParserCall [ '|' ] ])
                                                                                        )
                                                                                    , filterIntl
                                                                                        (CG.applyBinOp (p_map <| CG.fun <| lookup "stringToHtml")
                                                                                            CG.pipel
                                                                                            (p_succeed (CG.val <| lookup "numberFormatUnsafe")
                                                                                                |> p_drop_infix
                                                                                                    (p_token "N"
                                                                                                        |> p_keep_infix (CG.val ctx.intParserName |> p_keep_infix (CG.val <| lookup "argParser"))
                                                                                                    )
                                                                                            )
                                                                                        )
                                                                                    , filterIntl
                                                                                        (CG.applyBinOp (p_map <| CG.fun <| lookup "stringToHtml")
                                                                                            CG.pipel
                                                                                            (p_succeed (CG.val <| lookup "dateFormatUnsafe")
                                                                                                |> p_drop_infix
                                                                                                    (p_token "D"
                                                                                                        |> p_keep_infix (CG.val ctx.intParserName |> p_keep_infix (CG.val <| lookup "argParser"))
                                                                                                    )
                                                                                            )
                                                                                        )
                                                                                    , filterByFeature Features.CaseInterpolation
                                                                                        (p_succeed (CG.val <| lookup "matchStrings")
                                                                                            |> p_drop_infix
                                                                                                (p_token "S"
                                                                                                    |> p_keep_infix (CG.val ctx.intParserName)
                                                                                                    |> p_drop_infix (p_token "|")
                                                                                                    |> p_keep_infix (CG.parens <| CG.applyBinOp (p_map (CG.fqFun [ "Tuple" ] "first")) CG.pipel <| recursiveHtmlParserCall [ '|' ])
                                                                                                    |> p_keep_infix (CG.apply [ CG.val ctx.dictParserName, CG.parens <| recursiveHtmlParserCall [ '|' ] ])
                                                                                                )
                                                                                        )
                                                                                    , filterIntl
                                                                                        (p_succeed (CG.val <| lookup "matchNumbers")
                                                                                            |> p_drop_infix
                                                                                                (p_token "P"
                                                                                                    |> p_keep_infix (CG.val ctx.intParserName)
                                                                                                    |> p_drop_infix (p_token "|")
                                                                                                    |> p_keep_infix (CG.parens <| CG.applyBinOp (p_map (CG.fqFun [ "Tuple" ] "first")) CG.pipel <| recursiveHtmlParserCall [ '|' ])
                                                                                                    |> p_keep_infix (CG.apply [ CG.val ctx.dictParserName, CG.parens <| recursiveHtmlParserCall [ '|' ] ])
                                                                                                )
                                                                                        )
                                                                                    ]
                                                                            )
                                                                    )
                                                            , CG.pipe
                                                                (CG.apply
                                                                    [ CG.fqFun [ "Parser" ] "chompWhile"
                                                                    , CG.parens <|
                                                                        CG.lambda [ CG.varPattern <| lookup "c" ] <|
                                                                            CG.pipe
                                                                                (CG.binOpChain (CG.char '{')
                                                                                    CG.cons
                                                                                    [ CG.char '}', CG.char '\\', CG.val <| lookup "endSymbols" ]
                                                                                )
                                                                                [ CG.apply [ CG.fqFun [ "List" ] "member", CG.val <| lookup "c" ]
                                                                                , CG.fun "not"
                                                                                ]
                                                                    ]
                                                                )
                                                                [ p_getChompedString
                                                                , p_map <|
                                                                    CG.parens <|
                                                                        CG.chain (CG.val <| lookup "stringToHtml")
                                                                            [ CG.apply [ CG.fun "(++)", CG.val <| lookup "state" ], p_Loop ]
                                                                ]
                                                            ]
                                            ]
                            )

                    replacePlaceholdersType : CG.TypeAnnotation
                    replacePlaceholdersType =
                        (if Features.needsIntl features then
                            CG.funAnn (CG.typed ctx.names.i18nTypeName [])

                         else
                            identity
                        )
                        <|
                            CG.funAnn (CG.listAnn CG.stringAnn) (endoAnn CG.stringAnn)

                    replacePlaceholdersArgs =
                        List.filterMap identity
                            [ filterIntl <| CG.varPattern <| lookup "i18n"
                            , Just <| CG.varPattern <| lookup "argList"
                            ]

                    replacePlaceholdersDecl : CG.Declaration
                    replacePlaceholdersDecl =
                        CG.funDecl (CG.emptyDocComment |> CG.markdown "Replaces all placeholders with the given arguments using the Intl API on the marked spots" |> Just)
                            (Just replacePlaceholdersType)
                            ctx.replacePlaceholdersName
                            replacePlaceholdersArgs
                        <|
                            CG.chain
                                (CG.apply
                                    [ p_run
                                    , CG.parens <|
                                        CG.apply <|
                                            List.filterMap identity
                                                [ Just <| CG.val ctx.parserName
                                                , filterIntl <| CG.val <| lookup "i18n"
                                                , Just <| CG.list []
                                                , Just <| CG.parens <| CG.apply [ CG.fqFun [ "Array" ] "fromList", CG.val <| lookup "argList" ]
                                                ]
                                    ]
                                )
                                [ CG.apply [ CG.fqFun [ "Result" ] "map", CG.fqFun [ "Tuple" ] "first" ]
                                , CG.fqFun [ "Result" ] "toMaybe"
                                , CG.apply [ CG.fqFun [ "Maybe" ] "withDefault", CG.string "" ]
                                ]

                    replaceHtmlPlaceholdersType : CG.TypeAnnotation
                    replaceHtmlPlaceholdersType =
                        (if Features.needsIntl features then
                            CG.funAnn (CG.typed ctx.names.i18nTypeName [])

                         else
                            identity
                        )
                        <|
                            CG.funAnn (CG.listAnn CG.stringAnn) <|
                                CG.funAnn (CG.listAnn <| CG.listAnn <| CG.fqTyped [ "Html" ] "Attribute" [ CG.typed "Never" [] ]) <|
                                    CG.funAnn CG.stringAnn (CG.listAnn <| CG.fqTyped [ "Html" ] "Html" [ CG.typed "Never" [] ])

                    replaceHtmlPlaceholdersArgs =
                        List.filterMap identity
                            [ filterIntl <| CG.varPattern <| lookup "i18n"
                            , Just <| CG.varPattern <| lookup "argList"
                            , Just <| CG.varPattern <| lookup "extraHtmlAttrsList"
                            ]

                    replaceHtmlPlaceholdersDecl : CG.Declaration
                    replaceHtmlPlaceholdersDecl =
                        CG.funDecl (CG.emptyDocComment |> CG.markdown "Replaces all placeholders with the given arguments using the Intl API on the marked spots" |> Just)
                            (Just replaceHtmlPlaceholdersType)
                            ctx.replaceHtmlPlaceholdersName
                            replaceHtmlPlaceholdersArgs
                        <|
                            CG.chain
                                (CG.apply
                                    [ p_run
                                    , CG.parens <|
                                        CG.apply <|
                                            List.filterMap identity
                                                [ Just <| CG.val ctx.htmlParserName
                                                , filterIntl <| CG.val <| lookup "i18n"
                                                , Just <| CG.list []
                                                , Just <| CG.parens <| CG.apply [ CG.fqFun [ "Array" ] "fromList", CG.val <| lookup "argList" ]
                                                , Just <| CG.parens <| CG.apply [ CG.fqFun [ "Array" ] "fromList", CG.val <| lookup "extraHtmlAttrsList" ]
                                                ]
                                    ]
                                )
                                [ CG.apply [ CG.fqFun [ "Result" ] "map", CG.fqFun [ "Tuple" ] "first" ]
                                , CG.fqFun [ "Result" ] "toMaybe"
                                , CG.apply [ CG.fqFun [ "Maybe" ] "withDefault", CG.list [] ]
                                ]
                in
                { ctx
                    | file =
                        if Features.isEmpty features then
                            ctx.file

                        else
                            ctx.file
                                |> Shared.addDeclaration cfgParserDecl
                                |> (if Features.isActive Features.Html features then
                                        Shared.addDeclaration cfgHtmlParserDecl
                                            >> Shared.addDeclaration replaceHtmlPlaceholdersDecl

                                    else
                                        identity
                                   )
                                |> Shared.addDeclaration replacePlaceholdersDecl
                }
            )


addDictParserDeclaration :
    Unique.UniqueNameContext
        { ctx
            | names : Names
            , file : CG.File
            , state : State.NonEmptyState any
            , dictParserName : String
            , parserName : String
        }
    ->
        Unique.UniqueNameContext
            { ctx
                | names : Names
                , file : CG.File
                , state : State.NonEmptyState any
                , dictParserName : String
                , parserName : String
            }
addDictParserDeclaration =
    Unique.mapWithScope <|
        \lookup ctx ->
            let
                dictParserType =
                    CG.funAnn (CG.fqTyped [ "Parser" ] "Parser" [ CG.tupleAnn [ CG.typeVar "a", CG.maybeAnn CG.charAnn ] ]) <|
                        CG.fqTyped [ "Parser" ] "Parser" [ CG.fqTyped [ "Dict" ] "Dict" [ CG.stringAnn, CG.typeVar "a" ] ]

                dictParserDecl =
                    CG.funDecl Nothing (Just dictParserType) ctx.dictParserName [ CG.varPattern <| lookup "valueParser" ] <|
                        CG.apply
                            [ p_loop
                            , CG.tuple [ CG.fqVal [ "Maybe" ] "Nothing", CG.fqVal [ "Dict" ] "empty" ]
                            , CG.lambda [ CG.tuplePattern [ CG.varPattern <| lookup "prevKey", CG.varPattern <| lookup "cases" ] ] <|
                                CG.caseExpr (CG.val <| lookup "prevKey")
                                    [ ( CG.namedPattern "Just" [ CG.varPattern <| lookup "key" ]
                                      , CG.applyBinOp
                                            (CG.val <| lookup "valueParser")
                                            CG.piper
                                        <|
                                            p_map
                                                (CG.lambda
                                                    [ CG.tuplePattern
                                                        [ CG.varPattern <| lookup "val"
                                                        , CG.varPattern <| lookup "endSymbol"
                                                        ]
                                                    ]
                                                 <|
                                                    CG.caseExpr (CG.val <| lookup "endSymbol")
                                                        [ ( CG.namedPattern "Just" [ CG.charPattern '|' ]
                                                          , CG.apply
                                                                [ p_Loop
                                                                , CG.tuple
                                                                    [ CG.fqVal [ "Maybe" ] "Nothing"
                                                                    , CG.apply
                                                                        [ CG.fqFun [ "Dict" ] "insert"
                                                                        , CG.val <| lookup "key"
                                                                        , CG.val <| lookup "val"
                                                                        , CG.val <| lookup "cases"
                                                                        ]
                                                                    ]
                                                                ]
                                                          )
                                                        , ( CG.allPattern
                                                          , CG.apply
                                                                [ p_Done
                                                                , CG.parens <|
                                                                    CG.apply
                                                                        [ CG.fqFun [ "Dict" ] "insert"
                                                                        , CG.val <| lookup "key"
                                                                        , CG.val <| lookup "val"
                                                                        , CG.val <| lookup "cases"
                                                                        ]
                                                                ]
                                                          )
                                                        ]
                                                )
                                      )
                                    , ( CG.namedPattern "Nothing" []
                                      , p_oneOf
                                            [ p_succeed (CG.parens <| CG.apply [ p_Done, CG.val <| lookup "cases" ])
                                                |> p_drop_infix (p_token "}")
                                            , p_succeed
                                                (CG.lambda [ CG.varPattern <| lookup "key" ] <|
                                                    CG.apply
                                                        [ p_Loop
                                                        , CG.tuple
                                                            [ CG.apply
                                                                [ CG.fqFun [ "Maybe" ] "Just"
                                                                , CG.val <| lookup "key"
                                                                ]
                                                            , CG.val <| lookup "cases"
                                                            ]
                                                        ]
                                                )
                                                |> p_keep_infix (CG.parens <| CG.pipe (p_chompUntil "|") [ p_getChompedString ])
                                                |> p_drop_infix (p_token "|")
                                            ]
                                      )
                                    ]
                            ]
            in
            { ctx
                | file =
                    if
                        State.inferFeatures ctx.state
                            |> Features.oneIsActive
                                [ Features.CaseInterpolation, Features.Html, Features.IntlNumber, Features.IntlDate, Features.IntlPlural ]
                    then
                        ctx.file |> Shared.addDeclaration dictParserDecl

                    else
                        ctx.file
            }


generateArgParser : { lookup : String -> String } -> CG.Expression
generateArgParser { lookup } =
    p_oneOf
        [ p_succeed (CG.val <| lookup "wrappedLang") |> p_drop_infix (p_token "}")
        , p_succeed
            (CG.parens <|
                CG.lambda [ CG.varPattern <| lookup "str" ] <|
                    CG.binOpChain (CG.val <| lookup "wrappedLang") CG.append [ CG.string ",{", CG.val <| lookup "str", CG.string "}" ]
            )
            |> p_keep_infix
                (CG.parens (CG.applyBinOp (p_chompUntil "}") CG.piper p_getChompedString)
                    |> p_drop_infix (p_token "}")
                )
        ]


{-| Necessary because of some weird behaviour with parsing 0a vs 0b etc. with the default int parser
-}
addIntParserDeclaration : Unique.UniqueNameContext { ctx | file : CG.File, intParserName : String } -> Unique.UniqueNameContext { ctx | file : CG.File, intParserName : String }
addIntParserDeclaration =
    let
        body =
            CG.pipe (CG.apply [ CG.fqFun [ "Parser" ] "chompWhile", CG.fqFun [ "Char" ] "isDigit" ])
                [ p_getChompedString
                , CG.apply
                    [ CG.fqFun [ "Parser" ] "andThen"
                    , CG.parens <|
                        CG.chain (CG.fqFun [ "String" ] "toInt")
                            [ CG.apply [ CG.fqFun [ "Maybe" ] "map", CG.fqFun [ "Parser" ] "succeed" ]
                            , CG.apply
                                [ CG.fqFun [ "Maybe" ] "withDefault"
                                , CG.parens <| CG.apply [ CG.fqFun [ "Parser" ] "problem", CG.string "Expected int" ]
                                ]
                            ]
                    ]
                ]

        decl name =
            CG.valDecl Nothing
                (Just <| CG.fqTyped [ "Parser" ] "Parser" [ CG.intAnn ])
                name
                body
    in
    Unique.map (\ctx -> { ctx | file = ctx.file |> Shared.addDeclaration (decl ctx.intParserName) })


defaultMaybeToEmptyString : CG.Expression
defaultMaybeToEmptyString =
    CG.apply [ CG.fqFun [ "Maybe" ] "withDefault", CG.string "" ]


p_keep_infix : CG.Expression -> CG.Expression -> CG.Expression
p_keep_infix =
    customOp "|="


p_drop_infix : CG.Expression -> CG.Expression -> CG.Expression
p_drop_infix =
    customOp "|."


{-| reversed to better work with |>
left |> customOp "|." right
-}
customOp : String -> CG.Expression -> CG.Expression -> CG.Expression
customOp symbol right left =
    Expr.OperatorApplication symbol Infix.Left (Node emptyRange left) (Node emptyRange right)


p_chompUntil : String -> CG.Expression
p_chompUntil symbol =
    CG.apply [ CG.fqFun [ "Parser" ] "chompUntil", CG.string symbol ]


p_getChompedString : CG.Expression
p_getChompedString =
    CG.fqFun [ "Parser" ] "getChompedString"


p_loop : CG.Expression
p_loop =
    CG.fqFun [ "Parser" ] "loop"


p_Loop : CG.Expression
p_Loop =
    CG.fqFun [ "Parser" ] "Loop"


p_Done : CG.Expression
p_Done =
    CG.fqFun [ "Parser" ] "Done"


p_end : CG.Expression
p_end =
    CG.fqFun [ "Parser" ] "end"


p_map : CG.Expression -> CG.Expression
p_map expr =
    CG.apply [ CG.fqFun [ "Parser" ] "map", expr ]


p_succeed : CG.Expression -> CG.Expression
p_succeed expr =
    CG.apply [ CG.fqFun [ "Parser" ] "succeed", expr ]


p_lazy : CG.Expression -> CG.Expression
p_lazy expr =
    CG.applyBinOp (CG.fqFun [ "Parser" ] "lazy") CG.pipel (CG.lambda [ CG.allPattern ] expr)


p_oneOf : List CG.Expression -> CG.Expression
p_oneOf opts =
    CG.apply [ CG.fqFun [ "Parser" ] "oneOf", CG.list opts ]


p_token : String -> CG.Expression
p_token tok =
    CG.apply [ CG.fqFun [ "Parser" ] "token", CG.string tok ]


p_run : CG.Expression
p_run =
    CG.fqFun [ "Parser" ] "run"
