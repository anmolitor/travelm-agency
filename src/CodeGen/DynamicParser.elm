module CodeGen.DynamicParser exposing (addReplacePlaceholderDeclaration)

import CodeGen.DecodeM as DecodeM
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


{-| Generates part of the following code snippet

    parser : I18n -> List String -> Parser.Parser String
    parser ((I18n _ intl lang) as i18n) argList =
        let
            args =
                Array.fromList argList

            getArg n =
                Array.get n args |> Maybe.withDefault ""

            wrappedLang =
                "\"" ++ languageToString lang ++ "\""

            argParser =
                Parser.oneOf
                    [ Parser.succeed wrappedLang |. Parser.token "}"
                    , Parser.succeed (\str -> wrappedLang ++ ",{" ++ str ++ "}")
                        |= (Parser.chompUntil "}" |> Parser.getChompedString)
                        |. Parser.token "}"
                    ]

            matchParser =
                Parser.succeed Tuple.pair
                    |= (Parser.chompUntil "|" |> Parser.getChompedString)
                    |. Parser.token "|"
                    |= (Parser.chompUntil "}"
                            |> Parser.getChompedString
                            |> Parser.andThen
                                (\str ->
                                    case
                                        Json.Decode.decodeString (Json.Decode.dict Json.Decode.string) ("{" ++ str ++ "}")
                                    of
                                        Ok ok ->
                                            Parser.succeed ok

                                        Err err ->
                                            Parser.problem (Json.Decode.errorToString err)
                                )
                       )
                    |. Parser.token "}"

            numberFormatUnsafe n parsedArgString =
                Maybe.withDefault "" <|
                    Intl.unsafeAccess intl <|
                        "[\"NumberFormat\",["
                            ++ parsedArgString
                            ++ "],\"format\",["
                            ++ getArg n
                            ++ "]]"

            dateFormatUnsafe n parsedArgString =
                Maybe.withDefault "" <|
                    Intl.unsafeAccess intl <|
                        "[\"DateTimeFormat\",["
                            ++ parsedArgString
                            ++ "],\"format\",["
                            ++ getArg n
                            ++ "]]"

            matchStrings n ( default, cases ) =
                Dict.get (getArg n) cases
                    |> Maybe.withDefault default
                    |> Parser.run (parser i18n argList)
                    |> Result.toMaybe
                    |> Maybe.withDefault fallbackValue

            matchNumbers n ( default, cases ) =
                getArg n
                    |> String.toFloat
                    |> Maybe.andThen
                        (\i ->
                            Intl.determinePluralRuleFloat
                                intl
                                { language = languageToString lang, number = i, type_ = Intl.Cardinal }
                                |> Intl.pluralRuleToString
                                |> (\pluralRule -> Dict.get pluralRule cases)
                        )
                    |> Maybe.withDefault default
                    |> Parser.run (parser i18n argList)
                    |> Result.toMaybe
                    |> Maybe.withDefault fallbackValue
        in
        Parser.loop "" <|
            \state ->
                Parser.oneOf
                    [ Parser.succeed ((++) state >> Parser.Loop)
                        |. Parser.token "{"
                        |= Parser.oneOf
                            [ Parser.succeed getArg |= Parser.int |. Parser.token "}"
                            , Parser.succeed numberFormatUnsafe |. Parser.token "N" |= Parser.int |= argParser
                            , Parser.succeed dateFormatUnsafe |. Parser.token "D" |= Parser.int |= argParser
                            , Parser.succeed matchStrings
                                |. Parser.token "S"
                                |= Parser.int
                                |. Parser.token "|"
                                |= matchParser
                            , Parser.succeed matchNumbersCodeGen
                                |. Parser.token "P"
                                |= Parser.int
                                |. Parser.token "|"
                                |= matchParser
                            ]
                    , Parser.chompUntilEndOr "{"
                        |> Parser.getChompedString
                        |> Parser.map ((++) state)
                        |> Parser.andThen
                            (\str ->
                                Parser.oneOf
                                    [ Parser.succeed (Parser.Done str) |. Parser.end, Parser.succeed (Parser.Loop str) ]
                            )
                    ]

    replacePlaceholders : I18n -> List String -> String -> String
    replacePlaceholders i18n argList =
        Parser.run (parser i18n argList) >> Result.toMaybe >> Maybe.withDefault fallbackValue

-}
addReplacePlaceholderDeclaration :
    Unique.UniqueNameContext { ctx | names : Names, file : CG.File, state : State.NonEmptyState any, replacePlaceholdersName : String, fallbackValueName : String }
    -> Unique.UniqueNameContext { ctx | names : Names, file : CG.File, state : State.NonEmptyState any, replacePlaceholdersName : String, fallbackValueName : String }
addReplacePlaceholderDeclaration =
    Unique.andThen "parser" <|
        \lookup ctx parserName ->
            let
                features =
                    ctx.state |> State.inferFeatures

                filterByFeature feature =
                    if Features.isActive feature features then
                        Just

                    else
                        always Nothing

                filterByFeatures genIfActive =
                    if Features.oneIsActive genIfActive features then
                        Just

                    else
                        always Nothing

                filterIntl =
                    if Features.needsIntl features then
                        Just

                    else
                        always Nothing

                parserType : CG.TypeAnnotation
                parserType =
                    (if Features.needsIntl features then
                        CG.funAnn (CG.typed ctx.names.i18nTypeName [])

                     else
                        identity
                    )
                    <|
                        CG.funAnn (CG.listAnn CG.stringAnn) (CG.fqTyped [ "Parser" ] "Parser" [ CG.stringAnn ])

                parserArgs =
                    List.filterMap identity
                        [ CG.namedPattern ctx.names.i18nTypeName [ CG.allPattern, CG.varPattern <| lookup "intl", CG.varPattern <| lookup "lang" ]
                            |> (\pat -> CG.asPattern pat (lookup "i18n"))
                            |> filterIntl
                        , Just <| CG.varPattern <| lookup "argList"
                        ]

                parserDecl : CG.Declaration
                parserDecl =
                    CG.funDecl Nothing
                        (Just parserType)
                        parserName
                        parserArgs
                        (CG.letExpr
                            (List.filterMap identity
                                [ Just <| CG.letVal (lookup "args") (CG.apply [ CG.fqFun [ "Array" ] "fromList", CG.val <| lookup "argList" ])
                                , Just <|
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
                                    CG.letVal (lookup "argParser")
                                        (p_oneOf
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
                                        )
                                , filterByFeatures [ Features.CaseInterpolation, Features.IntlNumber ] <|
                                    CG.letVal (lookup "matchParser")
                                        (p_succeed (CG.fqFun [ "Tuple" ] "pair")
                                            |> p_keep_infix (CG.parens <| CG.applyBinOp (p_chompUntil "|") CG.piper p_getChompedString)
                                            |> p_drop_infix (p_token "|")
                                            |> p_keep_infix
                                                (CG.parens <|
                                                    CG.pipe (p_chompUntil "}")
                                                        [ p_getChompedString
                                                        , p_andThen <|
                                                            CG.lambda [ CG.varPattern <| lookup "str" ] <|
                                                                CG.caseExpr
                                                                    (CG.apply
                                                                        [ DecodeM.decodeString
                                                                        , CG.parens <| CG.apply [ DecodeM.dict, DecodeM.string ]
                                                                        , CG.parens <| appendAll (CG.string "{") [ CG.val <| lookup "str", CG.string "}" ]
                                                                        ]
                                                                    )
                                                                    [ ( CG.namedPattern "Ok" [ CG.varPattern <| lookup "ok" ], p_succeed <| CG.val <| lookup "ok" )
                                                                    , ( CG.namedPattern "Err" [ CG.varPattern <| lookup "err" ], p_problem <| CG.parens <| CG.apply [ DecodeM.errorToString, CG.val <| lookup "err" ] )
                                                                    ]
                                                        ]
                                                )
                                            |> p_drop_infix (p_token "}")
                                        )
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
                                    CG.letFunction (lookup "matchStrings") [ CG.varPattern <| lookup "n", CG.tuplePattern [ CG.varPattern <| lookup "default", CG.varPattern <| lookup "cases" ] ] <|
                                        CG.pipe (CG.apply [ CG.fqFun [ "Dict" ] "get", CG.parens <| CG.apply [ CG.fun <| lookup "getArg", CG.val <| lookup "n" ], CG.val <| lookup "cases" ])
                                            [ CG.apply [ CG.fqFun [ "Maybe" ] "withDefault", CG.val <| lookup "default" ]
                                            , CG.apply
                                                [ p_run
                                                , CG.parens <|
                                                    CG.apply <|
                                                        List.filterMap identity
                                                            [ Just <| CG.val parserName
                                                            , filterIntl <| CG.val <| lookup "i18n"
                                                            , Just <| CG.val <| lookup "argList"
                                                            ]
                                                ]
                                            , CG.fqFun [ "Result" ] "toMaybe"
                                            , CG.apply [ CG.fqFun [ "Maybe" ] "withDefault", CG.val ctx.fallbackValueName ]
                                            ]
                                , filterIntl <|
                                    CG.letFunction (lookup "matchNumbers")
                                        [ CG.varPattern <| lookup "n"
                                        , CG.tuplePattern [ CG.varPattern <| lookup "default", CG.varPattern <| lookup "cases" ]
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
                                            , CG.apply [ p_run, CG.parens <| CG.apply [ CG.val parserName, CG.val <| lookup "i18n", CG.val <| lookup "argList" ] ]
                                            , CG.fqFun [ "Result" ] "toMaybe"
                                            , CG.apply [ CG.fqFun [ "Maybe" ] "withDefault", CG.val ctx.fallbackValueName ]
                                            ]
                                ]
                            )
                         <|
                            CG.applyBinOp (CG.apply [ p_loop, CG.string "" ]) CG.pipel <|
                                CG.lambda [ CG.varPattern <| lookup "state" ] <|
                                    p_oneOf
                                        [ p_succeed
                                            (CG.parens <| CG.applyBinOp (CG.apply [ CG.fun "(++)", CG.val <| lookup "state" ]) CG.composer p_Loop)
                                            |> p_drop_infix
                                                (p_token "{"
                                                    |> p_keep_infix
                                                        (p_oneOf <|
                                                            List.filterMap identity
                                                                [ Just (p_succeed (CG.val <| lookup "getArg") |> p_keep_infix (p_int |> p_drop_infix (p_token "}")))
                                                                , filterIntl
                                                                    (p_succeed (CG.val <| lookup "numberFormatUnsafe")
                                                                        |> p_drop_infix
                                                                            (p_token "N"
                                                                                |> p_keep_infix (p_int |> p_keep_infix (CG.val <| lookup "argParser"))
                                                                            )
                                                                    )
                                                                , filterIntl
                                                                    (p_succeed (CG.val <| lookup "dateFormatUnsafe")
                                                                        |> p_drop_infix
                                                                            (p_token "D"
                                                                                |> p_keep_infix (p_int |> p_keep_infix (CG.val <| lookup "argParser"))
                                                                            )
                                                                    )
                                                                , filterByFeature Features.CaseInterpolation
                                                                    (p_succeed (CG.val <| lookup "matchStrings")
                                                                        |> p_drop_infix
                                                                            (p_token "S"
                                                                                |> p_keep_infix p_int
                                                                                |> p_drop_infix (p_token "|")
                                                                                |> p_keep_infix (CG.val <| lookup "matchParser")
                                                                            )
                                                                    )
                                                                , filterIntl
                                                                    (p_succeed (CG.val <| lookup "matchNumbers")
                                                                        |> p_drop_infix
                                                                            (p_token "P"
                                                                                |> p_keep_infix p_int
                                                                                |> p_drop_infix (p_token "|")
                                                                                |> p_keep_infix (CG.val <| lookup "matchParser")
                                                                            )
                                                                    )
                                                                ]
                                                        )
                                                )
                                        , CG.pipe (p_chompUntilEndOr "{")
                                            [ p_getChompedString
                                            , p_map <| CG.parens <| CG.apply [ CG.fun "(++)", CG.val <| lookup "state" ]
                                            , p_andThen <|
                                                CG.parens <|
                                                    CG.lambda [ CG.varPattern <| lookup "str" ] <|
                                                        p_oneOf
                                                            [ (p_succeed <| CG.parens <| CG.apply [ p_Done, CG.val <| lookup "str" ]) |> p_drop_infix p_end
                                                            , p_succeed <| CG.parens <| CG.apply [ p_Loop, CG.val <| lookup "str" ]
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
                                            [ Just <| CG.val parserName
                                            , filterIntl <| CG.val <| lookup "i18n"
                                            , Just <| CG.val <| lookup "argList"
                                            ]
                                ]
                            )
                            [ CG.fqFun [ "Result" ] "toMaybe"
                            , CG.apply [ CG.fqFun [ "Maybe" ] "withDefault", CG.val ctx.fallbackValueName ]
                            ]
            in
            { ctx
                | file =
                    if Features.isEmpty features then
                        ctx.file

                    else
                        ctx.file
                            |> Shared.addDeclaration parserDecl
                            |> Shared.addDeclaration replacePlaceholdersDecl
            }


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


p_chompUntilEndOr : String -> CG.Expression
p_chompUntilEndOr symbol =
    CG.apply [ CG.fqFun [ "Parser" ] "chompUntilEndOr", CG.string symbol ]


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


p_problem : CG.Expression -> CG.Expression
p_problem expr =
    CG.apply [ CG.fqFun [ "Parser" ] "problem", expr ]


p_oneOf : List CG.Expression -> CG.Expression
p_oneOf opts =
    CG.apply [ CG.fqFun [ "Parser" ] "oneOf", CG.list opts ]


p_andThen : CG.Expression -> CG.Expression
p_andThen expr =
    CG.apply [ CG.fqFun [ "Parser" ] "andThen", expr ]


p_token : String -> CG.Expression
p_token tok =
    CG.apply [ CG.fqFun [ "Parser" ] "token", CG.string tok ]


p_int : CG.Expression
p_int =
    CG.fqFun [ "Parser" ] "int"


p_run : CG.Expression
p_run =
    CG.fqFun [ "Parser" ] "run"
