module CodeGen.DynamicParser exposing (..)

import Array exposing (Array)
import CodeGen.Shared exposing (appendAll, endoAnn)
import Elm.CodeGen as CG
import Elm.Syntax.Expression as Expr
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (emptyRange)
import Generators.Names exposing (Names)
import Intl
import Parser exposing (..)


{-| Generates the following code

    replacePlaceholders : I18n -> List String -> String -> String
    replacePlaceholders ( _, intl, language ) argList =
        let
            args =
                Array.fromList argList

            getArg n =
                Array.get n args |> Maybe.withDefault ""

            wrappedLang =
                "\"" ++ language ++ "\""

            argParser : Parser String
            argParser =
                oneOf
                    [ succeed wrappedLang |. token "}"
                    , succeed (\str -> wrappedLang ++ ",{" ++ str ++ "}")
                        |= (chompUntil "}" |> getChompedString)
                        |. token "}"
                    ]

            numberFormatUnsafe : Int -> String -> String
            numberFormatUnsafe n parsedArgString =
                Maybe.withDefault "" <|
                    Intl.unsafeAccess intl <|
                        "[\"NumberFormat\",["
                            ++ parsedArgString
                            ++ "],\"format\",["
                            ++ getArg n
                            ++ "]]"

            dateFormatUnsafe : Int -> String -> String
            dateFormatUnsafe n parsedArgString =
                Maybe.withDefault "" <|
                    Intl.unsafeAccess intl <|
                        "[\"DateTimeFormat\",["
                            ++ parsedArgString
                            ++ "],\"format\",["
                            ++ getArg n
                            ++ "]]"

            parser =
                Parser.loop "" <|
                    \state ->
                        oneOf
                            [ succeed ((++) state >> Loop)
                                |. token "{"
                                |= oneOf
                                    [ succeed getArg |= int |. token "}"
                                    , succeed numberFormatUnsafe |. token "N" |= int |= argParser
                                    , succeed dateFormatUnsafe |. token "D" |= int |= argParser
                                    ]
                            , chompUntilEndOr "{"
                                |> getChompedString
                                |> map ((++) state)
                                |> andThen (\str -> oneOf [ succeed (Done str) |. end, succeed <| Loop str ])
                            ]
        in
        Parser.run parser >> Result.toMaybe >> Maybe.withDefault ""

-}
genParser : Names -> CG.Declaration
genParser names =
    CG.funDecl (CG.emptyDocComment |> CG.markdown "Replaces all placeholders with the given arguments using the Intl API on the marked spots" |> Just)
        (Just <| CG.funAnn (CG.typed names.i18nTypeName []) <| CG.funAnn (CG.listAnn CG.stringAnn) (endoAnn CG.stringAnn))
        "replacePlaceholders"
        [ CG.tuplePattern [ CG.allPattern, CG.varPattern "intl_", CG.varPattern "lang_" ], CG.varPattern "argList_" ]
        (CG.letExpr
            [ CG.letVal "args_" (CG.apply [ CG.fqFun [ "Array" ] "fromList", CG.val "argList_" ])
            , CG.letFunction "getArg_"
                [ CG.varPattern "n_" ]
                (CG.applyBinOp (CG.apply [ CG.fqFun [ "Array" ] "get", CG.val "n_", CG.val "args_" ])
                    CG.piper
                    defaultMaybeToEmptyString
                )
            , CG.letFunction "numberFormatUnsafe_"
                [ CG.varPattern "n_", CG.varPattern "parsedArgString_" ]
                (CG.applyBinOp defaultMaybeToEmptyString CG.pipel <|
                    CG.applyBinOp
                        (CG.apply [ CG.fqFun [ "Intl" ] "unsafeAccess", CG.val "intl_" ])
                        CG.pipel
                    <|
                        appendAll (CG.string "[\"NumberFormat\",[")
                            [ CG.val "parsedArgString_"
                            , CG.string "],\"format\",["
                            , CG.apply [ CG.val "getArg_", CG.val "n_" ]
                            , CG.string "]]"
                            ]
                )
            , CG.letFunction "dateFormatUnsafe_"
                [ CG.varPattern "n_", CG.varPattern "parsedArgString_" ]
                (CG.applyBinOp defaultMaybeToEmptyString CG.pipel <|
                    CG.applyBinOp
                        (CG.apply [ CG.fqFun [ "Intl" ] "unsafeAccess", CG.val "intl_" ])
                        CG.pipel
                    <|
                        CG.binOpChain
                            (CG.string "[\"DateTimeFormat\",[")
                            CG.append
                            [ CG.val "parsedArgString_"
                            , CG.string "],\"format\",["
                            , CG.apply [ CG.val "getArg_", CG.val "n_" ]
                            , CG.string "]]"
                            ]
                )
            , CG.letVal "parser_" <|
                CG.applyBinOp (CG.apply [ p_loop, CG.string "" ]) CG.pipel <|
                    CG.lambda [ CG.varPattern "state_" ] <|
                        CG.apply
                            [ p_oneOf
                            , CG.list
                                [ CG.apply
                                    [ p_succeed
                                    , CG.parens <| CG.applyBinOp (CG.apply [ CG.fun "(++)", CG.val "state_" ]) CG.composer p_Loop
                                    ]
                                    |> p_drop_infix
                                        (CG.apply [ p_token, CG.string "{" ]
                                            |> p_keep_infix
                                                (CG.apply
                                                    [ p_oneOf
                                                    , CG.list
                                                        []
                                                    ]
                                                )
                                        )
                                ]
                            ]
            ]
         <|
            CG.chain
                (CG.apply [ p_run, CG.val "parser_" ])
                [ CG.fqFun [ "Result" ] "toMaybe", defaultMaybeToEmptyString ]
        )


defaultMaybeToEmptyString : CG.Expression
defaultMaybeToEmptyString =
    CG.apply [ CG.fqFun [ "Maybe" ] "withDefault", CG.string "" ]


p_keep_infix : CG.Expression -> CG.Expression -> CG.Expression
p_keep_infix =
    customOp "|="


p_drop_infix : CG.Expression -> CG.Expression -> CG.Expression
p_drop_infix =
    customOp "|."



-- reversed to better work with |>
-- left |> customOp "|." right


customOp : String -> CG.Expression -> CG.Expression -> CG.Expression
customOp symbol right left =
    Expr.OperatorApplication symbol Infix.Left (Node emptyRange left) (Node emptyRange right)


p_loop : CG.Expression
p_loop =
    CG.fqFun [ "Parser" ] "loop"


p_Loop : CG.Expression
p_Loop =
    CG.fqFun [ "Parser" ] "Loop"


p_map : CG.Expression
p_map =
    CG.fqFun [ "Parser" ] "map"


p_succeed : CG.Expression
p_succeed =
    CG.fqFun [ "Parser" ] "succeed"


p_oneOf : CG.Expression
p_oneOf =
    CG.fqFun [ "Parser" ] "oneOf"


p_andThen : CG.Expression
p_andThen =
    CG.fqFun [ "Parser" ] "andThen"


p_token : CG.Expression
p_token =
    CG.fqFun [ "Parser" ] "token"


p_run : CG.Expression
p_run =
    CG.fqFun [ "Parser" ] "run"
