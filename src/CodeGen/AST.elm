module CodeGen.AST exposing (addASTDeclarations)

import CodeGen.Shared as Shared
import Elm.CodeGen as CG
import Types.UniqueName as Unique exposing (UniqueNameContext)


type alias Ctx ctx =
    { ctx
        | file : CG.File
    }


addASTDeclarations : UniqueNameContext (Ctx ctx) -> UniqueNameContext (Ctx ctx)
addASTDeclarations =
    Unique.andThen5 "AST"
        "HtmlNode"
        "TextNode"
        "interpret"
        "interpretList"
        (\lookup ctx astTypeName htmlNodeName textNodeName interpretName interpretListName ->
            let
                astTypeNames =
                    AstTypeNames astTypeName htmlNodeName textNodeName

                functionNames =
                    FunctionNames interpretName interpretListName
            in
            addTypeDecl lookup astTypeNames ctx
                |> addInterpretDecl lookup astTypeNames functionNames
                |> addInterpretListDecl lookup astTypeNames functionNames
        )


type alias AstTypeNames =
    { astTypeName : String, htmlNodeName : String, textNodeName : String }


type alias FunctionNames =
    { interpretName : String, interpretListName : String }


addTypeDecl :
    (String -> String)
    -> AstTypeNames
    -> { ctx | file : CG.File }
    -> { ctx | file : CG.File }
addTypeDecl lookup { astTypeName, htmlNodeName, textNodeName } ctx =
    let
        comment =
            CG.emptyDocComment
                |> CG.markdown """Slim Abstract Syntax Tree for Elm Html Generation.
Needed as an intermediate representation to allow usage with any `msg` type"""

        -- type H_or_T
        --   = H String Int (List (Html.Attribute Never)) (List H_or_T)
        --   | T String
        typeDecl =
            CG.customTypeDecl (Just comment)
                astTypeName
                []
                [ ( htmlNodeName
                  , [ CG.stringAnn
                    , CG.intAnn
                    , CG.listAnn <| CG.fqTyped [ "Html" ] "Attribute" [ CG.typed "Never" [] ]
                    , CG.listAnn <| CG.typed astTypeName []
                    ]
                  )
                , ( textNodeName, [ CG.stringAnn ] )
                ]
    in
    { ctx | file = Shared.addDeclaration typeDecl ctx.file }


addInterpretDecl :
    (String -> String)
    -> AstTypeNames
    -> FunctionNames
    -> { ctx | file : CG.File }
    -> { ctx | file : CG.File }
addInterpretDecl lookup { astTypeName } { interpretName, interpretListName } ctx =
    let
        comment =
            CG.emptyDocComment
                |> CG.markdown """Internal function to convert our AST to Elm's `Html` type."""

        --interpret : Array (List (Html.Attribute msg)) -> H_or_T -> Html.Html msg
        typeAnn =
            CG.funAnn (CG.fqTyped [ "Array" ] "Array" [ CG.listAnn <| CG.fqTyped [ "Html" ] "Attribute" [ CG.typeVar "msg" ] ])
                (CG.funAnn (CG.typed astTypeName []) (CG.fqTyped [ "Html" ] "Html" [ CG.typeVar "msg" ]))

        -- interpret extraAttrs h_or_t =
        -- let
        --     getAttrs i =
        --         Array.get i extraAttrs |> Maybe.withDefault []
        -- in
        -- case h_or_t of
        --     H tag id attrs children ->
        --         Html.node tag (List.map (Html.Attributes.map never) attrs ++ getAttrs id) <| interpretList extraAttrs children
        --     T txt ->
        --         Html.text txt
        interpretDecl =
            CG.funDecl (Just comment)
                (Just typeAnn)
                interpretName
                [ CG.varPattern <| lookup "extraAttrs", CG.varPattern <| lookup "ast" ]
            <|
                CG.letExpr
                    [ CG.letFunction (lookup "getAttrs") [ CG.varPattern <| lookup "id" ] <|
                        CG.applyBinOp (CG.apply [ CG.fqFun [ "Array" ] "get", CG.val <| lookup "id", CG.val <| lookup "extraAttrs" ])
                            CG.piper
                            (CG.apply [ CG.fqFun [ "Maybe" ] "withDefault", CG.list [] ])
                    ]
                <|
                    CG.caseExpr (CG.val <| lookup "ast")
                        [ ( CG.namedPattern (lookup "HtmlNode")
                                [ CG.varPattern <| lookup "tag"
                                , CG.varPattern <| lookup "id"
                                , CG.varPattern <| lookup "attrs"
                                , CG.varPattern <| lookup "children"
                                ]
                          , CG.apply
                                [ CG.fqFun [ "Html" ] "node"
                                , CG.val <| lookup "tag"
                                , CG.parens <|
                                    CG.applyBinOp
                                        (CG.apply
                                            [ CG.fqVal [ "List" ] "map"
                                            , CG.parens <| CG.apply [ CG.fqVal [ "Html", "Attributes" ] "map", CG.fun "never" ]
                                            , CG.val <| lookup "attrs"
                                            ]
                                        )
                                        CG.append
                                        (CG.apply [ CG.fun <| lookup "getAttrs", CG.val <| lookup "id" ])
                                , CG.parens <|
                                    CG.apply
                                        [ CG.fun interpretListName
                                        , CG.val <| lookup "extraAttrs"
                                        , CG.val <| lookup "children"
                                        ]
                                ]
                          )
                        , ( CG.namedPattern (lookup "TextNode") [ CG.varPattern <| lookup "text" ]
                          , CG.apply [ CG.fqFun [ "Html" ] "text", CG.val <| lookup "text" ]
                          )
                        ]
    in
    { ctx | file = Shared.addDeclaration interpretDecl ctx.file }


addInterpretListDecl :
    (String -> String)
    -> AstTypeNames
    -> FunctionNames
    -> { ctx | file : CG.File }
    -> { ctx | file : CG.File }
addInterpretListDecl lookup { astTypeName } { interpretName, interpretListName } ctx =
    let
        --  interpretList : Array (List (Html.Attribute msg)) -> List H_or_T -> List (Html.Html msg)
        typeAnn =
            CG.funAnn (CG.fqTyped [ "Array" ] "Array" [ CG.listAnn <| CG.fqTyped [ "Html" ] "Attribute" [ CG.typeVar "msg" ] ])
                (CG.funAnn (CG.listAnn <| CG.typed astTypeName [])
                    (CG.listAnn <| CG.fqTyped [ "Html" ] "Html" [ CG.typeVar "msg" ])
                )

        --  interpretList extraAttrs =
        --       List.map (interpret extraAttrs)
        interpretListDecl =
            CG.valDecl Nothing (Just typeAnn) interpretListName <|
                CG.applyBinOp (CG.fun interpretName) CG.composer (CG.fqFun [ "List" ] "map")
    in
    { ctx | file = Shared.addDeclaration interpretListDecl ctx.file }
