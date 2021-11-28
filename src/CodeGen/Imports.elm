module CodeGen.Imports exposing (extractImports)

import Elm.CodeGen as C
import Elm.Syntax.Declaration as Decl
import Elm.Syntax.Expression as Expr
import Elm.Syntax.Node as Node
import Elm.Syntax.Pattern as Pat
import Elm.Syntax.Signature as Sig
import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAnnotation as Ann
import Set exposing (Set)



extractImports : List C.Declaration -> Set C.ModuleName
extractImports =
    List.map extractImportsDecl >> concatSets >> Set.filter ((/=) [])


extractImportsDecl : C.Declaration -> Set C.ModuleName
extractImportsDecl decl =
    let
        extractImportsDeclInternal : Decl.Declaration -> Set C.ModuleName
        extractImportsDeclInternal d =
            case d of
                Decl.AliasDeclaration { typeAnnotation } ->
                    extractImportsTypeAnn <| Node.value typeAnnotation

                Decl.FunctionDeclaration funcDecl ->
                    extractImportsFun funcDecl

                Decl.CustomTypeDeclaration type_ ->
                    extractImportsType type_

                Decl.PortDeclaration sig ->
                    extractImportsSig sig

                Decl.InfixDeclaration _ ->
                    Set.empty

                Decl.Destructuring pat expr ->
                    Set.union
                        (extractImportsPat <| Node.value pat)
                        (extractImportsExpr <| Node.value expr)
    in
    case decl of
        C.DeclWithComment _ f ->
            extractImportsDeclInternal (f "")

        C.DeclNoComment d ->
            extractImportsDeclInternal d


extractImportsTypeAnn : C.TypeAnnotation -> Set C.ModuleName
extractImportsTypeAnn typeAnn =
    let
        extractImportsRecord =
            concatSets
                << List.map
                    (Node.value
                        >> Tuple.second
                        >> Node.value
                        >> extractImportsTypeAnn
                    )
    in
    case typeAnn of
        Ann.Typed (Node.Node _ ( moduleName, _ )) typeAnns ->
            Set.insert moduleName <|
                concatSets (List.map (Node.value >> extractImportsTypeAnn) typeAnns)

        Ann.Tupled typeAnns ->
            concatSets <| List.map (Node.value >> extractImportsTypeAnn) typeAnns

        Ann.Record recordFields ->
            extractImportsRecord recordFields

        Ann.GenericRecord _ (Node.Node _ recordFields) ->
            extractImportsRecord recordFields

        Ann.FunctionTypeAnnotation f1 f2 ->
            Set.union
                (extractImportsTypeAnn <| Node.value f1)
                (extractImportsTypeAnn <| Node.value f2)

        _ ->
            Set.empty


extractImportsFun : Expr.Function -> Set C.ModuleName
extractImportsFun { signature, declaration } =
    Maybe.map (Node.value >> extractImportsSig) signature
        |> Maybe.withDefault Set.empty
        |> Set.union (Node.value declaration |> .expression |> Node.value |> extractImportsExpr)


extractImportsSig : Sig.Signature -> Set C.ModuleName
extractImportsSig { typeAnnotation } =
    extractImportsTypeAnn <| Node.value typeAnnotation


extractImportsType : Type.Type -> Set C.ModuleName
extractImportsType { constructors } =
    concatSets <|
        List.concatMap
            (Node.value
                >> .arguments
                >> List.map (Node.value >> extractImportsTypeAnn)
            )
            constructors


extractImportsExpr : C.Expression -> Set C.ModuleName
extractImportsExpr expr =
    let
        extractImportsLetDecl : Expr.LetDeclaration -> Set C.ModuleName
        extractImportsLetDecl letDecl =
            case letDecl of
                Expr.LetFunction fun ->
                    extractImportsFun fun

                Expr.LetDestructuring pat e ->
                    Set.union
                        (extractImportsPat <| Node.value pat)
                        (extractImportsExpr <| Node.value e)

        extractImportsRecordSetter : List (Node.Node Expr.RecordSetter) -> Set C.ModuleName
        extractImportsRecordSetter recordSetter =
            concatSets <|
                List.map
                    (Node.value
                        >> Tuple.second
                        >> Node.value
                        >> extractImportsExpr
                    )
                    recordSetter
    in
    case expr of
        Expr.Application exprs ->
            concatSets <| List.map (Node.value >> extractImportsExpr) exprs

        Expr.OperatorApplication _ _ e1 e2 ->
            Set.union
                (extractImportsExpr <| Node.value e1)
                (extractImportsExpr <| Node.value e2)

        Expr.FunctionOrValue moduleName _ ->
            Set.singleton moduleName

        Expr.IfBlock e1 e2 e3 ->
            concatSets <| List.map (Node.value >> extractImportsExpr) [ e1, e2, e3 ]

        Expr.Negation e ->
            extractImportsExpr <| Node.value e

        Expr.TupledExpression exprs ->
            concatSets <| List.map (Node.value >> extractImportsExpr) exprs

        Expr.ParenthesizedExpression e ->
            extractImportsExpr <| Node.value e

        Expr.LetExpression letBlock ->
            Set.union (extractImportsExpr <| Node.value letBlock.expression)
                (concatSets <| List.map (Node.value >> extractImportsLetDecl) letBlock.declarations)

        Expr.CaseExpression caseBlock ->
            Set.union (extractImportsExpr <| Node.value caseBlock.expression)
                (concatSets <|
                    List.map
                        (\( pat, e ) ->
                            Set.union
                                (extractImportsPat <| Node.value pat)
                                (extractImportsExpr <| Node.value e)
                        )
                        caseBlock.cases
                )

        Expr.LambdaExpression lambda ->
            Set.union (extractImportsExpr <| Node.value lambda.expression)
                (concatSets <| List.map (Node.value >> extractImportsPat) lambda.args)

        Expr.RecordExpr recordSetter ->
            extractImportsRecordSetter recordSetter

        Expr.ListExpr list ->
            concatSets <| List.map (Node.value >> extractImportsExpr) list

        Expr.RecordAccess e _ ->
            extractImportsExpr <| Node.value e

        Expr.RecordUpdateExpression _ recordSetter ->
            extractImportsRecordSetter recordSetter

        _ ->
            Set.empty


extractImportsPat : C.Pattern -> Set C.ModuleName
extractImportsPat pat =
    case pat of
        Pat.TuplePattern pats ->
            concatSets <| List.map (Node.value >> extractImportsPat) pats

        Pat.UnConsPattern p1 p2 ->
            Set.union
                (extractImportsPat <| Node.value p1)
                (extractImportsPat <| Node.value p2)

        Pat.ListPattern pats ->
            concatSets <| List.map (Node.value >> extractImportsPat) pats

        Pat.NamedPattern { moduleName } pats ->
            Set.insert moduleName <|
                concatSets <|
                    List.map (Node.value >> extractImportsPat) pats

        Pat.AsPattern p _ ->
            extractImportsPat <| Node.value p

        Pat.ParenthesizedPattern p ->
            extractImportsPat <| Node.value p

        _ ->
            Set.empty


concatSets : List (Set comparable) -> Set comparable
concatSets =
    List.foldl Set.union Set.empty
