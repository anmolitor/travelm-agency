module CodeGen.Imports exposing (dictToImports, extractImports)

import CodeGen.Shared exposing (unwrapDecl)
import Dict exposing (Dict)
import Elm.CodeGen as CG
import Elm.Syntax.Declaration as Decl
import Elm.Syntax.Expression as Expr
import Elm.Syntax.Node as Node
import Elm.Syntax.Pattern as Pat
import Elm.Syntax.Signature as Sig
import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAnnotation as Ann
import Set exposing (Set)


dictToImports : Dict CG.ModuleName (Set String) -> List CG.Import
dictToImports =
    Dict.toList
        >> List.map
            (\( moduleName, exposes ) ->
                CG.importStmt moduleName
                    Nothing
                    (if Set.isEmpty exposes then
                        Nothing

                     else
                        Just <| CG.exposeExplicit <| List.map CG.funExpose <| Set.toList exposes
                    )
            )


extractImports : List CG.Declaration -> Dict CG.ModuleName (Set String)
extractImports =
    List.map extractImportsDecl >> concatDicts >> Dict.filter (\key _ -> key /= [])


extractImportsDecl : CG.Declaration -> Dict CG.ModuleName (Set String)
extractImportsDecl =
    let
        extractImportsDeclInternal : Decl.Declaration -> Dict CG.ModuleName (Set String)
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
                    Dict.empty

                Decl.Destructuring pat expr ->
                    Dict.union
                        (extractImportsPat <| Node.value pat)
                        (extractImportsExpr <| Node.value expr)
    in
    unwrapDecl >> extractImportsDeclInternal


extractImportsTypeAnn : CG.TypeAnnotation -> Dict CG.ModuleName (Set String)
extractImportsTypeAnn typeAnn =
    let
        extractImportsRecord =
            concatDicts
                << List.map
                    (Node.value
                        >> Tuple.second
                        >> Node.value
                        >> extractImportsTypeAnn
                    )
    in
    case typeAnn of
        Ann.Typed (Node.Node _ ( moduleName, _ )) typeAnns ->
            insertNoExpose moduleName <|
                concatDicts (List.map (Node.value >> extractImportsTypeAnn) typeAnns)

        Ann.Tupled typeAnns ->
            concatDicts <| List.map (Node.value >> extractImportsTypeAnn) typeAnns

        Ann.Record recordFields ->
            extractImportsRecord recordFields

        Ann.GenericRecord _ (Node.Node _ recordFields) ->
            extractImportsRecord recordFields

        Ann.FunctionTypeAnnotation f1 f2 ->
            merge
                (extractImportsTypeAnn <| Node.value f1)
                (extractImportsTypeAnn <| Node.value f2)

        _ ->
            Dict.empty


extractImportsFun : Expr.Function -> Dict CG.ModuleName (Set String)
extractImportsFun { signature, declaration } =
    Maybe.map (Node.value >> extractImportsSig) signature
        |> Maybe.withDefault Dict.empty
        |> merge (Node.value declaration |> .expression |> Node.value |> extractImportsExpr)


extractImportsSig : Sig.Signature -> Dict CG.ModuleName (Set String)
extractImportsSig { typeAnnotation } =
    extractImportsTypeAnn <| Node.value typeAnnotation


extractImportsType : Type.Type -> Dict CG.ModuleName (Set String)
extractImportsType { constructors } =
    concatDicts <|
        List.concatMap
            (Node.value
                >> .arguments
                >> List.map (Node.value >> extractImportsTypeAnn)
            )
            constructors


extractImportsExpr : CG.Expression -> Dict CG.ModuleName (Set String)
extractImportsExpr expr =
    let
        extractImportsLetDecl : Expr.LetDeclaration -> Dict CG.ModuleName (Set String)
        extractImportsLetDecl letDecl =
            case letDecl of
                Expr.LetFunction fun ->
                    extractImportsFun fun

                Expr.LetDestructuring pat e ->
                    merge
                        (extractImportsPat <| Node.value pat)
                        (extractImportsExpr <| Node.value e)

        extractImportsRecordSetter : List (Node.Node Expr.RecordSetter) -> Dict CG.ModuleName (Set String)
        extractImportsRecordSetter recordSetter =
            concatDicts <|
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
            concatDicts <| List.map (Node.value >> extractImportsExpr) exprs

        Expr.OperatorApplication symbol _ e1 e2 ->
            merge
                (extractImportsExpr <| Node.value e1)
                (extractImportsExpr <| Node.value e2)
                |> handleOperatorApplication symbol

        Expr.FunctionOrValue moduleName _ ->
            Dict.singleton moduleName Set.empty

        Expr.IfBlock e1 e2 e3 ->
            concatDicts <| List.map (Node.value >> extractImportsExpr) [ e1, e2, e3 ]

        Expr.Negation e ->
            extractImportsExpr <| Node.value e

        Expr.TupledExpression exprs ->
            concatDicts <| List.map (Node.value >> extractImportsExpr) exprs

        Expr.ParenthesizedExpression e ->
            extractImportsExpr <| Node.value e

        Expr.LetExpression letBlock ->
            merge (extractImportsExpr <| Node.value letBlock.expression)
                (concatDicts <| List.map (Node.value >> extractImportsLetDecl) letBlock.declarations)

        Expr.CaseExpression caseBlock ->
            merge (extractImportsExpr <| Node.value caseBlock.expression)
                (concatDicts <|
                    List.map
                        (\( pat, e ) ->
                            merge
                                (extractImportsPat <| Node.value pat)
                                (extractImportsExpr <| Node.value e)
                        )
                        caseBlock.cases
                )

        Expr.LambdaExpression lambda ->
            merge (extractImportsExpr <| Node.value lambda.expression)
                (concatDicts <| List.map (Node.value >> extractImportsPat) lambda.args)

        Expr.RecordExpr recordSetter ->
            extractImportsRecordSetter recordSetter

        Expr.ListExpr list ->
            concatDicts <| List.map (Node.value >> extractImportsExpr) list

        Expr.RecordAccess e _ ->
            extractImportsExpr <| Node.value e

        Expr.RecordUpdateExpression _ recordSetter ->
            extractImportsRecordSetter recordSetter

        _ ->
            Dict.empty


extractImportsPat : CG.Pattern -> Dict CG.ModuleName (Set String)
extractImportsPat pat =
    case pat of
        Pat.TuplePattern pats ->
            concatDicts <| List.map (Node.value >> extractImportsPat) pats

        Pat.UnConsPattern p1 p2 ->
            merge
                (extractImportsPat <| Node.value p1)
                (extractImportsPat <| Node.value p2)

        Pat.ListPattern pats ->
            concatDicts <| List.map (Node.value >> extractImportsPat) pats

        Pat.NamedPattern { moduleName } pats ->
            insertNoExpose moduleName <|
                concatDicts <|
                    List.map (Node.value >> extractImportsPat) pats

        Pat.AsPattern p _ ->
            extractImportsPat <| Node.value p

        Pat.ParenthesizedPattern p ->
            extractImportsPat <| Node.value p

        _ ->
            Dict.empty


handleOperatorApplication : String -> Dict CG.ModuleName (Set String) -> Dict CG.ModuleName (Set String)
handleOperatorApplication symbol =
    case symbol of
        "|=" ->
            insert [ "Parser" ] (Set.singleton "(|=)")

        "|." ->
            insert [ "Parser" ] (Set.singleton "(|.)")

        _ ->
            identity


concatDicts : List (Dict CG.ModuleName (Set String)) -> Dict CG.ModuleName (Set String)
concatDicts =
    List.foldl merge Dict.empty


insert : CG.ModuleName -> Set String -> Dict CG.ModuleName (Set String) -> Dict CG.ModuleName (Set String)
insert k v =
    Dict.update k (Maybe.map (Set.union v) >> Maybe.withDefault v >> Just)


insertNoExpose : CG.ModuleName -> Dict CG.ModuleName (Set String) -> Dict CG.ModuleName (Set String)
insertNoExpose k =
    insert k Set.empty


merge : Dict CG.ModuleName (Set String) -> Dict CG.ModuleName (Set String) -> Dict CG.ModuleName (Set String)
merge l r =
    Dict.merge insert (\k a b -> insert k a >> insert k b) insert l r Dict.empty
