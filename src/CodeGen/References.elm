module CodeGen.References exposing (..)

import CodeGen.Shared exposing (unwrapDecl)
import Elm.CodeGen as CG
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Node as Node


declName : CG.Declaration -> Maybe String
declName decl =
    case unwrapDecl decl of
        FunctionDeclaration { declaration } ->
            (Node.value declaration).name |> Node.value |> Just

        AliasDeclaration { name } ->
            Node.value name |> Just

        CustomTypeDeclaration { name } ->
            Node.value name |> Just

        PortDeclaration { name } ->
            Node.value name |> Just

        InfixDeclaration _ ->
            -- Cannot be declared outside of internal modules anyways
            Nothing

        Destructuring _ _ ->
            -- Cannot be top level
            Nothing
