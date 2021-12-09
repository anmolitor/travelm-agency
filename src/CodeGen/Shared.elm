module CodeGen.Shared exposing (Context, unwrapDecl)

import Elm.CodeGen as CG
import Elm.Syntax.Declaration exposing (Declaration)
import Generators.Names exposing (Names)


type alias Context =
    { version : String
    , moduleName : CG.ModuleName
    , names : Names
    , languages : List String
    }


unwrapDecl : CG.Declaration -> Declaration
unwrapDecl decl =
    case decl of
        CG.DeclWithComment _ f ->
            f ""

        CG.DeclNoComment d ->
            d
