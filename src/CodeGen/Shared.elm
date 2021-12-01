module CodeGen.Shared exposing (..)

import Elm.CodeGen as CG
import Elm.Syntax.Declaration exposing (Declaration)
import Placeholder.Internal as Placeholder exposing (Template)
import Generators.Names exposing (Names)


type alias Context =
    { version : String
    , moduleName : CG.ModuleName
    , names : Names
    , languages : List String
    }


templateTypeAnn : Template -> CG.TypeAnnotation
templateTypeAnn =
    Placeholder.getAlphabeticalPlaceholderNames
        >> List.foldl (always <| CG.funAnn CG.stringAnn) CG.stringAnn


templateTypeAnnRecord : Template -> CG.TypeAnnotation
templateTypeAnnRecord template =
    case Placeholder.getAlphabeticalPlaceholderNames template of
        [] ->
            CG.stringAnn

        [ _ ] ->
            CG.funAnn CG.stringAnn CG.stringAnn

        many ->
            many
                |> List.map (\name -> ( name, CG.stringAnn ))
                |> (\fields -> CG.funAnn (CG.extRecordAnn "a" fields) CG.stringAnn)


unwrapDecl : CG.Declaration -> Declaration
unwrapDecl decl =
    case decl of
        CG.DeclWithComment _ f ->
            f ""

        CG.DeclNoComment d ->
            d
