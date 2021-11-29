module CodeGen.Shared exposing (..)

import Elm.CodeGen as CG
import Placeholder.Internal as Placeholder exposing (Template)


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
