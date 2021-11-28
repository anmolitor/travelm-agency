module CodeGen.BasicM exposing (result)

import Elm.CodeGen as CG


result : CG.TypeAnnotation -> CG.TypeAnnotation -> CG.TypeAnnotation
result errType successType =
    CG.typed "Result" [ errType, successType ]
