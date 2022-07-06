module Types.InterpolationKind exposing (..)

import Elm.CodeGen as CG


type InterpolationKind
    = Simple
    | Typed { ann : CG.TypeAnnotation, toString : CG.Expression -> CG.Expression }


toTypeAnn : InterpolationKind -> CG.TypeAnnotation
toTypeAnn kind =
    case kind of
        Simple ->
            CG.stringAnn

        Typed { ann } ->
            ann


interpolatedValueToString : InterpolationKind -> CG.Expression -> CG.Expression
interpolatedValueToString kind =
    case kind of
        Simple ->
            identity

        Typed { toString } ->
            toString
