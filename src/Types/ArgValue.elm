module Types.ArgValue exposing (..)

import Elm.CodeGen as CG
import Json.Encode as E


type ArgValue
    = BoolArg Bool
    | StringArg String
    | NumberArg Float


encode : ArgValue -> E.Value
encode v =
    case v of
        BoolArg b ->
            E.bool b

        StringArg s ->
            E.string s

        NumberArg f ->
            E.float f


generateEncoded : ArgValue -> CG.Expression
generateEncoded v =
    case v of
        BoolArg b ->
            if b then
                CG.apply [ CG.fqFun [ "Json", "Encode" ] "bool", CG.val "True" ]

            else
                CG.apply [ CG.fqFun [ "Json", "Encode" ] "bool", CG.val "False" ]

        StringArg s ->
            CG.apply [ CG.fqFun [ "Json", "Encode" ] "string", CG.string s ]

        NumberArg f ->
            CG.apply [ CG.fqFun [ "Json", "Encode" ] "float", CG.float f ]
