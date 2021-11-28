module CodeGen.DecodeM exposing (andThen, array, decoder, fail, map, required, string, succeed)

import Elm.CodeGen as CG


succeed : CG.Expression
succeed =
    fun "succeed"


fail : CG.Expression
fail =
    fun "fail"


map : CG.Expression
map =
    fun "map"


required : CG.Expression
required =
    CG.fqFun [ "Json", "Decode", "Pipeline" ] "required"


string : CG.Expression
string =
    fun "string"


array : CG.Expression
array =
    fun "array"


andThen : CG.Expression
andThen =
    fun "andThen"


decoder : CG.TypeAnnotation -> CG.TypeAnnotation
decoder t =
    CG.fqTyped [ "Json", "Decode" ] "Decoder" [ t ]


fun : String -> CG.Expression
fun =
    CG.fqFun [ "Json", "Decode" ]
