port module FileWriter exposing (writeFile)

import Elm.CodeGen
import Elm.Pretty


port sendFile : { path : String, content : String } -> Cmd msg


writeFile : String -> Elm.CodeGen.File -> Cmd msg
writeFile path file =
    sendFile { path = path, content = Elm.Pretty.pretty 120 file }
