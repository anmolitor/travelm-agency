module Util exposing (keyToName, moduleName, resultToDecoder, safeName, emptyIntl)

import Elm.CodeGen exposing (ModuleName)
import Json.Decode as D
import String.Extra
import Intl exposing (Intl)
import Json.Encode

emptyIntl : Intl
emptyIntl =
    Json.Encode.object []

keyToName : List String -> String
keyToName =
    String.join "." >> String.Extra.classify >> String.Extra.decapitalize


safeName : String -> String
safeName name =
    name ++ "_"


moduleName : String -> ModuleName
moduleName =
    String.split "."
        >> List.map String.Extra.classify


resultToDecoder : Result String a -> D.Decoder a
resultToDecoder result =
    case result of
        Ok ok ->
            D.succeed ok

        Err err ->
            D.fail err
