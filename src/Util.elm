module Util exposing (emptyIntl, keyToName, moduleName, quoteString, resultToDecoder, safeName, combineErrors)

import Elm.CodeGen exposing (ModuleName)
import Intl exposing (Intl)
import Json.Decode as D
import Json.Encode
import Result.Extra
import String.Extra


emptyIntl : Intl
emptyIntl =
    Json.Encode.object []


keyToName : List String -> String
keyToName =
    String.join "." >> String.Extra.classify >> String.Extra.decapitalize


quoteString : String -> String
quoteString str =
    "\"" ++ str ++ "\""


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


combineErrors : List (Result x a) -> Result (List x) (List a)
combineErrors =
    Result.Extra.partition
        >> (\( successes, errors ) ->
                case errors of
                    [] ->
                        Ok successes

                    _ ->
                        Err errors
           )
