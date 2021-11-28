module ContentTypes.Json exposing (parse)

import Json.Decode as D
import Parser exposing ((|.), (|=))
import Placeholder.Internal as Placeholder exposing (Template)
import Result.Extra
import Types exposing (I18nPairs)
import Util


type Json
    = Object (List ( String, Json ))
    | StringValue String


type alias FlattenedJson =
    List ( List String, String )


parse : String -> D.Decoder I18nPairs
parse =
    let
        addContext : ( c, Result x a ) -> Result ( c, x ) ( c, a )
        addContext ( c, rx ) =
            Result.map (Tuple.pair c) rx
                |> Result.mapError (Tuple.pair c)

        resultsToDecoder : List ( List String, Result String Template ) -> D.Decoder I18nPairs
        resultsToDecoder =
            List.map addContext
                >> Result.Extra.partition
                >> (\( parsedJson, errors ) ->
                        case errors of
                            [] ->
                                D.succeed <| List.map (Tuple.mapFirst <| Util.keyToName) parsedJson

                            _ ->
                                List.map (\( key, err ) -> String.join "." key ++ ": " ++ err) errors
                                    |> String.join "\n"
                                    |> D.fail
                   )
    in
    jsonDecoder
        >> D.map flattenJson
        >> D.andThen (resultsToDecoder << List.map (Tuple.mapSecond <| Placeholder.parseTemplate { startSymbol = "{{", endSymbol = "}}" }))
        >> D.map (List.sortBy Tuple.first)



-- toContext : ParsedJson -> Bool -> Context
-- toContext json optimize =
--     let
--         entries =
--             List.map (Tuple.mapFirst Util.keyToName) json
--         i18nTypeAnn =
--             CG.typed "I18n" []
--         i18nConstructors =
--             [ ( "Loaded", [ CG.typed "I18nInstance" [] ] ), ( "NotLoaded", [] ) ]
--         init =
--             CG.fun "NotLoaded"
--         decoderDecl =
--             CG.funDecl Nothing
--                 (Just <| CodeGen.DecodeM.decoder i18nTypeAnn)
--                 "i18nDecoder"
--                 []
--                 (CG.applyBinOp
--                     (List.foldl
--                         (\( key, template ) pipeline ->
--                             CG.applyBinOp pipeline
--                                 CG.piper
--                                 (CG.apply
--                                     [ CodeGen.DecodeM.requiredAt
--                                     , List.map CG.string key |> CG.list
--                                     , chooseDecoder template
--                                     ]
--                                 )
--                         )
--                         (CG.apply [ CodeGen.DecodeM.succeed, CG.fun "I18nInstance" ])
--                         (if optimize then
--                             optimizeJsonHelper entries
--                          else
--                             json
--                         )
--                     )
--                     CG.piper
--                     (CG.apply [ CodeGen.DecodeM.map, CG.fun "Loaded" ])
--                 )
--         genAccessor : CG.Expression -> String -> List String -> CG.Expression
--         genAccessor i18nVar key placeholderNames =
--             let
--                 instanceVar =
--                     Util.safeName "instance"
--             in
--             CG.caseExpr i18nVar
--                 [ ( CG.namedPattern "Loaded" [ CG.varPattern instanceVar ]
--                   , CG.apply <|
--                         CG.access (CG.val instanceVar) (Util.safeName key)
--                             :: List.map (Util.safeName >> CG.val) placeholderNames
--                   )
--                 , ( CG.namedPattern "NotLoaded" []
--                   , CG.val "fallbackValue_"
--                   )
--                 ]
--     in
--     { entries = entries
--     , init = init
--     , i18nConstructors = i18nConstructors
--     , genAccessor = genAccessor
--     , extraDeclarations = [ ( True, decoderDecl ), ( False, stringParserToDecoder ) ]
--     }
-- optimizeJson : ParsedJson -> String
-- optimizeJson =
--     optimizeJsonHelper
--         >> List.map (Tuple.second >> Placeholder.templateToString)
--         >> Json.Encode.list Json.Encode.string
--         >> Json.Encode.encode 0
-- optimizeJsonHelper : List ( never, string ) -> List ( List String, string )
-- optimizeJsonHelper =
--     List.indexedMap (\index -> Tuple.mapFirst (always [ String.fromInt index ]))


jsonDecoder : String -> D.Decoder Json
jsonDecoder str =
    case D.decodeString decoder str of
        Ok json ->
            D.succeed json

        Err err ->
            D.fail <| D.errorToString err


flattenJson : Json -> FlattenedJson
flattenJson json =
    case json of
        StringValue _ ->
            []

        Object obj ->
            flattenJsonHelper obj


flattenJsonHelper : List ( String, Json ) -> FlattenedJson
flattenJsonHelper =
    List.concatMap <|
        \( key, value ) ->
            case value of
                StringValue str ->
                    [ ( [ key ], str ) ]

                Object innerObj ->
                    List.map (Tuple.mapFirst <| (::) key) <|
                        flattenJsonHelper innerObj


decoder : D.Decoder Json
decoder =
    let
        objectDecoder =
            D.keyValuePairs (D.lazy <| \_ -> decoder)
                |> D.map Object

        stringDecoder =
            D.string |> D.map StringValue
    in
    D.oneOf
        [ objectDecoder
        , stringDecoder
        ]
