module Util.Shared exposing (..)

import CodeGen.Shared exposing (Context)
import Dict exposing (Dict)
import Dict.NonEmpty
import Elm.CodeGen as CG
import Elm.Pretty
import Generators.Dynamic
import Generators.Inline
import Generators.Names exposing (defaultNames)
import Json.Decode as D
import Json.Encode as E
import Ports exposing (GeneratorMode(..))
import Result.Extra
import State exposing (NonEmptyState, OptimizedJson, State)
import Util
import Util.FilePort


type alias Generator =
    Program { name : String } () ()


type alias GenOptions =
    { mode : GeneratorMode
    , i18nArgFirst : Bool
    , addContentHash : Bool
    }


inlineOpts : GenOptions
inlineOpts =
    { mode = Inline, i18nArgFirst = False, addContentHash = False }


dynamicOpts : GenOptions
dynamicOpts =
    { mode = Dynamic, i18nArgFirst = False, addContentHash = False }


buildMain : List GenOptions -> State () -> Generator
buildMain opts state =
    Platform.worker
        { init =
            \{ name } ->
                ( ()
                , Cmd.batch <|
                    List.map (generate name state) opts
                )
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = always Sub.none
        }


generate : String -> State () -> GenOptions -> Cmd msg
generate name state opts =
    let
        moduleName =
            name ++ "Translations"
    in
    case opts.mode of
        Dynamic ->
            let
                stateWithResources =
                    State.validateState False state
                        |> Result.mapError (\err -> Debug.todo <| "State validation failed!" ++ Debug.toString err)
                        |> Result.Extra.merge
                        |> Dict.NonEmpty.map (Generators.Dynamic.optimizeJsonAllLanguages opts.addContentHash)
            in
            Cmd.batch
                [ Generators.Dynamic.toFile
                    { defaultContext
                        | moduleName = [ "Dynamic", moduleName ]
                        , i18nArgLast = not opts.i18nArgFirst
                    }
                    stateWithResources
                    |> writeFile ("gen_test_cases/Dynamic/" ++ moduleName ++ ".elm")
                , Dict.NonEmpty.toDict stateWithResources
                    |> State.getAllResources
                    |> writeServerProxy (name ++ "Server")
                ]

        Inline ->
            State.validateState False state
                |> Result.mapError (\err -> Debug.todo <| "State validation failed!" ++ Debug.toString err)
                |> Result.Extra.merge
                |> Generators.Inline.toFile
                    { defaultContext
                        | moduleName = [ "Inline", moduleName ]
                        , i18nArgLast = not opts.i18nArgFirst
                    }
                |> writeFile ("gen_test_cases/Inline/" ++ moduleName ++ ".elm")


defaultContext : Context
defaultContext =
    { moduleName = []
    , version = "TEST_VERSION"
    , names = defaultNames
    , intl = Util.emptyIntl
    , i18nArgLast = False
    }


sendRequest : Dict String String -> String -> D.Decoder a -> Result D.Error a
sendRequest server path decoder =
    case Dict.get path server of
        Nothing ->
            Err <| D.Failure "Cannot find resource with the given path" (E.string path)

        Just response ->
            D.decodeString decoder response


writeServerProxy : String -> List OptimizedJson -> Cmd msg
writeServerProxy moduleName resources =
    let
        jsonToExpr { filename, content } =
            CG.tuple [ CG.string filename, CG.string content ]

        -- Just use a dict as a "server" to lookup json by path
        serverExpr =
            CG.apply [ CG.fqFun [ "Dict" ] "fromList", CG.list <| List.map jsonToExpr resources ]

        serverType =
            CG.dictAnn CG.stringAnn CG.stringAnn

        serverDecl =
            CG.valDecl Nothing (Just serverType) "server" serverExpr
    in
    CG.file (CG.normalModule [ "Dynamic", moduleName ] [])
        [ CG.importStmt [ "Dict" ] Nothing (Just <| CG.exposeExplicit [ CG.closedTypeExpose "Dict" ]) ]
        [ serverDecl ]
        Nothing
        |> writeFile ("gen_test_cases/Dynamic/" ++ moduleName ++ ".elm")


writeFile : String -> CG.File -> Cmd msg
writeFile path file =
    Util.FilePort.sendFile { path = path, content = Elm.Pretty.pretty 120 file }
