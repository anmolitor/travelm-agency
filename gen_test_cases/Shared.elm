module Shared exposing (..)

import CodeGen.Shared exposing (Context)
import FileWriter exposing (writeFile)
import Generators.Inline
import Generators.Names exposing (defaultNames)
import State exposing (NonEmptyState)
import Util


buildMain : String -> NonEmptyState () -> Program () () msg
buildMain testCaseName state =
    let
        moduleName =
            testCaseName ++ "Translations"
    in
    Platform.worker
        { init =
            \_ ->
                ( ()
                , Generators.Inline.toFile (defaultContext [ "Inline", moduleName ]) state
                    |> writeFile ("gen_test_cases/Inline/" ++ moduleName ++ ".elm")
                )
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = always Sub.none
        }


defaultContext : List String -> Context
defaultContext moduleName =
    { moduleName = moduleName
    , version = "TEST_VERSION"
    , names = defaultNames
    , intl = Util.emptyIntl
    , i18nArgLast = False
    }
