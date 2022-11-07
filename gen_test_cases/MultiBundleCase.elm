module MultiBundleCase exposing (..)

import Dict
import Dict.NonEmpty
import State exposing (NonEmptyState, State)
import Types.Segment exposing (TSegment(..))
import Util.Shared exposing (Generator, buildMain, dynamicOpts, inlineOpts)


main : Generator
main =
    buildMain [ inlineOpts, dynamicOpts ] state


state : State ()
state =
    Dict.fromList
        [ ( "bundle_1"
          , Dict.NonEmpty.singleton "en"
                { pairs = Dict.fromList [ ( "text1", ( Text "text from bundle 1", [] ) ) ]
                , fallback = Nothing
                , resources = ()
                }
          )
        , ( "bundle_2"
          , Dict.NonEmpty.singleton "en"
                { pairs = Dict.fromList [ ( "text2", ( Text "text from bundle 2", [] ) ) ]
                , fallback = Nothing
                , resources = ()
                }
          )
        ]
