module PrefixFileIdentifierTestCase exposing (..)

import Dict
import Dict.NonEmpty
import State exposing (NonEmptyState)
import Types.Segment exposing (TSegment(..))
import Util.Shared exposing (Generator, buildMain, dynamicOpts, inlineOpts)


main : Generator
main =
    buildMain
        [ { inlineOpts | prefixFileIdentifier = True }
        , { dynamicOpts | prefixFileIdentifier = True }
        ]
        state


state : NonEmptyState ()
state =
    Dict.NonEmpty.fromList
        ( ( "bundle_1"
          , Dict.NonEmpty.singleton "en"
                { pairs = Dict.fromList [ ( "text", ( Text "text from bundle 1", [] ) ) ]
                , fallback = Nothing
                , resources = ()
                }
          )
        , [ ( "bundle_2"
            , Dict.NonEmpty.singleton "en"
                { pairs = Dict.fromList [ ( "text", ( Text "text from bundle 2", [] ) ) ]
                , fallback = Nothing
                , resources = ()
                }
            )
          ]
        )
