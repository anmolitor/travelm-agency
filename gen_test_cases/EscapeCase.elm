module EscapeCase exposing (..)

import Dict
import Dict.NonEmpty
import State exposing (NonEmptyState)
import Types.Segment exposing (TSegment(..))
import Util.Shared exposing (Generator, buildMain, dynamicOpts)


main : Generator
main =
    buildMain [ dynamicOpts ] state


state : NonEmptyState ()
state =
    Dict.NonEmpty.singleton "messages" <|
        Dict.NonEmpty.singleton
            "en"
            { pairs = Dict.fromList [ ( "text", ( Text "escaped interpolation { $var }, actual ", [ Interpolation "interpolation" ] ) ) ]
            , fallback = Nothing
            , resources = ()
            }
