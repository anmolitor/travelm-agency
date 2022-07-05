module MultiInterpolationCase exposing (main)

import Dict
import Dict.NonEmpty
import State exposing (NonEmptyState)
import Types.Segment exposing (TSegment(..))
import Util.Shared exposing (Generator, buildMain, inlineOpts)


main : Generator
main =
    buildMain [ inlineOpts ] state


state : NonEmptyState ()
state =
    Dict.NonEmpty.singleton "messages" <|
        Dict.NonEmpty.fromList
            ( ( "en"
              , { pairs = Dict.fromList [ ( "greeting", ( Text "Good ", [ Interpolation "timeOfDay", Text ", ", Interpolation "name" ] ) ) ]
                , fallback = Nothing
                , resources = ()
                }
              )
            , [ ( "de"
                , { pairs = Dict.fromList [ ( "greeting", ( Text "Guten ", [ Interpolation "timeOfDay" ] ) ) ]
                  , fallback = Nothing
                  , resources = ()
                  }
                )
              , ( "yoda"
                , { pairs = Dict.fromList [ ( "greeting", ( Interpolation "name", [ Text ", good ", Interpolation "timeOfDay" ] ) ) ]
                  , fallback = Nothing
                  , resources = ()
                  }
                )
              ]
            )
