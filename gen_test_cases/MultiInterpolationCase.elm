module MultiInterpolationCase exposing (main)

import Dict
import Dict.NonEmpty
import State exposing (State)
import Types.Segment exposing (TSegment(..))
import Util.Shared exposing (Generator, buildMain, dynamicOpts, inlineOpts)


main : Generator
main =
    buildMain [ inlineOpts, dynamicOpts ] state


state : State ()
state =
    Dict.singleton "messages" <|
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
