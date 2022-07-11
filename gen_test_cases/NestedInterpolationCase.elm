module NestedInterpolationCase exposing (..)

import Dict
import Dict.NonEmpty
import State exposing (NonEmptyState)
import Types.Segment exposing (TSegment(..))
import Util.Shared exposing (Generator, buildMain, dynamicOpts, inlineOpts)


main : Generator
main =
    buildMain [ inlineOpts, dynamicOpts ] state


state : NonEmptyState ()
state =
    Dict.NonEmpty.singleton "messages" <|
        Dict.NonEmpty.singleton "de"
            { pairs =
                Dict.fromList
                    [ ( "text"
                      , ( InterpolationCase "pronoun"
                            ( Interpolation "pronoun", [ Text " kauft ", Interpolation "objectsToBuy" ] )
                            (Dict.fromList
                                [ ( "Du", ( Text "Du kaufst ", [ Interpolation "objectsToBuy" ] ) )
                                , ( "Ich", ( Text "Ich kaufe ", [ Interpolation "objectsToBuy" ] ) )
                                ]
                            )
                        , [ Text "." ]
                        )
                      )
                    ]
            , fallback = Nothing
            , resources = ()
            }
