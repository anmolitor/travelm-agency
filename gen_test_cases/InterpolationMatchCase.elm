module InterpolationMatchCase exposing (..)

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
        Dict.NonEmpty.singleton "en"
            { pairs =
                Dict.fromList
                    [ ( "text"
                      , ( InterpolationCase "gender"
                            ( Text "It", [] )
                            (Dict.fromList
                                [ ( "female", ( Text "She", [] ) )
                                , ( "male", ( Text "He", [] ) )
                                ]
                            )
                        , [ Text " bought a cat." ]
                        )
                      )
                    ]
            , fallback = Nothing
            , resources = ()
            }
