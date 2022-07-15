module NamespacingCase exposing (..)

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
        Dict.NonEmpty.singleton "en"
            { pairs =
                Dict.fromList
                    [ ( "let", ( Text "elm keyword", [] ) )
                    , ( "init", ( Text "reserved name", [] ) )
                    ]
            , fallback = Nothing
            , resources = ()
            }
