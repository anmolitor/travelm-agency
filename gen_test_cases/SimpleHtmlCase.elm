module SimpleHtmlCase exposing (..)

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
                    [ ( "html", ( Html { tag = "a", id = "link", attrs = [ ( "href", ( Text "/", [] ) ) ], content = ( Text "Click me", [] ) }, [] ) )
                    ]
            , fallback = Nothing
            , resources = ()
            }
