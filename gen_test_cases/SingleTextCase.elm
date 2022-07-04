module SingleTextCase exposing (main)

import Dict
import Dict.NonEmpty
import Shared exposing (buildMain)
import State exposing (NonEmptyState)
import Types.Segment exposing (TSegment(..))


main : Program () () msg
main =
    buildMain "SingleText" state


state : NonEmptyState ()
state =
    Dict.NonEmpty.singleton "messages" <|
        Dict.NonEmpty.singleton "en"
            { pairs = Dict.fromList [ ( "singleText", ( Text "the text", [] ) ) ]
            , fallback = Nothing
            , resources = ()
            }
