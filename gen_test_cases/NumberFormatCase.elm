module NumberFormatCase exposing (..)

import Dict
import Dict.NonEmpty
import State exposing (NonEmptyState)
import Types.ArgValue exposing (ArgValue(..))
import Types.Segment exposing (TSegment(..))
import Util.Shared exposing (Generator, buildMain, dynamicOpts, inlineOpts)


main : Generator
main =
    buildMain [ inlineOpts, dynamicOpts ] state


state : NonEmptyState ()
state =
    Dict.NonEmpty.singleton "messages" <|
        Dict.NonEmpty.singleton
            "en"
            { pairs = Dict.fromList [ ( "text", ( Text "Price: ", [ FormatNumber "price" [ ( "style", StringArg "currency" ) ] ] ) ) ]
            , fallback = Nothing
            , resources = ()
            }
