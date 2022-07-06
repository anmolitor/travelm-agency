module SimpleI18nLastCase exposing (..)

import Dict
import Dict.NonEmpty
import State exposing (NonEmptyState)
import Types.Segment exposing (TSegment(..))
import Util.Shared exposing (Generator, buildMain, dynamicOpts, inlineOpts)


main : Generator
main =
    buildMain [ { inlineOpts | i18nArgLast = True }, { dynamicOpts | i18nArgLast = True } ] state


state : NonEmptyState ()
state =
    Dict.NonEmpty.singleton "messages" <|
        Dict.NonEmpty.singleton "en"
            { pairs =
                Dict.fromList
                    [ ( "singleText", ( Text "the text", [] ) )
                    , ( "interpolation", ( Text "Hello ", [Interpolation "planet", Text "!"] ) )
                    , ( "greeting", ( Text "Good ", [Interpolation "timeOfDay", Text ", ", Interpolation "name"] ) )
                    ]
            , fallback = Nothing
            , resources = ()
            }
