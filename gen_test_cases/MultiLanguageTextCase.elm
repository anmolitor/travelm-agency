module MultiLanguageTextCase exposing (main)

import Dict
import Dict.NonEmpty
import Shared exposing (buildMain)
import State exposing (NonEmptyState)
import Types.Segment exposing (TSegment(..))


main : Program () () msg
main =
    buildMain "MultiLanguageText" state


state : NonEmptyState ()
state =
    Dict.NonEmpty.singleton "messages" <|
        Dict.NonEmpty.fromList
            ( ( "en"
              , { pairs = Dict.fromList [ ( "text", ( Text "english text", [] ) ) ]
                , fallback = Nothing
                , resources = ()
                }
              )
            , [ ( "de"
                , { pairs = Dict.fromList [ ( "text", ( Text "german text", [] ) ) ]
                  , fallback = Nothing
                  , resources = ()
                  }
                )
              ]
            )
