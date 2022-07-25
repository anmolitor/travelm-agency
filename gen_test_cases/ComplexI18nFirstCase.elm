module ComplexI18nFirstCase exposing (..)

import Dict
import Dict.NonEmpty
import State exposing (NonEmptyState)
import Types.Segment exposing (TSegment(..))
import Util.Shared exposing (Generator, buildMain, dynamicOpts, inlineOpts)


main : Generator
main =
    buildMain [ { inlineOpts | i18nArgFirst = True }, { dynamicOpts | i18nArgFirst = True } ] state


state : NonEmptyState ()
state =
    Dict.NonEmpty.singleton "messages" <|
        Dict.NonEmpty.singleton "en"
            { pairs =
                Dict.fromList
                    [ ( "interpolationCase"
                      , ( InterpolationCase "var" ( Text "default", [] ) <|
                            Dict.fromList [ ( "one", ( Text "One", [] ) ) ]
                        , []
                        )
                      )
                    , ( "html"
                      , ( Html
                            { tag = "a"
                            , id = "link"
                            , attrs = [ ( "href", ( Text "/", [] ) ) ]
                            , content = ( Interpolation "test", [] )
                            }
                        , []
                        )
                      )
                    , ( "numberFormat", ( FormatNumber "num" [], [] ) )
                    ]
            , fallback = Nothing
            , resources = ()
            }
