module EscapeCase exposing (..)

import Dict
import Dict.NonEmpty
import State exposing (NonEmptyState)
import Types.Segment exposing (TSegment(..))
import Util.Shared exposing (Generator, buildMain, dynamicOpts)


main : Generator
main =
    buildMain [ dynamicOpts ] state


state : NonEmptyState ()
state =
    Dict.NonEmpty.singleton "messages" <|
        Dict.NonEmpty.singleton
            "en"
            { pairs =
                Dict.fromList
                    [ ( "text", ( Text "escaped interpolation { $var }, actual ", [ Interpolation "interpolation" ] ) )
                    , ( "html"
                      , ( Text "escaped interpolation { $var }, actual "
                        , [ Html
                                { tag = "b"
                                , id = "bold"
                                , attrs = []
                                , content = ( Interpolation "interpolation", [] )
                                }
                          ]
                        )
                      )
                    , ( "quotationMarkAndBackslash", ( Text "just a \\ and \"quotation mark\"", [ Interpolation "a" ] ) )
                    , ( "quotationMarkAndBackslashHtml"
                      , ( Text "just a \\ and \"quotation mark\""
                        , [ Html
                                { tag = "b"
                                , id = "bold"
                                , attrs = []
                                , content = ( Interpolation "interpolation", [] )
                                }
                          ]
                        )
                      )
                    , ( "pipeOperatorInterpolationCase"
                      , ( InterpolationCase "val" ( Text "just a | pipe", [] ) Dict.empty
                        , []
                        )
                      )
                    , ( "pipeOperatorHtml"
                      , ( Html { tag = "div", id = "div", content = ( Text "just a | pipe", [] ), attrs = [] }
                        , []
                        )
                      )
                    ]
            , fallback = Nothing
            , resources = ()
            }
