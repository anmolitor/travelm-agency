module NestedHtmlCase exposing (..)

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
                    [ ( "html"
                      , ( Html
                            { tag = "a"
                            , id = "link"
                            , attrs = [ ( "href", ( Text "/", [] ) ) ]
                            , content =
                                ( Html
                                    { tag = "span"
                                    , id = "text"
                                    , attrs =
                                        [ ( "width", ( Text "100", [] ) )
                                        , ( "height", ( Text "50", [] ) )
                                        ]
                                    , content = ( Text "Click me", [] )
                                    }
                                , [ Text "!"
                                  , Html
                                        { tag = "img"
                                        , id = "image"
                                        , attrs = [ ( "src", ( Text "/imgUrl.png", [] ) ) ]
                                        , content = ( Text "", [] )
                                        }
                                  ]
                                )
                            }
                        , []
                        )
                      )
                    ]
            , fallback = Nothing
            , resources = ()
            }
