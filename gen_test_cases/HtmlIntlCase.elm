module HtmlIntlCase exposing (..)

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
            { pairs =
                Dict.fromList
                    [ ( "formatNumber"
                      , ( Text "Price: "
                        , [ Html
                                { tag = "b"
                                , attrs = []
                                , id = "price"
                                , content = ( FormatNumber "price" [], [] )
                                }
                          ]
                        )
                      )
                    , ( "formatDate"
                      , ( Text "Today is "
                        , [ Html
                                { tag = "em"
                                , attrs = []
                                , id = "today"
                                , content = ( FormatDate "date" [], [] )
                                }
                          ]
                        )
                      )
                    , ( "pluralRules"
                      , ( Html
                            { tag = "p"
                            , attrs = []
                            , id = "plural"
                            , content = ( PluralCase "amount" [] ( Interpolation "amount", [] ) Dict.empty, [] )
                            }
                        , []
                        )
                      )
                    ]
            , fallback = Nothing
            , resources = ()
            }
