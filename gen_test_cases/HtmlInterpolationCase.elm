module HtmlInterpolationCase exposing (..)

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
    let
        htmlContentAdmin =
            ( Interpolation "username", [ Text " may click on this link." ] )

        hrefAdmin =
            ( Interpolation "adminLink", [] )

        adminView =
            ( Text "Thanks for logging in. "
            , [ Html
                    { tag = "a"
                    , id = "adminLink"
                    , attrs = [ ( "href", hrefAdmin ) ]
                    , content = htmlContentAdmin
                    }
              ]
            )

        normalView =
            ( Text "You (", [ Interpolation "username", Text ") are not an admin." ] )

        publicView =
            ( Text "You are not logged in.", [] )
    in
    Dict.singleton "messages" <|
        Dict.NonEmpty.singleton "en"
            { pairs =
                Dict.fromList
                    [ ( "text"
                      , ( InterpolationCase "role"
                            publicView
                          <|
                            Dict.fromList [ ( "admin", adminView ), ( "normal", normalView ) ]
                        , []
                        )
                      )
                    ]
            , fallback = Nothing
            , resources = ()
            }
