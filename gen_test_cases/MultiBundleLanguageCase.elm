module MultiBundleLanguageCase exposing (..)

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
    Dict.fromList
        [ ( "bundle_1"
          , Dict.NonEmpty.fromList
                ( ( "en"
                  , { pairs = Dict.fromList [ ( "text1", ( Text "english text 1", [] ) ) ]
                    , fallback = Nothing
                    , resources = ()
                    }
                  )
                , [ ( "de"
                    , { pairs = Dict.fromList [ ( "text1", ( Text "german text 1", [] ) ) ]
                      , fallback = Nothing
                      , resources = ()
                      }
                    )
                  ]
                )
          )
        , ( "bundle_2"
          , Dict.NonEmpty.fromList
                ( ( "en"
                  , { pairs = Dict.fromList [ ( "text2", ( Text "english text 2", [] ) ) ]
                    , fallback = Nothing
                    , resources = ()
                    }
                  )
                , [ ( "de"
                    , { pairs = Dict.fromList [ ( "text2", ( Text "german text 2", [] ) ) ]
                      , fallback = Nothing
                      , resources = ()
                      }
                    )
                  ]
                )
          )
        ]
