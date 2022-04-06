module StateTest exposing (..)

import Dict
import Dict.NonEmpty
import Elm.CodeGen as CG
import Expect exposing (Expectation)
import Fuzz
import State
import Test exposing (Test, describe, fuzz, fuzz2, test)
import Types.InterpolationKind as InterpolationKind
import Types.Segment as Segment


suite : Test
suite =
    describe "State"
        [ test "interpolation map for one language and one key" <|
            \_ ->
                Dict.NonEmpty.singleton
                    "de"
                    { pairs = Dict.fromList [ ( "key", ( Segment.Interpolation "a", [] ) ) ], resources = (), fallback = Nothing }
                    |> State.interpolationMap
                    |> Dict.get "key"
                    |> Expect.equal (Just <| Dict.singleton "a" InterpolationKind.Simple)
        , test "interpolation map for one languages and two keys" <|
            \_ ->
                Dict.NonEmpty.singleton
                    "de"
                    { pairs =
                        Dict.fromList
                            [ ( "key", ( Segment.Text "some text", [] ) )
                            , ( "key2", ( Segment.Interpolation "b", [] ) )
                            ]
                    , resources = ()
                    , fallback = Nothing
                    }
                    |> State.interpolationMap
                    |> Expect.equalDicts
                        (Dict.fromList
                            [ ( "key", Dict.empty )
                            , ( "key2", Dict.singleton "b" InterpolationKind.Simple )
                            ]
                        )
        , test "interpolation map for two languages and one key" <|
            \_ ->
                Dict.NonEmpty.fromList
                    ( ( "de"
                      , { pairs =
                            Dict.fromList
                                [ ( "key", ( Segment.Interpolation "a", [] ) )
                                ]
                        , resources = ()
                        , fallback = Nothing
                        }
                      )
                    , [ ( "en"
                        , { pairs =
                                Dict.fromList
                                    [ ( "key", ( Segment.Text "no interpolation", [] ) )
                                    ]
                          , resources = ()
                          , fallback = Nothing
                          }
                        )
                      ]
                    )
                    |> State.interpolationMap
                    |> Expect.equalDicts
                        (Dict.fromList
                            [ ( "key", Dict.singleton "a" InterpolationKind.Simple )
                            ]
                        )
        , test "interpolation map for two languages and one key with different placeholders" <|
            \_ ->
                Dict.NonEmpty.fromList
                    ( ( "de"
                      , { pairs =
                            Dict.fromList
                                [ ( "key", ( Segment.Interpolation "a", [ Segment.Interpolation "b" ] ) )
                                ]
                        , resources = ()
                        , fallback = Nothing
                        }
                      )
                    , [ ( "en"
                        , { pairs =
                                Dict.fromList
                                    [ ( "key", ( Segment.Interpolation "c", [ Segment.Interpolation "b" ] ) )
                                    ]
                          , resources = ()
                          , fallback = Nothing
                          }
                        )
                      ]
                    )
                    |> State.interpolationMap
                    |> Expect.equalDicts
                        (Dict.fromList
                            [ ( "key"
                              , Dict.fromList
                                    [ ( "a", InterpolationKind.Simple )
                                    , ( "b", InterpolationKind.Simple )
                                    , ( "c", InterpolationKind.Simple )
                                    ]
                              )
                            ]
                        )
        , test "interpolation map for two languages and one key with different interpolation kinds for the same placeholder" <|
            \_ ->
                let
                    mergedInterpolationKindExpectations =
                        [ Maybe.map InterpolationKind.toTypeAnn >> Expect.equal (Just CG.floatAnn)
                        ]
                in
                Dict.NonEmpty.fromList
                    ( ( "de"
                      , { pairs =
                            Dict.fromList
                                [ ( "key", ( Segment.Interpolation "a", [ Segment.PluralCase "b" [] ( Segment.Text "text", [] ) Dict.empty ] ) )
                                ]
                        , resources = ()
                        , fallback = Nothing
                        }
                      )
                    , [ ( "en"
                        , { pairs =
                                Dict.fromList
                                    [ ( "key", ( Segment.PluralCase "a" [] ( Segment.Text "text", [] ) Dict.empty, [ Segment.Interpolation "b" ] ) )
                                    ]
                          , resources = ()
                          , fallback = Nothing
                          }
                        )
                      ]
                    )
                    |> State.interpolationMap
                    |> Dict.get "key"
                    |> Expect.all
                        [ Maybe.andThen (Dict.get "a")
                            >> Expect.all mergedInterpolationKindExpectations
                        , Maybe.andThen (Dict.get "b")
                            >> Expect.all mergedInterpolationKindExpectations
                        ]
        ]
