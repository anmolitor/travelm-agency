module ContentTypes.JsonTest exposing (..)

import ContentTypes.Json as Json exposing (NestedJson(..))
import Expect
import Test exposing (Test, describe, test)
import Types.Segment exposing (TKey, TSegment(..), TValue)
import Types.Translation
import Dict


parserTests : Test
parserTests =
    describe "Json parser"
        [ test "single key object" <|
            \_ ->
                """{ "key": "value" }"""
                    |> expectParseTo [ ( "key", ( Text "value", [] ) ) ]
        , test "single nested key object" <|
            \_ ->
                """{ "level1": { "level2": "value" } }"""
                    |> expectParseTo [ ( "level1Level2", ( Text "value", [] ) ) ]
        , test "multiple key object" <|
            \_ ->
                """{ "key1": "val1", "key2": "val2", "nested": { "key3": "val3", "key4": "val4" } }"""
                    |> expectParseTo
                        [ ( "key1", ( Text "val1", [] ) )
                        , ( "key2", ( Text "val2", [] ) )
                        , ( "nestedKey3", ( Text "val3", [] ) )
                        , ( "nestedKey4", ( Text "val4", [] ) )
                        ]
        , test "single property with placeholder" <|
            \_ ->
                """{ "prop": "hi {name}" }"""
                    |> expectParseTo [ ( "prop", ( Text "hi ", [ Interpolation "name" ] ) ) ]
        , test "multiple placeholders" <|
            \_ ->
                """{ "prop": "hi { name }{other}" }"""
                    |> expectParseTo [ ( "prop", ( Text "hi ", [ Interpolation "name", Interpolation "other" ] ) ) ]
        , test "escaping {" <|
            \_ ->
                """{ "prop": "escaped \\{ woop" }"""
                    |> expectParseTo [ ( "prop", ( Text "escaped { woop", [] ) ) ]
        , test "escaping \\" <|
            \_ ->
                """{ "prop": "escaped \\\\ woop" }"""
                    |> expectParseTo [ ( "prop", ( Text "escaped \\ woop", [] ) ) ]
        , test "error when trying to escape normal char" <|
            \_ ->
                """{ "prop": "normal \\char" }"""
                    |> Json.parse
                    |> Expect.err
        , test "empty json string" <|
            \_ ->
                """{ "prop": "" }"""
                    |> expectParseTo [ ( "prop", ( Text "", [] ) ) ]
        , test "simple html" <|
            \_ ->
                """{ "a": "<span>Test</span>" }"""
                    |> expectParseTo [ ( "a", ( Html { tag = "span", attrs = [], content = ( Text "Test", [] ) }, [] ) ) ]
        , test "html with attributes" <|
            \_ ->
                """{ "a": "<span id="an id">Test</span>" }"""
                    |> expectParseTo
                        [ ( "a"
                          , ( Html
                                { tag = "span"
                                , attrs = [ ( "id", ( Text "an id", [] ) ) ]
                                , content = ( Text "Test", [] )
                                }
                            , []
                            )
                          )
                        ]
        , test "multiple html attributes" <|
            \_ ->
                """{ "a": "<span id="an id" data-testid="test">Test</span>" }"""
                    |> expectParseTo
                        [ ( "a"
                          , ( Html
                                { tag = "span"
                                , attrs = [ ( "id", ( Text "an id", [] ) ), ( "data-testid", ( Text "test", [] ) ) ]
                                , content = ( Text "Test", [] )
                                }
                            , []
                            )
                          )
                        ]
        , test "escaping html with \\" <|
            \_ ->
                """{ "a": "\\<span>\\</span>" }"""
                    |> expectParseTo [ ( "a", ( Text "<span></span>", [] ) ) ]
        , test "nested html" <|
            \_ ->
                """{ "a": "<a href="/"><div id="anId">test</div></a>" }"""
                    |> expectParseTo
                        [ ( "a"
                          , ( Html
                                { tag = "a"
                                , attrs = [ ( "href", ( Text "/", [] ) ) ]
                                , content =
                                    ( Html { tag = "div", attrs = [ ( "id", ( Text "anId", [] ) ) ], content = ( Text "test", [] ) }
                                    , []
                                    )
                                }
                            , []
                            )
                          )
                        ]
        , test "fallback directive" <|
            \_ ->
                """{ "--fallback-language": "en", "other": "test" }"""
                    |> Json.parse
                    |> Expect.equal
                        (Ok
                            { pairs = Dict.fromList [ ( "other", ( Text "test", [] ) ) ]
                            , fallback = Just "en"
                            , resources = ()
                            }
                        )
        ]


converterTests : Test
converterTests =
    describe "JSON to internal representation converter"
        [ test "single property" <|
            \_ ->
                [ ( "name", StringValue "test" ) ]
                    |> Json.jsonToInternalRep
                    |> Expect.equal (Ok <| Types.Translation.fromPairs [ ( "name", ( Text "test", [] ) ) ])
        , test "single nested property" <|
            \_ ->
                [ ( "level1", Object [ ( "level2", StringValue "test" ) ] ) ]
                    |> Json.jsonToInternalRep
                    |> Expect.equal (Ok <| Types.Translation.fromPairs [ ( "level1Level2", ( Text "test", [] ) ) ])
        , test "single property with placeholder" <|
            \_ ->
                [ ( "prop", StringValue "hi {name}" ) ]
                    |> Json.jsonToInternalRep
                    |> Expect.equal (Ok <| Types.Translation.fromPairs [ ( "prop", ( Text "hi ", [ Interpolation "name" ] ) ) ])
        , test "multiple placeholders" <|
            \_ ->
                [ ( "prop", StringValue "hi {name}{other}" ) ]
                    |> Json.jsonToInternalRep
                    |> Expect.equal (Ok <| Types.Translation.fromPairs [ ( "prop", ( Text "hi ", [ Interpolation "name", Interpolation "other" ] ) ) ])
        , test "escaped {" <|
            \_ ->
                [ ( "prop", StringValue "escaped \\{ woop" ) ]
                    |> Json.jsonToInternalRep
                    |> Expect.equal (Ok <| Types.Translation.fromPairs [ ( "prop", ( Text "escaped { woop", [] ) ) ])
        , test "escaped \\" <|
            \_ ->
                [ ( "prop", StringValue "escaped \\\\ woop" ) ]
                    |> Json.jsonToInternalRep
                    |> Expect.equal (Ok <| Types.Translation.fromPairs [ ( "prop", ( Text "escaped \\ woop", [] ) ) ])
        , test "trying to escape character that should not be escaped" <|
            \_ ->
                [ ( "prop", StringValue "\\idk" ) ]
                    |> Json.jsonToInternalRep
                    |> Expect.err
        , test "empty json string" <|
            \_ ->
                [ ( "prop", StringValue "" ) ]
                    |> Json.jsonToInternalRep
                    |> Expect.equal (Ok <| Types.Translation.fromPairs [ ( "prop", ( Text "", [] ) ) ])
        ]


expectParseTo : List ( TKey, TValue ) -> String -> Expect.Expectation
expectParseTo expected stringToParse =
    Json.parse stringToParse
        |> Expect.equal (Ok <| Types.Translation.fromPairs expected)
