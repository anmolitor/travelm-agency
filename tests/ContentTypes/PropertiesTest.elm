module ContentTypes.PropertiesTest exposing (..)

import ContentTypes.Properties as Properties
import Dict
import Expect
import Test exposing (Test, describe, test)
import Types.Segment exposing (TKey, TSegment(..), TValue)
import Types.Translation


parserTests : Test
parserTests =
    describe "Properties parser"
        [ test "single row" <|
            \_ ->
                "test.property=Some value"
                    |> expectParseTo [ ( "testProperty", ( Text "Some value", [] ) ) ]
        , test "empty value" <|
            \_ ->
                "test.property="
                    |> expectParseTo [ ( "testProperty", ( Text "", [] ) ) ]
        , test "multiple rows" <|
            \_ ->
                "test.property1=val1\ntest.property2=val2"
                    |> expectParseTo
                        [ ( "testProperty1", ( Text "val1", [] ) )
                        , ( "testProperty2", ( Text "val2", [] ) )
                        ]
        , test "empty rows" <|
            \_ ->
                """
prop1=A

prop2=B
                """ |> expectParseTo [ ( "prop1", ( Text "A", [] ) ), ( "prop2", ( Text "B", [] ) ) ]
        , test "single multi-line" <|
            \_ ->
                """
prop1=A \\
    test

prop2=B
                """ |> expectParseTo [ ( "prop1", ( Text "A test", [] ) ), ( "prop2", ( Text "B", [] ) ) ]
        , test "more multi-lines with vanishing indent" <|
            \_ ->
                """
prop=A \\
    test\\
  for\\
 this
                """ |> expectParseTo [ ( "prop", ( Text "A testforthis", [] ) ) ]
        , test "single placeholder" <|
            \_ ->
                "prop = hi {name}"
                    |> expectParseTo [ ( "prop", ( Text "hi ", [ Interpolation "name" ] ) ) ]
        , test "multiple placeholders" <|
            \_ ->
                "prop = hi {name} {abc}"
                    |> expectParseTo [ ( "prop", ( Text "hi ", [ Interpolation "name", Text " ", Interpolation "abc" ] ) ) ]
        , test "escaping { with '{' or \"{\"" <|
            \_ ->
                "a = needs '{' br } \"{\"ackets"
                    |> expectParseTo [ ( "a", ( Text "needs { br } {ackets", [] ) ) ]
        , test "escaping quotes" <|
            \_ ->
                "a = \"'\"quotes'\"'"
                    |> expectParseTo [ ( "a", ( Text "'quotes\"", [] ) ) ]
        , test "equals sign does not need escaping" <|
            \_ ->
                "a = abc=def"
                    |> expectParseTo [ ( "a", ( Text "abc=def", [] ) ) ]
        , test "simple html" <|
            \_ ->
                "a = <span>Test</span>"
                    |> expectParseTo [ ( "a", ( Html { tag = "span", attrs = [], content = ( Text "Test", [] ) }, [] ) ) ]
        , test "html with attributes" <|
            \_ ->
                "a = <span id=\"an id\">Test</span>"
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
                "a = <span id=\"an id\" data-testid=\"test\">Test</span>"
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
        , test "escaping html with quotes" <|
            \_ ->
                "a = '<'span>\"<\"/span>"
                    |> expectParseTo [ ( "a", ( Text "<span></span>", [] ) ) ]
        , test "nested html" <|
            \_ ->
                "a = <a href=\"/\"><div id=\"anId\">test</div></a>"
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
                """
# fallback-language: en
msg = abc
                """
                    |> Properties.parse
                    |> Expect.equal
                        (Ok
                            { pairs = Dict.fromList [ ( "msg", ( Text "abc", [] ) ) ]
                            , fallback = Just "en"
                            , resources = ()
                            }
                        )
        ]


expectParseTo : List ( TKey, TValue ) -> String -> Expect.Expectation
expectParseTo expected stringToParse =
    Properties.parse stringToParse
        |> Expect.equal (Ok <| Types.Translation.fromPairs expected)
