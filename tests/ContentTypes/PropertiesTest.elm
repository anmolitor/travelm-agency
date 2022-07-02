module ContentTypes.PropertiesTest exposing (..)

import ContentTypes.Properties as Properties exposing (Comment(..), Resource(..))
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


converterTests : Test
converterTests =
    describe "Properties to Internal Representation Converter"
        [ test "single key value pair" <|
            \_ ->
                Properties.propertiesToInternalRep [ PropertyResource ( [ "prop", "name" ], "value" ) ]
                    |> Expect.equal (Ok <| Types.Translation.fromPairs [ ( "propName", ( Text "value", [] ) ) ])
        , test "empty value" <|
            \_ ->
                Properties.propertiesToInternalRep [ PropertyResource ( [ "prop", "name" ], "" ) ]
                    |> Expect.equal (Ok <| Types.Translation.fromPairs [ ( "propName", ( Text "", [] ) ) ])
        , test "single placeholder" <|
            \_ ->
                Properties.propertiesToInternalRep [ PropertyResource ( [ "prop" ], "hi {name}" ) ]
                    |> Expect.equal (Ok <| Types.Translation.fromPairs [ ( "prop", ( Text "hi ", [ Interpolation "name" ] ) ) ])
        , test "multiple placeholders" <|
            \_ ->
                Properties.propertiesToInternalRep [ PropertyResource ( [ "prop" ], "hi {name} {abc}." ) ]
                    |> Expect.equal (Ok <| Types.Translation.fromPairs [ ( "prop", ( Text "hi ", [ Interpolation "name", Text " ", Interpolation "abc", Text "." ] ) ) ])
        , test "multiple pairs" <|
            \_ ->
                Properties.propertiesToInternalRep [ PropertyResource ( [ "prop1" ], "val1" ), PropertyResource ( [ "prop2" ], "val2" ) ]
                    |> Expect.equal (Ok <| Types.Translation.fromPairs [ ( "prop1", ( Text "val1", [] ) ), ( "prop2", ( Text "val2", [] ) ) ])
        , test "escaping { with double quote or single quote" <|
            \_ ->
                Properties.propertiesToInternalRep [ PropertyResource ( [ "prop1" ], "\"{\" '{'" ) ]
                    |> Expect.equal (Ok <| Types.Translation.fromPairs [ ( "prop1", ( Text "{ {", [] ) ) ])
        ]


expectParseTo : List ( TKey, TValue ) -> String -> Expect.Expectation
expectParseTo expected stringToParse =
    Properties.parse stringToParse
        |> Expect.equal (Ok <| Types.Translation.fromPairs expected)
