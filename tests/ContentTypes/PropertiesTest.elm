module ContentTypes.PropertiesTest exposing (..)

import ContentTypes.Properties as Properties
import Expect
import Test exposing (Test, describe, test)
import Types exposing (TSegment(..))


parserTests : Test
parserTests =
    describe "Properties parser"
        [ test "single row" <|
            \_ ->
                Properties.parseProperties "test.property=Some value"
                    |> Expect.equal (Ok [ ( [ "test", "property" ], "Some value" ) ])
        , test "empty value" <|
            \_ ->
                Properties.parseProperties "test.property="
                    |> Expect.equal (Ok [ ( [ "test", "property" ], "" ) ])
        , test "multiple rows" <|
            \_ ->
                Properties.parseProperties "test.property1=val1\ntest.property2=val2"
                    |> Expect.equal
                        (Ok
                            [ ( [ "test", "property1" ], "val1" )
                            , ( [ "test", "property2" ], "val2" )
                            ]
                        )
        , test "empty rows" <|
            \_ ->
                Properties.parseProperties """
prop1=A

prop2=B
                """ |> Expect.equal (Ok [ ( [ "prop1" ], "A" ), ( [ "prop2" ], "B" ) ])
        , test "single multi-line" <|
            \_ ->
                Properties.parseProperties """
prop1=A \\
    test

prop2=B
                """ |> Expect.equal (Ok [ ( [ "prop1" ], "A test" ), ( [ "prop2" ], "B" ) ])
        , test "more multi-lines with vanishing indent" <|
            \_ ->
                Properties.parseProperties """
prop=A \\
    test\\
  for\\
this
                """ |> Expect.equal (Ok [ ( [ "prop" ], "A testforthis" ) ])
        ]


converterTests : Test
converterTests =
    describe "Properties to Internal Representation Converter"
        [ test "single key value pair" <|
            \_ ->
                Properties.propertiesToInternalRep [ ( [ "prop", "name" ], "value" ) ]
                    |> Expect.equal (Ok [ ( "propName", ( Text "value", [] ) ) ])
        , test "empty value" <|
            \_ ->
                Properties.propertiesToInternalRep [ ( [ "prop", "name" ], "" ) ]
                    |> Expect.equal (Ok [ ( "propName", ( Text "", [] ) ) ])
        , test "single placeholder" <|
            \_ ->
                Properties.propertiesToInternalRep [ ( [ "prop" ], "hi {name}" ) ]
                    |> Expect.equal (Ok [ ( "prop", ( Text "hi ", [ Interpolation "name" ] ) ) ])
        , test "multiple placeholders" <|
            \_ ->
                Properties.propertiesToInternalRep [ ( [ "prop" ], "hi {name} {abc}." ) ]
                    |> Expect.equal (Ok [ ( "prop", ( Text "hi ", [ Interpolation "name", Text " ", Interpolation "abc", Text "." ] ) ) ])
        , test "multiple pairs" <|
            \_ ->
                Properties.propertiesToInternalRep [ ( [ "prop1" ], "val1" ), ( [ "prop2" ], "val2" ) ]
                    |> Expect.equal (Ok [ ( "prop1", ( Text "val1", [] ) ), ( "prop2", ( Text "val2", [] ) ) ])
        ]
