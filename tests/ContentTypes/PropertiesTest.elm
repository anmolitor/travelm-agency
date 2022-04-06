module ContentTypes.PropertiesTest exposing (..)

import ContentTypes.Properties as Properties exposing (Comment(..), Resource(..))
import Dict
import Expect
import State
import Test exposing (Test, describe, test)
import Types.Segment exposing (TSegment(..))


parserTests : Test
parserTests =
    describe "Properties parser"
        [ test "single row" <|
            \_ ->
                Properties.parseProperties "test.property=Some value"
                    |> Expect.equal (Ok [ PropertyResource ( [ "test", "property" ], "Some value" ) ])
        , test "empty value" <|
            \_ ->
                Properties.parseProperties "test.property="
                    |> Expect.equal (Ok [ PropertyResource ( [ "test", "property" ], "" ) ])
        , test "multiple rows" <|
            \_ ->
                Properties.parseProperties "test.property1=val1\ntest.property2=val2"
                    |> Expect.equal
                        (Ok
                            [ PropertyResource ( [ "test", "property1" ], "val1" )
                            , PropertyResource ( [ "test", "property2" ], "val2" )
                            ]
                        )
        , test "empty rows" <|
            \_ ->
                Properties.parseProperties """
prop1=A

prop2=B
                """ |> Expect.equal (Ok [ PropertyResource ( [ "prop1" ], "A" ), PropertyResource ( [ "prop2" ], "B" ) ])
        , test "single multi-line" <|
            \_ ->
                Properties.parseProperties """
prop1=A \\
    test

prop2=B
                """ |> Expect.equal (Ok [ PropertyResource ( [ "prop1" ], "A test" ), PropertyResource ( [ "prop2" ], "B" ) ])
        , test "more multi-lines with vanishing indent" <|
            \_ ->
                Properties.parseProperties """
prop=A \\
    test\\
  for\\
this
                """ |> Expect.equal (Ok [ PropertyResource ( [ "prop" ], "A testforthis" ) ])
        , test "comments are parsed" <|
            \_ ->
                Properties.parseProperties """
# some comment
msg = abc
# bla bla
                """
                    |> Expect.equal
                        (Ok
                            [ CommentResource (OtherComment "some comment")
                            , PropertyResource ( [ "msg" ], "abc" )
                            , CommentResource (OtherComment "bla bla")
                            ]
                        )
        ]


converterTests : Test
converterTests =
    describe "Properties to Internal Representation Converter"
        [ test "single key value pair" <|
            \_ ->
                Properties.propertiesToInternalRep [ PropertyResource ( [ "prop", "name" ], "value" ) ]
                    |> Expect.equal (Ok <| State.fromTranslations <| Dict.fromList [ ( "propName", ( Text "value", [] ) ) ])
        , test "empty value" <|
            \_ ->
                Properties.propertiesToInternalRep [ PropertyResource ( [ "prop", "name" ], "" ) ]
                    |> Expect.equal (Ok <| State.fromTranslations <| Dict.fromList [ ( "propName", ( Text "", [] ) ) ])
        , test "single placeholder" <|
            \_ ->
                Properties.propertiesToInternalRep [ PropertyResource ( [ "prop" ], "hi {name}" ) ]
                    |> Expect.equal (Ok <| State.fromTranslations <| Dict.fromList [ ( "prop", ( Text "hi ", [ Interpolation "name" ] ) ) ])
        , test "multiple placeholders" <|
            \_ ->
                Properties.propertiesToInternalRep [ PropertyResource ( [ "prop" ], "hi {name} {abc}." ) ]
                    |> Expect.equal (Ok <| State.fromTranslations <| Dict.fromList [ ( "prop", ( Text "hi ", [ Interpolation "name", Text " ", Interpolation "abc", Text "." ] ) ) ])
        , test "multiple pairs" <|
            \_ ->
                Properties.propertiesToInternalRep [ PropertyResource ( [ "prop1" ], "val1" ), PropertyResource ( [ "prop2" ], "val2" ) ]
                    |> Expect.equal (Ok <| State.fromTranslations <| Dict.fromList [ ( "prop1", ( Text "val1", [] ) ), ( "prop2", ( Text "val2", [] ) ) ])
        , test "escaping { with double quote or single quote" <|
            \_ ->
                Properties.propertiesToInternalRep [ PropertyResource ( [ "prop1" ], "\"{\" '{'" ) ]
                    |> Expect.equal (Ok <| State.fromTranslations <| Dict.fromList [ ( "prop1", ( Text "{ {", [] ) ) ])
        ]
