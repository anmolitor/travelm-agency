module ContentTypes.JsonTest exposing (..)

import ContentTypes.Json as Json exposing (NestedJson(..))
import Dict
import Expect
import State
import Test exposing (Test, describe, test)
import Types.Segment exposing (TSegment(..))
import Types.Translation


parserTests : Test
parserTests =
    describe "Json parser"
        [ test "single key object" <|
            \_ ->
                Json.parseJson """{ "key": "value" }"""
                    |> Expect.equal (Ok [ ( "key", StringValue "value" ) ])
        , test "single nested key object" <|
            \_ ->
                Json.parseJson """{ "level1": { "level2": "value" } }"""
                    |> Expect.equal (Ok [ ( "level1", Object [ ( "level2", StringValue "value" ) ] ) ])
        , test "multiple key object" <|
            \_ ->
                Json.parseJson """{ "key1": "val1", "key2": "val2", "nested": { "key3": "val3", "key4": "val4" } }"""
                    |> Expect.equal
                        (Ok
                            [ ( "key1", StringValue "val1" )
                            , ( "key2", StringValue "val2" )
                            , ( "nested", Object [ ( "key3", StringValue "val3" ), ( "key4", StringValue "val4" ) ] )
                            ]
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
