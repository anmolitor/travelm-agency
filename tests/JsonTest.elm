module JsonTest exposing (..)

import ContentTypes.Json as Json exposing (Json(..))
import Expect
import Test exposing (Test, describe, test)
import Types exposing (TSegment(..))


parserTests : Test
parserTests =
    describe "Json parser"
        [ test "single key object" <|
            \_ ->
                Json.parseJson """{ "key": "value" }"""
                    |> Expect.equal (Ok <| Object [ ( "key", StringValue "value" ) ])
        , test "single nested key object" <|
            \_ ->
                Json.parseJson """{ "level1": { "level2": "value" } }"""
                    |> Expect.equal (Ok <| Object [ ( "level1", Object [ ( "level2", StringValue "value" ) ] ) ])
        , test "multiple key object" <|
            \_ ->
                Json.parseJson """{ "key1": "val1", "key2": "val2", "nested": { "key3": "val3", "key4": "val4" } }"""
                    |> Expect.equal
                        (Ok <|
                            Object
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
                Object [ ( "name", StringValue "test" ) ]
                    |> Json.jsonToInternalRep
                    |> Expect.equal (Ok [ ( "name", ( Text "test", [] ) ) ])
        , test "single property with placeholder" <|
            \_ ->
                Object [ ( "prop", StringValue "hi {name}" ) ]
                    |> Json.jsonToInternalRep
                    |> Expect.equal (Ok [ ( "prop", ( Text "hi ", [ Interpolation "name" ] ) ) ])
        , test "multiple placeholders" <|
            \_ ->
                Object [ ( "prop", StringValue "hi {name}{other}" ) ]
                    |> Json.jsonToInternalRep
                    |> Expect.equal (Ok [ ( "prop", ( Text "hi ", [ Interpolation "name", Interpolation "other" ] ) ) ])
        ]
