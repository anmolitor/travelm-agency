module JsonTest exposing (suite)

import ContentTypes.Json as Json
import Expect
import Json.Decode as D
import Placeholder.DoubleCurly exposing (parsePlaceholder1)
import Placeholder.Internal exposing (Template, getPlaceholderNames)
import Test exposing (Test, describe, test)
import Types exposing (I18nPairs)


runParser : String -> Result D.Error I18nPairs
runParser str =
    D.decodeString (D.string |> D.andThen Json.parse |> D.field "wrapper") <|
        """{"wrapper": \""""
            ++ (String.replace "\"" "\\\"" >> String.replace "\n" "") str
            ++ "\"}"


mapTemplate : (Template -> b) -> Result x (List ( a, Template )) -> Result x (List ( a, b ))
mapTemplate =
    Result.map << List.map << Tuple.mapSecond


suite : Test
suite =
    describe "Json parser"
        [ test "single key object" <|
            \_ ->
                runParser """{ "key": "value" }"""
                    |> mapTemplate getPlaceholderNames
                    |> Expect.equal (Ok [ ( "key", [] ) ])
        , test "single nested key object" <|
            \_ ->
                runParser """{ "level1": { "level2": "value" } }"""
                    |> mapTemplate getPlaceholderNames
                    |> Expect.equal (Ok [ ( "level1Level2", [] ) ])
        , test "single key object with single placeholder" <|
            \_ ->
                runParser """{ "key": "a {{test}} case" }"""
                    |> mapTemplate getPlaceholderNames
                    |> Expect.equal (Ok [ ( "key", [ "test" ] ) ])
        , test "multiple key object" <|
            \_ ->
                runParser """{ "key1": "val1", "key2": "val2", "nested": { "key3": "val3", "key4": "val4" } }"""
                    |> mapTemplate getPlaceholderNames
                    |> Expect.equal
                        (Ok
                            [ ( "key1", [] )
                            , ( "key2", [] )
                            , ( "nestedKey3", [] )
                            , ( "nestedKey4", [] )
                            ]
                        )
        ]
