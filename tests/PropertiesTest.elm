module PropertiesTest exposing (..)

import ContentTypes.Properties as Props
import Expect
import Json.Decode as D
import Placeholder.Internal exposing (Template, getPlaceholderNames, getSegments)
import Test exposing (Test, describe, test)
import Types exposing (I18nPairs)


runParser : String -> Result D.Error I18nPairs
runParser str =
    D.decodeString (D.string |> D.andThen Props.parse |> D.field "wrapper") <|
        """{"wrapper": \""""
            ++ String.replace "\n" "\\n" str
            ++ "\"}"


mapTemplate : (Template -> b) -> Result x (List ( a, Template )) -> Result x (List ( a, b ))
mapTemplate =
    Result.map << List.map << Tuple.mapSecond


suite : Test
suite =
    describe "Properties parser"
        [ test "single row" <|
            \_ ->
                runParser "test.property=Some value"
                    |> mapTemplate getSegments
                    |> Expect.equal (Ok [ ( "testProperty", ( "Some value", [] ) ) ])
        , test "single row with placeholder" <|
            \_ ->
                runParser "test.property=Some {{val}}"
                    |> mapTemplate getPlaceholderNames
                    |> Expect.equal (Ok [ ( "testProperty", [ "val" ] ) ])
        , test "multiple rows" <|
            \_ ->
                runParser "test.property1=val1\ntest.property2=val2"
                    |> mapTemplate getSegments
                    |> Expect.equal (Ok [ ( "testProperty1", ( "val1", [] ) ), ( "testProperty2", ( "val2", [] ) ) ])
        ]
