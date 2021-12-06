module FluentTest exposing (..)

import ContentTypes.Fluent as F
import Expect
import Parser
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Fluent Parser"
        [ test "single message" <|
            \_ ->
                Parser.run F.message "test-identifier = some text"
                    |> Expect.equal (Ok { identifier = F.MessageIdentifier "test-identifier", content = [ F.TextContent "some text" ] })
        , test "single term" <|
            \_ ->
                Parser.run F.message "-test-term = some text"
                    |> Expect.equal (Ok { identifier = F.TermIdentifier "test-term", content = [ F.TextContent "some text" ] })
        , test "single message with variable placeable" <|
            \_ ->
                Parser.run F.message "test = some { $var } reference"
                    |> Expect.equal
                        (Ok
                            { identifier = F.MessageIdentifier "test"
                            , content = [ F.TextContent "some ", F.PlaceableContent (F.VarRef "var"), F.TextContent " reference" ]
                            }
                        )
        , test "single message with term placeable" <|
            \_ ->
                Parser.run F.message "test = some { -term } reference"
                    |> Expect.equal
                        (Ok
                            { identifier = F.MessageIdentifier "test"
                            , content = [ F.TextContent "some ", F.PlaceableContent (F.TermRef "term"), F.TextContent " reference" ]
                            }
                        )
        , test "multiple placeables" <|
            \_ ->
                Parser.run F.message "bla = { -back-to }{$back}"
                    |> Expect.equal
                        (Ok
                            { identifier = F.MessageIdentifier "bla"
                            , content = [ F.PlaceableContent (F.TermRef "back-to"), F.PlaceableContent (F.VarRef "back") ]
                            }
                        )
        ]
