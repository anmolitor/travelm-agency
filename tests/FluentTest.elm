module FluentTest exposing (..)

import ContentTypes.Fluent as F
import Expect
import Parser
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Fluent Parser"
        [ -- Message
          test "single message" <|
            \_ ->
                Parser.run F.message "test-identifier = some text"
                    |> Expect.equal (Ok { identifier = F.MessageIdentifier "test-identifier", content = ( F.TextContent "some text", [] ) })
        , test "single term" <|
            \_ ->
                Parser.run F.message "-test-term = some text"
                    |> Expect.equal (Ok { identifier = F.TermIdentifier "test-term", content = ( F.TextContent "some text", [] ) })
        , test "single message with variable placeable" <|
            \_ ->
                Parser.run F.message "test = some { $var } reference"
                    |> Expect.equal
                        (Ok
                            { identifier = F.MessageIdentifier "test"
                            , content = ( F.TextContent "some ", [ F.PlaceableContent (F.VarRef "var"), F.TextContent " reference" ] )
                            }
                        )
        , test "single message with term placeable" <|
            \_ ->
                Parser.run F.message "test = some { -term } reference"
                    |> Expect.equal
                        (Ok
                            { identifier = F.MessageIdentifier "test"
                            , content = ( F.TextContent "some ", [ F.PlaceableContent (F.TermRef "term"), F.TextContent " reference" ] )
                            }
                        )
        , test "multiple placeables" <|
            \_ ->
                Parser.run F.message "bla = { -back-to }{$back}"
                    |> Expect.equal
                        (Ok
                            { identifier = F.MessageIdentifier "bla"
                            , content = ( F.PlaceableContent (F.TermRef "back-to"), [ F.PlaceableContent (F.VarRef "back") ] )
                            }
                        )
        , test "single message with string literal" <|
            \_ ->
                Parser.run F.message """test = lit: { "st\\"\\\\ \\u1234 abc" }"""
                    |> Expect.equal
                        (Ok
                            { identifier = F.MessageIdentifier "test"
                            , content = ( F.TextContent "lit: ", [ F.PlaceableContent (F.StringLit "st\"\\ ሴ abc") ] )
                            }
                        )
        , test "two line message" <|
            \_ ->
                Parser.run F.message """test = a message on
                    multiple lines."""
                    |> Expect.equal
                        (Ok { identifier = F.MessageIdentifier "test", content = ( F.TextContent """a message on
multiple lines.""", [] ) })
        , test "two lines but seperate messages" <|
            \_ ->
                Parser.run F.message """test = a message on
just = kidding"""
                    |> Expect.equal
                        (Ok { identifier = F.MessageIdentifier "test", content = ( F.TextContent "a message on", [] ) })
        , test "multiline message" <|
            \_ ->
                Parser.run F.message """test = a message on
                    multiple lines
                with different
                  indents"""
                    |> Expect.equal
                        (Ok { identifier = F.MessageIdentifier "test", content = ( F.TextContent """a message on
    multiple lines
with different
  indents""", [] ) })
        , test "multiline message with placeable" <|
            \_ ->
                Parser.run F.message """test = a message on
                    multiple {-lines}
                with different
                  indents"""
                    |> Expect.equal
                        (Ok { identifier = F.MessageIdentifier "test", content = ( F.TextContent """a message on
    multiple """, [ F.PlaceableContent (F.TermRef "lines"), F.TextContent """
with different
  indents""" ] ) })
        , test "multiline message with multiple placeables at the edges of the lines" <|
            \_ ->
                Parser.run F.message """test = a message on
                {-multiple} {-lines}
                {-with} different
                  indents"""
                    |> Expect.equal
                        (Ok
                            { identifier = F.MessageIdentifier "test"
                            , content =
                                ( F.TextContent "a message on\n"
                                , [ F.PlaceableContent (F.TermRef "multiple")
                                  , F.TextContent " "
                                  , F.PlaceableContent (F.TermRef "lines")
                                  , F.TextContent "\n"
                                  , F.PlaceableContent (F.TermRef "with")
                                  , F.TextContent " different\n  indents"
                                  ]
                                )
                            }
                        )
        , test "multiline message with empty lines" <|
            \_ ->
                Parser.run F.message """test = a message with
   
                blank lines"""
                    |> Expect.equal
                        (Ok
                            { identifier = F.MessageIdentifier "test"
                            , content =
                                ( F.TextContent """a message with

blank lines"""
                                , []
                                )
                            }
                        )

        -- Complete AST
        , test "multiple messages" <|
            \_ ->
                Parser.run F.ast """
msg1 = A simple text
msg2 = Another text
"""
                    |> Expect.equal
                        (Ok
                            [ F.MessageResource { identifier = F.MessageIdentifier "msg1", content = ( F.TextContent "A simple text", [] ) }
                            , F.MessageResource { identifier = F.MessageIdentifier "msg2", content = ( F.TextContent "Another text", [] ) }
                            ]
                        )
        , test "multiple multiline messages" <|
            \_ ->
                Parser.run F.ast """
msg1 = A simple
 multiline text
msg2 = Another
 text
"""
                    |> Expect.equal
                        (Ok
                            [ F.MessageResource { identifier = F.MessageIdentifier "msg1", content = ( F.TextContent "A simple\nmultiline text", [] ) }
                            , F.MessageResource { identifier = F.MessageIdentifier "msg2", content = ( F.TextContent "Another\ntext", [] ) }
                            ]
                        )
        ]
