module ContentTypes.FluentTest exposing (..)

import ContentTypes.Fluent as F
import Dict
import Expect
import Parser
import State
import Test exposing (Test, describe, test)
import Types.Segment exposing (TSegment(..))
import Util exposing (emptyIntl)


parserTests : Test
parserTests =
    describe "Fluent Parser"
        [ -- Message
          test "single message" <|
            \_ ->
                Parser.run F.message "test-identifier = some text"
                    |> Expect.equal (Ok { identifier = F.MessageIdentifier "test-identifier", content = ( F.TextContent "some text", [] ), attrs = [] })
        , test "single term" <|
            \_ ->
                Parser.run F.message "-test-term = some text"
                    |> Expect.equal (Ok { identifier = F.TermIdentifier "test-term", content = ( F.TextContent "some text", [] ), attrs = [] })
        , test "single message with variable placeable" <|
            \_ ->
                Parser.run F.message "test = some { $var } reference"
                    |> Expect.equal
                        (Ok
                            { identifier = F.MessageIdentifier "test"
                            , content = ( F.TextContent "some ", [ F.PlaceableContent (F.VarRef "var"), F.TextContent " reference" ] )
                            , attrs = []
                            }
                        )
        , test "single message with term placeable" <|
            \_ ->
                Parser.run F.message "test = some { -term } reference"
                    |> Expect.equal
                        (Ok
                            { identifier = F.MessageIdentifier "test"
                            , content = ( F.TextContent "some ", [ F.PlaceableContent (F.TermRef "term" []), F.TextContent " reference" ] )
                            , attrs = []
                            }
                        )
        , test "multiple placeables" <|
            \_ ->
                Parser.run F.message "bla = { -back-to }{$back}"
                    |> Expect.equal
                        (Ok
                            { identifier = F.MessageIdentifier "bla"
                            , content = ( F.PlaceableContent (F.TermRef "back-to" []), [ F.PlaceableContent (F.VarRef "back") ] )
                            , attrs = []
                            }
                        )
        , test "single message with string literal" <|
            \_ ->
                Parser.run F.message """test = lit: { "st\\"\\\\ \\u1234 abc" }"""
                    |> Expect.equal
                        (Ok
                            { identifier = F.MessageIdentifier "test"
                            , content = ( F.TextContent "lit: ", [ F.PlaceableContent (F.StringLit "st\"\\ ሴ abc") ] )
                            , attrs = []
                            }
                        )
        , test "two line message" <|
            \_ ->
                Parser.run F.message """test = a message on
                    multiple lines."""
                    |> Expect.equal
                        (Ok { identifier = F.MessageIdentifier "test", content = ( F.TextContent """a message on
multiple lines.""", [] ), attrs = [] })
        , test "two lines but seperate messages" <|
            \_ ->
                Parser.run F.message """test = a message on
just = kidding"""
                    |> Expect.equal
                        (Ok { identifier = F.MessageIdentifier "test", content = ( F.TextContent "a message on", [] ), attrs = [] })
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
  indents""", [] ), attrs = [] })
        , test "multiline message with placeable" <|
            \_ ->
                Parser.run F.message """test = a message on
                    multiple {-lines}
                with different
                  indents"""
                    |> Expect.equal
                        (Ok { identifier = F.MessageIdentifier "test", content = ( F.TextContent """a message on
    multiple """, [ F.PlaceableContent (F.TermRef "lines" []), F.TextContent """
with different
  indents""" ] ), attrs = [] })
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
                                , [ F.PlaceableContent (F.TermRef "multiple" [])
                                  , F.TextContent " "
                                  , F.PlaceableContent (F.TermRef "lines" [])
                                  , F.TextContent "\n"
                                  , F.PlaceableContent (F.TermRef "with" [])
                                  , F.TextContent " different\n  indents"
                                  ]
                                )
                            , attrs = []
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
                            , attrs = []
                            }
                        )
        , test "calling term with arguments" <|
            \_ ->
                Parser.run F.message "msg = { -term(arg: \"hello\")}{-term( bla: 69)}"
                    |> Expect.equal
                        (Ok <|
                            { identifier = F.MessageIdentifier "msg"
                            , content =
                                ( F.PlaceableContent (F.TermRef "term" [ ( "arg", F.StringLiteral "hello" ) ])
                                , [ F.PlaceableContent (F.TermRef "term" [ ( "bla", F.NumberLiteral 69 ) ]) ]
                                )
                            , attrs = []
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
                            [ F.noAttrs { identifier = F.MessageIdentifier "msg1", content = ( F.TextContent "A simple text", [] ) }
                            , F.noAttrs { identifier = F.MessageIdentifier "msg2", content = ( F.TextContent "Another text", [] ) }
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
                            [ F.noAttrs { identifier = F.MessageIdentifier "msg1", content = ( F.TextContent "A simple\nmultiline text", [] ) }
                            , F.noAttrs { identifier = F.MessageIdentifier "msg2", content = ( F.TextContent "Another\ntext", [] ) }
                            ]
                        )
        , test "message with attribute" <|
            \_ ->
                Parser.run F.ast """
msg = some text
    .title = text title"""
                    |> Expect.equal
                        (Ok
                            [ F.MessageResource
                                { identifier = F.MessageIdentifier "msg"
                                , content = ( F.TextContent "some text", [] )
                                , attrs =
                                    [ { identifier = "title", content = ( F.TextContent "text title", [] ) }
                                    ]
                                }
                            ]
                        )
        , test "message with multiline attribute" <|
            \_ ->
                Parser.run F.ast """
msg = some multiline
    text
 .title = multiline
   title"""
                    |> Expect.equal
                        (Ok
                            [ F.MessageResource
                                { identifier = F.MessageIdentifier "msg"
                                , content = ( F.TextContent "some multiline\ntext", [] )
                                , attrs =
                                    [ { identifier = "title", content = ( F.TextContent "multiline\ntitle", [] ) }
                                    ]
                                }
                            ]
                        )
        , test "message with multiple attributes" <|
            \_ ->
                Parser.run F.ast """
msg = some text
 .title = multiline
   title
 .body = somebody
    once
 told {$person}"""
                    |> Expect.equal
                        (Ok
                            [ F.MessageResource
                                { identifier = F.MessageIdentifier "msg"
                                , content = ( F.TextContent "some text", [] )
                                , attrs =
                                    [ { identifier = "title", content = ( F.TextContent "multiline\ntitle", [] ) }
                                    , { identifier = "body", content = ( F.TextContent "somebody\n   once\ntold ", [ F.PlaceableContent (F.VarRef "person") ] ) }
                                    ]
                                }
                            ]
                        )
        ]


toInternalRepConverterTests : Test
toInternalRepConverterTests =
    describe "Fluent to internal representation converter"
        [ test "single message" <|
            \_ ->
                [ F.noAttrs { identifier = F.MessageIdentifier "msg", content = ( F.TextContent "some text", [] ) } ]
                    |> F.fluentToInternalRep emptyIntl "en"
                    |> Expect.equal (Ok <| State.fromTranslations <| Dict.fromList [ ( "msg", ( Text "some text", [] ) ) ])
        , test "single message with interpolation" <|
            \_ ->
                [ F.noAttrs { identifier = F.MessageIdentifier "msg", content = ( F.TextContent "some ", [ F.PlaceableContent (F.VarRef "name") ] ) } ]
                    |> F.fluentToInternalRep emptyIntl "en"
                    |> Expect.equal (Ok <| State.fromTranslations <| Dict.fromList [ ( "msg", ( Text "some ", [ Interpolation "name" ] ) ) ])
        , test "single message with term reference" <|
            \_ ->
                [ F.noAttrs { identifier = F.TermIdentifier "term", content = ( F.TextContent "World", [] ) }
                , F.noAttrs { identifier = F.MessageIdentifier "msg", content = ( F.TextContent "Hello ", [ F.PlaceableContent (F.TermRef "term" []) ] ) }
                ]
                    |> F.fluentToInternalRep emptyIntl "en"
                    |> Expect.equal (Ok <| State.fromTranslations <| Dict.fromList [ ( "msg", ( Text "Hello World", [] ) ) ])
        , test "errors out on term recursion" <|
            \_ ->
                [ F.noAttrs { identifier = F.TermIdentifier "term1", content = ( F.PlaceableContent (F.TermRef "term2" []), [] ) }
                , F.noAttrs { identifier = F.TermIdentifier "term2", content = ( F.PlaceableContent (F.TermRef "term3" []), [] ) }
                , F.noAttrs { identifier = F.TermIdentifier "term3", content = ( F.PlaceableContent (F.TermRef "term1" []), [] ) }
                , F.noAttrs { identifier = F.MessageIdentifier "msg", content = ( F.PlaceableContent (F.TermRef "term1" []), [] ) }
                ]
                    |> F.fluentToInternalRep emptyIntl "en"
                    |> Expect.equal (Err "Recursive term reference term1 <- term3 <- term2 <- term1")
        , test "term ref with arguments gets inlined" <|
            \_ ->
                [ F.noAttrs
                    { identifier = F.TermIdentifier "term"
                    , content =
                        ( F.TextContent "hi "
                        , [ F.PlaceableContent (F.VarRef "arg") ]
                        )
                    }
                , F.noAttrs
                    { identifier = F.TermIdentifier "term2"
                    , content =
                        ( F.TextContent "Cost: "
                        , [ F.PlaceableContent (F.VarRef "num") ]
                        )
                    }
                , F.noAttrs
                    { identifier = F.MessageIdentifier "msg"
                    , content =
                        ( F.TextContent "text: "
                        , [ F.PlaceableContent (F.TermRef "term" [ ( "arg", F.StringLiteral "world" ) ])
                          , F.PlaceableContent (F.TermRef "term2" [ ( "num", F.NumberLiteral 420.69 ) ])
                          ]
                        )
                    }
                ]
                    |> F.fluentToInternalRep emptyIntl "en"
                    |> Expect.equal (Ok <| State.fromTranslations <| Dict.fromList [ ( "msg", ( Text "text: hi worldCost: 420.69", [] ) ) ])
        , test "attributes get converted to extra keys" <|
            \_ ->
                [ F.MessageResource
                    { identifier = F.MessageIdentifier "msg"
                    , content = ( F.TextContent "some text", [] )
                    , attrs =
                        [ { identifier = "title", content = ( F.TextContent "multiline\ntitle", [] ) }
                        , { identifier = "body", content = ( F.TextContent "somebody\n   once\ntold ", [ F.PlaceableContent (F.VarRef "person") ] ) }
                        ]
                    }
                ]
                    |> F.fluentToInternalRep emptyIntl "en"
                    |> Expect.equal
                        (Ok <|
                            State.fromTranslations <|
                                Dict.fromList
                                    [ ( "msg", ( Text "some text", [] ) )
                                    , ( "msgTitle", ( Text "multiline\ntitle", [] ) )
                                    , ( "msgBody", ( Text "somebody\n   once\ntold ", [ Interpolation "person" ] ) )
                                    ]
                        )
        ]
