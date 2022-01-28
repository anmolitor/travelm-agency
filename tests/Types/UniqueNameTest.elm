module Types.UniqueNameTest exposing (..)

import Expect exposing (Expectation)
import Fuzz
import Set
import Test exposing (Test, describe, fuzz, fuzz2, test)
import Types.UniqueName as Unique


suite : Test
suite =
    describe "UniqueNameContext"
        [ fuzz Fuzz.string "gives a different name the second time when passing the same name twice" <|
            \name ->
                Unique.new ()
                    |> Unique.andThen name (\_ _ -> identity)
                    |> Unique.andThen name (\_ -> Tuple.pair)
                    |> Unique.unwrap
                    |> (\( name1, name2 ) -> name1 /= name2)
                    |> Expect.true "Expect generated names to be unique"
        , fuzz Fuzz.string "the new name always contains the given suggestion as a prefix" <|
            \name ->
                Unique.new ()
                    |> Unique.andThen name (\_ _ -> identity)
                    |> Unique.unwrap
                    |> expectToStartWith name
        , -- empty list is excluded here
          fuzz2 Fuzz.string (Fuzz.list Fuzz.string) "the lookup function results in a similar name for all passed names" <|
            \firstName otherNames ->
                let
                    names =
                        firstName :: otherNames
                in
                Unique.new ()
                    |> Unique.combineAndThen (always <| Set.fromList names) (\_ _ -> identity)
                    |> Unique.unwrap
                    |> Expect.all (List.map (\name lookup -> lookup name |> expectToStartWith name) names)
        , fuzz Fuzz.string "any invalid lookup results in an error string" <|
            \name ->
                Unique.new ()
                    |> Unique.combineAndThen (always Set.empty) (\_ _ -> identity)
                    |> Unique.unwrap
                    |> ((|>) name >> String.contains "This should not happen")
                    |> Expect.true "Expected error string"
        , test "normal words do not need any modification" <|
            \_ ->
                Unique.new ()
                    |> Unique.andThen "something" (\_ _ -> identity)
                    |> Unique.unwrap
                    |> Expect.equal "something"
        , test "Elm keywords are modified automatically" <|
            \_ ->
                Unique.new ()
                    |> Unique.andThen "type" (\_ _ -> identity)
                    |> Unique.unwrap
                    |> Expect.notEqual "type"
        , fuzz Fuzz.string "Scoping works correctly" <|
            \name ->
                Unique.new ()
                    |> Unique.scoped (Unique.andThen name <| \_ _ -> identity)
                    |> Unique.scoped (Unique.andThen name <| \_ -> Tuple.pair)
                    |> Unique.unwrap
                    |> (\( n1, n2 ) -> n1 == n2)
                    |> Expect.true "The names should be equal because they are in seperate scopes"
        ]


expectToStartWith : String -> String -> Expectation
expectToStartWith prefix str =
    String.startsWith prefix str |> Expect.true ("Expected '" ++ str ++ "' to start with '" ++ prefix ++ "'")
