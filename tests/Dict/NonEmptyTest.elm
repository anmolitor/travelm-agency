module Dict.NonEmptyTest exposing (..)

import Dict exposing (Dict)
import Dict.NonEmpty as DNE
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3)


fuzzDict : Fuzzer comparable -> Fuzzer v -> Fuzzer (Dict comparable v)
fuzzDict fuzzK fuzzV =
    Fuzz.map Dict.fromList
        (Fuzz.list <| Fuzz.pair fuzzK fuzzV)


fuzzNonEmptyDict : Fuzzer comparable -> Fuzzer v -> Fuzzer (DNE.NonEmpty comparable v)
fuzzNonEmptyDict fuzzK fuzzV =
    Fuzz.map3 (\k v lst -> DNE.fromList ( ( k, v ), lst ))
        fuzzK
        fuzzV
        (Fuzz.list <| Fuzz.pair fuzzK fuzzV)


suite : Test
suite =
    describe "NonEmpty Dict"
        [ fuzz2 Fuzz.char Fuzz.int "singleton" <|
            \k v ->
                DNE.singleton k v
                    |> DNE.toDict
                    |> Expect.equalDicts (Dict.singleton k v)
        , fuzz (fuzzDict Fuzz.char Fuzz.int) "converting to and from normal dict works as expected" <|
            \dict ->
                DNE.fromDict dict
                    |> Maybe.map DNE.toDict
                    |> Expect.equal
                        (if Dict.isEmpty dict then
                            Nothing

                         else
                            Just dict
                        )
        , fuzz3 Fuzz.char Fuzz.int (fuzzNonEmptyDict Fuzz.char Fuzz.int) "insertion works just like with a normal dict" <|
            \k v dict ->
                DNE.insert k v dict
                    |> DNE.toDict
                    |> Expect.equalDicts (DNE.toDict dict |> Dict.insert k v)
        , fuzz3 Fuzz.char Fuzz.int (fuzzNonEmptyDict Fuzz.char Fuzz.int) "updating works just like with a normal dict" <|
            \k v dict ->
                let
                    update =
                        Maybe.map ((+) v) >> Maybe.withDefault v
                in
                DNE.update k update dict
                    |> DNE.toDict
                    |> Expect.equalDicts (DNE.toDict dict |> Dict.update k (update >> Just))
        , fuzz (fuzzNonEmptyDict Fuzz.char Fuzz.int) "keys works just like with a normal dict" <|
            \dict ->
                DNE.keys dict
                    |> List.sort
                    |> Expect.equalLists (DNE.toDict dict |> Dict.keys |> List.sort)
        , fuzz (fuzzNonEmptyDict Fuzz.char Fuzz.int) "values works just like with a normal dict" <|
            \dict ->
                DNE.values dict
                    |> List.sort
                    |> Expect.equalLists (DNE.toDict dict |> Dict.values |> List.sort)
        , fuzz (fuzzNonEmptyDict Fuzz.char Fuzz.int) "map works just like with a normal dict" <|
            \dict ->
                let
                    mapFun char int =
                        Char.toCode char + int
                in
                DNE.map mapFun dict
                    |> DNE.toDict
                    |> Expect.equalDicts (DNE.toDict dict |> Dict.map mapFun)
        , fuzz (fuzzNonEmptyDict Fuzz.char Fuzz.int) "get first entry" <|
            \dict ->
                DNE.toList dict
                    |> List.member (DNE.getFirstEntry dict)
                    |> Expect.equal True
                    |> Expect.onFail "Expected first entry to be included in all entries"
        ]
