module Types.TranslationTest exposing (..)

import Dict
import Expect
import Fuzz
import Test exposing (..)
import Types.Error as Error
import Types.Segment exposing (TSegment(..))
import Types.Translation exposing (checkTranslationsForConsistency)


suite : Test
suite =
    describe "Translation"
        [ test "append lets the first translation win" <|
            \_ ->
                Types.Translation.append { pairs = Dict.empty, resources = 1, fallback = Just "first" }
                    { pairs = Dict.empty, resources = 2, fallback = Just "second" }
                    |> Expect.equal { pairs = Dict.empty, resources = 1, fallback = Just "first" }
        , test "append merges the translation pairs" <|
            \_ ->
                Types.Translation.append
                    (Types.Translation.fromPairs
                        [ ( "a", ( Text "1", [] ) )
                        , ( "b", ( Text "2", [] ) )
                        ]
                    )
                    (Types.Translation.fromPairs
                        [ ( "a", ( Text "3", [] ) )
                        , ( "c", ( Text "4", [] ) )
                        ]
                    )
                    |> Expect.equal
                        (Types.Translation.fromPairs
                            [ ( "a", ( Text "1", [] ) )
                            , ( "b", ( Text "2", [] ) )
                            , ( "c", ( Text "4", [] ) )
                            ]
                        )
        , describe "fallback" <|
            let
                fallbackLanguage =
                    Types.Translation.fromPairs
                        [ ( "a", ( Text "1", [] ) )
                        , ( "b", ( Text "2", [] ) )
                        ]

                fallbackLanguageName =
                    "fallback-en"

                languageMissingKey =
                    { pairs =
                        Dict.fromList
                            [ ( "a", ( Text "3", [] ) )
                            , ( "c", ( Text "4", [] ) )
                            ]
                    , fallback = Just fallbackLanguageName
                    , resources = ()
                    }

                getTranslationForLang lang =
                    if lang == fallbackLanguageName then
                        Just fallbackLanguage

                    else
                        Nothing
            in
            [ test "fallback onto another language adds the missing pairs to the current language" <|
                \_ ->
                    Types.Translation.completeFallback getTranslationForLang "de" languageMissingKey
                        |> Expect.equal
                            (Ok <|
                                { languageMissingKey
                                    | pairs =
                                        Dict.fromList
                                            [ ( "a", ( Text "3", [] ) )
                                            , ( "b", ( Text "2", [] ) )
                                            , ( "c", ( Text "4", [] ) )
                                            ]
                                }
                            )
            , test "fallback onto another language fails on mutual recursion" <|
                \_ ->
                    Types.Translation.completeFallback getTranslationForLang fallbackLanguageName { fallbackLanguage | fallback = Just fallbackLanguageName }
                        |> Expect.equal (Error.cyclicFallback [ fallbackLanguageName, fallbackLanguageName ])
            , test "translations stay the same if fallback could not be found" <|
                \_ ->
                    Types.Translation.completeFallback (always Nothing) "de" languageMissingKey
                        |> Expect.equal (Ok languageMissingKey)
            , test "translations stay the same if no fallback is configured" <|
                \_ ->
                    Types.Translation.completeFallback getTranslationForLang "de" { languageMissingKey | fallback = Nothing }
                        |> Expect.equal (Ok { languageMissingKey | fallback = Nothing })
            ]
        , describe "consistency check" <|
            let
                missingC =
                    Types.Translation.fromPairs
                        [ ( "a", ( Text "1", [] ) )
                        , ( "b", ( Text "2", [] ) )
                        ]

                missingAB =
                    Types.Translation.fromPairs
                        [ ( "c", ( Text "2", [] ) )
                        ]

                completeKeys =
                    Types.Translation.append missingC missingAB
            in
            [ fuzz (Fuzz.list Fuzz.string) "succeeds for two translations with the same keys" <|
                \keys ->
                    let
                        translations =
                            keys
                                |> List.map (\key -> ( key, ( Text "a", [] ) ))
                                |> Types.Translation.fromPairs
                    in
                    checkTranslationsForConsistency ( "langA", translations ) ( "langB", translations )
                        |> Expect.ok
            , test "fails when a key misses from the second translation" <|
                \_ ->
                    checkTranslationsForConsistency ( "langA", missingAB ) ( "langB", missingC )
                        |> Expect.equal
                            (Error.inconsistentKeys
                                { missesKeys = "langB", hasKeys = "langA", keys = [ "c" ] }
                            )
            , test "fails when a key misses from the first translation" <|
                \_ ->
                    checkTranslationsForConsistency ( "langA", missingAB ) ( "langB", completeKeys )
                        |> Expect.equal
                            (Error.inconsistentKeys
                                { missesKeys = "langA", hasKeys = "langB", keys = [ "a", "b" ] }
                            )
            ]
        ]
