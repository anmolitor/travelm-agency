module State exposing (..)

import Dict exposing (Dict)
import Dict.NonEmpty exposing (NonEmpty)
import List.NonEmpty
import Types exposing (I18nPairs)


type alias Identifier =
    String


type alias Language =
    String


type alias TranslationSet =
    NonEmpty Language I18nPairs


type alias State =
    Dict Identifier TranslationSet


type alias NonEmptyState =
    NonEmpty Identifier TranslationSet


getLanguages : NonEmptyState -> List Language
getLanguages =
    Dict.NonEmpty.getSomeEntry
        >> Tuple.second
        >> Dict.NonEmpty.keys


collectiveTranslationSet : NonEmptyState -> TranslationSet
collectiveTranslationSet =
    Dict.NonEmpty.toNonEmptyList
        >> List.NonEmpty.map Tuple.second
        >> List.NonEmpty.foldl1 combineTranslationSets


combineTranslationSets : TranslationSet -> TranslationSet -> TranslationSet
combineTranslationSets t =
    Dict.NonEmpty.toList
        >> List.foldl
            (\( lang, pairs ) acc ->
                let
                    merge val =
                        case val of
                            Just existingPairs ->
                                pairs ++ existingPairs

                            Nothing ->
                                pairs
                in
                Dict.NonEmpty.update lang merge acc
            )
            t
