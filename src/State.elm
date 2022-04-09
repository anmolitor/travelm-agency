module State exposing (..)

import Dict exposing (Dict)
import Dict.NonEmpty exposing (NonEmpty)
import List.NonEmpty
import Maybe.Extra
import Result.Extra
import Set
import Types.Basic exposing (Identifier, Language)
import Types.Error as Error exposing (Failable)
import Types.Features as Features exposing (Features)
import Types.InterpolationKind as InterpolationKind exposing (InterpolationKind)
import Types.Segment as Segment exposing (TKey)
import Types.Translation exposing (Translation)


type alias OptimizedJson =
    { filename : String
    , content : String
    }


type alias TranslationSet resources =
    NonEmpty Language (Translation resources)


type alias State resources =
    Dict Identifier (TranslationSet resources)


type alias NonEmptyState resources =
    NonEmpty Identifier (TranslationSet resources)


getLanguages : NonEmptyState resources -> List Language
getLanguages =
    Dict.NonEmpty.getFirstEntry
        >> Tuple.second
        >> Dict.NonEmpty.keys
        >> List.sort


collectiveTranslationSet : NonEmptyState any -> TranslationSet any
collectiveTranslationSet =
    Dict.NonEmpty.toNonEmptyList
        >> List.NonEmpty.map Tuple.second
        >> List.NonEmpty.foldl1 combineTranslationSets


combineTranslationSets : TranslationSet any -> TranslationSet any -> TranslationSet any
combineTranslationSets t =
    Dict.NonEmpty.toList
        >> List.foldl
            (\( lang, new ) acc ->
                let
                    merge val =
                        case val of
                            Just existing ->
                                Types.Translation.append existing new

                            Nothing ->
                                new
                in
                Dict.NonEmpty.update lang merge acc
            )
            t


getAllResources : State resources -> List resources
getAllResources =
    Dict.values >> List.concatMap (Dict.NonEmpty.values >> List.map .resources)


interpolationMap : TranslationSet any -> Dict TKey (Dict String InterpolationKind)
interpolationMap =
    Dict.NonEmpty.map (\_ ts -> Dict.map (\_ -> Segment.interpolationVars) ts.pairs)
        >> Dict.NonEmpty.foldl1
            (mergeDictIntoDict <|
                \key s1 s2 -> Dict.insert key <| mergeInterpolationKinds s1 s2
            )


mergeInterpolationKinds : Dict String InterpolationKind -> Dict String InterpolationKind -> Dict String InterpolationKind
mergeInterpolationKinds =
    mergeDictIntoDict <|
        \key i1 i2 ->
            case i1 of
                InterpolationKind.Simple ->
                    Dict.insert key i2

                InterpolationKind.Typed _ ->
                    Dict.insert key i1


mergeDictIntoDict :
    (comparable -> v -> v -> Dict comparable v -> Dict comparable v)
    -> Dict comparable v
    -> Dict comparable v
    -> Dict comparable v
mergeDictIntoDict f d1 d2 =
    Dict.merge Dict.insert f Dict.insert d1 d2 Dict.empty


inferFeatures : NonEmptyState any -> Features
inferFeatures =
    Dict.NonEmpty.values >> Features.combineMap inferFeaturesTranslationSet


inferFeaturesTranslationSet : TranslationSet any -> Features
inferFeaturesTranslationSet =
    Dict.NonEmpty.values >> Features.combineMap Types.Translation.inferFeatures


isIntlNeededForKey : TKey -> NonEmptyState () -> Bool
isIntlNeededForKey key =
    collectiveTranslationSet
        >> interpolationMap
        >> Dict.get key
        >> Maybe.withDefault Dict.empty
        >> Dict.toList
        >> List.any (Tuple.second >> InterpolationKind.isIntlInterpolation)


allTranslationKeys : NonEmptyState any -> List TKey
allTranslationKeys =
    collectiveTranslationSet >> interpolationMap >> Dict.keys


addTranslations : Identifier -> Language -> Translation () -> State () -> State ()
addTranslations identifier language translations state =
    let
        insert tset =
            Dict.insert identifier tset state
    in
    case Dict.get identifier state of
        Just translationSet ->
            insert (Dict.NonEmpty.insert language translations translationSet)

        Nothing ->
            insert (Dict.NonEmpty.singleton language translations)


validateState : Bool -> State any -> Failable (NonEmptyState any)
validateState devMode =
    Dict.NonEmpty.fromDict
        >> Maybe.map
            (\nonEmptyState ->
                if devMode then
                    Ok nonEmptyState

                else
                    Dict.NonEmpty.toList nonEmptyState
                        |> List.map (\( key, value ) -> validateTranslationSet value |> Error.addTranslationFileNameCtx key)
                        |> Error.combineList
                        |> Result.map (always nonEmptyState)
            )
        >> Maybe.withDefault Error.noTranslationFiles


validateTranslationSet : TranslationSet any -> Failable ()
validateTranslationSet translationSet =
    let
        lookupTranslationForLang lang =
            Dict.NonEmpty.get lang translationSet
    in
    Dict.NonEmpty.map (Types.Translation.completeFallback lookupTranslationForLang) translationSet
        |> Dict.NonEmpty.toList
        |> List.map Result.Extra.combineSecond
        |> Error.combineList
        |> Result.andThen
            (\translations ->
                List.map2 Types.Translation.checkTranslationsForConsistency translations (List.drop 1 translations)
                    |> Error.combineList
                    |> Result.map (always ())
            )
