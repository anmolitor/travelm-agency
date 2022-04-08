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
import Types.Segment as Segment exposing (TKey, TValue)


type alias Translations =
    Dict TKey TValue


type alias Translation resources =
    { pairs : Translations
    , resources : resources
    , fallback : Maybe Language
    }


fromTranslations : Translations -> Translation ()
fromTranslations pairs =
    { pairs = pairs, resources = (), fallback = Nothing }


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


combineTranslations : Translation any -> Translation any -> Translation any
combineTranslations first second =
    { pairs = Dict.union first.pairs second.pairs
    , fallback = Maybe.Extra.or first.fallback second.fallback
    , resources = first.resources
    }


foldTranslations : List (Translation ()) -> Translation ()
foldTranslations =
    List.foldl combineTranslations { pairs = Dict.empty, fallback = Nothing, resources = () }


combineTranslationSets : TranslationSet any -> TranslationSet any -> TranslationSet any
combineTranslationSets t =
    Dict.NonEmpty.toList
        >> List.foldl
            (\( lang, new ) acc ->
                let
                    merge val =
                        case val of
                            Just existing ->
                                combineTranslations existing new

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
    Dict.NonEmpty.values >> Features.combineMap (.pairs >> inferFeaturesTranslations)


inferFeaturesTranslations : Translations -> Features
inferFeaturesTranslations =
    Dict.values >> Features.combineMap Segment.inferFeatures


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


validateState : State any -> Failable (NonEmptyState any)
validateState =
    Dict.NonEmpty.fromDict
        >> Maybe.map
            (\nonEmptyState ->
                Dict.NonEmpty.toList nonEmptyState
                    |> List.map (\( key, value ) -> validateTranslationSet value |> Error.addTranslationFileNameCtx key)
                    |> Error.combineList
                    |> Result.map (always nonEmptyState)
            )
        >> Maybe.withDefault Error.noTranslationFiles


validateTranslationSet : TranslationSet any -> Failable ()
validateTranslationSet translationSet =
    Dict.NonEmpty.map (completeFallback translationSet) translationSet
        |> Dict.NonEmpty.toList
        |> List.map Result.Extra.combineSecond
        |> Error.combineList
        |> Result.map (.pairs |> Tuple.mapSecond |> List.map)
        |> Result.andThen
            (\translations ->
                List.map2 checkTranslationsForConsistency translations (List.drop 1 translations)
                    |> Error.combineList
                    |> Result.map (always ())
            )


completeFallback : TranslationSet resources -> Language -> Translation resources -> Failable (Translation resources)
completeFallback translationSet language =
    let
        go seenLanguages translation =
            case translation.fallback of
                Just lang ->
                    case ( Dict.NonEmpty.get lang translationSet, List.member lang seenLanguages ) of
                        ( Just fallbackTranslation, False ) ->
                            let
                                recursiveResult =
                                    go (lang :: seenLanguages) fallbackTranslation
                            in
                            recursiveResult
                                |> Result.map
                                    (\{ pairs } ->
                                        { translation | pairs = Dict.union translation.pairs pairs }
                                    )

                        ( _, True ) ->
                            Error.cyclicFallback (List.reverse <| lang :: seenLanguages)

                        ( Nothing, False ) ->
                            Ok translation

                Nothing ->
                    Ok translation
    in
    go [ language ]


checkTranslationsForConsistency : ( Language, Translations ) -> ( Language, Translations ) -> Failable ()
checkTranslationsForConsistency ( lang1, pairs1 ) ( lang2, pairs2 ) =
    let
        keys1 =
            Dict.keys pairs1 |> Set.fromList

        keys2 =
            Dict.keys pairs2 |> Set.fromList

        missingKeysInLang2 =
            Set.diff keys1 keys2

        extraKeysInLang2 =
            Set.diff keys2 keys1
    in
    if Set.isEmpty missingKeysInLang2 then
        if Set.isEmpty extraKeysInLang2 then
            Ok ()

        else
            Error.inconsistentKeys { keys = Set.toList extraKeysInLang2, missesKeys = lang1, hasKeys = lang2 }

    else
        Error.inconsistentKeys { keys = Set.toList missingKeysInLang2, missesKeys = lang2, hasKeys = lang1 }
