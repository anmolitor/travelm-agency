module State exposing (..)

import Dict exposing (Dict)
import Dict.NonEmpty exposing (NonEmpty)
import List.NonEmpty
import Set
import Types.Features as Features exposing (Features)
import Types.InterpolationKind as InterpolationKind exposing (InterpolationKind)
import Types.Segment as Segment exposing (TKey, TValue)


type alias Identifier =
    String


type alias Language =
    String


type alias Translations =
    Dict TKey TValue


type alias Translation resources =
    { pairs : Translations
    , resources : resources
    , fallback : Maybe Language
    }


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
                                { pairs = Dict.union existing.pairs new.pairs
                                , resources = existing.resources
                                , fallback = existing.fallback
                                }

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


addTranslations : Bool -> Identifier -> Language -> Translation () -> State () -> Result String (State ())
addTranslations devMode identifier language translations state =
    let
        insert tset =
            Dict.insert identifier tset state
    in
    case Dict.get identifier state of
        Just translationSet ->
            case translations.fallback of
                Just _ ->
                    insert (Dict.NonEmpty.insert language translations translationSet)
                        |> Ok

                Nothing ->
                    case ( devMode, hasSameSignatureAsExistingTranslations translations.pairs translationSet ) of
                        ( False, Just err ) ->
                            Err err

                        _ ->
                            insert (Dict.NonEmpty.insert language translations translationSet)
                                |> Ok

        Nothing ->
            insert
                (Dict.NonEmpty.singleton language translations)
                |> Ok


hasSameSignatureAsExistingTranslations : Translations -> TranslationSet () -> Maybe String
hasSameSignatureAsExistingTranslations pairs translationSet =
    let
        ( _, v ) =
            Dict.NonEmpty.getFirstEntry translationSet

        existingKeys =
            Dict.keys v.pairs |> Set.fromList

        keysOfNewLanguage =
            Dict.keys pairs |> Set.fromList

        missingKeysInNewLanguage =
            Set.diff existingKeys keysOfNewLanguage

        extraKeysInNewLanguage =
            Set.diff keysOfNewLanguage existingKeys
    in
    if Set.isEmpty missingKeysInNewLanguage then
        if Set.isEmpty extraKeysInNewLanguage then
            Nothing

        else
            Just <| "Found extra keys: " ++ (String.join ", " <| Set.toList extraKeysInNewLanguage) ++ "."

    else
        Just <| "Missing keys: " ++ (String.join ", " <| Set.toList missingKeysInNewLanguage) ++ "."
