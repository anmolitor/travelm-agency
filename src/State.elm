module State exposing (..)

import Dict exposing (Dict)
import Dict.NonEmpty exposing (NonEmpty)
import List.NonEmpty
import Result.Extra
import Set exposing (Set)
import Types.Basic exposing (Identifier, Language)
import Types.Error as Error exposing (Failable)
import Types.Features as Features exposing (Features)
import Types.InterpolationKind as InterpolationKind exposing (InterpolationKind)
import Types.Segment as Segment exposing (TKey)
import Types.Translation as Translation exposing (Translation)


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


completeFallback : NonEmptyState any -> NonEmptyState any
completeFallback =
    Dict.NonEmpty.map
        (\_ translationSet ->
            let
                getTranslationForLang lang =
                    Dict.NonEmpty.get lang translationSet
            in
            Dict.NonEmpty.map
                (\lang translations ->
                    Translation.completeFallback getTranslationForLang lang translations |> Result.withDefault translations
                )
                translationSet
        )


prefixTranslationsWithIdentifiers : NonEmptyState any -> NonEmptyState any
prefixTranslationsWithIdentifiers =
    Dict.NonEmpty.map prefixTranslationSet


combineTranslationSets : TranslationSet any -> TranslationSet any -> TranslationSet any
combineTranslationSets t =
    Dict.NonEmpty.toList
        >> List.foldl
            (\( lang, new ) acc ->
                let
                    merge val =
                        case val of
                            Just existing ->
                                Translation.append existing new

                            Nothing ->
                                new
                in
                Dict.NonEmpty.update lang merge acc
            )
            t


prefixTranslationSet : String -> TranslationSet any -> TranslationSet any
prefixTranslationSet prefix =
    Dict.NonEmpty.map
        (\_ translations -> Translation.prefix prefix translations)


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
    Dict.NonEmpty.values >> Features.combineMap Translation.inferFeatures


getHtmlIds : NonEmptyState any -> Dict TKey (Set String)
getHtmlIds =
    collectiveTranslationSet
        >> getHtmlIdsForTranslationSet


getHtmlIdsForTranslationSet : TranslationSet any -> Dict TKey (Set String)
getHtmlIdsForTranslationSet =
    Dict.NonEmpty.map (\_ ts -> Dict.map (\_ -> Segment.getHtmlIds) ts.pairs)
        >> Dict.NonEmpty.foldl1
            (mergeDictIntoDict <|
                \key s1 s2 -> Dict.insert key <| Set.union s1 s2
            )


isIntlNeededForKey : TKey -> NonEmptyState () -> Bool
isIntlNeededForKey key =
    collectiveTranslationSet
        >> featureMap
        >> Dict.get key
        >> Maybe.map Features.needsIntl
        >> Maybe.withDefault False


featureMap : TranslationSet any -> Dict TKey Features
featureMap =
    Dict.NonEmpty.map (\_ ts -> Dict.map (\_ -> Segment.inferFeatures) ts.pairs)
        >> Dict.NonEmpty.foldl1
            (mergeDictIntoDict <|
                \key s1 s2 -> Dict.insert key <| Features.union s1 s2
            )


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
                let
                    allLanguages =
                        getAllLanguages nonEmptyState
                in
                if devMode then
                    nonEmptyState
                        |> Dict.NonEmpty.map
                            (\_ translationSet ->
                                case completeFallbacks translationSet of
                                    Ok (first :: rest) ->
                                        Dict.NonEmpty.fromList ( first, rest )

                                    _ ->
                                        translationSet
                            )
                        |> Ok

                else
                    Dict.NonEmpty.toList nonEmptyState
                        |> List.map (\( key, value ) -> ( key, validateTranslationSet allLanguages value |> Error.addTranslationFileNameCtx key ))
                        |> List.map Result.Extra.combineSecond
                        |> Error.combineList
                        |> Result.andThen
                            (\translationSets ->
                                case translationSets of
                                    first :: rest ->
                                        Ok <| Dict.NonEmpty.fromList ( first, rest )

                                    [] ->
                                        Error.noTranslationFiles
                            )
            )
        >> Maybe.withDefault Error.noTranslationFiles


completeFallbacks : TranslationSet any -> Failable (List ( Language, Translation any ))
completeFallbacks translationSet =
    let
        lookupTranslationForLang lang =
            Dict.NonEmpty.get lang translationSet
    in
    Dict.NonEmpty.map (Translation.completeFallback lookupTranslationForLang) translationSet
        |> Dict.NonEmpty.toList
        |> List.map Result.Extra.combineSecond
        |> Error.combineList


validateTranslationSet : Set Language -> TranslationSet any -> Failable (TranslationSet any)
validateTranslationSet allLanguages translationSet =
    let
        languageDiff =
            Set.diff allLanguages (Set.fromList <| Dict.NonEmpty.keys translationSet)
    in
    if not <| Set.isEmpty languageDiff then
        Error.inconsistentLanguages { missingLanguages = Set.toList languageDiff }

    else
        completeFallbacks translationSet
            |> Result.andThen
                (\translations ->
                    List.map2 Translation.checkTranslationsForConsistency translations (List.drop 1 translations)
                        |> Error.combineList
                        |> Result.andThen
                            (always <|
                                case translations of
                                    first :: rest ->
                                        Ok <| Dict.NonEmpty.fromList ( first, rest )

                                    [] ->
                                        Error.noTranslationFiles
                            )
                )


getAllLanguages : NonEmptyState any -> Set Language
getAllLanguages =
    Dict.NonEmpty.values
        >> List.map (Dict.NonEmpty.keys >> Set.fromList)
        >> List.foldl Set.union Set.empty
