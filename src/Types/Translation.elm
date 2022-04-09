module Types.Translation exposing (Translation, append, checkTranslationsForConsistency, completeFallback, concat, empty, fromPairs, inferFeaturesTranslations, map)

import Dict exposing (Dict)
import Maybe.Extra
import Set
import Types.Basic exposing (Language)
import Types.Error as Error exposing (Failable)
import Types.Features as Features exposing (Features)
import Types.Segment as Segment exposing (TKey, TValue)


type alias Translation resources =
    { pairs : Translations
    , resources : resources
    , fallback : Maybe Language
    }


type alias Translations =
    Dict TKey TValue


fromPairs : Translations -> Translation ()
fromPairs pairs =
    { pairs = pairs, resources = (), fallback = Nothing }


empty : Translation ()
empty =
    fromPairs Dict.empty


map : (a -> b) -> Translation a -> Translation b
map f { pairs, resources, fallback } =
    { pairs = pairs, resources = f resources, fallback = fallback }


append : Translation any -> Translation any -> Translation any
append first second =
    { pairs = Dict.union first.pairs second.pairs
    , fallback = Maybe.Extra.or first.fallback second.fallback
    , resources = first.resources
    }


concat : List (Translation ()) -> Translation ()
concat =
    List.foldl append { pairs = Dict.empty, fallback = Nothing, resources = () }


inferFeaturesTranslations : Translation () -> Features
inferFeaturesTranslations =
    .pairs >> Dict.values >> Features.combineMap Segment.inferFeatures


completeFallback : (Language -> Maybe (Translation resources)) -> Language -> Translation resources -> Failable (Translation resources)
completeFallback getTranslationForLang language =
    let
        go seenLanguages translation =
            case translation.fallback of
                Just lang ->
                    case ( getTranslationForLang lang, List.member lang seenLanguages ) of
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


checkTranslationsForConsistency : ( Language, Translation () ) -> ( Language, Translation () ) -> Failable ()
checkTranslationsForConsistency ( lang1, t1 ) ( lang2, t2 ) =
    let
        keys1 =
            Dict.keys t1.pairs |> Set.fromList

        keys2 =
            Dict.keys t2.pairs |> Set.fromList

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
