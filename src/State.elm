module State exposing (..)

import Dict exposing (Dict)
import Dict.NonEmpty exposing (NonEmpty)
import FNV1a
import Json.Encode as E
import List.NonEmpty
import Types exposing (InterpolationKind, Translations)


type alias Identifier =
    String


type alias Language =
    String


type alias Translation resources =
    { pairs : Translations
    , resources : resources
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


collectiveTranslationSet : NonEmptyState () -> TranslationSet ()
collectiveTranslationSet =
    Dict.NonEmpty.toNonEmptyList
        >> List.NonEmpty.map Tuple.second
        >> List.NonEmpty.foldl1 combineTranslationSets


combineTranslationSets : TranslationSet () -> TranslationSet () -> TranslationSet ()
combineTranslationSets t =
    Dict.NonEmpty.toList
        >> List.foldl
            (\( lang, { pairs } ) acc ->
                let
                    merge val =
                        case val of
                            Just existing ->
                                { pairs = pairs ++ existing.pairs, resources = () }

                            Nothing ->
                                { pairs = pairs, resources = () }
                in
                Dict.NonEmpty.update lang merge acc
            )
            t


optimizeJsonAllLanguages : Bool -> Identifier -> TranslationSet resources -> TranslationSet OptimizedJson
optimizeJsonAllLanguages addContentHash identifier =
    Dict.NonEmpty.map <|
        \language { pairs } ->
            { pairs = pairs
            , resources =
                let
                    content =
                        Types.optimizeJson pairs |> E.encode 0
                in
                { content = content
                , filename =
                    String.join "." <|
                        List.filter (not << String.isEmpty)
                            [ identifier
                            , language
                            , if addContentHash then
                                FNV1a.hash content |> String.fromInt

                              else
                                ""
                            , "json"
                            ]
                }
            }


getAllResources : State resources -> List resources
getAllResources =
    Dict.values >> List.concatMap (Dict.NonEmpty.values >> List.map .resources)


interpolationMap : TranslationSet any -> Dict Types.TKey (Dict String InterpolationKind)
interpolationMap =
    Dict.NonEmpty.map (\_ ts -> List.map (Tuple.mapSecond Types.getInterpolationVarNames) ts.pairs |> Dict.fromList)
        >> Dict.NonEmpty.foldl1
            (\t1 t2 ->
                Dict.merge
                    Dict.insert
                    (\key s1 s2 -> Dict.insert key <| Dict.union s1 s2)
                    Dict.insert
                    t1
                    t2
                    Dict.empty
            )


stateNeedsIntl : NonEmptyState any -> Bool
stateNeedsIntl =
    Dict.NonEmpty.values >> List.any translationSetNeedsIntl


translationSetNeedsIntl : TranslationSet any -> Bool
translationSetNeedsIntl =
    Dict.NonEmpty.values >> List.any (.pairs >> translationsNeedIntl)


translationsNeedIntl : Types.Translations -> Bool
translationsNeedIntl =
    List.any (Tuple.second >> valNeedsIntl)


valNeedsIntl : Types.TValue -> Bool
valNeedsIntl =
    List.NonEmpty.any segNeedsIntl


segNeedsIntl : Types.TSegment -> Bool
segNeedsIntl seg =
    case seg of
        Types.FormatNumber _ _ ->
            True

        Types.FormatDate _ _ ->
            True

        Types.PluralCase _ _ _ _ ->
            True

        _ ->
            False
