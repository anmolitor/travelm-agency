module State exposing (..)

import Array
import Dict exposing (Dict)
import Dict.NonEmpty exposing (NonEmpty)
import FNV1a
import Json.Encode as E
import List.NonEmpty
import Placeholder.Internal as Placeholder
import Types exposing (I18nPairs)


type alias Identifier =
    String


type alias Language =
    String


type alias Translation resources =
    { pairs : I18nPairs
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
    Dict.NonEmpty.getSomeEntry
        >> Tuple.second
        >> Dict.NonEmpty.keys


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
                        optimizeJson pairs |> E.encode 0
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


optimizeJson : I18nPairs -> E.Value
optimizeJson =
    Array.fromList
        >> E.array
            (\( _, template ) ->
                template
                    |> Placeholder.mapPlaceholders (\i _ -> String.fromInt i)
                    |> Placeholder.templateToString
                    |> E.string
            )


getAllResources : State resources -> List resources
getAllResources =
    Dict.values >> List.concatMap (Dict.NonEmpty.values >> List.map .resources)
