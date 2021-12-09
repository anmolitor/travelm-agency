module Types exposing
    ( TKey
    , TSegment(..)
    , TValue
    , Translations
    , concatenateTextSegments
    , getInterpolationVarNames
    , indicifyInterpolations
    , optimizeJson
    )

import Array
import Json.Encode as E
import List.NonEmpty exposing (NonEmpty)
import Set exposing (Set)



-- Internal representation all formats get converted to


type alias Translations =
    List ( TKey, TValue )


type alias TValue =
    NonEmpty TSegment


type alias TKey =
    String


type TSegment
    = -- a simple text
      Text String
      -- {$var}
    | Interpolation String
      -- {$var -> case var of [List String TValue]} [TValue]
    | InterpolationCase String (NonEmpty ( TPattern, TValue ))


type TPattern
    = StringPattern String
    | RangePattern Int Int


interpolationVar : TSegment -> Maybe String
interpolationVar segment =
    case segment of
        Interpolation var ->
            Just var

        InterpolationCase var _ ->
            Just var

        _ ->
            Nothing


{-| Replaces all interpolations with numbers starting from 0.
Interpolations are assigned numbers in alphabetical order.
Multiple interpolations with the same key get the same number.
-}
indicifyInterpolations : TValue -> TValue
indicifyInterpolations =
    let
        sortByInterpolation : TSegment -> String
        sortByInterpolation =
            interpolationVar >> Maybe.withDefault ""
    in
    List.NonEmpty.indexedMap Tuple.pair
        >> List.NonEmpty.sortBy (Tuple.second >> sortByInterpolation)
        >> (\( first, rest ) ->
                List.foldl
                    (\( i, segment ) ( currentIndex, previousVar, segments ) ->
                        case segment of
                            Interpolation var ->
                                if previousVar == Just var then
                                    ( currentIndex, Just var, List.NonEmpty.cons ( i, Interpolation <| String.fromInt <| currentIndex - 1 ) segments )

                                else
                                    ( currentIndex + 1, Just var, List.NonEmpty.cons ( i, Interpolation <| String.fromInt currentIndex ) segments )

                            InterpolationCase var cases ->
                                if previousVar == Just var then
                                    ( currentIndex, Just var, List.NonEmpty.cons ( i, InterpolationCase (String.fromInt <| currentIndex - 1) cases ) segments )

                                else
                                    ( currentIndex + 1, Just var, List.NonEmpty.cons ( i, InterpolationCase (String.fromInt currentIndex) cases ) segments )

                            _ ->
                                ( currentIndex, previousVar, List.NonEmpty.cons ( i, segment ) segments )
                    )
                    (case first of
                        ( i, Interpolation var ) ->
                            ( 1, Just var, List.NonEmpty.singleton ( i, Interpolation "0" ) )

                        ( i, InterpolationCase var _ ) ->
                            ( 1, Just var, List.NonEmpty.singleton ( i, Interpolation "0" ) )

                        _ ->
                            ( 0, Nothing, List.NonEmpty.singleton first )
                    )
                    rest
           )
        >> (\( _, _, indicedSegments ) -> indicedSegments)
        >> List.NonEmpty.sortBy Tuple.first
        >> List.NonEmpty.map Tuple.second


optimizeJson : Translations -> E.Value
optimizeJson translations =
    let
        optimizeSegments : TValue -> String
        optimizeSegments =
            indicifyInterpolations
                >> List.NonEmpty.map
                    (\segment ->
                        case segment of
                            Text str ->
                                str

                            Interpolation var ->
                                "{" ++ var ++ "}"

                            InterpolationCase var _ ->
                                "{" ++ var ++ "}"
                    )
                >> List.NonEmpty.toList
                >> String.join ""
    in
    translations
        |> List.map (Tuple.second >> optimizeSegments)
        |> Array.fromList
        |> E.array E.string


getInterpolationVarNames : NonEmpty TSegment -> Set String
getInterpolationVarNames =
    List.NonEmpty.toList
        >> List.filterMap interpolationVar
        >> Set.fromList


{-| Concatenate multiple text segments that occur after each other
-}
concatenateTextSegments : NonEmpty TSegment -> NonEmpty TSegment
concatenateTextSegments ( first, rest ) =
    List.foldl
        (\segment (( mostRecentSeg, otherSegs ) as segs) ->
            case ( segment, mostRecentSeg ) of
                ( Text t1, Text t2 ) ->
                    ( Text (t2 ++ t1), otherSegs )

                _ ->
                    List.NonEmpty.cons segment segs
        )
        (List.NonEmpty.singleton first)
        rest
        |> List.NonEmpty.reverse
