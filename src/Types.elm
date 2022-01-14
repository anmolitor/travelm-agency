module Types exposing
    ( InterpolationKind(..)
    , TKey
    , TSegment(..)
    , TValue
    , Translations
    , concatenateTextSegments
    , getInterpolationVarNames
    , indicifyInterpolations
    , interpolationKindToTypeAnn
    , optimizeJson, isIntlInterpolation
    )

import Array
import Dict exposing (Dict)
import Elm.CodeGen as CG
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
      -- {NUMBER($var, minimumFractionDigits: 2)}
    | FormatNumber String (List ( String, String ))
      -- {DATE($var, hour12: true)}
    | FormatDate String (List ( String, String ))


type TPattern
    = StringPattern String
    | RangePattern Int Int


type InterpolationKind
    = SimpleInterpolation
    | IntlInterpolation CG.TypeAnnotation


interpolationKindToTypeAnn : InterpolationKind -> CG.TypeAnnotation
interpolationKindToTypeAnn kind =
    case kind of
        SimpleInterpolation ->
            CG.stringAnn

        IntlInterpolation ann ->
            ann


isIntlInterpolation : InterpolationKind -> Bool
isIntlInterpolation kind =
    case kind of
        SimpleInterpolation ->
            False

        IntlInterpolation _ ->
            True


classifyInterpolationSegment : TSegment -> Maybe ( String, InterpolationKind )
classifyInterpolationSegment segment =
    case segment of
        Interpolation var ->
            Just ( var, SimpleInterpolation )

        InterpolationCase var _ ->
            Just ( var, SimpleInterpolation )

        FormatNumber var _ ->
            Just ( var, IntlInterpolation CG.floatAnn )

        FormatDate var _ ->
            Just ( var, IntlInterpolation <| CG.fqTyped [ "Time" ] "Posix" [] )

        Text _ ->
            Nothing


{-| Replaces all interpolations with numbers starting from 0.
Interpolations are assigned numbers in alphabetical order.
Multiple interpolations with the same key get the same number.
-}
indicifyInterpolations : TValue -> TValue
indicifyInterpolations =
    let
        sortByInterpolation =
            classifyInterpolationSegment >> Maybe.map Tuple.first >> Maybe.withDefault ""
    in
    List.NonEmpty.indexedMap Tuple.pair
        >> List.NonEmpty.sortBy (Tuple.second >> sortByInterpolation)
        >> (\( first, rest ) ->
                List.foldl
                    (\( i, segment ) ( currentIndex, previousVar, segments ) ->
                        let
                            handleInterpolation var toSegment =
                                if previousVar == Just var then
                                    ( currentIndex, Just var, List.NonEmpty.cons ( i, toSegment <| String.fromInt <| currentIndex - 1 ) segments )

                                else
                                    ( currentIndex + 1, Just var, List.NonEmpty.cons ( i, toSegment <| String.fromInt currentIndex ) segments )
                        in
                        case segment of
                            Interpolation var ->
                                handleInterpolation var Interpolation

                            InterpolationCase var cases ->
                                handleInterpolation var (\v -> InterpolationCase v cases)

                            FormatNumber var args ->
                                handleInterpolation var (\v -> FormatNumber v args)

                            FormatDate var args ->
                                handleInterpolation var (\v -> FormatDate v args)

                            Text _ ->
                                ( currentIndex, previousVar, List.NonEmpty.cons ( i, segment ) segments )
                    )
                    (case first of
                        ( i, Interpolation var ) ->
                            ( 1, Just var, List.NonEmpty.singleton ( i, Interpolation "0" ) )

                        ( i, InterpolationCase var cases ) ->
                            ( 1, Just var, List.NonEmpty.singleton ( i, InterpolationCase "0" cases ) )

                        ( i, FormatNumber var args ) ->
                            ( 1, Just var, List.NonEmpty.singleton ( i, FormatNumber var args ) )

                        ( i, FormatDate var args ) ->
                            ( 1, Just var, List.NonEmpty.singleton ( i, FormatDate var args ) )

                        ( _, Text _ ) ->
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
        wrapVar : String -> String
        wrapVar var =
            "{" ++ var ++ "}"

        optimizeSegments : TValue -> String
        optimizeSegments =
            indicifyInterpolations
                >> List.NonEmpty.map
                    (\segment ->
                        case segment of
                            Text str ->
                                str

                            Interpolation var ->
                                wrapVar var

                            InterpolationCase var _ ->
                                wrapVar var

                            FormatNumber var _ ->
                                wrapVar <| "N" ++ var

                            FormatDate var _ ->
                                wrapVar <| "D" ++ var
                    )
                >> List.NonEmpty.toList
                >> String.join ""
    in
    translations
        |> List.map (Tuple.second >> optimizeSegments)
        |> Array.fromList
        |> E.array E.string


getInterpolationVarNames : NonEmpty TSegment -> Dict String InterpolationKind
getInterpolationVarNames =
    List.NonEmpty.toList
        >> List.filterMap classifyInterpolationSegment
        >> Dict.fromList


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
