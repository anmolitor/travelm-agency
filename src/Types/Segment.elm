module Types.Segment exposing (TKey, TSegment(..), TValue, concatenateTextSegments, inferFeatures, interpolationVars, modifyVars)

import Dict exposing (Dict)
import Elm.CodeGen as CG
import List.NonEmpty exposing (NonEmpty)
import Types.ArgValue exposing (ArgValue)
import Types.Features as Features exposing (Features)
import Types.InterpolationKind as InterpolationKind exposing (InterpolationKind)


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
    | InterpolationCase String TValue (Dict String TValue)
      -- {$var -> case var of [List String TValue]} [TValue]
    | PluralCase String (List ( String, ArgValue )) TValue (Dict String TValue)
      -- {NUMBER($var, minimumFractionDigits: 2)}
    | FormatNumber String (List ( String, ArgValue ))
      -- {DATE($var, hour12: true)}
    | FormatDate String (List ( String, ArgValue ))


{-| Modify the variables inside of a value with the given function.
-}
modifyVars : (String -> String) -> TValue -> TValue
modifyVars modify =
    List.NonEmpty.map <|
        \segment ->
            case segment of
                Text text ->
                    Text text

                Interpolation var ->
                    Interpolation (modify var)

                InterpolationCase var default cases ->
                    InterpolationCase (modify var) (modifyVars modify default) (Dict.map (always <| modifyVars modify) cases)

                PluralCase var opts default cases ->
                    PluralCase (modify var) opts (modifyVars modify default) (Dict.map (always <| modifyVars modify) cases)

                FormatNumber var opts ->
                    FormatNumber (modify var) opts

                FormatDate var opts ->
                    FormatDate (modify var) opts


{-| Concatenate multiple text segments that occur after each other
-}
concatenateTextSegments : TValue -> TValue
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


{-| Determine the interpolation variables and their kind used in a value
-}
interpolationVars : TValue -> Dict String InterpolationKind
interpolationVars =
    List.NonEmpty.toList
        >> List.concatMap classifyInterpolationSegment
        >> Dict.fromList


{-| Infer needed code generation features from the given AST.
-}
inferFeatures : TValue -> Features
inferFeatures val =
    let
        needsIntl =
            List.NonEmpty.toList val
                |> List.concatMap classifyInterpolationSegment
                |> List.any (Tuple.second >> InterpolationKind.isIntlInterpolation)
    in
    (if needsIntl then
        Features.addFeature Features.Intl

     else
        identity
    )
        Features.default


classifyInterpolationSegment : TSegment -> List ( String, InterpolationKind )
classifyInterpolationSegment =
    classifyInterpolationSegmentHelper Dict.empty


classifyInterpolationSegmentHelper : Dict String InterpolationKind -> TSegment -> List ( String, InterpolationKind )
classifyInterpolationSegmentHelper knownInterpolationKinds segment =
    case segment of
        Interpolation var ->
            [ ( var, Dict.get var knownInterpolationKinds |> Maybe.withDefault InterpolationKind.Simple ) ]

        InterpolationCase var default otherOptions ->
            ( var, InterpolationKind.Simple )
                :: (List.NonEmpty.toList default |> List.concatMap (classifyInterpolationSegmentHelper knownInterpolationKinds))
                ++ (Dict.values otherOptions
                        |> List.concatMap List.NonEmpty.toList
                        |> List.concatMap (classifyInterpolationSegmentHelper knownInterpolationKinds)
                   )

        PluralCase var _ default otherOptions ->
            let
                kind =
                    InterpolationKind.Typed
                        { ann = CG.floatAnn
                        , toString = \expr -> CG.apply [ CG.fqFun [ "String" ] "fromFloat", expr ]
                        }

                recurseWithUpdatedKnownInterpolationKinds =
                    Dict.insert var kind knownInterpolationKinds
                        |> classifyInterpolationSegmentHelper
                        |> List.concatMap
            in
            ( var
            , kind
            )
                :: (List.NonEmpty.toList default |> recurseWithUpdatedKnownInterpolationKinds)
                ++ (Dict.values otherOptions |> List.concatMap List.NonEmpty.toList |> recurseWithUpdatedKnownInterpolationKinds)

        FormatNumber var _ ->
            [ ( var
              , InterpolationKind.Typed
                    { ann = CG.floatAnn
                    , toString = \expr -> CG.apply [ CG.fqFun [ "String" ] "fromFloat", expr ]
                    }
              )
            ]

        FormatDate var _ ->
            [ ( var
              , InterpolationKind.Typed
                    { ann = CG.fqTyped [ "Time" ] "Posix" []
                    , toString =
                        \expr ->
                            CG.applyBinOp
                                (CG.fqFun [ "String" ] "fromInt")
                                CG.pipel
                                (CG.apply [ CG.fqFun [ "Time" ] "posixToMillis", expr ])
                    }
              )
            ]

        Text _ ->
            []
