module Types.Segment exposing (TKey, TSegment(..), TValue, concatenateTextSegments, getHtmlIds, htmlIdsForSegment, inferFeatures, interpolationVars, modifyHtmlIds, modifyVars)

import Dict exposing (Dict)
import Elm.CodeGen as CG
import List.NonEmpty exposing (NonEmpty)
import Set exposing (Set)
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
      -- <a href="attr" value="{$blub}">
    | Html { tag : String, id : String, attrs : List ( String, TValue ), content : TValue }


{-| Modify the variables inside of a value with the given function.
-}
modifyVars : (String -> String) -> TValue -> TValue
modifyVars modify =
    List.NonEmpty.map (modifySegment modify)


modifyHtmlIds : (String -> String) -> TValue -> TValue
modifyHtmlIds modify =
    List.NonEmpty.map (modifySegmentHtmlId modify)


modifySegmentHtmlId : (String -> String) -> TSegment -> TSegment
modifySegmentHtmlId modify segment =
    case segment of
        Html html ->
            Html
                { html
                    | id = modify html.id
                    , attrs = List.map (Tuple.mapSecond <| modifyHtmlIds modify) html.attrs
                    , content = modifyHtmlIds modify html.content
                }

        InterpolationCase var default cases ->
            InterpolationCase var (modifyHtmlIds modify default) (Dict.map (always <| modifyHtmlIds modify) cases)

        PluralCase var opts default cases ->
            PluralCase var opts (modifyHtmlIds modify default) (Dict.map (always <| modifyHtmlIds modify) cases)

        FormatNumber _ _ ->
            segment

        FormatDate _ _ ->
            segment

        Interpolation _ ->
            segment

        Text _ ->
            segment


modifySegment : (String -> String) -> TSegment -> TSegment
modifySegment modify segment =
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

        Html { tag, id, attrs, content } ->
            Html
                { tag = tag
                , id = id
                , attrs = List.map (Tuple.mapSecond <| modifyVars modify) attrs
                , content = modifyVars modify content
                }


{-| Concatenate multiple text segments that occur after each other
-}
concatenateTextSegments : TValue -> TValue
concatenateTextSegments ( first, rest ) =
    List.foldl
        (\segment (( mostRecentSeg, otherSegs ) as segs) ->
            case ( segment, mostRecentSeg ) of
                ( Text t1, Text t2 ) ->
                    ( Text (t2 ++ t1), otherSegs )

                ( Text "", _ ) ->
                    segs

                ( _, Text "" ) ->
                    ( segment, otherSegs )

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


inferFeaturesForSegment : TSegment -> Features
inferFeaturesForSegment seg =
    case seg of
        Text _ ->
            Features.default

        Interpolation _ ->
            Features.singleton Features.Interpolation

        InterpolationCase _ default cases ->
            Features.combine <|
                Features.singleton Features.CaseInterpolation
                    :: inferFeatures default
                    :: (Dict.values cases |> List.map inferFeatures)

        FormatDate _ _ ->
            Features.singleton Features.IntlDate

        FormatNumber _ _ ->
            Features.singleton Features.IntlNumber

        PluralCase _ _ default cases ->
            Features.combine <|
                Features.singleton Features.IntlPlural
                    :: inferFeatures default
                    :: (Dict.values cases |> List.map inferFeatures)

        Html html ->
            Features.combine <|
                Features.singleton Features.Html
                    :: inferFeatures html.content
                    :: List.map (Tuple.second >> inferFeatures) html.attrs


{-| Infer needed code generation features from the given AST.
-}
inferFeatures : TValue -> Features
inferFeatures val =
    List.NonEmpty.toList val
        |> Features.combineMap inferFeaturesForSegment


getHtmlIds : TValue -> Set String
getHtmlIds =
    List.NonEmpty.toList >> List.map htmlIdsForSegment >> List.foldl Set.union Set.empty


htmlIdsForSegment : TSegment -> Set String
htmlIdsForSegment seg =
    case seg of
        Html html ->
            Set.insert html.id <| getHtmlIds html.content

        InterpolationCase _ default cases ->
            List.NonEmpty.fromCons (getHtmlIds default)
                (List.map getHtmlIds <| Dict.values cases)
                |> List.NonEmpty.foldl1 Set.union

        PluralCase _ _ default cases ->
            List.NonEmpty.fromCons (getHtmlIds default)
                (List.map getHtmlIds <| Dict.values cases)
                |> List.NonEmpty.foldl1 Set.union

        _ ->
            Set.empty


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

        Html { attrs, content } ->
            (List.concatMap (Tuple.second >> List.NonEmpty.toList) attrs
                |> List.concatMap (classifyInterpolationSegmentHelper knownInterpolationKinds)
            )
                ++ List.concatMap (classifyInterpolationSegmentHelper knownInterpolationKinds) (List.NonEmpty.toList content)
