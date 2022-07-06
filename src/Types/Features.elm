module Types.Features exposing (Feature(..), Features, addFeature, combine, combineMap, default, fromList, isActive, isEmpty, needsIntl, singleton, union, oneIsActive)

{-| Conditionals that change the output of the code generator that are inferred by the given translation files

    Any Intl Feature : Need dependency on the intl-proxy package. Allows for usage of the Browsers Intl API in generated code.

    IntlNumber: specifically the ability to format numbers
    IntlDate: specifically the ability to format dates
    IntlPlural: specifically the ability to match on plural cases

    Interpolation: classic string interpolation
    Case Interpolation: match on given string values at run time to decide which path to take

    Html : Will produce Html instead of String as a return value (or Element/Element.WithContext (TODO))

-}

import Set exposing (Set)


type Feature
    = IntlNumber
    | IntlDate
    | IntlPlural
    | CaseInterpolation
    | Interpolation
    | Html


serialize : Feature -> String
serialize feature =
    case feature of
        IntlNumber ->
            "IntlNumber"

        IntlDate ->
            "IntlDate"

        IntlPlural ->
            "IntlPlural"

        CaseInterpolation ->
            "CaseInterpolation"

        Interpolation ->
            "Interpolation"

        Html ->
            "Html"


type Features
    = Features (Set String)


default : Features
default =
    Features Set.empty


singleton : Feature -> Features
singleton =
    serialize >> Set.singleton >> Features


fromList : List Feature -> Features
fromList =
    List.map serialize >> Set.fromList >> Features


isEmpty : Features -> Bool
isEmpty (Features features) =
    Set.isEmpty features


addFeature : Feature -> Features -> Features
addFeature =
    serialize >> Set.insert >> lift


isActive : Feature -> Features -> Bool
isActive feature (Features features) =
    Set.member (serialize feature) features


oneIsActive : List Feature -> Features -> Bool
oneIsActive list features =
    List.any (\feature -> isActive feature features) list


needsIntl : Features -> Bool
needsIntl =
    oneIsActive [ IntlNumber, IntlDate, IntlPlural ]


combineMap : (a -> Features) -> List a -> Features
combineMap f =
    List.map f >> combine


combine : List Features -> Features
combine =
    List.foldl union default


union : Features -> Features -> Features
union (Features features) =
    lift <| Set.union features


lift : (Set String -> Set String) -> Features -> Features
lift f (Features features) =
    Features <| f features
