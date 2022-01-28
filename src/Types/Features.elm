module Types.Features exposing (Feature(..), Features, addFeature, default, isActive, combineMap, combine)

{-| Conditionals that change the output of the code generator

    Intl : Need dependency on the intl-proxy package. Allows for usage of the Browsers Intl API in generated code.

    Debug : Change default values to show errors when they happen.

-}

import List.Extra


type Feature
    = Intl
    | Debug


type Features
    = Features (List Feature)


default : Features
default =
    Features []


addFeature : Feature -> Features -> Features
addFeature feature (Features features) =
    Features (feature :: features)


isActive : Feature -> Features -> Bool
isActive feature (Features features) =
    List.member feature features


combineMap : (a -> Features) -> List a -> Features
combineMap f =
    List.map f >> combine


combine : List Features -> Features
combine =
    List.concatMap unwrap >> List.Extra.unique >> Features


unwrap : Features -> List Feature
unwrap (Features features) =
    features
