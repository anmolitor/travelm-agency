module Dict.NonEmpty exposing (..)

import Dict exposing (Dict)
import List.NonEmpty as List


type alias NonEmpty k v =
    ( ( k, v ), Dict k v )


singleton : k -> v -> NonEmpty k v
singleton k v =
    ( ( k, v ), Dict.empty )


getSomeEntry : NonEmpty k v -> ( k, v )
getSomeEntry =
    Tuple.first


keys : NonEmpty k v -> List k
keys ( ( k, _ ), rest ) =
    k :: Dict.keys rest


insert : comparable -> v -> NonEmpty comparable v -> NonEmpty comparable v
insert k v ( ( firstKey, firstValue ), rest ) =
    if k == firstKey then
        ( ( firstKey, v ), rest )

    else
        ( ( firstKey, firstValue ), Dict.insert k v rest )


toNonEmptyList : NonEmpty k v -> List.NonEmpty ( k, v )
toNonEmptyList ( firstPair, rest ) =
    ( firstPair, Dict.toList rest )


toList : NonEmpty k v -> List ( k, v )
toList =
    toNonEmptyList >> List.toList


map : (k -> u -> v) -> NonEmpty k u -> NonEmpty k v
map f ( ( k, v ), rest ) =
    ( ( k, f k v ), Dict.map f rest )
