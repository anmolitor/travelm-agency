module Dict.NonEmpty exposing
    ( NonEmpty
    , foldl
    , foldl1
    , fromDict
    , fromList
    , get
    , getFirstEntry
    , insert
    , keys
    , map
    , singleton
    , toDict
    , toList
    , toNonEmptyList
    , update
    , values
    )

import Dict exposing (Dict)
import List.NonEmpty


type NonEmpty k v
    = NonEmpty ( ( k, v ), Dict k v )


singleton : k -> v -> NonEmpty k v
singleton k v =
    NonEmpty ( ( k, v ), Dict.empty )


fromDict : Dict comparable v -> Maybe (NonEmpty comparable v)
fromDict =
    Dict.toList >> List.NonEmpty.fromList >> Maybe.map fromList


get : comparable -> NonEmpty comparable v -> Maybe v
get k (NonEmpty ( ( firstKey, firstValue ), rest )) =
    if k == firstKey then
        Just firstValue

    else
        Dict.get k rest


getFirstEntry : NonEmpty comparable v -> ( comparable, v )
getFirstEntry =
    toNonEmptyList >> List.NonEmpty.sortBy Tuple.first >> List.NonEmpty.head


keys : NonEmpty k v -> List k
keys (NonEmpty ( ( k, _ ), rest )) =
    k :: Dict.keys rest


values : NonEmpty k v -> List v
values (NonEmpty ( ( _, v ), rest )) =
    v :: Dict.values rest


toDict : NonEmpty comparable v -> Dict comparable v
toDict (NonEmpty ( ( k, v ), rest )) =
    Dict.insert k v rest


fromList : List.NonEmpty.NonEmpty ( comparable, v ) -> NonEmpty comparable v
fromList ( ( firstK, firstV ), tail ) =
    List.foldl (\( k, v ) -> insert k v) (singleton firstK firstV) tail


insert : comparable -> v -> NonEmpty comparable v -> NonEmpty comparable v
insert k v (NonEmpty ( ( firstKey, firstValue ), rest )) =
    if k == firstKey then
        NonEmpty ( ( firstKey, v ), rest )

    else
        NonEmpty ( ( firstKey, firstValue ), Dict.insert k v rest )


update : comparable -> (Maybe v -> v) -> NonEmpty comparable v -> NonEmpty comparable v
update k alter (NonEmpty ( ( firstKey, firstValue ), rest )) =
    if k == firstKey then
        NonEmpty ( ( firstKey, alter <| Just firstValue ), rest )

    else
        NonEmpty ( ( firstKey, firstValue ), Dict.update k (alter >> Just) rest )


toNonEmptyList : NonEmpty k v -> List.NonEmpty.NonEmpty ( k, v )
toNonEmptyList (NonEmpty ( firstPair, rest )) =
    ( firstPair, Dict.toList rest )


toList : NonEmpty k v -> List ( k, v )
toList =
    toNonEmptyList >> List.NonEmpty.toList


map : (k -> u -> v) -> NonEmpty k u -> NonEmpty k v
map f (NonEmpty ( ( k, v ), rest )) =
    NonEmpty ( ( k, f k v ), Dict.map f rest )


foldl : (k -> v -> b -> b) -> b -> NonEmpty k v -> b
foldl f acc (NonEmpty ( ( k, v ), rest )) =
    Dict.foldl f (f k v acc) rest


foldl1 : (v -> v -> v) -> NonEmpty k v -> v
foldl1 f (NonEmpty ( ( _, v ), rest )) =
    Dict.foldl (always f) v rest
