module Dict.NonEmpty exposing (..)

import Dict exposing (Dict)
import List.NonEmpty


type alias NonEmpty k v =
    ( ( k, v ), Dict k v )


singleton : k -> v -> NonEmpty k v
singleton k v =
    ( ( k, v ), Dict.empty )


getFirstEntry : NonEmpty comparable v -> ( comparable, v )
getFirstEntry =
    toNonEmptyList >> List.NonEmpty.sortBy Tuple.first >> List.NonEmpty.head


keys : NonEmpty k v -> List k
keys ( ( k, _ ), rest ) =
    k :: Dict.keys rest


values : NonEmpty k v -> List v
values ( ( _, v ), rest ) =
    v :: Dict.values rest


toNonEmpty : Dict comparable v -> Maybe (NonEmpty comparable v)
toNonEmpty =
    Dict.toList
        >> List.NonEmpty.fromList
        >> Maybe.map fromList


toDict : NonEmpty comparable v -> Dict comparable v
toDict ( ( k, v ), rest ) =
    Dict.insert k v rest


fromList : List.NonEmpty.NonEmpty ( comparable, v ) -> NonEmpty comparable v
fromList ( head, tail ) =
    ( head, Dict.fromList tail )


insert : comparable -> v -> NonEmpty comparable v -> NonEmpty comparable v
insert k v ( ( firstKey, firstValue ), rest ) =
    if k == firstKey then
        ( ( firstKey, v ), rest )

    else
        ( ( firstKey, firstValue ), Dict.insert k v rest )


update : comparable -> (Maybe v -> v) -> NonEmpty comparable v -> NonEmpty comparable v
update k alter ( ( firstKey, firstValue ), rest ) =
    if k == firstKey then
        ( ( firstKey, alter <| Just firstValue ), rest )

    else
        ( ( firstKey, firstValue ), Dict.update k (alter >> Just) rest )


toNonEmptyList : NonEmpty k v -> List.NonEmpty.NonEmpty ( k, v )
toNonEmptyList ( firstPair, rest ) =
    ( firstPair, Dict.toList rest )


toList : NonEmpty k v -> List ( k, v )
toList =
    toNonEmptyList >> List.NonEmpty.toList


map : (k -> u -> v) -> NonEmpty k u -> NonEmpty k v
map f ( ( k, v ), rest ) =
    ( ( k, f k v ), Dict.map f rest )
