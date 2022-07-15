module Types.UniqueName exposing (UniqueNameContext, andThen, andThen2, andThen3, andThen4, andThen6, combineAndThen, map, mapWithScope, new, scoped, unwrap, andThen5)

{-| This module provides a monad for unique name generation given a context of already taken names.
Elm keywords are automatically added to the set of taken names.
It will postfix already taken names with underscores ("\_").
-}

import Dict exposing (Dict)
import Set exposing (Set)


type UniqueNameContext a
    = Context (Set String) a


new : a -> UniqueNameContext a
new =
    Context <| Set.fromList elmKeywords


unwrap : UniqueNameContext a -> a
unwrap (Context _ a) =
    a


map : (a -> b) -> UniqueNameContext a -> UniqueNameContext b
map f (Context s a) =
    Context s (f a)


mapWithScope : ((String -> String) -> a -> b) -> UniqueNameContext a -> UniqueNameContext b
mapWithScope f (Context s a) =
    Context s (f (\name -> findUnique name s |> Tuple.first) a)


andThen : String -> (ScopedLookup -> a -> String -> b) -> UniqueNameContext a -> UniqueNameContext b
andThen nameSuggestion doWithName ((Context s _) as ctx) =
    let
        ( uniqueName, newUsedNames ) =
            findUnique nameSuggestion s
    in
    Context newUsedNames (threadCtx ctx doWithName uniqueName)


threadCtx : UniqueNameContext a -> (ScopedLookup -> a -> b) -> b
threadCtx (Context s a) f =
    f (\name -> findUnique name s |> Tuple.first) a


andThen2 : String -> String -> (ScopedLookup -> a -> String -> String -> b) -> UniqueNameContext a -> UniqueNameContext b
andThen2 n1 n2 doWithName ctx =
    andThen n1 doWithName ctx |> andThen n2 (always (<|))


andThen3 : String -> String -> String -> (ScopedLookup -> a -> String -> String -> String -> b) -> UniqueNameContext a -> UniqueNameContext b
andThen3 n1 n2 n3 doWithName ctx =
    andThen2 n1 n2 doWithName ctx |> andThen n3 (always (<|))


andThen4 : String -> String -> String -> String -> (ScopedLookup -> a -> String -> String -> String -> String -> b) -> UniqueNameContext a -> UniqueNameContext b
andThen4 n1 n2 n3 n4 doWithName ctx =
    andThen3 n1 n2 n3 doWithName ctx |> andThen n4 (always (<|))


andThen5 :
    String
    -> String
    -> String
    -> String
    -> String
    -> (ScopedLookup -> a -> String -> String -> String -> String -> String -> b)
    -> UniqueNameContext a
    -> UniqueNameContext b
andThen5 n1 n2 n3 n4 n5 doWithName ctx =
    andThen4 n1 n2 n3 n4 doWithName ctx |> andThen n5 (always (<|))


andThen6 :
    String
    -> String
    -> String
    -> String
    -> String
    -> String
    -> (ScopedLookup -> a -> String -> String -> String -> String -> String -> String -> b)
    -> UniqueNameContext a
    -> UniqueNameContext b
andThen6 n1 n2 n3 n4 n5 n6 doWithName ctx =
    andThen5 n1 n2 n3 n4 n5 doWithName ctx |> andThen n6 (always (<|))


type alias ScopedLookup =
    String -> String


{-| Run unique name generation inside of a scope. This means that the names already in the set are relevant for the generation,
but the generated names inside of the scope do not leak out. So if I open two scopes and want a unique name for "test" in each scope,
I will get "test" in both scopes. Without the scope, the latter one would be "test\_" for uniqueness reasons.
-}
scoped : (UniqueNameContext a -> UniqueNameContext b) -> UniqueNameContext a -> UniqueNameContext b
scoped doWithContext ((Context s _) as ctx) =
    doWithContext ctx |> unwrap |> Context s


{-| Get a set of unique names for a set of suggestions. The new names are then given as a lookup function.

    combineAndThen (\_ -> Set.fromList [ "hi", "type" ]) (\lookupName a ->
      let
        newNameForHi = lookupName "hi"
        newNameForType = lookupName "type"
      in
      -- do something with the new names
    )

-}
combineAndThen : (a -> Set String) -> (ScopedLookup -> a -> (String -> String) -> b) -> UniqueNameContext a -> UniqueNameContext b
combineAndThen nameSuggestions doWithNames ((Context s a) as ctx) =
    let
        ( lookupDict, newUsedNames ) =
            findUniqueSet (nameSuggestions a) s

        lookup name =
            Dict.get name lookupDict
                |> Maybe.withDefault ("Failed to lookup name '" ++ name ++ "'. This should not happen! Open an issue at https://github.com/andreasewering/travelm-agency/issues please, so this will not happen again.")
    in
    Context newUsedNames (threadCtx ctx doWithNames lookup)


findUniqueSet : Set String -> Set String -> ( Dict String String, Set String )
findUniqueSet suggestions usedNamesStart =
    Set.foldl
        (\suggestion ( dict, usedNames ) ->
            let
                ( uniqueName, newUsedNames ) =
                    findUnique suggestion usedNames
            in
            ( Dict.insert suggestion uniqueName dict, newUsedNames )
        )
        ( Dict.empty, usedNamesStart )
        suggestions


findUnique : String -> Set String -> ( String, Set String )
findUnique suggestion usedNames =
    if Set.member suggestion usedNames then
        findUnique (suggestion ++ "_") usedNames

    else
        ( suggestion, Set.insert suggestion usedNames )


elmKeywords : List String
elmKeywords =
    [ "type"
    , "alias"
    , "port"
    , "if"
    , "then"
    , "else"
    , "case"
    , "of"
    , "let"
    , "in"
    , "infix"
    , "module"
    , "import"
    , "exposing"
    , "as"
    , "where"
    ]
