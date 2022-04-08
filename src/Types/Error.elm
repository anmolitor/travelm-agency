module Types.Error exposing (Failable, addAdditionalCtx, addContentTypeCtx, addLanguageCtx, addTranslationFileNameCtx, combineList, cyclicFallback, cyclicTermReference, failedToFormatStringAsNumber, failedToParseStringAsNumber, formatFail, inconsistentKeys, noTranslationFiles, unresolvableTermReference)

import Dict exposing (Dict)
import Json.Encode
import Result.Extra
import Types.Basic exposing (Identifier, Language)


type alias Context =
    Dict String String


type alias InconsistentKeysInfo =
    { keys : List String, hasKeys : Language, missesKeys : Language }


type Error
    = NoTranslationFiles
    | CyclicFallback (List String)
    | InconsistentKeys InconsistentKeysInfo
    | CannotFormatStringAsNumber String
    | CannotParseStringAsNumber String
    | CyclicTermReference (List String)
    | UnresolvableTermReference String


{-| A type that represents a potentially "failed" computation. As this is a simple type alias for a specific `Result`,
functions from `Result.Extra` can be used to guide this through tuples, lists etc.
This module exports a few additional convenience functions.
-}
type alias Failable a =
    Result (List ( Error, Context )) a


{-| Fail because we are missing translation files to do work on.
-}
noTranslationFiles : Failable a
noTranslationFiles =
    errorToFailable NoTranslationFiles


{-| Fail because of mutually recursive fallbacks. The list of strings should provide a proof of the cyclic dependency.
-}
cyclicFallback : List String -> Failable a
cyclicFallback =
    errorToFailable << CyclicFallback


{-| Fail because of inconsistent keys across translation files, i.e. one file contains keys that another does not.
-}
inconsistentKeys : InconsistentKeysInfo -> Failable a
inconsistentKeys =
    errorToFailable << InconsistentKeys


{-| Fail when formatting a non-number string as a number.
-}
failedToFormatStringAsNumber : String -> Failable a
failedToFormatStringAsNumber =
    errorToFailable << CannotFormatStringAsNumber


{-| Fail when parsing a non-number string to a number.
-}
failedToParseStringAsNumber : String -> Failable a
failedToParseStringAsNumber =
    errorToFailable << CannotParseStringAsNumber


{-| Fail because of a cyclic term reference.
-}
cyclicTermReference : List String -> Failable a
cyclicTermReference =
    errorToFailable << CyclicTermReference


{-| Fail because of an unresolvable term reference
-}
unresolvableTermReference : String -> Failable a
unresolvableTermReference =
    errorToFailable << UnresolvableTermReference


errorToFailable : Error -> Failable a
errorToFailable error =
    Err [ ( error, Dict.empty ) ]


{-| Add the language of the currently worked on translation file to the internal context to improve error reporting.
-}
addLanguageCtx : Language -> Failable a -> Failable a
addLanguageCtx =
    addToCtx "language"


{-| Add the content type of the currently worked on translation file (i.e. ".json") to the internal context to improve error reporting.
-}
addContentTypeCtx : String -> Failable a -> Failable a
addContentTypeCtx =
    addToCtx "fileExtension"


{-| Add the name of the currently worked on translation file (i.e. "translations") to the internal context to improve error reporting.
-}
addTranslationFileNameCtx : Identifier -> Failable a -> Failable a
addTranslationFileNameCtx =
    addToCtx "translationFileName"


{-| Add additional contextual information that does not belong in one of the
categories covered by the other context-related functions.
-}
addAdditionalCtx : String -> Failable a -> Failable a
addAdditionalCtx info =
    Result.mapError
        (List.map <|
            Tuple.mapSecond <|
                Dict.update "info" <|
                    \val ->
                        case val of
                            Just existingInfo ->
                                Just <| existingInfo ++ "; " ++ info

                            Nothing ->
                                Just info
        )


addToCtx : String -> String -> Failable a -> Failable a
addToCtx k v =
    Result.mapError (List.map <| Tuple.mapSecond <| Dict.insert k v)


{-| Traverse through a list, collecting all errors and failing if one or more of the subcomputations failed.
-}
combineList : List (Failable a) -> Failable (List a)
combineList =
    Result.Extra.partition
        >> (\( successes, errors ) ->
                case errors of
                    [] ->
                        Ok successes

                    _ ->
                        Err (List.concat errors)
           )


{-| Format a failure scenario into readable and hopefully understandable output.
-}
formatFail : Failable a -> Result String a
formatFail =
    Result.mapError <| String.join "\n\n" << List.map formatErrorWithContext


formatErrorWithContext : ( Error, Context ) -> String
formatErrorWithContext ( error, context ) =
    formatError error
        ++ "\n\tContext: "
        ++ (Json.Encode.dict identity Json.Encode.string context
                |> Json.Encode.encode 2
           )


formatError : Error -> String
formatError error =
    case error of
        NoTranslationFiles ->
            "Did not receive any translation files yet, cannot finish Elm module."

        CyclicFallback trace ->
            "Detected mutually recursive fallbacks. This is not allowed, because it can lead to indefinite recursion at runtime or unresolved keys.\n\tTrace: "
                ++ String.join " --> " trace

        InconsistentKeys inconsistency ->
            "Found inconsistent keys in translation files.\n\tLanguage '"
                ++ inconsistency.hasKeys
                ++ "' includes the keys ["
                ++ String.join ", " inconsistency.keys
                ++ " but language '"
                ++ inconsistency.missesKeys
                ++ "' does not.\n\tEither delete the keys in '"
                ++ inconsistency.hasKeys
                ++ "', add the keys in '"
                ++ inconsistency.missesKeys
                ++ "', or opt into a fallback behaviour by adding '# fallback-language: "
                ++ inconsistency.hasKeys
                ++ "' into the '"
                ++ inconsistency.missesKeys
                ++ "' translation file."

        CannotFormatStringAsNumber str ->
            "Cannot format the given string '" ++ str ++ "' as a number."

        CannotParseStringAsNumber str ->
            "Cannot parse the given string '" ++ str ++ "' as a number."

        CyclicTermReference trace ->
            "Detected mutually recursive term references in translation file.\n\tTrace: "
                ++ String.join " --> " trace

        UnresolvableTermReference termName ->
            "Failed to resolve reference to unknown term '" ++ termName ++ "' in translation file."
