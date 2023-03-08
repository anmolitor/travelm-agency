module Types.Error exposing (Failable, addAdditionalCtx, addContentTypeCtx, addLanguageCtx, addTranslationFileNameCtx, combineList, combineNonEmpty, cyclicFallback, cyclicTermReference, failedToFormatStringAsNumber, failedToParseStringAsDate, failedToParseStringAsNumber, formatFail, inconsistentKeys, inconsistentLanguages, joinErr, noTranslationFiles, requestDecodeError, runParser, translationFileParsingError, unresolvableTermReference, unsupportedContentType)

import Dict exposing (Dict)
import Json.Decode
import List.NonEmpty exposing (NonEmpty)
import Parser exposing (Parser)
import Parser.DeadEnds
import Result.Extra
import Types.Basic exposing (Identifier, Language)


type alias Context =
    Dict String String


type alias InconsistentKeysInfo =
    { keys : List String, hasKeys : Language, missesKeys : Language }


type alias InconsistentLanguageInfo =
    { missingLanguages : List String }


type Error
    = NoTranslationFiles
    | CyclicFallback (List String)
    | InconsistentKeys InconsistentKeysInfo
    | InconsistentLanguages InconsistentLanguageInfo
    | CannotFormatStringAsNumber String
    | CannotParseStringAsNumber String
    | CannotParseStringAsDate String
    | CyclicTermReference (List String)
    | UnresolvableTermReference String
    | RequestDecodeError Json.Decode.Error
    | UnsupportedContentType String
    | TranslationFileParsingError String


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


{-| Fail because of inconsistent keys across translation files, i.e. one file contains keys that another does not.
-}
inconsistentLanguages : InconsistentLanguageInfo -> Failable a
inconsistentLanguages =
    errorToFailable << InconsistentLanguages


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


{-| Fail when parsing a non-date string to a date
-}
failedToParseStringAsDate : String -> Failable a
failedToParseStringAsDate =
    errorToFailable << CannotParseStringAsDate


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


{-| Fail because of an unresolvable term reference
-}
requestDecodeError : Json.Decode.Error -> Failable a
requestDecodeError =
    errorToFailable << RequestDecodeError


{-| Fail because of an unknown/unsupported content type
-}
unsupportedContentType : String -> Failable a
unsupportedContentType =
    errorToFailable << UnsupportedContentType


{-| Fail when parsing a translation file
-}
translationFileParsingError : String -> Failable a
translationFileParsingError =
    errorToFailable << TranslationFileParsingError


{-| Run a given `Parser` and let fails be handled by the `Failable` construct.
-}
runParser : Parser a -> String -> Failable a
runParser parser =
    Parser.run parser
        >> Result.mapError Parser.DeadEnds.deadEndsToString
        >> Result.mapError (\err -> [ ( TranslationFileParsingError err, Dict.empty ) ])


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


{-| Traverse through a non-empty list, collecting all errors and failing if one or more of the subcomputations failed.
-}
combineNonEmpty : NonEmpty (Failable a) -> Failable (NonEmpty a)
combineNonEmpty =
    List.NonEmpty.map (Result.map List.NonEmpty.singleton)
        >> List.NonEmpty.foldr1
            (\f1 f2 ->
                case ( f1, f2 ) of
                    ( Ok a1, Ok a2 ) ->
                        Ok <| List.NonEmpty.append a1 a2

                    ( Ok _, Err e2 ) ->
                        Err e2

                    ( Err e1, Ok _ ) ->
                        Err e1

                    ( Err e1, Err e2 ) ->
                        Err (e1 ++ e2)
            )


{-| Flatten a nested failable.
-}
joinErr : Result (Failable a) a -> Failable a
joinErr result =
    case result of
        Ok ok ->
            Ok ok

        Err (Ok ok) ->
            Ok ok

        Err (Err err) ->
            Err err


{-| Format a failure scenario into readable and hopefully understandable output.
-}
formatFail : Failable a -> Result String a
formatFail =
    Result.mapError <| String.join "\n\n" << List.map formatErrorWithContext


formatErrorWithContext : ( Error, Context ) -> String
formatErrorWithContext ( error, context ) =
    let
        ( firstLine, rest ) =
            formatError error

        contextLines =
            if Dict.isEmpty context then
                []

            else
                "Context:" :: (List.map indent <| formatContext context)
    in
    String.join "\n" <| firstLine :: List.map indent (rest ++ contextLines)


formatContext : Context -> List String
formatContext =
    Dict.toList
        >> List.map (\( k, v ) -> "- " ++ k ++ ": " ++ v)


formatError : Error -> NonEmpty String
formatError error =
    case error of
        NoTranslationFiles ->
            ( "Did not receive any translation files yet, cannot finish Elm module.", [] )

        CyclicFallback trace ->
            ( "Detected mutually recursive fallbacks. This is not allowed, because it can lead to indefinite recursion at runtime or unresolved keys."
            , [ "Trace: "
                    ++ String.join " --> " trace
              ]
            )

        InconsistentKeys inconsistency ->
            ( "Found inconsistent keys in translation files."
            , [ "Language '"
                    ++ inconsistency.hasKeys
                    ++ "' includes the keys ["
                    ++ String.join ", " inconsistency.keys
                    ++ " but language '"
                    ++ inconsistency.missesKeys
                    ++ "' does not."
              , "Either delete the keys in '"
                    ++ inconsistency.hasKeys
                    ++ "', add the keys in '"
                    ++ inconsistency.missesKeys
                    ++ "', or opt into a fallback behaviour by adding '# fallback-language: "
                    ++ inconsistency.hasKeys
                    ++ "' into the '"
                    ++ inconsistency.missesKeys
                    ++ "' translation file."
              ]
            )

        InconsistentLanguages inconsistency ->
            ( "Found bundles with inconsistent languages."
            , [ "Bundle does not exist in the following languages ["
                    ++ String.join ", " inconsistency.missingLanguages
              , "You can solve this by creating translation files for the missing languages and adding the required translations."
              ]
            )

        CannotFormatStringAsNumber str ->
            ( "Cannot format the given string '" ++ str ++ "' as a number.", [] )

        CannotParseStringAsNumber str ->
            ( "Cannot parse the given string '" ++ str ++ "' as a number.", [] )

        CannotParseStringAsDate str ->
            ( "Cannot parse the given string '" ++ str ++ "' as a date.", [] )

        CyclicTermReference trace ->
            ( "Detected mutually recursive term references in translation file."
            , [ "Trace: "
                    ++ String.join " --> " trace
              ]
            )

        UnresolvableTermReference termName ->
            ( "Failed to resolve reference to unknown term '" ++ termName ++ "' in translation file.", [] )

        RequestDecodeError decodeError ->
            ( "Failed to decode request: " ++ Json.Decode.errorToString decodeError, [] )

        UnsupportedContentType contentType ->
            ( "Unsupported content type '" ++ contentType ++ "'."
            , [ "Currently .json, .properties and .ftl formats are supported."
              ]
            )

        TranslationFileParsingError msg ->
            ( "Failed to parse translation file: " ++ msg, [] )


indent : String -> String
indent =
    (++) "\t"
