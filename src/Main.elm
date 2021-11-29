module Main exposing (main)

import Array
import Dict exposing (Dict)
import Dict.NonEmpty exposing (NonEmpty)
import Elm.Pretty as Pretty
import Generators.DynamicArray
import Generators.DynamicElmPH
import Generators.Inline
import Json.Decode as D
import Json.Encode as E
import Placeholder.Internal as Placeholder
import Platform
import Ports exposing (GeneratorMode(..))
import Set
import Types exposing (I18nPairs)
import Util


type alias Flags =
    { version : String }


type alias Model =
    { version : String
    , state : Dict String State
    }


type alias State =
    NonEmpty String I18nPairs


init : String -> Model
init version =
    { version = version
    , state = Dict.empty
    }


type Msg
    = GotRequest Ports.Request
    | UnexpectedRequest D.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRequest (Ports.AddTranslation req) ->
            case Dict.get req.identifier model.state of
                Just state ->
                    case hasSameSignatureAsState req.content state of
                        Nothing ->
                            ( { model
                                | state =
                                    Dict.insert req.identifier (Dict.NonEmpty.insert req.language req.content state) model.state
                              }
                            , Cmd.none
                            )

                        Just errMessage ->
                            ( model
                            , Ports.respond <|
                                Err <|
                                    "Inconsistent keys in translations of identifier '"
                                        ++ req.identifier
                                        ++ "' at language '"
                                        ++ req.language
                                        ++ ". "
                                        ++ errMessage
                            )

                Nothing ->
                    ( { model
                        | state =
                            Dict.insert req.identifier
                                (Dict.NonEmpty.singleton req.language req.content)
                                model.state
                      }
                    , Cmd.none
                    )

        GotRequest (Ports.FinishModule { elmModuleName, identifier, generatorMode }) ->
            ( model
            , case Dict.get identifier model.state of
                Just state ->
                    Ports.respond <|
                        Ok
                            { elmFile =
                                (case generatorMode of
                                    Inline ->
                                        Generators.Inline.toFile

                                    Dynamic ->
                                        Generators.DynamicArray.toFile
                                )
                                    { moduleName = Util.moduleName elmModuleName
                                    , identifier = identifier
                                    }
                                    state
                                    |> Pretty.pretty 120
                            , optimizedJson =
                                case generatorMode of
                                    Inline ->
                                        []

                                    Dynamic ->
                                        optimizeJsonAllLanguages state
                            }

                Nothing ->
                    Ports.respond <|
                        Err <|
                            "Cannot finish module '"
                                ++ elmModuleName
                                ++ "' because the given identifier '"
                                ++ identifier
                                ++ "' has not been associated with any translation files yet."
            )

        UnexpectedRequest err ->
            ( model, Ports.respond <| Err <| D.errorToString err )


optimizeJsonAllLanguages : State -> List ( String, String )
optimizeJsonAllLanguages =
    (Dict.NonEmpty.map <| always (optimizeJson >> E.encode 0))
        >> Dict.NonEmpty.toList


hasSameSignatureAsState : I18nPairs -> State -> Maybe String
hasSameSignatureAsState pairs state =
    let
        ( _, v ) =
            Dict.NonEmpty.getSomeEntry state

        existingKeys =
            List.map Tuple.first v |> Set.fromList

        keysOfNewLanguage =
            List.map Tuple.first pairs |> Set.fromList

        missingKeysInNewLanguage =
            Set.diff existingKeys keysOfNewLanguage

        extraKeysInNewLanguage =
            Set.diff keysOfNewLanguage existingKeys
    in
    if Set.isEmpty missingKeysInNewLanguage then
        if Set.isEmpty extraKeysInNewLanguage then
            Nothing

        else
            Just <| "Found extra keys: " ++ (String.join ", " <| Set.toList extraKeysInNewLanguage) ++ "."

    else
        Just <| "Missing keys: " ++ (String.join ", " <| Set.toList missingKeysInNewLanguage) ++ "."


optimizeJson : I18nPairs -> E.Value
optimizeJson =
    Array.fromList
        >> E.array
            (\( _, template ) ->
                template
                    |> Placeholder.mapPlaceholders (\i _ -> String.fromInt i)
                    |> Placeholder.templateToString
                    |> E.string
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.subToRequests <|
        \result ->
            case result of
                Ok req ->
                    GotRequest req

                Err err ->
                    UnexpectedRequest err


main : Program Flags Model Msg
main =
    Platform.worker
        { init = \flags -> ( init flags.version, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }
