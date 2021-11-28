module Main exposing (main)

import Array
import Dict exposing (Dict)
import Elm.Pretty as Pretty
import Generator
import Json.Decode as D
import Json.Encode as E
import Placeholder.Internal as Placeholder
import Platform
import Ports
import Types exposing (I18nPairs)
import Util


type alias Flags =
    { version : String }


type alias Model =
    { version : String
    , state : Dict String State
    }


type alias State =
    { translations : I18nPairs
    , optimizedJson : List ( String, E.Value )
    }


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
                    ( { model
                        | state =
                            Dict.insert req.identifier (addOptimizedJson req state) model.state
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model
                        | state =
                            Dict.insert req.identifier
                                ({ translations = req.content
                                 , optimizedJson = []
                                 }
                                    |> addOptimizedJson req
                                )
                                model.state
                      }
                    , Cmd.none
                    )

        GotRequest (Ports.FinishModule { elmModuleName, identifier }) ->
            ( model
            , case Dict.get identifier model.state of
                Just { translations, optimizedJson } ->
                    Ports.respond <|
                        Ok
                            { elmFile =
                                Generator.toFile
                                    { moduleName = Util.moduleName elmModuleName
                                    , identifier = identifier
                                    , languages = List.map Tuple.first optimizedJson |> List.sort
                                    }
                                    translations
                                    |> Pretty.pretty 120
                            , optimizedJson = List.map (Tuple.mapSecond <| E.encode 0) optimizedJson
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


addOptimizedJson : Ports.TranslationRequest -> State -> State
addOptimizedJson req state =
    { state
        | optimizedJson = ( req.language, optimizeJson req.content ) :: state.optimizedJson
    }


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
