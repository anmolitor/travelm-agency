module Main exposing (main)

import Array
import Dict exposing (Dict)
import Dict.NonEmpty exposing (NonEmpty)
import Elm.Pretty as Pretty
import Generator
import Inline
import Json.Decode as D
import Json.Encode as E
import Placeholder.Internal as Placeholder
import Platform
import Ports exposing (GeneratorMode(..))
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
                    ( { model
                        | state =
                            Dict.insert req.identifier (Dict.NonEmpty.insert req.language req.content state) model.state
                      }
                    , Cmd.none
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
                                        Inline.toFile

                                    Dynamic ->
                                        Generator.toFile
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
