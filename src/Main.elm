module Main exposing (main)

import Dict
import Dict.NonEmpty
import Elm.Pretty as Pretty
import Generators.Dynamic
import Generators.Inline
import Generators.Names exposing (defaultNames)
import Intl exposing (Intl)
import Json.Decode as D
import Platform
import Ports exposing (GeneratorMode(..))
import State exposing (State)
import Types.Features exposing (Feature(..))
import Util


type alias Flags =
    { version : String, intl : Intl, devMode : Bool }


type alias Model =
    { version : String
    , state : State ()
    , intl : Intl
    , devMode : Bool
    }


init : String -> Intl -> Bool -> Model
init version intl devMode =
    { version = version
    , state = Dict.empty
    , intl = intl
    , devMode = devMode
    }


type Msg
    = GotRequest Ports.Request
    | UnexpectedRequest D.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRequest (Ports.AddTranslation req) ->
            ( { model | state = State.addTranslations req.identifier req.language req.content model.state }
            , Cmd.none
            )

        GotRequest (Ports.FinishModule req) ->
            ( model
            , onFinishModule model req
            )

        UnexpectedRequest err ->
            ( model, Ports.respond <| Err <| D.errorToString err )


onFinishModule : Model -> Ports.FinishRequest -> Cmd Msg
onFinishModule model { generatorMode, elmModuleName, addContentHash, i18nArgLast } =
    case Dict.NonEmpty.fromDict model.state of
        Nothing ->
            Ports.respond <| Err "Did not receive any translation files yet, cannot finish Elm module."

        Just nonEmptyState ->
            (Ports.respond << Ok) <|
                let
                    context =
                        { moduleName = Util.moduleName elmModuleName
                        , version = model.version
                        , names = defaultNames
                        , intl = model.intl
                        , i18nArgLast = i18nArgLast
                        }
                in
                case generatorMode of
                    Inline ->
                        { elmFile = Generators.Inline.toFile context nonEmptyState |> Pretty.pretty 120
                        , optimizedJson = []
                        }

                    Dynamic ->
                        let
                            stateWithResources =
                                Dict.NonEmpty.map (Generators.Dynamic.optimizeJsonAllLanguages addContentHash) nonEmptyState
                        in
                        { elmFile = Generators.Dynamic.toFile context stateWithResources |> Pretty.pretty 120
                        , optimizedJson = Dict.NonEmpty.toDict stateWithResources |> State.getAllResources
                        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.subToRequests model.intl <|
        \result ->
            case result of
                Ok req ->
                    GotRequest req

                Err err ->
                    UnexpectedRequest err


main : Program Flags Model Msg
main =
    Platform.worker
        { init = \flags -> ( init flags.version flags.intl flags.devMode, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }
