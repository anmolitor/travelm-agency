module Main exposing (main)

import Array
import Dict exposing (Dict)
import Dict.NonEmpty exposing (NonEmpty)
import Elm.Pretty as Pretty
import FNV1a
import Generators.DynamicArray
import Generators.Inline
import Generators.Names exposing (defaultNames)
import Json.Decode as D
import Json.Encode as E
import Placeholder.Internal as Placeholder
import Platform
import Ports exposing (GeneratorMode(..))
import Set
import State exposing (State, TranslationSet)
import Types exposing (I18nPairs)
import Util


type alias Flags =
    { version : String }


type alias Model =
    { version : String
    , state : State ()
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
                    case hasSameSignatureAsExistingTranslations req.content state of
                        Nothing ->
                            ( { model
                                | state =
                                    Dict.insert req.identifier (Dict.NonEmpty.insert req.language { pairs = req.content, resources = () } state) model.state
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
                                (Dict.NonEmpty.singleton req.language { pairs = req.content, resources = () })
                                model.state
                      }
                    , Cmd.none
                    )

        GotRequest (Ports.FinishModule req) ->
            ( model
            , onFinishModule model req
            )

        UnexpectedRequest err ->
            ( model, Ports.respond <| Err <| D.errorToString err )


hasSameSignatureAsExistingTranslations : I18nPairs -> TranslationSet () -> Maybe String
hasSameSignatureAsExistingTranslations pairs translationSet =
    let
        ( _, v ) =
            Dict.NonEmpty.getSomeEntry translationSet

        existingKeys =
            List.map Tuple.first v.pairs |> Set.fromList

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


onFinishModule : Model -> Ports.FinishRequest -> Cmd Msg
onFinishModule model { generatorMode, elmModuleName } =
    case Dict.NonEmpty.toNonEmpty model.state of
        Nothing ->
            Ports.respond <| Err "Did not receive any translation files yet, cannot finish Elm module."

        Just nonEmptyState ->
            (Ports.respond << Ok) <|
                let
                    context =
                        { moduleName = Util.moduleName elmModuleName
                        , version = model.version
                        , languages = State.getLanguages nonEmptyState
                        , names = defaultNames
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
                                Dict.NonEmpty.map State.optimizeJsonAllLanguages nonEmptyState
                        in
                        { elmFile = Generators.DynamicArray.toFile context stateWithResources |> Pretty.pretty 120
                        , optimizedJson = Dict.NonEmpty.toDict stateWithResources |> State.getAllResources
                        }


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
