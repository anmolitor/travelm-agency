module Main exposing (main, tryAddTranslation, tryFinishModule)

import ContentTypes.Fluent
import ContentTypes.Json
import ContentTypes.Properties
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
import Types.Error as Error exposing (Failable)
import Types.Translation exposing (Translation)
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
            onAddTranslation model req

        GotRequest (Ports.FinishModule req) ->
            ( model
            , onFinishModule model req
            )

        UnexpectedRequest err ->
            ( model, Ports.respond <| Error.requestDecodeError err )


onAddTranslation : Model -> Ports.TranslationRequest -> ( Model, Cmd msg )
onAddTranslation model req =
    case tryAddTranslation req model of
        Ok newModel ->
            ( newModel
            , Cmd.none
            )

        Err err ->
            ( model, Ports.respond <| Err err )


tryAddTranslation : Ports.TranslationRequest -> Model -> Error.Failable Model
tryAddTranslation req model =
    parseTranslationContent model.intl req
        |> Result.map (\content -> { model | state = State.addTranslations req.identifier req.language content model.state })


onFinishModule : Model -> Ports.FinishRequest -> Cmd Msg
onFinishModule model req =
    tryFinishModule defaultFileWidth req model |> Ports.respond


defaultFileWidth : number
defaultFileWidth =
    120


tryFinishModule : Int -> Ports.FinishRequest -> Model -> Failable Ports.ResponseContent
tryFinishModule fileWidth { generatorMode, elmModuleName, addContentHash, i18nArgLast } model =
    let
        context =
            { moduleName = Util.moduleName elmModuleName
            , version = model.version
            , names = defaultNames
            , intl = model.intl
            , i18nArgLast = i18nArgLast
            }

        generate validatedState =
            case generatorMode of
                Inline ->
                    { elmFile = Generators.Inline.toFile context validatedState |> Pretty.pretty fileWidth
                    , optimizedJson = []
                    }

                Dynamic ->
                    let
                        stateWithResources =
                            Dict.NonEmpty.map (Generators.Dynamic.optimizeJsonAllLanguages addContentHash) validatedState
                    in
                    { elmFile = Generators.Dynamic.toFile context stateWithResources |> Pretty.pretty fileWidth
                    , optimizedJson = Dict.NonEmpty.toDict stateWithResources |> State.getAllResources
                    }
    in
    State.validateState model.devMode model.state |> Result.map generate


parseTranslationContent : Intl -> Ports.TranslationRequest -> Failable (Translation ())
parseTranslationContent intl { identifier, language, extension, content } =
    (case extension of
        "json" ->
            ContentTypes.Json.parse content

        "properties" ->
            ContentTypes.Properties.parse content

        "ftl" ->
            ContentTypes.Fluent.runFluentParser content |> Result.andThen (ContentTypes.Fluent.fluentToInternalRep intl language)

        _ ->
            Error.unsupportedContentType extension
    )
        |> Error.addTranslationFileNameCtx identifier
        |> Error.addContentTypeCtx extension
        |> Error.addLanguageCtx language


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
        { init = \flags -> ( init flags.version flags.intl flags.devMode, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }
