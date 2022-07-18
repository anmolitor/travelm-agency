module InputType exposing (..)


type InputType
    = Json
    | Properties
    | Fluent


toString : InputType -> String
toString inputType =
    case inputType of
        Json ->
            "json"

        Properties ->
            "properties"

        Fluent ->
            "ftl"


fromString : String -> Maybe InputType
fromString str =
    case String.toLower str of
        "json" ->
            Just Json

        "properties" ->
            Just Properties

        "ftl" ->
            Just Fluent

        _ ->
            Nothing
