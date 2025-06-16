module Util.CustomHtmlAttributes exposing (Attribute, attribute, unpackAttribute)

import Html
import Html.Attributes


type Attribute msg
    = CustomAttribute (Html.Attribute msg)


attribute : String -> String -> Attribute msg
attribute name value =
    Html.Attributes.attribute name value |> CustomAttribute


unpackAttribute : Attribute msg -> Html.Attribute msg
unpackAttribute (CustomAttribute attr) =
    attr
