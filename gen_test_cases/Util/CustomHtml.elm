module Util.CustomHtml exposing (Attribute, Html, node, text, unpackHtml)

import Html
import Util.CustomHtmlAttributes as CustomHtmlAttributes exposing (unpackAttribute)


type Html msg
    = CustomHtml (Html.Html msg)


type alias Attribute msg =
    CustomHtmlAttributes.Attribute msg


text : String -> Html msg
text str =
    CustomHtml (Html.text str)


node : String -> List (Attribute msg) -> List (Html msg) -> Html msg
node name attrs children =
    Html.node name (List.map unpackAttribute attrs) (List.map unpackHtml children)
        |> CustomHtml


unpackHtml : Html msg -> Html.Html msg
unpackHtml (CustomHtml html) =
    html
