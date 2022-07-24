module Accordion exposing (..)

import Dict
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Material.Icons
import Material.Icons.Types exposing (Coloring(..))
import Model exposing (Model)
import Msg exposing (Msg(..))
import Translations exposing (I18n)


onClick : (Int -> msg) -> Html.Attribute msg
onClick listener =
    Html.Events.on "click"
        (Decode.map listener maxHeight)


maxHeight : Decoder Int
maxHeight =
    Decode.at
        [ "currentTarget"
        , "parentElement"
        , "children"
        , "1"
        , "scrollHeight"
        ]
        Decode.int


view :
    { headline : String
    , content : List (Html Msg)
    , id : String
    }
    -> Model
    -> Html Msg
view { headline, content, id } { openAccordionElements } =
    let
        height =
            Dict.get id openAccordionElements |> Maybe.withDefault 0

        isOpen =
            height /= 0

        icon =
            (if isOpen then
                Material.Icons.expand_less

             else
                Material.Icons.expand_more
            )
                50
                Inherit
    in
    Html.div []
        [ Html.h3 [ onClick <| ToggleAccordionElement id, class "accordion-headline" ] [ Html.text headline, icon ]
        , Html.p [ class "accordion-content", style "max-height" <| String.fromInt height ++ "px" ] content
        ]
