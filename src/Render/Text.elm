module Render.Text exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)

import Render.Core as Render exposing (..)
import Render.Grid as Render exposing (..)
import Render.Flat as Render exposing (..)

import WFC.Vec2 exposing (..)
import WFC.Matches as Matches exposing (..)
import WFC.Plane exposing (Cell)
import WFC.Plane.Impl.Text exposing (..)
import WFC.Plane.Impl.Tracing exposing (..)


char : Char -> Html msg
char c =
    span
        [ style "display" "inline-block"
        , style "width" "9px"
        , style "background-color" <| symbolBg c
        , style "padding" "2px 8px"
        , style "color" <|
            if symbolBg c == "black" then
                "rgba(255,255,255,0.3)"
            else
                "rgba(0,0,0,0.3)"
        ]
        [ text <| String.fromChar c ]


symbolBg : Char -> String
symbolBg symbol =
    case symbol of
        '0' -> "white"
        '1' -> "black"
        '2' -> "red"
        '3' -> "aqua"
        '4' -> "blue"
        '5' -> "green"
        '6' -> "teal"
        '7' -> "maroon"
        '8' -> "#85C1E9"
        '9' -> "#5D6D7E"
        'A' -> "#5DADE2"
        'B' -> "#F9E79F"
        'C' -> "#F4D03F"
        'D' -> "#E74C3C"
        'E' -> "#BFC9CA"
        'F' -> "#2E86C1"
        'x' -> "salmon"
        _ -> "lightgray"


scaled : Float -> Char -> Html msg
scaled scale c =
    span
        [ style "transform" ("scale(" ++ String.fromFloat scale ++ ")")
        , style "width" "10px"
        , style "height" "10px" ]
        [ text <| String.fromChar c ]


spec : Spec Vec2 Char msg
spec =
    { default = '?'
    , contradiction = '∅'
    , a = char
    , v = Render.coord
    , scaled = scaled
    , vToString = Render.coordText
    }
