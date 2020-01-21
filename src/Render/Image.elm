module Render.Image exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)

import Color exposing (Color)
import Color as Color exposing (..)
import Image exposing (Image)

import Render.Core as Render exposing (..)
import Render.Grid as Render exposing (..)
import Render.Flat as Render exposing (..)

import WFC.Vec2 exposing (..)
import WFC.Matches as Matches exposing (..)
import WFC.Plane exposing (Cell)
import WFC.Plane.Impl.Image exposing (..)
import WFC.Plane.Impl.Tracing exposing (..)


pixel : Color -> Html msg
pixel c =
    span
        [ style "display" "inline-block"
        , style "width" "9px"
        , style "height" "9px"
        , style "background-color" <| Color.toCssString c
        ]
        [ ]


scaled : Float -> Color -> Html msg
scaled scale c =
    span
        [ style "transform" ("scale(" ++ String.fromFloat scale ++ ")")
        ]
        [ pixel c
        ]


spec : Spec Vec2 Color msg
spec =
    { default = Color.rgba 0 0 0 0
    , contradiction = Color.purple
    , a = pixel
    , v = Render.coord
    , scaled = scaled
    , merge = merge
    , vToString = Render.coordText
    }
