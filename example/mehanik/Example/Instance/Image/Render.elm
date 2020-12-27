module Example.Instance.Image.Render exposing (..)


import Color exposing (Color)
import Image exposing (Image, dimensions)

import Html exposing (..)
import Html.Attributes exposing (..)

import Kvant.Vec2 exposing (..)

import Example.Render as Render exposing (..)



{- spec : HtmlSpec Vec2 Char msg
spec =
    { default = '?'
    , contradiction = '∅'
    , a = char
    , v = Render.coord
    , merge = merge
    , scaled = scaled
    , vToString = Render.coordText
    } -}

make : Renderer Vec2 Image Color (Html msg)
make =
    { source = always <| div [] []
    , plane = always <| div [] []
    , tracingPlane = always <| div [] []
    , tracingCell = always <| div [] []
    }
