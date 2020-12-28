module Example.Instance.Image.Render exposing (..)


import Color exposing (Color)
import Image exposing (Image, dimensions)
import Image.Color as ImageC exposing (fromList2d)

import Html exposing (..)
import Html.Attributes exposing (..)

import Kvant.Vec2 exposing (..)
import Kvant.Plane.Flat as Plane exposing (unpack)

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
    { source = drawImage
    , plane =
        Plane.unpack
            >> List.map (List.map <| Maybe.withDefault Color.purple)
            >> ImageC.fromList2d
            >> drawImage
    , tracingPlane = always <| div [] []
    , tracingCell = always <| div [] []
    }


drawImage : Image -> Html msg
drawImage srcImage =
    img
        [ src <| Image.toPngUrl srcImage
        , style "min-width" "50px"
        , style "min-height" "50px"
        , style "image-rendering" "pixelated"
        ]
        []
