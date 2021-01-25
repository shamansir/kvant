module Example.Instance.Image.Render exposing (..)


import Array exposing (Array)
import Color exposing (Color)
import Image exposing (Image)
import Image as Image
import Image.Color as ImageC

import Html exposing (..)
import Html.Attributes exposing (..)

import Kvant.Vec2 exposing (..)
import Kvant.Plane as Plane

import Example.Render exposing (Renderer)
import Example.Instance.Image.Plane exposing (merge, pixelToColor)



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


make : Renderer Image Color (Html msg)
make =
    ( drawImage
    , Plane.toList2d
            >> List.map (List.map <| Maybe.withDefault Color.purple)
            >> ImageC.fromList2d
            >> drawImage
        -- Array.repeat 120 (Array.repeat 120 Color.purple)
        --     |> ImageC.fromArray2d
        --     |> drawImage
        --     |> always
    )


drawFromGrid : Array (Array (Array Int)) -> Html msg
drawFromGrid grid =
    grid
        |> Array.map (Array.map (Array.toList >> List.map pixelToColor >> merge))
        |> ImageC.fromArray2d
        |> drawImage


drawImage : Image -> Html msg
drawImage srcImage =
    img
        [ src <| Image.toPngUrl srcImage
        , style "min-width" "100px"
        , style "min-height" "100px"
        , style "image-rendering" "pixelated"
        ]
        []
