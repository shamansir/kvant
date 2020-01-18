module WFC.Plane.Impl.Image exposing (..)


import Array exposing (Array)
import Color exposing (Color)

import WFC.Vec2 exposing (..)
import WFC.Plane exposing (..)
import WFC.Plane.Flat exposing (..)


type alias ImagePlane = Plane Vec2 Color


make : Vec2 -> Array (Array Color) -> ImagePlane
make ( width, height ) pixels =
    Plane
        ( width, height )
        (\(x, y) ->
            if (x < width) && (y < height) then
                pixels
                    |> Array.get y
                    |> Maybe.andThen (Array.get x)
            else Nothing
        )
