module WFC.Plane.Text exposing (..)

import Array


import WFC.Vec2 exposing (..)
import WFC.Plane.Plane exposing (..)
import WFC.Plane.Flat exposing (..)


type alias TextPlane = Plane Vec2 Char


make : Vec2 -> String -> TextPlane
make ( width, height ) src =
    let
        charArray = String.toList src |> Array.fromList
    in
        Plane
            ( width, height )
            (\(x, y) ->
                if (x < width) && (y < height) then
                    charArray |>
                        Array.get (y * height + x)
                else Nothing
            )


toString : TextPlane -> String
toString plane =
    unpack plane
        |> List.concat
        |> List.filterMap identity
        |> String.fromList
