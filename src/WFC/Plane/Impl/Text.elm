module WFC.Plane.Impl.Text exposing (..)

import Array


import WFC.Vec2 exposing (..)
import WFC.Plane exposing (..)
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


toGrid : Vec2 -> String -> List (List Char)
toGrid (width, height) string =
    let
        splitBy : Int -> String -> List String
        splitBy w src =
            let
                next = src |> String.left w
                left = src |> String.dropLeft w
            in
                if String.length left > 0 then
                    next :: splitBy w left
                else
                    [ next ]
    in
        string
            |> splitBy width
            |> List.map (String.toList)
