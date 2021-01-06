module Example.Instance.Text.Plane exposing (..)

import Array exposing (Array)


import Kvant.Vec2 as V2 exposing (..)
import Kvant.Plane exposing (..)
import Kvant.Plane.Flat exposing (..)


type alias TextPlane = Plane Vec2 Char


type alias BoundedString = (Vec2, String)


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


toBoundedString : TextPlane -> BoundedString
toBoundedString (Plane size _ as plane) =
    ( size
    , plane |> toString
    )


toBoundedStringFromGrid : Array (Array Char) -> BoundedString
toBoundedStringFromGrid grid =
    ( V2.loadSize grid |> Maybe.withDefault (0, 0)
    , grid
        |> Array.map Array.toList
        |> Array.toList
        |> List.concat
        |> String.fromList
    )


boundedStringToGrid : BoundedString -> List (List Char)
boundedStringToGrid ((width, height), string) =
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


merge : List Char -> Char
merge chars =
    let
        optionsCount = List.length chars
        maybeFirst = List.head chars
    in
        if optionsCount <= 1 then
            maybeFirst |> Maybe.withDefault 'x'
        else if optionsCount <= 20 then
            Char.fromCode <| 9312 + (optionsCount - 1)
        else Char.fromCode <| 9398 + optionsCount
