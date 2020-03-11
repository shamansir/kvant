module Kvant.Plane.Impl.Image exposing (..)


import Array exposing (Array)
import Color exposing (Color)
import Color
import Image
import Image exposing (Image)
import Image.Color as ImageC exposing (fromList2d)

import Kvant.Vec2 exposing (..)
import Kvant.Plane exposing (..)
import Kvant.Plane.Flat exposing (..)


type alias ImagePlane = Plane Vec2 Color


transparent : Color
transparent = Color.rgba 0.0 0.0 0.0 0.0


make : Array (Array Color) -> ImagePlane
make pixels =
    makeInBounds
        (loadSize pixels |> Maybe.withDefault (0, 0))
        pixels


makeInBounds : Vec2 -> Array (Array Color) -> ImagePlane
makeInBounds ( width, height ) pixels =
    Plane
        ( width, height )
        (\(x, y) ->
            if (x < width) && (y < height) then
                pixels
                    |> Array.get y
                    |> Maybe.andThen (Array.get x)
            else Nothing
        )


mixHsla : Color -> Color -> Color
mixHsla colorA colorB =
    let
        a = Color.toHsla colorA
        b = Color.toHsla colorB
    in
        Color.fromHsla
            { hue = (a.hue + b.hue) / 2
            , saturation = (a.saturation + b.saturation) / 2
            , lightness = (a.lightness + b.lightness) / 2
            , alpha = (a.alpha + b.alpha) / 2
            }


mixRgb : Color -> Color -> Color
mixRgb colorA colorB =
    let
        a = Color.toRgba colorA
        b = Color.toRgba colorB
    in
        Color.fromRgba
            { red = (a.red + b.red) / 2
            , blue = (a.blue + b.blue) / 2
            , green = (a.green + b.green) / 2
            , alpha = (a.alpha + b.alpha) / 2
            }


merge : List Color -> Color
merge colors =
    let
        optionsCount = List.length colors
        maybeFirst = List.head colors
    in
        if optionsCount <= 0 then Color.purple
        else
            List.foldl
                mixHsla
                (List.head colors |> Maybe.withDefault transparent)
                (List.tail colors |> Maybe.withDefault [])


toGrid : ImagePlane -> List (List Color)
toGrid plane =
    unpack plane
        |> List.map (List.map <| Maybe.withDefault transparent)


toArrayGrid : ImagePlane -> Array (Array Color)
toArrayGrid =
    toGrid >> List.map Array.fromList >> Array.fromList


fromImage : Image -> ImagePlane
fromImage =
    ImageC.toArray2d >> make


fromImageInBounds : Vec2 -> Image -> ImagePlane
fromImageInBounds size image =
    ImageC.toArray2d image
        |> makeInBounds size


toImage : ImagePlane -> Image
toImage = toGrid >> ImageC.fromList2d
