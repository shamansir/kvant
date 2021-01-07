module Example.Instance.Image.Plane exposing (..)


import Array exposing (Array)
import Color exposing (Color)
import Color
import Image
import Image exposing (Image, Pixel)
import Image.Color as ImageC exposing (fromList2d)
import Bitwise

import Kvant.Vec2 exposing (..)
import Kvant.Plane exposing (..)
import Kvant.Plane.Flat exposing (..)


type alias Pixels = Array (Array Color)

type alias ImagePlane = Plane Vec2 Color
type alias PixelsPlane = Plane Vec2 Color


transparent : Color
transparent = Color.rgba 0.0 0.0 0.0 0.0


make : Pixels -> ImagePlane
make pixels =
    makeInBounds
        (loadSize pixels |> Maybe.withDefault (0, 0))
        pixels


makeInBounds : Vec2 -> Pixels -> ImagePlane
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


colorToPixel : Color -> Pixel
colorToPixel = colorToInt32


pixelToColor : Pixel -> Color
pixelToColor = int32ToColor


toGrid : ImagePlane -> List (List Color)
toGrid plane =
    unpack plane
        |> List.map (List.map <| Maybe.withDefault transparent)


toArrayGrid : ImagePlane -> Pixels
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


toPixels : PixelsPlane -> Pixels
toPixels = toGrid >> List.map (Array.fromList) >> Array.fromList


int32ToColor : Int -> Color  -- Copied from Image.Color
int32ToColor int =
    let
        a =
            int
                |> Bitwise.and 0xFF
                |> toFloat

        b =
            int
                |> Bitwise.shiftRightBy 8
                |> Bitwise.and 0xFF
                |> toFloat

        g =
            int
                |> Bitwise.shiftRightBy 16
                |> Bitwise.and 0xFF
                |> toFloat

        r =
            int
                |> Bitwise.shiftRightZfBy 24
                |> Bitwise.and 0xFF
                |> toFloat
    in
    Color.rgba (r / 255) (g / 255) (b / 255) (a / 255)


colorToInt32 : Color -> Int -- Copied from Image.Color
colorToInt32 color =
    let
        record =
            Color.toRgba color

        byte1 =
            (record.alpha * 255)
                |> round

        byte2 =
            (record.blue * 255)
                |> round
                |> Bitwise.shiftLeftBy 8

        byte3 =
            (record.green * 255)
                |> round
                |> Bitwise.shiftLeftBy 16

        byte4 =
            (record.red * 255)
                |> round
                |> Bitwise.shiftLeftBy 24
    in
    Bitwise.or byte1 byte2
        |> Bitwise.or byte3
        |> Bitwise.or byte4
        |> Bitwise.shiftRightZfBy 0
