module WFC.Plane.Impl.Image exposing (..)


import Array exposing (Array)
import Color exposing (Color)
import Color
import Image
import Image exposing (Image)
import Image.Color as ImageC exposing (fromList2d)

import WFC.Vec2 exposing (..)
import WFC.Plane exposing (..)
import WFC.Plane.Flat exposing (..)


type alias ImagePlane = Plane Vec2 Color


transparent : Color
transparent = Color.rgba 1.0 1.0 1.0 0.0


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

merge : List Color -> Color
merge _ = Color.red


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
