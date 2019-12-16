module WFC.Plane exposing (..)


import Array


type Plane pos a = Plane (pos -> Maybe a)


type alias TextPlane = Plane (Int, Int) Char


makeTextPlane : String -> (Int, Int) -> TextPlane
makeTextPlane src ( width, height ) =
    let
        charArray = String.toList src |> Array.fromList
    in
        Plane
            (\(x, y) ->
                if (x <= width) && (y <= height) then
                    charArray |>
                        Array.get
                            (x * width + y)
                else Nothing
            )

