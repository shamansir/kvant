module WFC.Plane exposing (..)


import Array


type Plane pos a = Plane (pos -> Maybe a)


type alias TextPlane = Plane (Int, Int) Char


makeTextPlane : (Int, Int) -> String -> TextPlane
makeTextPlane ( width, height ) src =
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


unpack2 : (Int, Int) -> Plane (Int, Int) a -> List (List (Maybe a))
unpack2 ( width, height ) (Plane f) =
    List.repeat height []
        |> List.indexedMap (\y _ ->
                List.repeat width Nothing
                    |> List.indexedMap (\x _ -> f (x, y))
            )
