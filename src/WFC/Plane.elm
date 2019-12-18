module WFC.Plane exposing (..)


import Array


type Plane v a = Plane v (v -> Maybe a)


type alias Pattern pos a = Plane pos a


type alias TextPlane = Plane (Int, Int) Char


makeTextPlane : (Int, Int) -> String -> TextPlane
makeTextPlane ( width, height ) src =
    let
        charArray = String.toList src |> Array.fromList
    in
        Plane
            ( width, height )
            (\(x, y) ->
                if (x <= width) && (y <= height) then
                    charArray |>
                        Array.get
                            (x * width + y)
                else Nothing
            )


unpack : Plane (Int, Int) a -> List (List (Maybe a))
unpack (Plane (width, height) f) =
    List.repeat height []
        |> List.indexedMap (\y _ ->
                List.repeat width Nothing
                    |> List.indexedMap (\x _ -> f (x, y))
            )


textPlaneToString : TextPlane -> String
textPlaneToString plane =
    unpack plane
        |> List.concat
        |> List.filterMap identity
        |> String.fromList


findPatterns : Plane v a -> List (Pattern v a)
findPatterns plane =
    []
