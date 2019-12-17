module WFC.Plane exposing (..)


import Array


type Plane pos a = Plane (pos -> Maybe a)


type alias Pattern pos a = Plane pos a


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


textPlaneToString : (Int, Int) -> Plane (Int, Int) Char -> String
textPlaneToString size plane =
    unpack2 size plane
        |> List.concat
        |> List.filterMap identity
        |> String.fromList


findPatterns : Plane pos a -> List (Pattern pos a)
findPatterns plane =
    []
