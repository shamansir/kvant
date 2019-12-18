module WFC.Plane exposing (..)


import Array


type Plane v a = Plane v (v -> Maybe a)


type alias Pattern pos a = Plane pos a


type Orientaion
    = North
    | West
    | East
    | South


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


sub : v -> Plane v a -> Plane v a
sub dstSize (Plane srcSize planeF) =
    (Plane srcSize planeF)


rotate : Orientaion -> Plane v a -> Plane v a
rotate orientation (Plane srcSize planeF) =
    (Plane srcSize planeF)


equal : Plane (Int, Int) a -> Plane (Int, Int) a -> Bool
equal ((Plane sizeA fA) as planeA) ((Plane sizeB fB) as planeB) =
    if sizeA == sizeB then
        let
            ( width, height ) = sizeA -- sizeA == sizeB, so it's safe
        in
            planeA
                |> materializeFlatten
                |> List.map (\(pos, valA) -> valA == fB pos)
                |> List.foldl ((&&)) True
    else False


empty : (Int, Int) -> Plane (Int, Int) a
empty size = Plane size <| always Nothing


materialize : Plane (Int, Int) a -> List (List ((Int, Int), Maybe a))
materialize (Plane (width, height) f) =
    List.repeat height []
        |> List.map (always <| List.repeat width Nothing)
        |> List.indexedMap
            (\y row -> List.indexedMap (\x _-> ((x, y), f (x, y))) row)


materializeFlatten : Plane (Int, Int) a -> List ((Int, Int), Maybe a)
materializeFlatten = materialize >> List.concat
