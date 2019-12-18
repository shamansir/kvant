module WFC.Plane exposing (..)


import Array


type Plane v a = Plane v (v -> Maybe a)
type Pattern v a = Pattern v (v -> Maybe a)


type Orientation
    = North
    | West
    | East
    | South


type Flip
    = Horizontal
    | Vertical


empty : v -> Plane v a
empty size = Plane size <| always Nothing


toPattern : Plane v a -> Pattern v a
toPattern (Plane size f) = Pattern size f


fromPattern : Pattern v a -> Plane v a
fromPattern (Pattern size f) = Plane size f


sub : (Int, Int) -> (Int, Int) -> Plane (Int, Int) a -> Plane (Int, Int) a
sub (shiftX, shiftY) dstSize (Plane srcSize planeF) =
    (Plane srcSize planeF)


rotate : Orientation -> Plane (Int, Int) a -> Plane (Int, Int) a
rotate orientation (Plane (width, height) planeF) =
    Plane (width, height)
        <| case orientation of
            North -> planeF
            West -> \(x, y) -> planeF (height - 1 - y, x)
            South -> \(x, y) -> planeF (width - 1 - x, height - 1 - y)
            East -> \(x, y) -> planeF (y, width - 1 - x)


flip : Flip -> Plane (Int, Int) a -> Plane (Int, Int) a
flip how (Plane (width, height) planeF) =
    Plane (width, height)
        <| case how of
            Horizontal -> \(x, y) -> planeF (width - 1 - x, y)
            Vertical -> \(x, y) -> planeF (x, height - 1 - y)


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


unpack : Plane (Int, Int) a -> List (List (Maybe a))
unpack (Plane (width, height) f) =
    List.repeat height []
        |> List.indexedMap (\y _ ->
                List.repeat width Nothing
                    |> List.indexedMap (\x _ -> f (x, y))
            )


materialize : Plane (Int, Int) a -> List (List ((Int, Int), Maybe a))
materialize (Plane (width, height) f) =
    List.repeat height []
        |> List.map (always <| List.repeat width Nothing)
        |> List.indexedMap
            (\y row -> List.indexedMap (\x _-> ((x, y), f (x, y))) row)


materializeFlatten : Plane (Int, Int) a -> List ((Int, Int), Maybe a)
materializeFlatten = materialize >> List.concat


findPatterns : Plane v a -> List (Pattern v a)
findPatterns plane =
    []


type alias TextPlane = Plane (Int, Int) Char


makeTextPlane : (Int, Int) -> String -> TextPlane
makeTextPlane ( width, height ) src =
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


textPlaneToString : TextPlane -> String
textPlaneToString plane =
    unpack plane
        |> List.concat
        |> List.filterMap identity
        |> String.fromList
