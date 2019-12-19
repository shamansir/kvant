module WFC.Plane exposing (..)


import Array


type Plane v a = Plane v (v -> Maybe a)


type alias Cell v a = (v, Maybe a)


type alias Vec2 = (Int, Int)


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


getSize : Plane v a -> v
getSize (Plane size _) = size


map : (a -> b) -> Plane v a -> Plane v b
map f (Plane size srcF) =
    Plane size (Maybe.map f << srcF)


foldMap : (Cell Vec2 a -> b) -> Plane Vec2 a -> List (List b)
foldMap f (Plane (width, height) planeF) =
    List.repeat height []
        |> List.map (always <| List.repeat width Nothing)
        |> List.indexedMap
            (\y row ->
                List.indexedMap
                    (\x _ -> ((x, y), planeF (x, y)))
                    row
                    |> List.map f
            )


foldl : (Cell Vec2 a -> b -> b) -> b -> Plane Vec2 a -> b
foldl f def plane =
    foldMap identity plane
        |> List.concat
        |> List.foldl f def


sub : Vec2 -> Plane Vec2 a -> Plane Vec2 a
sub = subAt (0, 0)


subAt : Vec2 -> Vec2 -> Plane Vec2 a -> Plane Vec2 a
subAt (shiftX, shiftY) (dstWidth, dstHeight) (Plane _ planeF) =
    (Plane (dstWidth, dstHeight)
        <| \(x, y) ->
            if (x < dstWidth) && (y < dstHeight) then
                planeF (x + shiftX, y + shiftY)
            else
                Nothing
        )


equal : Plane Vec2 a -> Plane Vec2 a -> Bool
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


unpack : Plane Vec2 a -> List (List (Maybe a))
unpack (Plane (width, height) f) =
    List.repeat height []
        |> List.indexedMap (\y _ ->
                List.repeat width Nothing
                    |> List.indexedMap (\x _ -> f (x, y))
            )


materialize : Plane Vec2 a -> List (List (Cell Vec2 a))
materialize = foldMap identity


materializeFlatten : Plane Vec2 a -> List (Cell Vec2 a)
materializeFlatten = materialize >> List.concat


coords : Plane Vec2 a -> List Vec2
coords = foldMap Tuple.first >> List.concat


rotate : Orientation -> Plane Vec2 a -> Plane Vec2 a
rotate orientation (Plane (width, height) planeF) =
    Plane (width, height)
        <| case orientation of
            North -> planeF
            West -> \(x, y) -> planeF (height - 1 - y, x)
            South -> \(x, y) -> planeF (width - 1 - x, height - 1 - y)
            East -> \(x, y) -> planeF (y, width - 1 - x)


flip : Flip -> Plane Vec2 a -> Plane Vec2 a
flip how (Plane (width, height) planeF) =
    Plane (width, height)
        <| case how of
            Horizontal -> \(x, y) -> planeF (width - 1 - x, y)
            Vertical -> \(x, y) -> planeF (x, height - 1 - y)


allRotations : Plane Vec2 a -> List (Plane Vec2 a)
allRotations plane =
    [ plane
    , plane |> rotate West
    , plane |> rotate South
    , plane |> rotate East
    ]


bothFlips : Plane Vec2 a -> List (Plane Vec2 a)
bothFlips plane =
    [ plane |> flip Horizontal
    , plane |> flip Vertical
    ]


allViews : Plane Vec2 a -> List (Plane Vec2 a)
allViews plane =
    [ plane
    , plane |> flip Horizontal
    , plane |> flip Vertical
    , plane |> rotate West
    , plane |> rotate West |> flip Horizontal
    , plane |> rotate West |> flip Vertical
    , plane |> rotate South
    , plane |> rotate South |> flip Horizontal
    , plane |> rotate South |> flip Vertical
    , plane |> rotate East
    , plane |> rotate East |> flip Horizontal
    , plane |> rotate East |> flip Vertical
    ]


type alias TextPlane = Plane Vec2 Char


makeTextPlane : Vec2 -> String -> TextPlane
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
