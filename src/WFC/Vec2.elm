module WFC.Vec2 exposing (..)


type alias Vec2 = (Int, Int)


above : Vec2 -> List (List Vec2)
above (width, height) =
    List.range 0 (width - 1)
        |> List.map (\x ->
            List.range 0 (height - 1) |> List.map (Tuple.pair x))


rect : { from: Vec2, to : Vec2 } -> List (List Vec2)
rect { from, to } =
    let
        ( fromX, fromY ) = from
        ( toX, toY ) = to
    in
        List.range fromX toX
            |> List.map (\x ->
                List.range fromY toY |> List.map (Tuple.pair x))


sub : Vec2 -> Vec2 -> Vec2
sub ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )


add : Vec2 -> Vec2 -> Vec2
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )

