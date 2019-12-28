module WFC.Vec2 exposing (..)


type alias Vec2 = (Int, Int)


swap : Vec2 -> Vec2
swap (x, y) = (y, x)


above : Vec2 -> List (List Vec2)
above (width, height) =
    rect { from = (0, 0), to = (width - 1, height - 1) }


rect : { from: Vec2, to : Vec2 } -> List (List Vec2)
rect { from, to } =
    let
        ( fromX, fromY ) = from
        ( toX, toY ) = to
    in
        List.range fromX toX
            |> List.map (\x ->
                List.range fromY toY |> List.map (Tuple.pair x >> swap))

