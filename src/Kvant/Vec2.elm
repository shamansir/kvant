module Kvant.Vec2 exposing (..)


import Random
import Array exposing (Array)
import Array as Array exposing (..)


type alias Vec2 = (Int, Int)


swap : Vec2 -> Vec2
swap (x, y) = (y, x)


coords : Vec2 -> List (List Vec2)
coords (width, height) =
    rect { from = (0, 0), to = (width - 1, height - 1) }


rect : { from: Vec2, to : Vec2 } -> List (List Vec2)
rect { from, to } =
    let
        ( fromX, fromY ) = from
        ( toX, toY ) = to
    in
        List.range fromY toY
            |> List.map (\y ->
                List.range fromX toX
                    |> List.map (Tuple.pair y >> swap))


loadSize : Array (Array a) -> Maybe Vec2
loadSize grid =
    Array.get 0 grid
        |> Maybe.map (Array.length)
        |> Maybe.map (Tuple.pair <| Array.length grid)
        |> Maybe.map swap


random : Vec2 -> Random.Generator Vec2
random ( limitX, limitY ) =
    Random.map2
        Tuple.pair
        (Random.int 0 limitX)
        (Random.int 0 limitY)
