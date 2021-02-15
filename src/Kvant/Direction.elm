module Kvant.Direction exposing (..)


import Kvant.Vec2 exposing (Vec2)


type Direction
    = NW | N | NE
    |  W | X | E
    | SW | S | SE


all : List Direction
all =
    [ NW, N, NE, W, X, E, SW, S, SE ]


cardinal : List Direction
cardinal = [ N, E, S, W ]


around : List Direction
around =
    all |> List.filter ((/=) X)


opposite : Direction -> Direction
opposite direction =
    case direction of
        NW -> SE
        N  -> S
        NE -> SW
        W  -> E
        X  -> X
        E  -> W
        SW -> NE
        S  -> N
        SE -> NW


move : Vec2 -> Direction -> Vec2
move (x, y) direction =
    case toOffset direction of
        ( offX, offY ) -> ( x + offX, y + offY )


toString : Direction -> String
toString direction =
    case direction of
        NW -> "NW"
        N  -> "N"
        NE -> "NE"
        W  -> "W"
        X  -> "X"
        E  -> "E"
        SW -> "SW"
        S  -> "S"
        SE -> "SE"


fromString : String -> Maybe Direction
fromString str =
    case str of
        "NW" -> Just NW
        "N"  -> Just N
        "NE" -> Just NE
        "W"  -> Just W
        "X"  -> Just X
        "E"  -> Just E
        "SW" -> Just SW
        "S"  -> Just S
        "SE" -> Just SE
        _    -> Nothing


toOffset : Direction -> Vec2
toOffset direction =
    case direction of
        NW -> ( -1, -1 )
        N  -> (  0, -1 )
        NE -> (  1, -1 )
        W  -> ( -1,  0 )
        X  -> (  0,  0 )
        E  -> (  1,  0 )
        SW -> ( -1,  1 )
        S  -> (  0,  1 )
        SE -> (  1,  1 )


fromOffset : Vec2 -> Direction
fromOffset (x, y) =
    if (x < 0) && (y < 0) then NE
    else if (x == 0) && (y < 0) then N
    else if (x > 0) && (y < 0) then NW
    else if (x < 0) && (y == 0) then E
    else if (x == 0) && (y == 0) then X
    else if (x > 0) && (y == 0) then W
    else if (x < 0) && (y > 0) then SE
    else if (x == 0) && (y > 0) then S
    else SW


isCardinal : Direction -> Bool
isCardinal dir =
    case dir of
        N -> True
        W -> True
        X -> False
        E -> True
        S -> True
        _ -> False
