module WFC.Core exposing
    ( .. )


import WFC.Plane exposing (..)
import WFC.Solver exposing (..)


type WFC pos size fmt item =
    WFC (fmt -> fmt)


string : Options (Int, Int) -> WFC (Int, Int) (Int, Int) String Char
string options =
    WFC <| \input ->
                let
                    solver : Solver (Int, Int) (Int, Int) String Char
                    solver = Solver options input
                in input
