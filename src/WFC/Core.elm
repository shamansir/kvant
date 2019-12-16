module WFC.Core exposing
    ( WFC, Instance(..)
    , text, TextOptions, TextWFC, TextSolver
    , run
    )


import WFC.Plane exposing (..)
import WFC.Solver exposing (..)


type WFC pos size fmt item =
    WFC (fmt -> fmt)


type Instance
    = Text TextWFC


type alias TextOptions = Options (Int, Int)
type alias TextWFC = WFC (Int, Int) (Int, Int) String Char
type alias TextSolver = Solver (Int, Int) (Int, Int) String Char



text : TextOptions -> TextWFC
text options =
    WFC <| \input ->
                let
                    solver : TextSolver
                    solver = Solver options input
                in input


-- load : Instance -> WFC pos size fmt item
-- load instance =
--     case instance of
--         Text wfc -> wfc


run : fmt -> WFC pos size fmt item -> fmt
run input (WFC wfc) = wfc input
