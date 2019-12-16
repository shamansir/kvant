module WFC.Core exposing
    ( WFC, Instance(..)
    , text, TextWFC
    , run
    )


import WFC.Plane exposing (..)
import WFC.Solver exposing (..)


type WFC pos size fmt item =
    WFC (fmt -> fmt)


type Instance
    = Text TextWFC


type alias TextWFC = WFC (Int, Int) (Int, Int) String Char


text : TextOptions -> TextWFC
text options =
    WFC <| \input ->
                let
                    solver : TextSolver
                    solver = Solver options <| makeTextPlane input options.inputSize
                in input


-- load : Instance -> WFC pos size fmt item
-- load instance =
--     case instance of
--         Text wfc -> wfc


run : fmt -> WFC pos size fmt item -> fmt
run input (WFC wfc) = wfc input
