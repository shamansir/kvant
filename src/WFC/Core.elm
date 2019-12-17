module WFC.Core exposing
    ( WFC, Instance(..)
    , text, TextWFC
    , run
    )


import WFC.Plane exposing (..)
import WFC.Solver exposing (..)


type WFC pos size fmt item =
    WFC ( (Step item, fmt) -> (Step item, fmt) )


type Instance
    = Text TextWFC


type alias TextWFC = WFC (Int, Int) (Int, Int) String Char


text : TextOptions -> TextWFC
text options =
    WFC <| \(step, input) ->
                let
                    -- plane : TextPlane
                    plane = input |> makeTextPlane options.inputSize
                    -- patterns : List TextPlane
                    patterns = findPatterns plane
                    -- solver : TextSolver
                    solver = Solver options plane patterns
                in
                    solver
                        |> solve step
                        |> Tuple.mapSecond (textPlaneToString options.outputSize)


-- load : Instance -> WFC pos size fmt item
-- load instance =
--     case instance of
--         Text wfc -> wfc


run : fmt -> WFC pos size fmt item -> fmt
run input (WFC wfc) = Tuple.second <| wfc ( Step 0, input )
