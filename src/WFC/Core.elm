module WFC.Core exposing
    ( WFC, Instance(..)
    , text, TextWFC
    , run
    )


import Random


import WFC.Vec2 exposing (..)
import WFC.Plane.Text as TextPlane exposing (make, toString)
import WFC.Plane.Plane exposing (Plane)
import WFC.Solver exposing (Solver)
import WFC.Solver as Solver exposing (initFlat, Step(..), findUniquePatterns, getSource)


type WFC v fmt a =
    WFC (Solver v a) ( Solver.Step v -> (Solver.Step v, fmt) )


type Instance
    = Text (String -> TextWFC)


type alias TextWFC = WFC Vec2 String Char


text : Solver.TextOptions -> (String -> TextWFC)
text options input =
    let
        plane = input |> TextPlane.make options.inputSize
        -- solver : TextSolver
        solver = Solver.initFlat plane options
    in
        WFC solver <|
            \nextStep ->
                case Solver.solve solver nextStep of
                    lastStep ->
                        ( lastStep
                        , Solver.apply plane lastStep
                            |> TextPlane.toString
                        )


-- load : Instance -> WFC pos size fmt item
-- load instance =
--     case instance of
--         Text wfc -> wfc


run : Random.Seed -> WFC v fmt a -> fmt
run seed (WFC solver wfc) = Tuple.second <| wfc <| Solver.firstStep seed


step : Step v -> WFC v fmt a -> ( Step v, fmt )
step stepToPerform (WFC _ wfc) =
    wfc stepToPerform
