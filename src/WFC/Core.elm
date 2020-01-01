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
import WFC.Solver as Solver exposing (initFlat, Step(..), findUniquePatterns)


type WFC v fmt a =
    WFC ( fmt -> ( Solver.Step v -> (Solver.Step v, fmt) ) )


type Instance
    = Text TextWFC


type alias TextWFC = WFC Vec2 String Char


text : Solver.TextOptions -> TextWFC
text options =
    WFC <|
        \input ->
            let
                plane = input |> TextPlane.make options.inputSize
                -- solver : TextSolver
                solver = Solver.initFlat plane options
            in
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


run : Random.Seed -> fmt -> WFC v fmt a -> fmt
run seed input (WFC wfc) = Tuple.second <| wfc input <| Solver.firstStep seed


-- step : Step v -> WFC v fmt a -> ( Step v, fmt )
-- step prevStep (WFC wfc) =
