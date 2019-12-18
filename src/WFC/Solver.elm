module WFC.Solver exposing (..)


import WFC.Plane exposing (..)


type Approach
    = Overlapping
    | Tiled {- Rules -}


type alias Options v =
    { approach: Approach
    , patternSize: v
    , inputSize: v
    , outputSize: v
    }


type Step a
    = Step Int


type Solver v a = Solver (Options v) (Plane v a) (List (Pattern v a))


solve : Step a -> Solver v a -> ( Step a, Plane v a )
solve step (Solver options sourcePlane patterns) =
    ( step, sourcePlane )




type alias TextOptions = Options (Int, Int)
type alias TextSolver = Solver (Int, Int) Char
