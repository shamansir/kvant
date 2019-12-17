module WFC.Solver exposing (..)


import WFC.Plane exposing (..)


type Approach
    = Overlapping
    | Tiled {- Rules -}


type alias Options size =
    { approach: Approach
    , tileSize: size
    , inputSize: size
    , outputSize: size
    }


type Step item
    = Step Int


type Solver pos size item = Solver (Options size) (Plane pos item)


type alias TextOptions = Options (Int, Int)
type alias TextSolver = Solver (Int, Int) (Int, Int) Char
