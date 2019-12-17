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


type Solver pos size item = Solver (Options size) (Plane pos item) (List (Pattern pos item))


solve : Step item -> Solver pos size item -> ( Step item, Plane pos item )
solve step (Solver options sourcePlane patterns) =
    ( step, sourcePlane )




type alias TextOptions = Options (Int, Int)
type alias TextSolver = Solver (Int, Int) (Int, Int) Char
