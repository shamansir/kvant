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


type Solver pos size fmt item = Solver (Options size) fmt
