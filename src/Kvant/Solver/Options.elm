module Kvant.Solver.Options exposing (..)


import Kvant.Plane exposing (Plane(..), Size, Boundary(..), Symmetry(..))

-- import Xml.Decode as Xml


type alias Options =
    { approach : Approach
    , outputBoundary : Boundary
    , outputSize : Size
    }


type Approach
    = Overlapping
        { patternSize : Size -- FIXME: use just square patterns
        , inputBoundary : Boundary
        , symmetry : Symmetry -- FIXME: use in search
        -- TODO: ground : Int
        }
    | Tiled
