module Kvant.Solver.Options exposing (..)


import Kvant.Plane exposing (Plane(..), Size, Boundary(..), Symmetry(..))

-- import Xml.Decode as Xml


type alias Overlapping =
    { patternSize : Size -- FIXME: use just square patterns
    , boundary : Boundary
    , symmetry : Symmetry -- FIXME: use in search
    -- TODO: ground : Int
    }


type alias Output = ( Boundary, Size )


