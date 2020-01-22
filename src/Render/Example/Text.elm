module Render.Example.Text exposing (..)


import Render.Example exposing (TextExample, Status(..))
import Render.Example as Example exposing (make)


import WFC.Vec2 exposing (..)
import WFC.Plane exposing (Cell, N(..))
import WFC.Plane.Flat exposing (SearchMethod(..))
import WFC.Core as WFC exposing (..)
import WFC.Solver exposing (Approach(..))
import WFC.Solver as WFC exposing (Step(..), Options)
import WFC.Plane.Impl.Text as TextPlane exposing (make)


options : WFC.Options Vec2
options =
    { approach = Overlapping
    , patternSearch = Bounded -- Periodic
    , patternSize = N ( 2, 2 )
    , inputSize = ( 4, 4 )
    , outputSize = ( 10, 10 )
    -- , advanceRule = WFC.MaximumAttempts 50
    , advanceRule = WFC.AdvanceManually
    }


quick : String -> Vec2 -> TextExample
quick src size =
    Example.make
        (WFC.text options src)
        (WFC.textTracing options src)
        options
        src
        (TextPlane.make size src)
