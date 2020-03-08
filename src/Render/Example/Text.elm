module Render.Example.Text exposing (..)

import Render.Example exposing (TextExample, Status(..))
import Render.Example as Example exposing (make)


import WFC.Vec2 exposing (..)
import WFC.Plane exposing (Cell, N(..))
import WFC.Plane.Flat exposing (Boundary(..), Symmetry(..))
import WFC.Core as WFC exposing (..)
import WFC.Solver exposing (Approach(..))
import WFC.Solver as WFC exposing (Step(..), Options)
import WFC.Plane.Impl.Text as TextPlane exposing (make)


options : WFC.AdvanceRule -> WFC.Options Vec2 Char
options advanceRule =
    { approach =
        Overlapping
            { searchBoundary = Bounded -- Periodic
            , patternSize = N ( 2, 2 )
            , symmetry = FlipAndRotate
            }
    , outputSize = ( 10, 10 )
    , outputBoundary = Bounded
    -- , advanceRule = WFC.MaximumAttempts 50
    , advanceRule = advanceRule
    }


quick : String -> Vec2 -> TextExample
quick src size =
    let
        boundedSrc
            = (size, src)
    in
        Example.make
            (WFC.text (options <| WFC.MaximumAttempts 200) boundedSrc)
            (WFC.textTracing (options WFC.AdvanceManually) boundedSrc)
            (options WFC.AdvanceManually)
            boundedSrc
            (TextPlane.make size src)
