module Render.Example.Text exposing (..)

import Render.Example exposing (TextExample, Status(..), AdvanceMode(..))
import Render.Example as Example exposing (make)


import WFC.Vec2 exposing (..)
import WFC.Plane exposing (Cell, N(..))
import WFC.Plane.Flat exposing (Boundary(..), Symmetry(..))
import WFC.Core as WFC exposing (..)
import WFC.Solver exposing (Approach(..))
import WFC.Solver as WFC exposing (Step(..), Options)
import WFC.Plane.Impl.Text as TextPlane exposing (make)



quick : WFC.Options Vec2 Char -> BoundedString -> TextExample
quick options ((size, src) as boundedSrc) =
    Example.make
        (\advanceMode ->
            ( case advanceMode of
                AtOnce -> WFC.text options boundedSrc
                StepByStep -> WFC.textAdvancing options boundedSrc
            , WFC.textTracing options boundedSrc
            )
        )
        options
        boundedSrc
        (TextPlane.make size src)
