module Render.Example.Text exposing (..)

import Render.Example exposing (TextExample, Status(..), AdvanceMode(..))
import Render.Example as Example exposing (make)


import Kvant.Vec2 exposing (..)
import Kvant.Plane exposing (Cell, N(..))
import Kvant.Plane.Flat exposing (Boundary(..), Symmetry(..))
import Kvant.Core as Kvant exposing (..)
import Kvant.Solver exposing (Approach(..))
import Kvant.Solver as Solver exposing (Step(..), Options)
import Kvant.Plane.Impl.Text as TextPlane exposing (make)



quick : Solver.Options Vec2 Char -> BoundedString -> TextExample
quick options ((size, src) as boundedSrc) =
    Example.make
        (\advanceMode ->
            ( case advanceMode of
                AtOnce -> Kvant.text options boundedSrc
                StepByStep -> Kvant.textAdvancing options boundedSrc
            , Kvant.textTracing options boundedSrc
            )
        )
        options
        boundedSrc
        (TextPlane.make size src)
