module Example.Text exposing (..)

import Kvant.Vec2 exposing (..)
import Kvant.Plane exposing (Cell, N(..))
import Kvant.Plane.Flat exposing (Boundary(..), Symmetry(..))
import Kvant.Core as Kvant exposing (..)
import Kvant.Solver exposing (Approach(..))
import Kvant.Solver as Solver exposing (Step(..), Options)

import Example.Plane.Impl.Text as TextPlane exposing (make)


import Example.Main exposing (TextExample, Status(..), AdvanceMode(..))
import Example.Main as Example exposing (make)

type alias BoundedString = (Vec2, String)


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
