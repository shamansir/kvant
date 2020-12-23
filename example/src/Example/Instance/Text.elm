module Example.Instance.Text exposing (..)

import Kvant.Vec2 exposing (..)
import Kvant.Plane exposing (Cell, N(..))
import Kvant.Plane.Flat exposing (Boundary(..), Symmetry(..))
import Kvant.Core as Kvant exposing (..)
import Kvant.Solver exposing (Approach(..))
import Kvant.Solver as Solver exposing (Step(..), Options)

import Example.Instance.Text.Plane as TextPlane exposing (make)

import Example.Example exposing (Example, make)
import Example.Example as Example exposing (make)
import Example.Advance exposing (..)
import Example.Instance exposing (..)


type alias TextExample = Example Vec2 BoundedString Char

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
