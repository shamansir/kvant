module Example.Instance.Text exposing (..)

import Kvant.Vec2 exposing (..)
import Kvant.Plane exposing (Cell, N(..))
import Kvant.Plane as Plane exposing (empty)
import Kvant.Plane.Flat exposing (Boundary(..), Symmetry(..))
import Kvant.Plane.Impl.Tracing exposing (TracingPlane)
import Kvant.Core as Kvant exposing (..)
import Kvant.Solver as Solver exposing (Step(..))
import Kvant.Solver.Options exposing (Approach(..))
import Kvant.Solver.Options as Solver exposing (Options)
import Kvant.Solver.Flat as FlatSolver exposing (..)

import Example.Instance.Text.Plane as TextPlane exposing (make, BoundedString)

import Example.Example exposing (Example, make)
import Example.Example as Example exposing (make)
import Example.Advance exposing (..)


type alias TextWfc = Wfc Vec2 BoundedString Char

type alias TextTracingWfc = TracingWfc Vec2 Char

type alias TextTracingPlane = TracingPlane Vec2 Char

type alias TextExample = Example Vec2 BoundedString Char


quick : Solver.Options Vec2 -> BoundedString -> TextExample
quick options ((size, src) as boundedSrc) =
    Example.make
        (\advanceMode ->
            ( case advanceMode of
                AtOnce -> text options boundedSrc
                StepByStep -> textAdvancing options boundedSrc
            , textTracing options boundedSrc
            )
        )
        options
        boundedSrc
        (TextPlane.make size src)


text : Solver.Options Vec2 -> (BoundedString -> TextWfc)
text options =
    makeFn
        (Convert
            { fromInput = \(size, str) -> TextPlane.make size str
            , toElement = always TextPlane.merge
            , toOutput = TextPlane.toBoundedString
            }
        )
        (FlatSolver.init options)


textAdvancing  : Solver.Options Vec2 -> (BoundedString -> TextWfc)
textAdvancing options =
    makeAdvancingFn
        (Convert
            { fromInput = \(size, str) -> TextPlane.make size str
            , toElement = always TextPlane.merge
            , toOutput = TextPlane.toBoundedString
            }
        )
        (FlatSolver.init options)


textTracing : Solver.Options Vec2 -> (BoundedString -> TextTracingWfc)
textTracing options =
    \(size, input) ->
        makeAdvancingFn
            (Convert
                { fromInput = identity
                , toElement = Tuple.pair
                , toOutput = identity
                }
            )
            (\_ ->
                input
                    |> TextPlane.make size
                    |> FlatSolver.init options
            )
            (Plane.empty options.outputSize)
