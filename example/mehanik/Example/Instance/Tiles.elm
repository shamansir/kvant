module Example.Instance.Tiles exposing (..)


import Array exposing (Array)
import Dict exposing (Dict)
import Color exposing (Color)

import Image exposing (Image, dimensions)
import Image.Color as ImageC exposing (toArray2d)

import Kvant.Vec2 exposing (..)
import Kvant.Plane exposing (Cell, N(..))
import Kvant.Plane as Plane exposing (empty)
import Kvant.Plane.Flat exposing (Boundary(..), Symmetry(..))
import Kvant.Plane.Impl.Tracing exposing (TracingPlane)
import Kvant.Core as Kvant exposing (..)
import Kvant.Solver exposing (Approach(..))
import Kvant.Solver as Solver exposing (Step(..), Options)
import Kvant.Solver.Flat as FlatSolver exposing (init)

import Example.Example exposing (Example)
import Example.Example as Example exposing (make)
import Example.Advance exposing (Status(..), AdvanceMode(..))
import Example.Instance.Tiles.Plane exposing (TileId, TileGrid)
import Example.Instance.Tiles.Plane as TilesPlane exposing (make)


type alias TilesRegistry = Dict TileId Image


type TilingRules
    = FromGrid TileGrid
    | FromRules ()


type alias TilesExample = Example Vec2 TileGrid TileId


type alias TilesWfc = Wfc Vec2 TileGrid TileId
type alias TilesTracingWfc = TracingWfc Vec2 TileId
type alias TilesTracingPlane = TracingPlane Vec2 TileId


type alias TilesOptions = Solver.Options Vec2 TileId


quick : TilesOptions -> TileGrid -> TilesExample
quick options tileGrid =
    Example.make
        (\advanceMode ->
            ( case advanceMode of
                AtOnce -> tiles options tileGrid
                StepByStep -> tilesAdvancing options tileGrid
            , tilesTracing options tileGrid
            )
        )
        options
        tileGrid
        (tileGrid |> TilesPlane.make)


tiles : TilesOptions -> (TileGrid -> TilesWfc)
tiles options =
    makeFn
        (Convert
            { fromInput = TilesPlane.make
            , toElement = \_ _ -> -1
            , toOutput = TilesPlane.toGrid >> List.map (Array.fromList) >> Array.fromList
            }
        )
        (FlatSolver.init options)


tilesAdvancing  : TilesOptions -> (TileGrid -> TilesWfc)
tilesAdvancing options =
    makeAdvancingFn
        (Convert
            { fromInput = TilesPlane.make
            , toElement = \_ _ -> -1
            , toOutput = TilesPlane.toGrid >> List.map (Array.fromList) >> Array.fromList
            }
        )
        (FlatSolver.init options)


tilesTracing : TilesOptions -> (TileGrid -> TilesTracingWfc)
tilesTracing options =
    \input ->
        makeAdvancingFn
            (Convert
                { fromInput = identity
                , toElement = Tuple.pair
                , toOutput = identity
                }
            )
            (\_ ->
                input
                    |> TilesPlane.make
                    |> FlatSolver.init options
            )
            (Plane.empty options.outputSize)
