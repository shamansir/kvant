module Example.Instance.Tiles exposing (..)


import Array exposing (Array)
import Dict exposing (Dict)
import Color exposing (Color)
import Set exposing (Set)


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
import Example.Instance.Tiles.Plane exposing (TileKey, TileGrid, noTile)
import Example.Instance.Tiles.Plane as TilesPlane exposing (make)


type TilingRules
    = FromGrid TileSet TileGrid
    | FromRules ()


type alias TilesExample = Example Vec2 TileGrid TileKey


type alias TileSet = ( Int -> Maybe TileKey, TileKey -> Maybe Int )


type alias TilesWfc = Wfc Vec2 TileGrid TileKey
type alias TilesTracingWfc = TracingWfc Vec2 TileKey
type alias TilesTracingPlane = TracingPlane Vec2 TileKey


type alias TilesOptions = Solver.Options Vec2 TileKey


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
            , toElement = \_ _ -> "none"
            , toOutput = TilesPlane.toGrid >> List.map (Array.fromList) >> Array.fromList
            }
        )
        (FlatSolver.init options)


tilesAdvancing  : TilesOptions -> (TileGrid -> TilesWfc)
tilesAdvancing options =
    makeAdvancingFn
        (Convert
            { fromInput = TilesPlane.make
            , toElement = \_ _ -> "none"
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


buildTileset : Set TileKey -> TileSet
buildTileset =
    Set.toList
        >> List.indexedMap
            (\index key ->
                ( ( index, key )
                , ( key, index )
                )
            )
        >> (\list ->
                ( List.map Tuple.first <| list
                , List.map Tuple.second <| list
                )
           )
        >> Tuple.mapBoth Dict.fromList Dict.fromList
        >> Tuple.mapBoth
            (\dict -> \v -> Dict.get v dict)
            (\dict -> \v -> Dict.get v dict)


toIndexInSet : TileSet -> TileKey -> Int
toIndexInSet ( _, toIndex ) =
    toIndex >> Maybe.withDefault -1


fromIndexInSet : TileSet -> Int -> TileKey
fromIndexInSet ( fromIndex , _ ) =
    fromIndex >> Maybe.withDefault noTile


toIndexGrid : TileSet -> Array (Array TileKey) -> Array (Array Int)
toIndexGrid tileSet =
    Array.map << Array.map <| toIndexInSet tileSet



fromIndexGrid : TileSet -> Array (Array Int) -> Array (Array TileKey)
fromIndexGrid tileSet =
    Array.map << Array.map <| fromIndexInSet tileSet
