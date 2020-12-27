module Example.Example exposing (..)


import Kvant.Core exposing (Wfc, TracingWfc)
import Kvant.Plane exposing (Plane)
import Kvant.Solver as Solver exposing (Options)
import Example.Advance exposing (..)

type alias Example v fmt a =
    { source : fmt
    , sourcePlane : Plane v a
    , options : Solver.Options v a
    , wfc : ( Wfc v fmt a, TracingWfc v a )
    , makeWfc : AdvanceMode -> ( Wfc v fmt a, TracingWfc v a )
    , status : Status v fmt a
    }


make
    :  ( AdvanceMode -> ( Wfc v fmt a, TracingWfc v a ) )
    -> Solver.Options v a
    -> fmt
    -> Plane v a
    -> Example v fmt a
make makeWfc options src sourcePlane =
    { source = src
    , sourcePlane = sourcePlane
    , options = options
    , wfc = makeWfc AtOnce
    , makeWfc = makeWfc
    , status = None
    } -- |> initExpands
