module Render.Example.Flat exposing (..)


import Dict

import Html exposing (..)
import Html.Attributes exposing (..)

import Kvant.Vec2 exposing (..)
import Kvant.Plane exposing (..)
import Kvant.Plane.Flat exposing (..)
import Kvant.Plane.Flat as Plane exposing (allViews)
import Kvant.Plane.Impl.Tracing exposing (..)
import Kvant.Solver exposing (..)
import Kvant.Solver.Flat as Solver
import Kvant.Solver.History as H exposing (..)
import Kvant.Matches as Matches exposing (..)

import Render.Core as Render exposing (..)
import Render.Grid as Render exposing (..)
import Render.Flat as Render exposing (..)
import Render.Example exposing (Renderer)


subPlanesCoords : List (Vec2, Vec2)
subPlanesCoords =
    [ ( (0, 0), (2, 2) )
    , ( (0, 0), (3, 3) )
    , ( (1, 1), (2, 2) )
    , ( (1, 1), (3, 3) )
    , ( (0, 1), (3, 3) )
    , ( (0, 1), (2, 3) )
    , ( (3, 3), (1, 1) )
    , ( (3, 3), (4, 4) )
    ]


periodicSubPlanesCoords : List (Vec2, Vec2)
periodicSubPlanesCoords =
    subPlanesCoords ++
        [ ( (2, 3), (4, 4) )
        , ( (-2, -2), (4, 4) )
        ]


make
    :  (fmt -> List (List a))
    -> Spec Vec2 a msg
    -> Renderer Vec2 fmt a msg
make fmtToGrid spec =
    let

        source fmt =
            fmtToGrid fmt
                |> Render.grid spec.a

        withCoords = Render.withCoords spec.v spec.a

        subPlanes plane =
            subPlanesCoords
                |> List.map
                    (\(origin, size) ->
                        ( spec.vToString origin ++ " " ++ spec.vToString size
                        , subAt origin (N size) plane
                        )
                    )
                |> List.map (Tuple.mapSecond <| Maybe.withDefault plane)
                |> Render.labeledList spec.default withCoords

        periodicSubPlanes plane =
            periodicSubPlanesCoords
                |> List.map
                    (\(origin, size) ->
                        ( spec.vToString origin ++ " " ++ spec.vToString size
                        , periodicSubAt origin (N size) plane
                        )
                    )
                |> Render.labeledList spec.default withCoords

        allViews =
            Plane.allViews
                >> Render.indexedList spec.default withCoords

        allSubPlanes method size =
            findAllSubsAlt method size
                >> Render.indexedList spec.default withCoords
        materialized =
            listBy (\(v, maybeA) -> withCoords v <| Maybe.withDefault spec.default <| maybeA)
                << materializeFlatten

        patterns method n plane =
                let
                    uniquePatterns = Solver.findUniquePatterns method n plane
                in
                    Render.listBy identity
                        <| Dict.values
                        <| Dict.map
                            -- (Render.pattern spec.default withCoords uniquePatterns)
                            (Render.pattern spec.default (always spec.a) uniquePatterns)
                            uniquePatterns

        rotationsAndFlips p =
            [ ( "Original", p )
            , ( "North", rotateTo North p )
            , ( "West", rotateTo West p )
            , ( "South", rotateTo South p )
            , ( "East", rotateTo East p )
            , ( "Horz", flipBy Horizontal p )
            , ( "Vert", flipBy Vertical p )
            , ( "rotate once", rotate p )
            , ( "rotate twice", rotate <| rotate p )
            , ( "rotate triple times", rotate <| rotate <| rotate p )
            , ( "flip", flip p )
            , ( "flip rotated", flip <| rotate p )
            , ( "rotate flipped", rotate <| flip p )
            ]
            |> Render.labeledList spec.default withCoords

    in
        { source = source
        , tracing = Render.tracing spec.contradiction spec.a spec.v
        , tracingTiny = Render.tracingTiny spec.default spec.scaled spec.v
        , subPlanes = subPlanes
        , periodicSubPlanes = periodicSubPlanes
        , allViews = allViews
        , rotationsAndFlips = rotationsAndFlips
        , allSubPlanes = allSubPlanes
        , materialized = materialized
        , patterns = patterns
        , step = Render.step spec.v
        , history = Render.history <| Render.step spec.v
        }
