module Plane2d exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import WFC.Plane.Offset exposing (Offset(..))
import WFC.Plane.Vec2 as Plane2D
import WFC.Plane.Text as TextPlane


testPlane2x2
    =  "12"
    ++ "34"
testPlane3x3
    =  "ABC"
    ++ "DEF"
    ++ "GHI"
testPlane4x4
    =  "0123"
    ++ "4567"
    ++ "89AB"
    ++ "CDEF"
testPlane4x4_
    =  "0000"
    ++ "0111"
    ++ "0121"
    ++ "0111"


suite : Test
suite =
    describe "The 2D planes (test with Text)"
        [ test "getting coords for 2x2 is in order" <|
            \_ ->
                Expect.equal
                    (TextPlane.make (2, 2) testPlane2x2 |> Plane2D.coords)
                    (
                        [ [ (0, 0), (1, 0) ]
                        , [ (0, 1), (1, 1) ]
                    ])

        , test "getting coords for 2x2 is in order, even if they're flat" <|
            \_ ->
                Expect.equal
                    (TextPlane.make (2, 2) testPlane2x2 |> Plane2D.coordsFlat)
                    (
                        [ (0, 0), (1, 0), (0, 1), (1, 1) ]
                    )

        , test "materializes 2x2 plane properly" <|
            \_ ->
                Expect.equal
                    (TextPlane.make (2, 2) testPlane2x2 |> Plane2D.materialize)
                    (
                        [ [ ((0, 0), Just '1'), ((1, 0), Just '2') ]
                        , [ ((0, 1), Just '3'), ((1, 1), Just '4') ]
                    ])

        , test "materializes 2x2 plane properly to the flat list" <|
            \_ ->
                Expect.equal
                    (TextPlane.make (2, 2) testPlane2x2 |> Plane2D.materializeFlatten)
                    (
                        [ ((0, 0), Just '1')
                        , ((1, 0), Just '2')
                        , ((0, 1), Just '3')
                        , ((1, 1), Just '4')
                        ]
                    )

        , test "materializes 2x2 plane properly to the flat list with only existing values" <|
            \_ ->
                Expect.equal
                    (TextPlane.make (2, 2) testPlane2x2 |> Plane2D.materializeExists)
                    (
                        [ ((0, 0), '1')
                        , ((1, 0), '2')
                        , ((0, 1), '3')
                        , ((1, 1), '4')
                        ]
                    )

        , test "rotates 2x2 plane properly" <|
            \_ ->
                Expect.equal
                    (TextPlane.make (2, 2) testPlane2x2 |> Plane2D.rotate |> Plane2D.materialize)
                    (
                        [ [ ((0, 0), Just '3'), ((1, 0), Just '1') ]
                        , [ ((0, 1), Just '4'), ((1, 1), Just '2') ]
                    ])

        , test "flips 2x2 plane properly" <|
            \_ ->
                Expect.equal
                    (TextPlane.make (2, 2) testPlane2x2 |> Plane2D.flip |> Plane2D.materialize)
                    (
                        [ [ ((0, 0), Just '3'), ((1, 0), Just '4') ]
                        , [ ((0, 1), Just '1'), ((1, 1), Just '2') ]
                    ])

        ,
            [
                ( Offset (-1, -1)
                ,
                    [ [ ((0, 0), Just '4'), ((1, 0), Nothing ) ]
                    , [ ((0, 1), Nothing),  ((1, 1), Nothing ) ]
                    ]
                )
            ,
                ( Offset (-1, 0)
                ,
                    [ [ ((0, 0), Just '2'), ((1, 0), Nothing ) ]
                    , [ ((0, 1), Just '4'), ((1, 1), Nothing ) ]
                    ]
                )
            ,
                ( Offset (1, 0)
                ,
                    [ [ ((0, 0), Just '1'), ((1, 0), Nothing ) ]
                    , [ ((0, 1), Just '3'), ((1, 1), Nothing ) ]
                    ]
                )
            ] |> List.map
                (\(Offset (x, y) as offset, sample) ->
                    test ("shifts to " ++ String.fromInt x ++ "," ++ String.fromInt y) <|
                        \_ ->
                            Expect.equal
                            (TextPlane.make (2, 2) testPlane2x2
                                |> Plane2D.shift offset
                                |> Plane2D.materialize)
                            sample
                )
            |> describe "shifts 2x2 plane properly"

        ,
            [ (Offset (-1, -1), [ (0, 0) ])
            , (Offset ( 0, -1), [ (0, 0), (1, 0) ])
            , (Offset ( 1, -1), [ (1, 0) ])
            , (Offset (-1,  0), [ (0, 0), (0, 1) ])
            , (Offset ( 0,  0), [ (0, 0), (1, 0), (0, 1), (1, 1) ])
            , (Offset ( 1,  0), [ (1, 0), (1, 1) ])
            , (Offset (-1,  1), [ (0, 1) ])
            , (Offset ( 0,  1), [ (0, 1), (1, 1) ])
            , (Offset ( 1,  1), [ (1, 1) ])
            ] |> List.map
                (\(Offset (x, y) as offset, sample) ->
                    test ("for offset " ++ String.fromInt x ++ "," ++ String.fromInt y) <|
                        \_ ->
                            Expect.equal
                            (TextPlane.make (2, 2) testPlane2x2
                                |> Plane2D.overlappingCoords offset)
                            sample
                )
            |> describe "calculates overlapping points for 2x2 plane properly"
        ]
