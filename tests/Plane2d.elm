module Plane2d exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

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


suite : Test
suite =
    describe "The 2D planes (test with Text)"
        [ test "materializes 2x2 plane propely" <|
            \_ ->
                Expect.equal
                    (TextPlane.make (2, 2) testPlane2x2 |> Plane2D.materialize)
                    (
                        [ [ ( (0, 0), Just '1'), ( (1, 0), Just '2') ]
                        , [ ( (0, 1), Just '3'), ( (1, 1), Just '4') ]
                    ])
        ]
