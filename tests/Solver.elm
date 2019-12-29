module Solver exposing (suite)

import Dict

import Expect exposing (Expectation, pass)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
-- import Test.Internal as Internal

import WFC.Plane.Text as TextPlane
import WFC.Solver as Solver
-- import WFC.Frequency


testPlane4x4_
    =  "0000"
    ++ "0111"
    ++ "0121"
    ++ "0111"


suite : Test
suite =
    describe "The Solver"
        [ test "finds unique patterns in the plane"
            <| \_ ->
                let
                    expectToContain =
                        [
                            [  "00"
                            ++ "01"

                            ,  "00"
                            ++ "10"

                            ,  "01"
                            ++ "00"

                            ,  "10"
                            ++ "00"

                            ,  "00"
                            ++ "11"

                            ,  "01"
                            ++ "01"

                            ,  "10"
                            ++ "10"

                            ,  "11"
                            ++ "00"

                            ,  "11"
                            ++ "12"

                            ,  "11"
                            ++ "21"

                            ,  "12"
                            ++ "11"

                            ,  "21"
                            ++ "11"
                            ]

                        ]
                in
                    TextPlane.make (4, 4) testPlane4x4_
                        |> always (Expect.true "aa" True)
        ]

