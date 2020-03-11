module Plane2d exposing (suite)

import Dict

import Expect exposing (Expectation, pass)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
-- import Test.Internal as Internal

import Kvant.Plane.Offset exposing (Offset(..))
import Kvant.Plane exposing (N(..))
import Kvant.Plane.Flat as Plane2D
import Kvant.Plane.Impl.Text as TextPlane
import Kvant.Plane.Offset as OffsetPlane
-- import Kvant.Frequency


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

        , describe "shifts 2x2 plane properly" (
            [
                ( Offset (-1, -1)
                ,
                    [ [ ((0, 0), Just '4'), ((1, 0), Nothing) ]
                    , [ ((0, 1), Nothing),  ((1, 1), Nothing) ]
                    ]
                )
            ,
                ( Offset (-1, 0)
                ,
                    [ [ ((0, 0), Just '2'), ((1, 0), Nothing) ]
                    , [ ((0, 1), Just '4'), ((1, 1), Nothing) ]
                    ]
                )
            ,
                ( Offset (1, 0)
                ,
                    [ [ ((0, 0), Nothing), ((1, 0), Just '1') ]
                    , [ ((0, 1), Nothing), ((1, 1), Just '3') ]
                    ]
                )
            ,
                ( Offset (0, 1)
                ,
                    [ [ ((0, 0), Nothing),  ((1, 0), Nothing) ]
                    , [ ((0, 1), Just '1'), ((1, 1), Just '2') ]
                    ]
                )
            ] |> List.map
                (\(Offset (x, y) as offset, sample) ->
                    test ("shifts to " ++ String.fromInt x ++ "," ++ String.fromInt y) <|
                        \_ ->
                            Expect.equal
                            (TextPlane.make (2, 2) testPlane2x2
                                |> Plane2D.shiftCut offset
                                |> Plane2D.materialize)
                            sample
                )
        )

        , describe "calculates overlapping points for 2x2 plane properly" (
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
        )

        , describe "stringify works" <|
            ( [ (testPlane2x2, (2, 2), "1234") ]
            |> List.map
                (\(plane, (w, h), expectedStr) ->
                    test ("for the plane of size " ++ String.fromInt w ++ "," ++ String.fromInt h)
                        <| \_ ->
                            Expect.equal
                            (TextPlane.make (w, h) plane |> TextPlane.toString)
                            expectedStr
                )
            )

        , test "getting all views is working" <|
            \_ ->
                let
                    sample
                        =  "12"
                        ++ "34"
                    expectedViews =
                        -- Initial
                        [  "12"
                        ++ "34"
                        -- rotate
                        ,  "31"
                        ++ "42"
                        -- rotate > rotate
                        ,  "43"
                        ++ "21"
                        -- rotate > rotate > rotate
                        ,  "24"
                        ++ "13"
                        -- flip
                        ,  "34"
                        ++ "12"
                        -- rotate > flip
                        ,  "42"
                        ++ "31"
                        -- rotate > rotate > flip
                        ,  "21"
                        ++ "43"
                        -- flip > rotate
                        ,  "13"
                        ++ "24"
                        ]
            in
                Expect.equal
                    (Plane2D.allViews (TextPlane.make (2, 2) sample)
                        |> List.map TextPlane.toString)
                    expectedViews

        , test "properly finds matches to the known sample" <|
            \_ ->
                let
                    sample
                        =  "00"
                        ++ "01"
                    patterns =
                        -- 0
                        [  "00"
                        ++ "01"
                        -- 1
                        ,  "00"
                        ++ "10"
                        -- 2
                        ,  "01"
                        ++ "00"
                        -- 3
                        ,  "10"
                        ++ "00"
                        -- 4
                        ,  "00"
                        ++ "11"
                        -- 5
                        ,  "01"
                        ++ "01"
                        -- 6
                        ,  "10"
                        ++ "10"
                        -- 7
                        ,  "11"
                        ++ "00"
                        -- 8
                        ,  "11"
                        ++ "12"
                        -- 9
                        ,  "11"
                        ++ "21"
                        -- 10
                        ,  "12"
                        ++ "11"
                        -- 11
                        ,  "21"
                        ++ "11"
                        ]
                    expectations =
                        [ ((-1, -1), [1, 2, 3, 6, 7])
                        , (( 0, -1), [2, 3, 7])
                        , (( 1, -1), [0, 2, 3, 5, 7])
                        , ((-1,  0), [1, 3, 6])
                        , (( 0,  0), [0])
                        , (( 1,  0), [1, 4])
                        , ((-1,  1), [0, 1, 3, 4, 6])
                        , (( 0,  1), [2, 5])
                        , (( 1,  1), [3, 6, 7, 8, 9, 10])
                        ]
                    samplePlane = TextPlane.make (2, 2) sample
                in
                    Expect.equal
                            (Plane2D.findMatches
                                (patterns
                                    |> List.map (TextPlane.make (2, 2))
                                    |> List.indexedMap Tuple.pair
                                    |> Dict.fromList)
                                samplePlane
                                |> OffsetPlane.materializeExists)
                            expectations

        , test "properly finds subplanes"
            <| \_ ->
                let
                    expectations =
                        [ (
                            (0, 0), (2, 2)
                            ,  "00"
                            ++ "01"
                        )
                        , (
                            (0, 0), (3, 3)
                            ,  "000"
                            ++ "011"
                            ++ "012"
                        )
                        , (
                            (1, 1), (2, 2)
                            ,  "11"
                            ++ "12"
                        )
                        , (
                            (2, 2), (2, 2)
                            ,  "21"
                            ++ "11"
                        )
                        , (
                            (1, 1), (3, 3)
                            ,  "111"
                            ++ "121"
                            ++ "111"
                        )
                        , (
                            (0, 1), (3, 3)
                            ,  "011"
                            ++ "012"
                            ++ "011"
                        )
                        , (
                            (0, 1), (2, 3)
                            ,  "01"
                            ++ "01"
                            ++ "01"
                        )
                        , (
                            (3, 3), (1, 1)
                            ,  "1"
                        )
                        ]
                in
                    Expect.equal
                        (expectations
                            |> List.map (\(shift, size, _) ->
                                testPlane4x4_
                                    |> TextPlane.make (4, 4)
                                    |> Plane2D.subAt shift (N size))
                            |> List.filterMap identity
                            |> List.map TextPlane.toString
                            )
                        (expectations |> List.map (\(_, _, str) -> str))

        , test "materializing subplanes"
            <| \_ ->
                let
                    expectation =
                        [
                            [ ((0,0), Just '0'), ((1,0), Just '1') ],
                            [ ((0,1), Just '0'), ((1,1), Just '1') ],
                            [ ((0,2), Just '0'), ((1,2), Just '1') ]
                        ]
                in
                    Expect.equal
                        (testPlane4x4_
                            |> TextPlane.make (4, 4)
                            |> Plane2D.subAt (0, 1) (N (2, 3))
                            |> Maybe.map (Plane2D.materialize)
                            |> Maybe.withDefault [ [] ])
                        expectation

        , test "finds unique subplanes in the plane"
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
                    Expect.true "aa" True
        ]

