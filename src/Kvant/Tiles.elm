module Kvant.Tiles exposing (..)


import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


import Kvant.Plane exposing (Plane, Offset)
import Kvant.Adjacency as Adjacency exposing (Adjacency)
import Kvant.Neighbours exposing (Cardinal(..), Direction)
import Kvant.Neighbours as Neighbours
import Kvant.Neighbours as D exposing (Direction(..))
import Kvant.Matches as Matches exposing (Matches)
import List
import Dict


type alias TileKey = String


type alias Format = String


type alias Rotation = Int


maxRotations = 4


type Symmetry
    = I | X | L | T | S | A | Q -- S meaning Slope, Q means none


type alias TileInfo =
    { key : String
    , symmetry : Maybe Symmetry
    , weight : Maybe Float
    }


type alias TileSet = ( Format, List TileInfo )


type alias TilesPlane = Plane (TileKey, Rotation)


type alias TileAdjacency = Adjacency (TileKey, Rotation) (TileKey, Rotation)


type alias TileGrid = Array (Array (TileKey, Rotation))


type alias Rule =
    { left : ( TileKey, Rotation )
    , right : ( TileKey, Rotation )
    }



type alias TileMapping =
    ( Dict Int ( TileKey, Rotation )
    , Dict ( TileKey, Rotation ) Int
    )


noTile : TileKey
noTile = "none"


buildMapping : TileSet -> TileMapping
buildMapping =
    Tuple.second
        >> List.map .key
        >> List.concatMap
            (List.repeat maxRotations >> List.indexedMap Tuple.pair)
        >> List.indexedMap
            (\index ( rot, key ) ->
                ( ( index, ( key, rot ) )
                , ( ( key, rot ), index )
                )
            )
        >> (\list ->
                ( List.map Tuple.first <| list
                , List.map Tuple.second <| list
                )
           )
        >> Tuple.mapBoth Dict.fromList Dict.fromList


noMapping : TileMapping
noMapping = ( Dict.empty, Dict.empty )


toIndexInSet : TileMapping -> ( TileKey, Rotation ) -> Int
toIndexInSet ( _, toIndex ) key =
    Dict.get key toIndex |> Maybe.withDefault -1


fromIndexInSet : TileMapping -> Int -> ( TileKey, Rotation )
fromIndexInSet ( fromIndex , _ ) key =
    Dict.get key fromIndex |> Maybe.withDefault ( noTile, 0 )


toIndexGrid : TileMapping -> Array (Array ( TileKey, Rotation )) -> Array (Array Int)
toIndexGrid tileMapping =
    Array.map << Array.map <| toIndexInSet tileMapping


fromIndexGrid : TileMapping -> Array (Array Int) -> Array (Array ( TileKey, Rotation ))
fromIndexGrid tileMapping =
    Array.map << Array.map <| fromIndexInSet tileMapping


symmetryFromString : String -> Maybe Symmetry
symmetryFromString str =
    case str of
        "I" -> Just I
        "X" -> Just X
        "L" -> Just L
        "T" -> Just T
        "\\" -> Just S
        "A" -> Just A
        "Q" -> Just Q
        _ -> Nothing


symmetryToString : Symmetry -> String
symmetryToString symmetry =
    case symmetry of
        I -> "I"
        X -> "X"
        L -> "L"
        T -> "T"
        S -> "\\"
        A -> "A"
        Q -> "Q"


keyRotFromString : String -> ( TileKey, Rotation )
keyRotFromString str =
    case String.split " " str of
        key::rotation::_ ->
            ( key, String.toInt rotation |> Maybe.withDefault 0 )
        [ key ] ->
            ( key, 0 )
        [] ->
            ( noTile, 0 )


next : Rotation -> Rotation
next rotation =
    case rotation of
        0 -> 1
        1 -> 2
        2 -> 3
        3 -> 0
        _ -> rotation


rotateTo : Direction -> Rotation -> Rotation
rotateTo dir rotation =
    case dir of
        D.N -> rotation |> next
        D.E -> rotation |> next |> next
        D.S -> rotation |> next |> next |> next
        D.W -> rotation
        _ -> rotation


rotateTileTo : Direction -> ( TileKey, Rotation ) -> ( TileKey, Rotation )
rotateTileTo =
    Tuple.mapSecond << rotateTo


allowedByRule : Rule -> ( TileKey, Rotation ) -> ( TileKey, Rotation ) -> Bool
allowedByRule { left, right } ( tileAtLeft, rotationAtLeft ) ( tileAtRight, rotationAtRight )
    = case ( left, right ) of
        ( ( requiredAtLeft, requiredRotationAtLeft ), ( requiredAtRight, requiredRotationAtRight ) ) ->
            requiredAtLeft == tileAtLeft
                && requiredAtRight == tileAtRight
                && requiredRotationAtLeft == rotationAtLeft
                && requiredRotationAtRight == rotationAtRight


allowedByRules : List Rule -> Direction -> ( TileKey, Rotation ) -> ( TileKey, Rotation ) -> Bool
allowedByRules rules dir leftTile rightTile =
    (rules
        |> List.filter
            (\rule ->
                allowedByRule
                    rule
                    (rotateTileTo dir leftTile)
                    (rotateTileTo dir rightTile)
            )
        |> List.length) > 0


findMatches
    :  List TileInfo
    -> List Rule
    -> ( TileInfo, Rotation )
    -> Dict Offset (Matches (TileKey, Rotation))
findMatches tiles rules ( currentTile, currentRotation ) =
    Neighbours.cardinal
        |> List.foldl
            (\dir neighbours ->
                let
                    byRules
                        = tiles
                            |> List.foldl
                                (\otherTile neighbours_ ->
                                    List.range 0 (maxRotations - 1)
                                        |> List.foldl
                                            (\otherRotation neighbours__ ->
                                                if
                                                    allowedByRules
                                                        rules
                                                        dir
                                                        ( currentTile.key, currentRotation )
                                                        ( otherTile.key, otherRotation )
                                                    || allowedByRules
                                                        rules
                                                        (D.opposite dir)
                                                        ( otherTile.key, otherRotation )
                                                        ( currentTile.key, currentRotation )
                                                    then neighbours__
                                                        |> Neighbours.at dir
                                                            ((::) ( otherTile.key, otherRotation ))
                                                    else neighbours__
                                            )
                                            neighbours_

                                )
                                neighbours
                in byRules
            )
            (Neighbours.fill [])
        |> Neighbours.map Matches.fromList
        |> Neighbours.toDict


uniqueTurns : List TileInfo -> List (TileInfo, Rotation)
uniqueTurns tiles =
    tiles
        |> List.concatMap
            (\info ->
                info.symmetry
                    |> Maybe.withDefault X
                    |> uniqueRotationsBySymmetry
                    |> List.map (Tuple.pair info)
            )


mergeBySymmetry
    :   { subject: ( Symmetry, ( TileKey, Rotation ) )
        , weight : Float
        , matches : Dict Offset (Matches ( TileKey, Rotation ) )
        }
    -> Adjacency
            ( TileKey, Rotation )
            ( Symmetry, ( TileKey, Rotation ) )
    -> Adjacency
            ( TileKey, Rotation )
            ( Symmetry, ( TileKey, Rotation ) )
mergeBySymmetry tile adjacencySoFar =
    let
        ( symmetry, ( key, currentRotation ) ) = tile.subject
    in
        similarRotationsBySymmetry currentRotation symmetry
            |> List.foldl
                (\anotherRotation adjacency_ ->
                    Maybe.map2
                        Adjacency.merge
                        (adjacency_
                            |> Dict.get ( key, currentRotation )
                            |> Maybe.map .matches)
                        (adjacency_
                            |> Dict.get ( key, anotherRotation )
                            |> Maybe.map .matches)
                    |> Maybe.map
                        (\mergedMatches ->
                            adjacency_ |>
                                Dict.insert
                                    ( key, currentRotation )
                                    { subject = tile.subject
                                    , weight = tile.weight
                                    , matches = mergedMatches
                                    }
                        )
                    |> Maybe.withDefault adjacency_
                )
                adjacencySoFar


buildAdjacencyRules : List TileInfo -> List Rule -> TileAdjacency
buildAdjacencyRules tiles rules =
    let
        rulesApplied =
            tiles
                |> List.concatMap
                    (\tile ->
                        List.range 0 (maxRotations - 1)
                            |> List.map (\rotation ->
                                    ( ( tile.key, rotation ), tile )
                                )
                    )
                |> Dict.fromList
                |> Dict.map
                    (\(tileKey, rotation) tile ->
                        { subject = ( tile.symmetry |> Maybe.withDefault Q, ( tileKey, rotation ) )
                        , weight = tile.weight |> Maybe.withDefault 1
                        , matches = ( tile, rotation ) |> findMatches tiles rules
                        }
                    )
    in
        rulesApplied
        |> Dict.foldl (always mergeBySymmetry) rulesApplied
        |> Dict.map
            (\_ tile ->
                { subject = Tuple.second tile.subject
                , weight = tile.weight
                , matches = tile.matches
                }
            )


symmetryToIndices : Symmetry -> Cardinal Int
symmetryToIndices symmetry =
    case symmetry of
        X ->
            Cardinal
                   1
                1  0  1
                   1
        I ->
            Cardinal
                   2
                1  0  1
                   2
        L ->
            Cardinal
                   2
                1  0  2
                   1
        T ->
            Cardinal
                   2
                1  0  1
                   1
        S -> -- a.k.a `\`
            Cardinal
                   2
                1  0  2
                   1
        A ->
            Cardinal
                   1
                1  0  1
                   0
        Q -> -- a.k.a `\`
            Cardinal
                   1
                2  0  4
                   3


matchesBySymmetry : Direction -> Symmetry -> Symmetry -> Bool
matchesBySymmetry dir symmetryA symmetryB =
    (Neighbours.getCardinal dir <| symmetryToIndices symmetryA)
    == (Neighbours.getCardinal (Neighbours.opposite dir) <| symmetryToIndices symmetryB)


rotate : Rotation -> Direction -> Direction
rotate r dir =
    case ( r, dir ) of
        ( 0, _    ) -> dir
        ( _, D.X  ) -> D.X

        ( 1, D.NW ) -> D.NE
        ( 1, D.N  ) -> D.E
        ( 1, D.NE ) -> D.SE
        ( 1, D.E  ) -> D.S
        ( 1, D.SE ) -> D.SW
        ( 1, D.S  ) -> D.W
        ( 1, D.SW ) -> D.NW
        ( 1, D.W  ) -> D.N

        ( 2, D.NW ) -> D.SE
        ( 2, D.N  ) -> D.S
        ( 2, D.NE ) -> D.SW
        ( 2, D.E  ) -> D.W
        ( 2, D.SE ) -> D.NW
        ( 2, D.S  ) -> D.N
        ( 2, D.SW ) -> D.SW
        ( 2, D.W  ) -> D.E

        ( 3, D.NW ) -> D.SW
        ( 3, D.N  ) -> D.W
        ( 3, D.NE ) -> D.NW
        ( 3, D.E  ) -> D.N
        ( 3, D.SE ) -> D.NE
        ( 3, D.S  ) -> D.E
        ( 3, D.SW ) -> D.SE
        ( 3, D.W  ) -> D.S

        ( _, _    ) -> dir


uniqueRotationsBySymmetry : Symmetry -> List Rotation
uniqueRotationsBySymmetry symmetry =
    case symmetry of
        I -> [ 0, 1 ]
        X -> [ 0 ]
        S -> [ 0, 1 ]
        _ -> [ 0, 1, 2, 3 ]


similarRotationsBySymmetry : Rotation -> Symmetry -> List Rotation
similarRotationsBySymmetry rotation symmetry =
    case ( rotation, symmetry ) of

        ( 0, I ) -> [ 2 ]
        ( 1, I ) -> [ 3 ]
        ( 2, I ) -> [ 0 ]
        ( 3, I ) -> [ 1 ]

        ( 0, X ) -> [ 1, 2, 3 ]
        ( 1, X ) -> [ 0, 2, 3 ]
        ( 2, X ) -> [ 0, 1, 3 ]
        ( 3, X ) -> [ 0, 1, 2 ]

        ( 0, S ) -> [ 2 ]
        ( 1, S ) -> [ 3 ]
        ( 2, S ) -> [ 0 ]
        ( 3, S ) -> [ 1 ]

        _ -> [ ]


equalBySymmetry : Symmetry -> Rotation -> Rotation -> Bool
equalBySymmetry symmetry rotA rotB =
    if symmetry == X then True
    else case ( symmetry, rotA, rotB ) of
        ( I, 0, 2 ) -> True
        ( I, 2, 0 ) -> True
        ( I, 1, 3 ) -> True
        ( I, 3, 1 ) -> True
        ( S, 0, 2 ) -> True
        ( S, 2, 0 ) -> True
        ( S, 1, 3 ) -> True
        ( S, 3, 1 ) -> True
        _ -> False


-- TODO:
-- filterBySymmetry : List TileInfo -> List TileInfo
-- filterBySymmetry
--     = List.foldl
--         (info)
--         []


rotationToString : Rotation -> String
rotationToString rotation =
    case rotation of
        0 -> "0°"
        1 -> "90°"
        2 -> "180°"
        3 -> "270°"
        _ -> "?°"
