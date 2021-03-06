module Kvant.Tiles exposing (..)


import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


import Kvant.Plane exposing (Plane, Offset)
import Kvant.Adjacency as Adjacency exposing (Adjacency)
import Kvant.Neighbours exposing (Cardinal(..))
import Kvant.Neighbours as Neighbours
import Kvant.Direction as D exposing (Direction(..))
import Kvant.Matches as Matches exposing (Matches)
import Kvant.Rotation as Rotation exposing (Rotation(..), RotationId)
import Kvant.Symmetry as Symmetry exposing (Symmetry(..))
import List
import Dict



type alias TileKey = String


type alias TileInfo =
    { key : TileKey
    , symmetry : Maybe Symmetry
    , weight : Maybe Float
    }


type alias TilesPlane = Plane (TileKey, Rotation)


type alias TileAdjacency = Adjacency (TileKey, RotationId) (TileKey, Rotation)


type alias TileGrid = Array (Array (TileKey, Rotation))


type alias Rule =
    { left : ( TileKey, Rotation )
    , right : ( TileKey, Rotation )
    }



type alias TileMapping =
    ( Dict Int ( TileKey, Rotation )
    , Dict ( TileKey, RotationId ) Int -- RotationId is comparable, while Rotation is not
    )


noTile : TileKey
noTile = "none"


buildMapping : List TileInfo -> TileMapping
buildMapping =
    List.concatMap
        (\tile ->
            Rotation.uniqueFor (tile.symmetry |> Maybe.withDefault Symmetry.default)
                |> List.map (Tuple.pair tile.key)
        )
    >> List.indexedMap
        (\index ( key, rot ) ->
            ( ( index, ( key, rot ) )
            , ( ( key, Rotation.toId rot ), index )
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
    Dict.get (key |> Tuple.mapSecond Rotation.toId) toIndex |> Maybe.withDefault -1


fromIndexInSet : TileMapping -> Int -> ( TileKey, Rotation )
fromIndexInSet ( fromIndex , _ ) key =
    Dict.get key fromIndex |> Maybe.withDefault ( noTile, Original )


toIndexGrid : TileMapping -> Array (Array ( TileKey, Rotation )) -> Array (Array Int)
toIndexGrid tileMapping =
    Array.map << Array.map <| toIndexInSet tileMapping


fromIndexGrid : TileMapping -> Array (Array Int) -> Array (Array ( TileKey, Rotation ))
fromIndexGrid tileMapping =
    Array.map << Array.map <| fromIndexInSet tileMapping



keyRotFromString : String -> ( TileKey, Rotation )
keyRotFromString str =
    case String.split " " str of
        key::rotation::_ ->
            ( key,
                String.toInt rotation
                    |> Maybe.map Rotation.fromId
                    |> Maybe.withDefault Rotation.Original
            )
        [ key ] ->
            ( key, Rotation.Original )
        [] ->
            ( noTile, Rotation.Original )


rotateTileTo : Direction -> ( TileKey, Rotation ) -> ( TileKey, Rotation )
rotateTileTo =
    Tuple.mapSecond << Rotation.to


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



{- getPossibleVariants : List TileInfo -> Dict (TileKey, RotationId) TileInfo
getPossibleVariants _ =
    Dict.empty -}


applySymmetry : List TileInfo -> List Rule -> List Rule
applySymmetry tiles rules =
    let
        tilesDict = tiles |> List.map (\t -> ( t.key, t ) ) |> Dict.fromList
    in
        rules
            |> List.map
                (\{ left, right } ->
                    { left =
                        case left of
                            ( key, curRotation ) ->

                                ( key
                                , Rotation.apply
                                    (tilesDict
                                        |> Dict.get key
                                        |> Maybe.andThen .symmetry
                                        |> Maybe.withDefault Symmetry.default)
                                    curRotation
                                )
                    , right =
                        case right of
                            ( key, curRotation ) ->

                                ( key
                                , Rotation.apply
                                    (tilesDict
                                        |> Dict.get key
                                        |> Maybe.andThen .symmetry
                                        |> Maybe.withDefault Symmetry.default)
                                    curRotation
                                )
                    }
                )


buildAdjacencyRules : List TileInfo -> List Rule -> TileAdjacency
buildAdjacencyRules tiles rules =
    let
        tilesDict = tiles |> List.map (\t -> ( t.key, t ) ) |> Dict.fromList

        getWeight tileKey =
            tilesDict
                |> Dict.get tileKey
                |> Maybe.andThen .weight
                |> Maybe.withDefault 1
        getSymmetry tileKey =
            tilesDict
                |> Dict.get tileKey
                |> Maybe.andThen .symmetry
                |> Maybe.withDefault Symmetry.default
        adjacencyDict =
            List.concatMap
                (\info  ->
                    Rotation.uniqueFor (info.symmetry |> Maybe.withDefault Symmetry.default)
                        |> List.map
                            (\rotation ->
                                ( ( info.key, Rotation.toId rotation )
                                ,
                                    { subject = ( info.key, rotation )
                                    , weight = getWeight info.key
                                    , matches = Adjacency.noMatches
                                    }
                                )
                            )
                )
                tiles
                |> Dict.fromList

        addMatchesFromRule dir ( key, rotation ) matches =
            matches
                |> Dict.update
                    (D.toOffset dir)
                    (Maybe.map <|
                        Matches.add
                            (
                                ( key
                                , rotation |> Rotation.toId
                                )
                            )
                    )
    in
        rules
            |> applySymmetry tiles
            |> List.foldl
                    (\rule adjacency ->

                        case ( rule.left, rule.right ) of
                            ( ( lKey, lRotation ), ( rKey, rRotation ) ) ->

                                D.cardinal

                                    |> List.foldl
                                        (\dir ->

                                            Dict.update

                                                ( rKey
                                                , Rotation.toId <| Rotation.to dir rRotation
                                                )

                                                (Maybe.map
                                                    (\curTile ->
                                                        { curTile
                                                        | matches =
                                                            curTile.matches
                                                                |> addMatchesFromRule
                                                                    dir
                                                                    ( lKey
                                                                    , Rotation.to dir lRotation
                                                                        |> Rotation.apply (getSymmetry lKey)
                                                                    )
                                                        }
                                                    )
                                                )

                                            >> Dict.update

                                                ( lKey
                                                , Rotation.toId
                                                    <| Rotation.to (D.opposite dir) lRotation
                                                )

                                                (Maybe.map
                                                    (\curTile ->
                                                        { curTile
                                                        | matches =
                                                            curTile.matches
                                                                |> addMatchesFromRule
                                                                    dir-- (D.opposite dir)
                                                                    ( rKey
                                                                    , Rotation.to (D.opposite dir) rRotation
                                                                        |> Rotation.apply (getSymmetry rKey)
                                                                    )
                                                        }
                                                    )
                                                )

                                        )
                                        adjacency

                    )
                    adjacencyDict




