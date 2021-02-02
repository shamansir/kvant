module Kvant.Tiles exposing (..)


import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)



import Kvant.Plane exposing (Plane)
import Kvant.Adjacency as A


type alias TileKey = String


type alias Format = String


type alias Rotation = Int


maxRotations = 4


type Symmetry
    = I | X | L | T | S -- S meaning Slope


type alias TileInfo =
    { key : String
    , symmetry : Maybe Symmetry
    , weight : Maybe Float
    }


type alias TileSet = ( Format, List TileInfo )


type alias TilesPlane = Plane (TileKey, Rotation)


type alias Adjacency = A.Adjacency (TileKey, Rotation) (TileKey, Rotation)


type alias TileGrid = Array (Array (TileKey, Rotation))


type alias Rule =  -- TODO: allow directions
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
        _ -> Nothing


symmetryToString : Symmetry -> String
symmetryToString symmetry =
    case symmetry of
        I -> "I"
        X -> "X"
        L -> "L"
        T -> "T"
        S -> "\\"

keyRotFromString : String -> ( TileKey, Rotation )
keyRotFromString str =
    case String.split " " str of
        key::rotation::_ ->
            ( key, String.toInt rotation |> Maybe.withDefault 0 )
        [ key ] ->
            ( key, 0 )
        [] ->
            ( noTile, 0 )

