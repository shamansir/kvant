module Kvant.Xml.Adjacency exposing (..)


import Xml.Decode as D
import Array


import Kvant.Tiles exposing (TileKey, Rotation, keyRotFromString)
import Kvant.Adjacency exposing (Adjacency(..), Repetition, TileGrid, Rule)


decode : D.Decoder Adjacency
decode =
    D.map2
        (\maybeRules maybeGrid ->
            case ( maybeRules, maybeGrid ) of
                ( Just [], Just grid ) -> FromGrid grid
                ( Just rules, _ ) -> FromRules rules
                ( _, Just grid ) -> FromGrid grid
                ( Nothing, Nothing ) -> FromRules []
        )
        (D.maybe <| decodeRules)
        (D.maybe <| decodeGrid)


decodeGrid : D.Decoder TileGrid
decodeGrid =
    D.path [ "grid" ]
        <| D.single <| tilesGridDecoder


tilesGridDecoder : D.Decoder TileGrid
tilesGridDecoder =
    (D.path [ "row" ]
        <| D.list
        <| D.path [ "tile" ]
        <| D.list
        <| D.map2
            Tuple.pair
            (D.stringAttr "name")
            (D.maybe <| D.intAttr "times")
    )
        |> D.map
            (List.map
                (List.map (Tuple.mapSecond <| Maybe.withDefault 1)
                    >> unwrapRow)
                )
        |> D.map (Array.fromList >> Array.map Array.fromList)


unwrapRow : List ( String, Repetition ) -> List ( TileKey, Rotation )
unwrapRow =
    List.foldl
        (\(tile, count) prev ->
            if count == 1 then
                tile :: prev
            else if count > 0 then
                List.repeat count tile ++ prev
            else prev
        )
        []
    >> List.map keyRotFromString
    >> List.reverse


decodeRules : D.Decoder (List Rule)
decodeRules =
    D.path [ "neighbors", "neighbor" ] <| D.list decodeRule


decodeRule : D.Decoder Rule
decodeRule =
    D.map2
        Rule
        (D.stringAttr "left" |> D.map keyRotFromString)
        (D.stringAttr "right" |> D.map keyRotFromString)
