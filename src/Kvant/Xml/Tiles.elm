module Kvant.Xml.Tiles exposing (..)


import Dict exposing (Dict)
import Set
import Array
import Xml.Decode as D exposing (..)


import Kvant.Tiles exposing (..)


-- type alias TileSpec = { name : String, symmetry : .., weight : .. }


decoder : Decoder ( TileSet, TileGrid )
decoder =
    (path [ "tiles", "tile" ] <| list tilesSpecDecoder)
    |> D.map
        (\tiles ->
            buildTileset <| Set.fromList <| tiles
        )
    |> D.andThen
        (\tileSet ->
            (path [ "grid" ] <| single <| tilesGridDecoder)
                |> D.map (Array.fromList >> Array.map Array.fromList)
                |> D.map (Tuple.pair tileSet)
        )



tilesSpecDecoder : Decoder String
tilesSpecDecoder =
    stringAttr "name"
    {-
    D.map2
        Tuple.pair
        (stringAttr "name")
        (stringAttr "alias")
    -}


tilesGridDecoder : Decoder (List (List String))
tilesGridDecoder =
    (path [ "row" ]
        <| list
        <| path [ "tile" ]
        <| list
        <| D.map2
            Tuple.pair
            (stringAttr "name")
            (maybe <| intAttr "times")
    )
        |> D.map
            (List.map
                (List.map (Tuple.mapSecond <| Maybe.withDefault 1)
                    >> unwrapRow)
                )


unwrapRow : List (String, Int) -> List String
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
    >> List.reverse
