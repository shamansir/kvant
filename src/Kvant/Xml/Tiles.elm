module Kvant.Xml.Tiles exposing (..)


import Dict exposing (Dict)
import Set
import Array
import Xml.Decode as D exposing (..)


import Kvant.Tiles exposing (..)


type alias AliasToName = Dict String String


-- type alias TileSpec = { alias : String, name : String }


decoder : Decoder ( TileSet, TileGrid )
decoder =
    (path [ "tiles", "tile" ] <| list tilesSpecDecoder)
    |> D.map
        (\tiles ->
            ( buildTileset <| Set.fromList <| List.map Tuple.first <| tiles
            , Dict.fromList <| List.map (\(name, alias) -> (alias, name)) <| tiles )
        )
    |> D.andThen
        (\( tileSet, aliasToName ) ->
            (path [ "grid" ] <| single <| tilesGridDecoder aliasToName)
                |> D.map (Array.fromList >> Array.map Array.fromList)
                |> D.map (Tuple.pair tileSet)
        )



tilesSpecDecoder : Decoder ( String, String )
tilesSpecDecoder =
    D.map2
        Tuple.pair
        (stringAttr "name")
        (stringAttr "alias")


tilesGridDecoder : AliasToName -> Decoder (List (List String))
tilesGridDecoder aliasToName =
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
        |> D.map
            (List.map
                (List.map
                    (\alias_ -> Dict.get alias_ aliasToName |> Maybe.withDefault alias_)
                )
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
