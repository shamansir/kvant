module Mehanik.Server exposing (..)


import Http
import Dict
import Json.Decode as D

import Kvant.Tiles exposing (TileSet)


serverUrl = "https://kraken.labs.jb.gg"
sceneServer = "http://limb.labs.jb.gg"
storageUrl = "https://d3r50642m20jlp.cloudfront.net"


requestTilesets : (Result Http.Error (List String{-TileGroup-}) -> msg) -> Cmd msg
requestTilesets receivedSets =
    Http.get
        { url = serverUrl ++ "/get_tilesets"
        , expect = Http.expectJson receivedSets
            <| D.map Dict.keys
            <| D.field "tile_sets" <| D.dict <| D.list D.string
        }


requestTileset : String -> (Result Http.Error (List String) {-TileSet-} -> msg) -> Cmd msg
requestTileset set receivedTiles =
    Http.get
        { url = serverUrl ++ "/get_tilesets?prefix=" ++ set
        , expect = Http.expectJson receivedTiles
            <| D.map (Dict.get set >> Maybe.withDefault [])
            <| D.field "tile_sets" <| D.dict <| D.list D.string
        }


requestTilesDefinition : String -> (Result Http.Error String {-TileSet-} -> msg) -> Cmd msg
requestTilesDefinition set receivedDescriptionFile =
    Http.get
        { url = storageUrl ++ "/tiles/" ++ set ++ "/data.xml"
        , expect = Http.expectString receivedDescriptionFile
        }

--saveSolution : Cmd msg
