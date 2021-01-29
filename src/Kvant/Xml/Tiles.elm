module Kvant.Xml.Tiles exposing (..)


import Xml.Decode as D

import Kvant.Tiles exposing (..)


-- type alias TileSpec = { name : String, symmetry : .., weight : .. }


decode : D.Decoder TileSet
decode =
    D.map2
        Tuple.pair
        (D.path [ "tiles" ] <| D.single <| D.stringAttr "format")
        (D.path [ "tiles", "tile" ] <| D.list decodeTileInfo)


decodeTileInfo : D.Decoder TileInfo
decodeTileInfo =
    D.map3
        TileInfo
        (D.stringAttr "name")
        (D.map (Maybe.andThen symmetryFromString) <| D.maybe <| D.stringAttr "symmetry")
        (D.maybe <| D.floatAttr "weight")
