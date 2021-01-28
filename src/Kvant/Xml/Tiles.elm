module Kvant.Xml.Tiles exposing (..)


import Xml.Decode as D exposing (..)

import Kvant.Tiles exposing (..)


-- type alias TileSpec = { name : String, symmetry : .., weight : .. }


decode : Decoder TileSet
decode =
    path [ "tiles", "tile" ] <| list decodeTileInfo


decodeTileInfo : Decoder TileInfo
decodeTileInfo =
    D.map3
        TileInfo
        (stringAttr "name")
        (D.map (Maybe.andThen symmetryFromString) <| maybe <| stringAttr "symmetry")
        (maybe <| floatAttr "weight")
