module Kvant.Xml.Tiles exposing (..)


import Xml.Decode as D

import Kvant.Tiles exposing (..)
import Kvant.Symmetry as Symmetry

-- type alias TileSpec = { name : String, symmetry : .., weight : .. }


decode : D.Decoder TileSet
decode =
    D.map2
        Tuple.pair
        (D.path [ "tiles" ]
            <| D.single
            <| D.map (Maybe.withDefault "png")
            <| D.maybe <| D.stringAttr "format"
        )
        (D.path [ "tiles", "tile" ]
            <| D.list decodeTileInfo
        )


decodeTileInfo : D.Decoder TileInfo
decodeTileInfo =
    D.map3
        TileInfo
        (D.stringAttr "name")
        (D.map (Maybe.andThen Symmetry.fromString) <| D.maybe <| D.stringAttr "symmetry")
        (D.maybe <| D.floatAttr "weight")
