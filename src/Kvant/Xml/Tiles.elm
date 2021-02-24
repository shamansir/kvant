module Kvant.Xml.Tiles exposing (..)


import Either exposing (Either(..))

import Xml.Decode as D

import Kvant.Tiles exposing (..)
import Kvant.Symmetry as Symmetry
import Kvant.Xml.Adjacency as Adjacency

-- type alias TileSpec = { name : String, symmetry : .., weight : .. }


type alias Format = String


type Rotations
    = Manual
    | Unique


type alias TileSet =
    { format : Format
    , tiles : List TileInfo
    , rules : Either (List Rule) TileGrid
    , rotations : Rotations
    }


decode : D.Decoder TileSet
decode =
    D.map4
        TileSet
        (D.path [ "tiles" ]
            <| D.single
            <| D.map (Maybe.withDefault "png")
            <| D.maybe <| D.stringAttr "format"
        )
        (D.path [ "tiles", "tile" ]
            <| D.list decodeTileInfo
        )
        (D.map2
            (\maybeRules maybeGrid ->
                case ( maybeRules, maybeGrid ) of
                    ( Just rules, _ ) -> Left rules
                    ( _, Just grid ) -> Right grid
                    _ -> Left []
            )
            (D.maybe Adjacency.decodeRules)
            (D.maybe Adjacency.decodeGrid)
        )
        (D.stringAttr "unique"
            |> D.map (\uniqueStr ->
                case uniqueStr of
                    "True" -> Unique
                    "true" -> Unique
                    _ -> Manual
            )
        )


decodeTileInfo : D.Decoder TileInfo
decodeTileInfo =
    D.map3
        TileInfo
        (D.stringAttr "name")
        (D.map (Maybe.andThen Symmetry.fromString) <| D.maybe <| D.stringAttr "symmetry")
        (D.maybe <| D.floatAttr "weight")
