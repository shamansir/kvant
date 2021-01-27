module Kvant.TilingRules exposing (..)


import Kvant.Tiles exposing (TileGrid, TileSet)


type TilingRules
    = FromGrid TileSet TileGrid
    | FromRules ()


