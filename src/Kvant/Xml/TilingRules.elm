module Kvant.Xml.TilingRules exposing (..)


import Xml.Decode as D


import Kvant.TilingRules exposing (TilingRules(..))
import Kvant.Xml.Tiles exposing (decoder)


decode : String -> Result String TilingRules
decode =
    decoder
        |> D.map (\(tileSet, tileGrid) -> FromGrid tileSet tileGrid)
        |> D.run
