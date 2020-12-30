module Example.Instance.Tiles.Rules exposing (..)

import Dict exposing (Dict)
import Array
import Xml.Decode as D exposing (..)


import Example.Instance.Tiles exposing (..)


type alias TileMap = Dict String String


decoder : Decoder TilingRules
decoder =
    (path [ "grid" ] <| single tilesGridDecoder)
        |> D.map (Array.fromList >> Array.map Array.fromList)
        |> D.map FromGrid


tilesGridDecoder : Decoder (List (List String))
tilesGridDecoder =
    (path [ "row" ] <| list <| stringAttr "tiles")
        |> D.map (List.map unwrapRow)


unwrapRow : String -> List String
unwrapRow = String.split ":" -- TODO


decode : String -> Result String TilingRules
decode = run decoder
