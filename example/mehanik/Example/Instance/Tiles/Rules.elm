module Example.Instance.Tiles.Rules exposing (..)

import Dict exposing (Dict)
import Array
import Xml.Decode as D exposing (..)


import Example.Instance.Tiles exposing (..)


type alias AliasToName = Dict String String


-- type alias TileSpec = { alias : String, name : String }


decoder : Decoder TilingRules
decoder =
    (path [ "tiles", "tile" ] <| list tilesSpecDecoder)
    |> D.map Dict.fromList
    |> D.andThen
        (\aliasToName ->
            (path [ "grid" ] <| single <| tilesGridDecoder aliasToName)
                |> D.map (Array.fromList >> Array.map Array.fromList)
                |> D.map FromGrid
        )



tilesSpecDecoder : Decoder ( String, String )
tilesSpecDecoder =
    D.map2
        Tuple.pair
        (stringAttr "alias")
        (stringAttr "name")


tilesGridDecoder : AliasToName -> Decoder (List (List String))
tilesGridDecoder aliasToName =
    (path [ "row" ] <| list <| stringAttr "tiles")
        |> D.map (List.map unwrapRow)
        |> D.map
            (List.map
                (List.map
                    (\alias_ -> Dict.get alias_ aliasToName |> Maybe.withDefault alias_)
                )
            )


unwrapRow : String -> List String
unwrapRow =
    String.split ":"
        >> List.foldl
            (\tile prev ->
                case String.split "*" tile of
                    t::countStr::_ ->
                        case String.toInt countStr of
                            Just count -> List.repeat count t ++ prev
                            Nothing -> tile :: prev
                    _ -> tile :: prev
            )
            []
        >> List.reverse


decode : String -> Result String TilingRules
decode = run decoder
