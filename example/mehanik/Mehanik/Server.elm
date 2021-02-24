module Mehanik.Server exposing (..)


import Http
import Bytes exposing (Bytes)
import Image exposing (Image)

import Dict
import Json.Decode as D
import Xml.Decode as XD
import Either exposing (Either(..))

import Kvant.Xml.Tiles as Tiles


serverUrl = "https://kraken.labs.jb.gg/tiles"
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


requestRules
    :  String
    -> (Result String Tiles.TileSet -> msg)
    -> Cmd msg
requestRules set gotTilesetRules =
    Http.get
        { url = storageUrl ++ "/tiles/" ++ set ++ "/data.xml"
        , expect = expectTileRules
        }
    |> Cmd.map gotTilesetRules


--saveSolution : Cmd msg

expectTileRules : Http.Expect (Result String Tiles.TileSet)
expectTileRules =
    Http.expectString
            (Result.mapError errorToString
            >> Result.andThen (XD.run Tiles.decode)
            )


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Http.Timeout ->
            "Unable to reach the server, try again"
        Http.NetworkError ->
            "Unable to reach the server, check your network connection"
        Http.BadStatus 500 ->
            "The server had a problem, try again later"
        Http.BadStatus 400 ->
            "Verify your information and try again"
        Http.BadStatus _ ->
            "Unknown error"
        Http.BadBody errorMessage ->
            errorMessage


loadImage : Http.Response Bytes -> Result Http.Error Image
loadImage response =
    case response of
        Http.BadUrl_ url ->
          Err (Http.BadUrl url)

        Http.Timeout_ ->
          Err Http.Timeout

        Http.NetworkError_ ->
          Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
          Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ _ bytes ->
          case Image.decode bytes of
            Just image ->
              Ok image

            Nothing ->
              Err (Http.BadBody "Image failed to be decoded")
