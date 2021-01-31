module Kvant.Json.Matches exposing (..)


import Json.Encode as E
import Json.Decode as D


import Kvant.Matches exposing (..)
import Kvant.Matches as Matches
import Kvant.Neighbours as Neighbours
import Kvant.Neighbours exposing (Neighbours)


encode : Matches Int -> E.Value
encode =
    Matches.toList >> E.list E.int


encodeNeighbours : Neighbours (Matches Int) -> E.Value
encodeNeighbours neighbours =
    neighbours
        |> Neighbours.toList
        |> List.map
            (\(dir, matches) ->
                case Neighbours.offsetFor dir of
                    ( x, y ) ->
                        E.object
                            [ ( "x", E.int x )
                            , ( "y", E.int y )
                            , ( "dir", E.string <| Neighbours.dirToString dir )
                            , ( "matches", encode matches )
                            ]
            )
        |> E.list identity


decode : D.Decoder (Matches Int)
decode =
    D.list D.int
        |> D.map Matches.fromList


decodeNeighbours : D.Decoder (Neighbours (Matches Int))
decodeNeighbours =
    D.list
        (D.map2
            (\maybeDir matches ->
                maybeDir |>
                    Maybe.map (\dir -> ( dir, matches) )
            )
            (D.field "dir" D.string |> D.map Neighbours.dirFromString)
            (D.field "matches" decode)
        )
    |> D.map (List.filterMap identity)
    |> D.map Neighbours.fromList
    |> D.map (Neighbours.map <| Maybe.withDefault Matches.none)
