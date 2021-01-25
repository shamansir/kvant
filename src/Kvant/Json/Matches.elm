module Kvant.Json.Matches exposing (..)


import Kvant.Matches exposing (..)
import Kvant.Matches as Matches
import Json.Encode as E


encode : Matches Int -> E.Value
encode =
    Matches.toList >> E.list E.int

