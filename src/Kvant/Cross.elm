module Kvant.Cross
    exposing (..)


import Kvant.Neighbours exposing (..)


type Direction
    = N | W | X | S | E


type Cross a = Cross (Neighbours (Maybe a))
