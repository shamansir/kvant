module WFC.Cross
    exposing (..)


import WFC.Neighbours exposing (..)


type Direction
    = N | W | X | S | E


type Cross a = Cross (Neighbours (Maybe a))
