module Kvant.Cardinal
    exposing (..)


import Kvant.Neighbours exposing (..)


type Direction
    = N | W | X | S | E


type Cardinal a = Cardinal (Neighbours (Maybe a))
