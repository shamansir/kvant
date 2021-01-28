module Kvant.Adjacency exposing (..)

import Array exposing (Array)

import Kvant.Tiles exposing (TileKey, Rotation)

type alias Repetition = Int


type alias TileGrid = Array (Array (TileKey, Rotation))


type Adjacency
    = FromGrid TileGrid
    | FromRules ()


