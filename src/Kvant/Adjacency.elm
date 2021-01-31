module Kvant.Adjacency exposing (..)

import Array exposing (Array)

import Kvant.Tiles exposing (TileKey, Rotation)


type alias Repetition = Int


type alias TileGrid = Array (Array (TileKey, Rotation))


-- TODO: Adjacency a = Dict Offset (List a) --> Patterns: PatternId, Tiles: TileId

-- TODO: Adjacencies a = Dict a (Adjacency a)

-- or just : Ajacency a = Dict a (Dict Offset (List a))


type Adjacency
    = FromGrid TileGrid -- > fromGrid -> UniquePatterns (containing Dict PatternId (Adjacency PatternId))
    | FromRules (List Rule) -- > Dict TileId (Adjacency TileId)


type alias Rule =  -- TODO: allow directions
    { left : ( TileKey, Rotation )
    , right : ( TileKey, Rotation)
    }
