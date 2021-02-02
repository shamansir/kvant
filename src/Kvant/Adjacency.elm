module Kvant.Adjacency exposing (..)

import Dict exposing (Dict)

import Kvant.Matches exposing (Matches)
import Kvant.Plane exposing (Offset)


type alias Repetition = Int


type alias SubjectId = Int


type alias Adjacency subj_id subj =
    Dict
        subj_id
        { subject: subj
        , weight : Float
        , matches : Dict Offset (Matches subj_id)
        }



-- fromGrid : TileGrid -> Adjacency (TileKey, Rotation) (TileKey, Rotation)
