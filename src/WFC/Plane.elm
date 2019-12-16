module WFC.Plane exposing (..)


type Plane pos a = Plane (pos -> Maybe a)
