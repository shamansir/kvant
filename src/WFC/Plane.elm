module WFC.Plane exposing (..)


type Plane a = Plane (Int -> Int -> Maybe a)
