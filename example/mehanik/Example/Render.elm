module Example.Render exposing (..)


import Kvant.Plane exposing (Plane)


type alias Renderer fmt a target =
    ( fmt -> target
    , Plane a -> target
    )
