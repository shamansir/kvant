module Example.Render exposing (..)


import Array

import Html exposing (..)
import Html.Attributes exposing (..)

import Kvant.Plane exposing (..)


type alias Renderer fmt a target =
    ( fmt -> target
    , Plane a -> target
    )
