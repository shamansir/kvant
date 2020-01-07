module WFC.Neighbours
    exposing ( Neighbours(..), Direction(..) )


-- TODO: Move to Plane.Walker

type Neighbours a =
    Neighbours
        a a a
        a   a
        a a a


type Direction
    = NW | N | NE
    | W  | X |  E
    | SW | S | SE


none : Neighbours (List a)
none =
    Neighbours
        [] [] []
        []    []
        [] [] []
