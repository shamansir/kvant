module WFC.Neighbours
    exposing (..)


-- TODO: Move to Plane.Walker

type Neighbours a =
    Neighbours
        a a a
        a a a -- we could exclude center
        a a a


type Direction
    = NW | N | NE
    |  W | X | E
    | SW | S | SE


none : Neighbours (List a)
none =
    Neighbours
        [] [] []
        [] [] []
        [] [] []


fill : a -> Neighbours a
fill a =
    Neighbours
        a a a
        a a a
        a a a


map : (a -> b) -> Neighbours a -> Neighbours b
map f neighbours =
    let
        (Neighbours
            nw n ne
             w x e
            sw s se) =
                neighbours
    in
        Neighbours
            (f nw) (f n) (f ne)
            (f  w) (f x) (f e )
            (f sw) (f s) (f se)


create : (Direction -> a) -> Neighbours a
create f =
    Neighbours
        (f NW) (f N) (f NW)
        (f  W) (f X) (f E )
        (f SW) (f S) (f SE)


apply : (a -> Direction -> b) -> Neighbours a -> Neighbours b
apply f neighbours =
    let
        (Neighbours
            nw n ne
             w x e
            sw s se) =
                neighbours
    in
        Neighbours
            (f nw NW) (f n N) (f ne NE)
            (f  w  W) (f x X) (f e  E )
            (f sw SW) (f s S) (f se SE)



allDirections : List Direction
allDirections =
    [ NW, N, NE, W, X, E, SW, S, SE ]


offsets : Neighbours ( Int, Int )
offsets = create offsetFor


offsetFor : Direction -> ( Int, Int )
offsetFor direction =
    case direction of
        NW -> ( -1, -1 )
        N  -> (  0, -1 )
        NE -> (  1, -1 )
        W  -> ( -1,  0 )
        X  -> (  0,  0 )
        E  -> (  1,  0 )
        SW -> ( -1,  1 )
        S  -> (  0,  1 )
        SE -> (  1,  1 )


move : ( Int, Int ) -> Direction -> ( Int, Int )
move (x, y) direction =
    case offsetFor direction of
        ( offX, offY ) -> ( x + offX, y + offY )


get : Direction -> Neighbours a -> a
get dir neighbours =
    let
        (Neighbours
            nw n ne
             w x e
            sw s se) =
                neighbours
    in
        case dir of
            NW -> nw
            N  -> n
            NE -> ne
            W  -> w
            X  -> x
            E  -> e
            SW -> sw
            S  -> s
            SE -> se



collect : v -> (v -> Direction -> v) -> (v -> Maybe a) -> Neighbours (Maybe a)
collect focus customMove f =
    create (f << customMove focus)


collectFlat : ( Int, Int ) -> ((Int, Int) -> Maybe a) -> Neighbours (Maybe a)
collectFlat focus = collect focus move
