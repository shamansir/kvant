module Kvant.Neighbours
    exposing (..)


import Dict exposing (Dict)

import Kvant.Vec2 exposing (..)
import Kvant.Direction as Dir exposing (Direction(..))


type Neighbours a =
    Neighbours
        a a a
        a a a -- we could exclude center
        a a a


type Cardinal a =
    Cardinal
          a
        a a a -- we could exclude center
          a

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
map = mapBy << always


mapBy : (Direction -> a -> b) -> Neighbours a -> Neighbours b
mapBy f neighbours =
    let
        (Neighbours
            nw n ne
             w x e
            sw s se) =
                neighbours
    in
        Neighbours
            (f NW nw) (f N n) (f NE ne)
            (f  W  w) (f X x) (f E  e )
            (f SW sw) (f S s) (f SE se)


at : Direction -> (a -> a) -> Neighbours a -> Neighbours a
at dir f = mapBy (\otherDir v -> if otherDir == dir then f v else v)


create : (Direction -> a) -> Neighbours a
create f =
    Neighbours
        (f NW) (f N) (f NE)
        (f  W) (f X) (f E )
        (f SW) (f S) (f SE)


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


set : Direction -> a -> Neighbours a -> Neighbours a
set dir val =
    mapBy
        (\otherDir cur ->
            if otherDir == dir then val else cur
        )


setCenter : a -> Neighbours a -> Neighbours a
setCenter = set X


getCenter : Neighbours a -> a
getCenter = get X


-- TODO: focus should not be needed, just `(Direction -> v)`
collect : v -> (v -> Direction -> v) -> (v -> Maybe a) -> Neighbours (Maybe a)
collect focus customMove f =
    create (f << customMove focus)


toList : Neighbours a -> List (Direction, a)
toList neighbours =
    Dir.all
        |> List.map (\dir -> (dir, get dir neighbours))


fromList : List (Direction, a) -> Neighbours (Maybe a)
fromList =
    List.foldl
        (\(dir, val) ->
            set dir <| Just val
        )
        (fill Nothing)


toDict : Neighbours a -> Dict Vec2 a
toDict =
    toList
        >> List.map (Tuple.mapFirst Dir.toOffset)
        >> Dict.fromList


flatten : Neighbours a -> List a
flatten = toList >> List.map Tuple.second


centerAndOthers : Neighbours a -> (a, List (Direction, a))
centerAndOthers neighbours =
    ( get X neighbours
    , Dir.around
        |> List.map (\dir -> (dir, get dir neighbours)) )


headTail : Neighbours a -> (a, List a)
headTail =
    centerAndOthers >> Tuple.mapSecond (List.map Tuple.second)


foldl : (Direction -> a -> b -> b) -> b -> Neighbours a -> b
foldl f init =
    toList >>
        List.foldl
            (\(dir, val) prev ->
                f dir val prev
            )
            init


load : a -> List (Direction, a) -> Neighbours a
load default =
    List.foldl
        (\(dir, val) neighbours ->
            neighbours |> set dir val
        )
        (fill default)


tryLoad : a -> List a -> Neighbours a
tryLoad default =
    load default
        << List.map2
            Tuple.pair
            Dir.all


-- FIXME: we also need `equals` here
byCoord : v -> (v -> Direction -> v) -> Neighbours a -> (v -> Maybe a)
byCoord focus customMove neighbours =
    \otherCoord ->
        -- FIXME: improve somehow
        if      otherCoord == customMove focus NW then Just <| get NW neighbours
        else if otherCoord == customMove focus N  then Just <| get N  neighbours
        else if otherCoord == customMove focus NE then Just <| get NE neighbours
        else if otherCoord == customMove focus W  then Just <| get W  neighbours
        else if otherCoord == customMove focus X  then Just <| get X  neighbours
        else if otherCoord == customMove focus E  then Just <| get E  neighbours
        else if otherCoord == customMove focus SW then Just <| get SW neighbours
        else if otherCoord == customMove focus S  then Just <| get S  neighbours
        else if otherCoord == customMove focus SE then Just <| get SE neighbours
        else Nothing


offsets : Neighbours Vec2
offsets = create Dir.toOffset


withOffsets : Neighbours a -> Neighbours (Vec2, a)
withOffsets = mapBy (Dir.toOffset >> Tuple.pair)


collectFlat : Vec2 -> (Vec2 -> Maybe a) -> Neighbours (Maybe a)
collectFlat focus = collect focus Dir.move


toString : (a -> String) -> Neighbours a -> String
toString itemToString =
    foldl
        (\dir item strings ->
            (Dir.toString dir ++ " : " ++ itemToString item) :: strings
        )
        []
        >> String.join " "


-- ensure : Neighbours (Maybe a) -> Maybe (Neighbours a)


-- encode : (a -> E.Value) -> Neighbours a -> E.Value


-- decode : D.Decoder a -> D.Decoder (Neighbours a)


toCardinal : Neighbours a -> Cardinal a
toCardinal n =
    Cardinal
                  (get N n)
        (get W n) (get X n) (get E n)
                  (get S n)


fromCardinal : Cardinal a -> Neighbours (Maybe a)
fromCardinal c =
    case c of
        Cardinal n w x e s ->
            Neighbours
                Nothing  (Just n) Nothing
                (Just w) (Just x) (Just e)
                Nothing  (Just s) Nothing
        {- Cardinal _ _ _ _ _ ->
            Neighbours
                Nothing Nothing Nothing
                Nothing Nothing Nothing
                Nothing Nothing Nothing -}

getCardinal : Direction -> Cardinal a -> a
getCardinal dir (Cardinal n w x e s) =
    case dir of
        N -> n
        W -> w
        X -> x
        E -> e
        S -> s
        _ -> x
