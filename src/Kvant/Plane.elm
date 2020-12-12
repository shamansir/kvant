module Kvant.Plane exposing (..)


import Array
import Dict
import Dict exposing (Dict)

import Kvant.Neighbours exposing (Neighbours)
import Kvant.Neighbours as Neighbours exposing (..)

-- import Kvant.Neighbours exposing (..)
-- import Kvant.Occurence exposing (Occurence)
-- import Kvant.Occurence as Occurence


-- type Plane x v a = Plane x (v -> Maybe a)

-- TODO: use AnyDict (https://github.com/turboMaCk/any-dict/tree/2.1.0) for those kind of storages?


type Plane v a = Plane v (v -> Maybe a)
 -- TODO: should check incoming v's by `v -> Bool` before, like, if they fit?
 --       or some advancing, like `v -> Direction -> Maybe v`? which could never end?
 -- TODO: Include Walker and default value in every plane
 -- TODO: store privately some stack counter since if we change a function a lot of times but keep
 --       it dependent on the previous function, it will require more and more memory to hold,
 --       getting over the stack limit could trigger the recalculation of all the items and using --       the dictionary as the backing function (`coords` -> `f coord` -> `Dict` -> make `f` from
 --       `Dict.get`, I would name it `refill`), which is slow, but guarantees not to blow up
 --       the Plane at some point.

type alias Cell v a = (v, Maybe a)


type N v = N v -- FIXME: should be N Int, actually, since all patterns should have equal sides


empty : v -> Plane v a
empty size = Plane size <| always Nothing


get : v -> Plane v a -> Maybe a
get v (Plane _ f) = f v


-- FIXME: could overflow the stack if the Plane is not backed by Dict
-- Temporary solution: Convert to List/Dict and back after a bunch of such operations
set : v -> a -> Plane v a -> Plane v a
set pos value (Plane size f) =
    Plane size <| \otherPos ->
        -- FIXME: `pos` should be `comparable` for that comparison! use `Walker`
        if otherPos == pos then Just value else f otherPos


adjust : (Cell v a -> Maybe b) -> Plane v a -> Plane v b
adjust f (Plane size srcF) =
    Plane size (\coord -> f (coord, srcF coord))


adjustAt : (a -> a) -> v -> Plane v a -> Plane v a
adjustAt f pos plane =
     -- FIXME: since uses `Plane.set`, same speed problems are applied here
    case plane |> get pos of
        Just v -> plane |> set pos (f v)
        Nothing -> plane


getSize : Plane v a -> v
getSize (Plane size _) = size


map : (a -> b) -> Plane v a -> Plane v b
map f (Plane size srcF) =
    Plane size (srcF >> Maybe.map f)


andThen : (a -> Maybe b) -> Plane v a -> Plane v b
andThen f (Plane size srcF) =
    Plane size (srcF >> Maybe.andThen f)


filled : v -> a -> Plane v a
filled size v =
    Plane size <| always <| Just v


transform : (v -> v) -> Plane v a -> Plane v a
transform f (Plane size srcF) =
    Plane size (srcF << f)


transformBy : (v -> z) -> (z -> v) -> Plane v a -> Plane z a
transformBy vToZ zToV (Plane size srcF) =
    Plane (vToZ size) (srcF << zToV)


equalAt : List v -> Plane v a -> Plane v a -> Bool
equalAt atCoords (Plane _ aF) (Plane _ bF) =
    -- FIXME: use `equal`
    atCoords
        |> List.foldl
            (\v before -> before && (aF v == bF v))
            True


cellToMaybe : Cell v a -> Maybe (v, a)
cellToMaybe (v, maybeVal) =
    maybeVal |> Maybe.map (Tuple.pair v)


fromList : comparable -> List (Cell comparable a) -> Plane comparable a
fromList size list =
    list
        |> List.map cellToMaybe
        |> List.filterMap identity
        |> Dict.fromList
        |> fromDict size


fromDict : comparable -> Dict comparable a -> Plane comparable a
fromDict size dict =
    Plane size <| \v -> Dict.get v dict


-- TODO: When Walker will be inside the Panel, we may use `Walker.all` for that purpose
toList : List v -> Plane v a -> List (Cell v a)
toList all (Plane _ f) =
    all |> List.map (\v -> (v, f v))


toDict : List comparable -> Plane comparable a -> Dict comparable a
toDict all =
    toList all
        >> List.map cellToMaybe
        >> List.filterMap identity
        >> Dict.fromList


-- FIXME: could overflow the stack if the Plane is not backed by Dict
-- Temporary solution: Convert to List/Dict and back after a bunch of such operations
add : v -> a -> Plane v a -> Plane v a
add v a =
    adjust
        (\(otherV, cur) ->
            if otherV == v then Just a else cur
        )


addFrom : List (comparable, a) -> Plane comparable a -> Plane comparable a
addFrom values =
    let
        valuesDict = values |> Dict.fromList
    in
        adjust
            (\(v, cur) ->
                valuesDict
                        |> Dict.get v
                        |> Maybe.map Just
                        |> Maybe.withDefault cur)


-- addBy : (v -> Maybe a) -> Plane v a -> Plane v a
-- addBy f (Plane size srcF) =
--     Plane size (\v -> f v |> (Maybe.withDefault <| srcF v))


loadNeighbours : v -> (v -> Direction -> v) -> Plane v a -> Neighbours (Maybe a)
loadNeighbours focus move (Plane _ f) =
    Neighbours.collect focus move f


apply : v -> (v -> Direction -> v) -> Neighbours a -> Plane v a -> Plane v a
apply focus move neighbours plane =
    let
        fromNeighbours = Neighbours.byCoord focus move neighbours
    in
        plane
            |> adjust
                (\(v, maybeCur) ->
                    fromNeighbours v
                        |> Maybe.map Just
                        |> Maybe.withDefault maybeCur
                )
