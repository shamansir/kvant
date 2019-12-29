module WFC.Plane.Plane exposing (..)


import Array
import Dict
import Dict exposing (Dict)

-- import WFC.Neighbours exposing (..)
-- import WFC.Occurence exposing (Occurence)
-- import WFC.Occurence as Occurence


-- type Plane x v a = Plane x (v -> Maybe a)

-- TODO: use AnyDict (https://github.com/turboMaCk/any-dict/tree/2.1.0) for those kind of storages?


type Plane v a = Plane v (v -> Maybe a)
 -- TODO: should check incoming v's by `v -> Bool` before, like, if they fit?


type alias Cell v a = (v, Maybe a)


type N v = N v -- FIXME: should be N Int, actually, since all patterns should have equal sides


empty : v -> Plane v a
empty size = Plane size <| always Nothing


get : Plane v a -> v -> Maybe a
get (Plane _ f) = f


getSize : Plane v a -> v
getSize (Plane size _) = size


map : (a -> b) -> Plane v a -> Plane v b
map f (Plane size srcF) =
    Plane size (Maybe.map f << srcF)


transform : (v -> v) -> Plane v a -> Plane v a
transform f (Plane size srcF) =
    Plane size (srcF << f)


transformBy : (v -> z) -> (z -> v) -> Plane v a -> Plane z a
transformBy vToZ zToV (Plane size srcF) =
    Plane (vToZ size) (srcF << zToV)


adjust : (Cell v a -> Maybe b) -> Plane v a -> Plane v b
adjust f (Plane size srcF) =
    Plane size (\coord -> f (coord, srcF coord))


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
