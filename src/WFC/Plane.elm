module WFC.Plane exposing (..)


import Array
import Dict
import Dict exposing (Dict)

import WFC.Neighbours exposing (..)
import WFC.Occured exposing (Occured)
import WFC.Occured as Occured


-- type Plane x v a = Plane x (v -> Maybe a)

type Plane v a = Plane v (v -> Maybe a)


type alias Cell v a = (v, Maybe a)


type alias Vec2 = (Int, Int)


type N v = N v -- FIXME: should be N Int, actually, since all patterns should have equal sides


type Offset v = Offset v


type Orientation
    = North
    | West
    | East
    | South


type Flip
    = Horizontal
    | Vertical


type SearchMethod
    = Bounded
    | Periodic


empty : v -> Plane v a
empty size = Plane size <| always Nothing


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


disregardOffsets : Plane (Offset v) a -> Plane v a
disregardOffsets = transformBy (\(Offset off) -> off) Offset


foldMap : (Cell Vec2 a -> b) -> Plane Vec2 a -> List (List b)
foldMap f (Plane _ planeF as plane) =
    coords plane
        |> List.map (List.map <| \v -> f (v, planeF v))


foldl : (Cell Vec2 a -> b -> b) -> b -> Plane Vec2 a -> b
foldl f def plane =
    foldMap identity plane
        |> List.concat
        |> List.foldl f def


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


toDict : Plane Vec2 a -> Dict Vec2 a
toDict plane =
    materializeFlatten plane
        |> List.map cellToMaybe
        |> List.filterMap identity
        |> Dict.fromList


sub : N Vec2 -> Plane Vec2 a -> Maybe (Plane Vec2 a)
sub = subAt (0, 0)


subAt : Vec2 -> N Vec2 -> Plane Vec2 a -> Maybe (Plane Vec2 a)
subAt (shiftX, shiftY) (N (nX, nY)) (Plane (srcWidth, srcHeight) planeF) =
    if (shiftX + nX <= srcWidth) && (shiftY + nY <= srcHeight) then
        Just (Plane (nX, nY)
            <| \(x, y) ->
                if (x < nX) && (y < nY) then
                    planeF (x + shiftX, y + shiftY)
                else
                    Nothing
            )
    else Nothing


periodicSubAt : Vec2 -> N Vec2 -> Plane Vec2 a -> Plane Vec2 a
periodicSubAt (shiftX, shiftY) (N (nX, nY)) (Plane (srcWidth, srcHeight) planeF) =
    let
        periodicCoord (x, y) =
            ( if shiftX + x >= 0
                then shiftX + x |> modBy srcWidth
                else srcWidth - (abs (shiftX + x) |> modBy srcWidth)
            , if shiftY + y >= 0
                then shiftY + y |> modBy srcHeight
                else srcHeight - (abs (shiftY + y) |> modBy srcHeight)
            )
    in
        Plane (nX, nY) (planeF << periodicCoord)


equal : Plane Vec2 a -> Plane Vec2 a -> Bool
equal (Plane sizeA fA as planeA) (Plane sizeB fB) =
    if sizeA == sizeB then
        planeA
            |> materializeFlatten
            |> List.map (\(pos, valA) -> valA == fB pos)
            |> List.foldl ((&&)) True
    else False


unpack : Plane Vec2 a -> List (List (Maybe a))
unpack = foldMap Tuple.second


-- fold2d : a -> (a -> b) -> Plane Vec2 a -> b
-- travel default f plane =
--     unpack plane
--         |> List.map (List.map <| Maybe.withDefault default)
--         |> List.foldl


materialize : Plane Vec2 a -> List (List (Cell Vec2 a))
materialize = foldMap identity


materializeFlatten : Plane Vec2 a -> List (Cell Vec2 a)
materializeFlatten = materialize >> List.concat


toList : Plane Vec2 a -> List (Cell Vec2 a)
toList = materializeFlatten


rotate : Plane Vec2 a -> Plane Vec2 a
rotate = rotateTo East


flip : Plane Vec2 a -> Plane Vec2 a
flip = flipBy Vertical


shift : Offset Vec2 -> Plane Vec2 a -> Plane Vec2 a
shift (Offset (offX, offY)) plane =
    plane |> transform (\(x, y) -> (x + offX, y + offY))


coordsFlat : Plane Vec2 a -> List Vec2
-- coords = foldMap Tuple.first >> List.concat
coordsFlat = coords >> List.concat


coords : Plane Vec2 a -> List (List Vec2)
coords (Plane (width, height) _) =
    List.range 0 (width - 1)
        |> List.map (\x ->
            List.range 0 (height - 1) |> List.map (Tuple.pair x))


overlappingCoords : Offset Vec2 -> Plane Vec2 a -> List Vec2
overlappingCoords (Offset (offX, offY)) (Plane (width, height) _ as plane) =
    coordsFlat plane
        |> List.foldl
            (\(x, y) prev ->
                if (x + offX >= 0) &&
                   (y + offY >= 0) &&
                   (x + offX < width) &&
                   (y + offY < height) then (x, y) :: prev else prev
            )
            []


rotateTo : Orientation -> Plane Vec2 a -> Plane Vec2 a
rotateTo orientation (Plane (width, height) _ as plane) =
    plane |>
        transform
            (case orientation of
                North -> identity
                West -> \(x, y) -> (height - 1 - y, x)
                South -> \(x, y) -> (width - 1 - x, height - 1 - y)
                East -> \(x, y) -> (y, width - 1 - x))


flipBy : Flip -> Plane Vec2 a -> Plane Vec2 a
flipBy how (Plane (width, height) _ as plane) =
    plane |>
        transform
            (case how of
                Horizontal -> \(x, y) -> (width - 1 - x, y)
                Vertical -> \(x, y) -> (x, height - 1 - y))


allRotations : Plane Vec2 a -> List (Plane Vec2 a)
allRotations plane =
    [ plane
    , plane |> rotate -- rotateTo East
    , plane |> rotate |> rotate -- rotateTo South
    , plane |> rotate |> rotate |> rotate -- rotateTo West
    ]


bothFlips : Plane Vec2 a -> List (Plane Vec2 a)
bothFlips plane =
    [ plane |> flip
    , plane |> rotate |> flip
    ]


allViews : Plane Vec2 a -> List (Plane Vec2 a)
allViews plane =
    [ plane
    , plane |> rotate -- rotateTo East
    , plane |> rotate |> rotate -- rotateTo South
    , plane |> rotate |> rotate |> rotate -- rotateTo West
    , plane |> flip
    , plane |> rotate |> flip
    , plane |> rotate |> rotate |> flip
    , plane |> flip |> rotate
    ]


isAmong : List (Plane Vec2 a) -> Plane Vec2 a -> Bool
isAmong planes subject =
    planes
        |> List.foldl
                (\other wasBefore ->
                    wasBefore
                        || equal subject other
                )
           False


memberAt : List (Plane Vec2 a) -> Plane Vec2 a -> Maybe Int
memberAt planes subject =
    planes
        |> List.indexedMap Tuple.pair
        |> List.foldl
                (\(idx, other) wasBefore ->
                    case wasBefore of
                        Just _ -> wasBefore
                        Nothing ->
                            if equal subject other then Just idx
                            else Nothing
                )
           Nothing


findAllSubs : SearchMethod -> N Vec2 -> Plane Vec2 a -> List (Plane Vec2 a)
findAllSubs method ofSize inPlane =
    inPlane
        |> allViews
        |> List.concatMap
            (\view ->
                coordsFlat view
                    |> case method of
                        Periodic ->
                            List.map (\coord -> periodicSubAt coord ofSize view)
                        Bounded ->
                            List.map (\coord -> subAt coord ofSize view)
                            >> List.filterMap identity
            )


findOccurence : List (Plane Vec2 a) -> List (Occured, Plane Vec2 a)
findOccurence allPlanes =
    let
        unique =
            allPlanes
                |> List.foldl
                    (\pattern uniqueOthers ->
                        if pattern |> isAmong uniqueOthers
                            then uniqueOthers
                            else pattern :: uniqueOthers
                    )
                    []
    in
        unique
            |> List.map
                (
                    \subPlane ->
                        ( allPlanes
                            |> List.filter (equal subPlane)
                            |> List.length
                            |> Occured.times
                        , subPlane
                        )
                )
            |> List.sortBy (Tuple.first >> Occured.toInt)


matchesAt : Offset Vec2 -> Dict Int (Plane Vec2 a) -> Plane Vec2 a -> List Int
matchesAt offset from plane =
    let oCoords = plane |> overlappingCoords offset
    in from
        |> Dict.foldl
            (\idx otherPlane matches -> -- ensure plane is the same size as the source
                if equalAt oCoords plane otherPlane
                    then idx :: matches
                    else matches
            )
            []


offsetsFor : Vec2 -> List (Offset Vec2)
offsetsFor (w, h) =
    -- TODO: offsets for 1x1
    List.range (-1 * (h - 1)) (h - 1)
        |> List.map (\y -> List.range (-1 * (w - 1)) (w - 1) |> List.map (Tuple.pair y))
        |> List.concat
        |> List.map Offset


 -- FIXME: size should be `Vec2` for the resulting plane, but the "key" should be `Offset`
findMatches : Dict Int (Plane Vec2 a) -> Plane Vec2 a -> Plane (Offset Vec2) (List Int)
findMatches from (Plane size f as plane) =
    let
        offsetToMatches =
            offsetsFor size
                |> List.map (\(Offset offset) -> ( offset, matchesAt (Offset offset) from plane ))
                |> Dict.fromList
    in
        Plane (Offset size) (\(Offset v) -> Dict.get v offsetToMatches)


type alias TextPlane = Plane Vec2 Char


makeTextPlane : Vec2 -> String -> TextPlane
makeTextPlane ( width, height ) src =
    let
        charArray = String.toList src |> Array.fromList
    in
        Plane
            ( width, height )
            (\(x, y) ->
                if (x < width) && (y < height) then
                    charArray |>
                        Array.get (y * height + x)
                else Nothing
            )


textPlaneToString : TextPlane -> String
textPlaneToString plane =
    unpack plane
        |> List.concat
        |> List.filterMap identity
        |> String.fromList
