module Kvant.Plane exposing (..)


import Array exposing (Array)
import Dict
import Dict exposing (Dict)

import Kvant.Vec2 exposing (Vec2)
import Kvant.Vec2 as Vec2 exposing (rect)
import Html exposing (b)


type alias Coord = Vec2


type alias Size = Vec2


type alias Offset = Vec2


type Plane a = Plane Size (Array (Array (Maybe a)))


type Boundary
    = Bounded
    | Periodic


type Symmetry
    = NoSymmetry
    | FlipOnly
    | RotateOnly
    | FlipAndRotate


type Orientation
    = North
    | West
    | East
    | South


type Flip
    = Horizontal
    | Vertical



map : (a -> b) -> Plane a -> Plane b
map f (Plane rect grid) =
    Plane rect <| Array.map (Array.map (Maybe.map f)) <| grid


positionedMap : (Coord -> a -> b) -> Plane a -> Plane b
positionedMap f (Plane rect grid) =
    Plane rect <|
        (grid
            |> Array.indexedMap
                (\y row ->
                    row
                        |> Array.indexedMap
                            (\x -> Maybe.map <| f (x, y))
                )
        )


empty : Size -> Plane a
empty size_ = Plane size_ Array.empty


fits : Coord -> Plane a -> Bool
fits (x, y) (Plane ( w, h ) _) =
    (x >= 0) && (y >= 0) && (x < w) && (y < h)


get : Coord -> Plane a -> Maybe a
get (x, y) (Plane _ grid as plane) =
    if plane |> fits (x, y)
        then grid
            |> Array.get y
            |> Maybe.andThen
                (Array.get x >> Maybe.andThen identity)
    else Nothing


getPeriodic : Coord -> Plane a -> Maybe a
getPeriodic (x, y) (Plane ( width, height ) _ as plane) =
    let
        modX = modBy width x
        modY = modBy height y
    in
        plane |>
            get
                ( if x >= 0 then modX else width - modX
                , if y >= 0 then modY else height - modY
                )



set : Coord -> a -> Plane a -> Plane a
set (x, y) value (Plane ( w, h ) grid) =
    Plane (w, h)
        <| if x >= w || y >= h then grid
           else if x < 0 || y < 0 then grid
           else
                let
                    maybeRow = grid |> Array.get y
                in case maybeRow of
                    Just row ->
                        grid |> Array.set y (row |> Array.set x (Just value))
                    Nothing -> grid


size : Plane a -> Size
size (Plane s _) = s


foldl : (a -> b -> b) -> b -> Plane a -> b
foldl f def = values >> List.foldl f def


filled : Size -> a -> Plane a
filled (w, h) v =
    Plane (w, h)
        <| Array.repeat h
        <| Array.repeat w (Just v)


equal : Plane comparable -> Plane comparable -> Bool
equal = equalBy (==)


equalBy : (a -> a -> Bool) -> Plane a -> Plane a -> Bool
equalBy compare planeA planeB =
    if size planeA /= size planeB then False
    else -- could be faster to use Array.foldl
        let
            valuesA = toList planeA
            valuesB = toList planeB
        in
            if List.length valuesA == List.length valuesB then
                List.map2
                    (\(coordA, a) (coordB, b) ->
                        coordA == coordB && compare a b
                    )
                    (toList planeA)
                    (toList planeB)
                    |> List.foldl (&&) True
            else False



equalAt : List Coord -> Plane comparable -> Plane comparable -> Bool
equalAt = equalAtBy (==)


equalAtBy : (a -> a -> Bool) -> List Coord -> Plane a -> Plane a -> Bool
equalAtBy compareF atCoords planeA planeB =
    atCoords
        |> List.foldl
            (\coord before ->
                before &&
                    ( case ( planeA |> get coord, planeB |> get coord) of
                        ( Just valueA, Just valueB )
                            -> compareF valueA valueB
                        _ -> False
                    )
            )
            True


setAll : List (Coord, a) -> Plane a -> Plane a
setAll values_ start =
    List.foldl
        (\(v, a) plane -> plane |> set v a)
        start
        values_


fromList : Size -> List (Coord, a) -> Plane a
fromList size_ list =
    empty size_ |> setAll list


coords : Plane a -> List Coord
coords (Plane size_ _) =
    Vec2.rectFlat { from = (0, 0), to = size_ }


coords2d : Plane a -> List (List Coord)
coords2d (Plane size_ _) =
    Vec2.rect { from = (0, 0), to = size_ }


values : Plane a -> List a
values (Plane _ grid) =
    grid
        |> Array.map Array.toList
        |> Array.toList
        |> List.concat
        |> List.filterMap identity


toList : Plane a -> List (Coord, a)
toList (Plane _ grid) =
    grid
        |> Array.indexedMap
            (\y row ->
                row
                    |> Array.indexedMap (\x v -> ( ( x, y ), v ) )
                    |> Array.toList
            )
        |> Array.toList
        |> List.concat
        |> List.filterMap
            (\(coord, maybeVal) ->
                case maybeVal of
                    Just val -> Just (coord, val)
                    Nothing -> Nothing
            )


{-
toList2d : Plane a -> List (List (Vec2, a))
toList2d (Plane ( (ox, oy), _ ) grid) = -- zip all + coords ?
    grid
        |> Array.indexedMap
            (\y row ->
                row
                    |> Array.indexedMap (\x v -> ( ( ox + x, oy + y ), v ) )
                    |> Array.toList
            )
        |> Array.toList -}


shift : Offset -> Plane a -> Plane a
shift (offX, offY) (Plane (width, height) _ as plane) =
    toList plane
        |> List.map
            (\( ( x, y ) , value) ->
                let
                    ( newX, newY ) =
                        ( if offX < 0 then x + offX else x
                        , if offY < 0 then y + offY else y
                        )
                in
                    if newX >= 0 && newX < (width - abs offX) &&
                       newY >= 0 && newY < (height - abs offY)
                        then Just ( ( newX, newY ), value )
                        else Nothing
            )
        |> List.filterMap identity
        |> fromList (width - abs offX, height - abs offY)


transform : (Coord -> Coord) -> Plane a -> Plane a
transform f plane =
    (toList plane
        |> List.map (Tuple.mapFirst f)
        |> setAll) plane


flipBy : Flip -> Plane a -> Plane a
flipBy how (Plane (width, height) _ as plane) =
    plane |>
        transform
            (case how of
                Horizontal -> \(x, y) -> (width - 1 - x, y)
                Vertical -> \(x, y) -> (x, height - 1 - y))


rotate : Plane a -> Plane a
rotate = rotateTo East


flip : Plane a -> Plane a
flip = flipBy Vertical


rotateTo : Orientation -> Plane a -> Plane a
rotateTo orientation (Plane (width, height) _ as plane) =
    plane |>
        transform
            (case orientation of
                North -> identity
                West -> \(x, y) -> (height - 1 - y, x)
                South -> \(x, y) -> (width - 1 - x, height - 1 - y)
                East -> \(x, y) -> (y, width - 1 - x))


subPlane : Size -> Plane a -> Plane a
subPlane = subPlaneAt (0, 0)


subPlaneAt : Offset -> Size -> Plane a -> Plane a
subPlaneAt (shiftX, shiftY) (nX, nY) plane =
    Vec2.rectFlat { from = (shiftX, shiftY), to = ( nX, nY ) }
        |> List.map (\(x, y) ->
                get (x, y) plane
                    |> Maybe.map (Tuple.pair ( x - shiftX, y - shiftY ))
            )
        |> List.filterMap identity
        |> fromList ( nX, nY )


periodicSubPlaneAt : Offset -> Vec2 -> Plane a -> Plane a
periodicSubPlaneAt (shiftX, shiftY) (nX, nY) plane =
    Vec2.rectFlat { from = (shiftX, shiftY), to = ( nX, nY ) }
        |> List.map (\(x, y) ->
                getPeriodic (x, y) plane
                    |> Maybe.map (Tuple.pair ( x - shiftX, y - shiftY ))
            )
        |> List.filterMap identity
        |> fromList ( nX, nY )


allRotations : Plane a -> List (Plane a)
allRotations plane =
    [ plane
    , plane |> rotate -- rotateTo East
    , plane |> rotate |> rotate -- rotateTo South
    , plane |> rotate |> rotate |> rotate -- rotateTo West
    ]


bothFlips : Plane a -> List (Plane a)
bothFlips plane =
    [ plane |> flip
    , plane |> rotate |> flip
    ]


allViews : Plane a -> List (Plane a)
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


views : Symmetry -> Plane a -> List (Plane a)
views symmetry source =
    source |> case Debug.log "symmetry" symmetry of
        NoSymmetry -> List.singleton
        FlipOnly -> bothFlips
        RotateOnly -> allRotations
        FlipAndRotate -> allViews


{-
findSubPlanes : Symmetry -> Boundary -> Vec2 -> Plane a -> List (Plane a)
findSubPlanes symmetry boundary ofSize inPlane =
    inPlane
        |> views symmetry
        -- first rotate and then search for subs or search for subs and rotate them?
        |> List.concatMap
            (\view ->
                coordsFlat view
                    |> case boundary of
                        Periodic ->
                            List.map (\coord -> periodicSubAt coord ofSize view)
                        Bounded ->
                            List.map (\coord -> subAt coord ofSize view)
                            >> List.filterMap identity
            ) -}


-- FIXME: move to separate module (PlaneStats?)
subPlanes
    :  Symmetry
    -> Boundary
    -> Size
    -> Plane comparable
    -> List (Plane comparable)
subPlanes
    = subPlanesBy (==)


subPlanesBy
    :  (a -> a -> Bool)
    -> Symmetry
    -> Boundary
    -> Size
    -> Plane a
    -> List (Plane a)
subPlanesBy compare symmetry method ofSize inPlane =
    inPlane
        |> coords
        |> (
            case method of
                Periodic ->
                    List.map (\coord -> periodicSubPlaneAt coord ofSize inPlane)
                Bounded ->
                    List.map (\coord -> subPlaneAt coord ofSize inPlane)
           )
        |> List.concatMap (views symmetry)


{- fromDict : Vec2 -> Dict Vec2 a -> Plane (Maybe a)
fromDict size dict =
    Plane size <| \v -> Dict.get v dict -}


-- TODO: When Walker will be inside the Panel, we may use `Walker.all` for that purpose

{-}
take : List Vec2 -> Plane a -> List (Vec2, Maybe a)
take all (Plane _ f) =
    all |> List.map (\v -> (v, f v))


takeAsDict : List Vec2 -> Plane a -> Dict Vec2 a
takeAsDict all =
    toList all
        >> List.map cellToMaybe
        >> List.filterMap identity
        >> Dict.fromList -}


{- loadNeighbours : Vec2 -> (Vec2 -> Direction -> Vec2) -> Plane a -> Neighbours (Maybe a)
loadNeighbours focus move (Plane _ f) =
    Neighbours.collect focus move f -}


{- apply : Vec2 -> (Vec2 -> Direction -> Vec2) -> Neighbours a -> Plane a -> Plane a
apply focus move neighbours =
    let
        fromNeighbours = neighbours |> Neighbours.byCoord focus move
    in
        positionedMap
            (\v cur ->
                fromNeighbours v
                    |> Maybe.map Just
                    |> Maybe.withDefault cur
            ) -}
