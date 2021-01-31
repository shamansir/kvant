module Kvant.Plane exposing (..)


import Array exposing (Array)

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
empty ( width, height ) =
    Plane ( width, height )
        <| Array.repeat height
        <| Array.repeat width Nothing


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


fromArray2d : Size -> Array (Array a) -> Plane a
fromArray2d size_ =
    Array.map (Array.map Just) >> Plane size_


coords : Plane a -> List Coord
coords (Plane size_ _) =
    Vec2.rectFlat { from = (0, 0), to = size_ }


coords2d : Plane a -> List (List Coord)
coords2d (Plane size_ _) =
    Vec2.rect { from = (0, 0), to = size_ }


coordsArray2d : Plane a -> Array (Array Coord)
coordsArray2d =
    coords2d >> List.map Array.fromList >> Array.fromList


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
                maybeVal |> Maybe.map (Tuple.pair coord)
            )

toList2d : Plane a -> List (List (Maybe a))
toList2d (Plane _ grid) =
    grid |> Array.map Array.toList |> Array.toList


toArray2d : Plane a -> Array (Array (Maybe a))
toArray2d (Plane _ grid) = grid


shift : Offset -> Plane a -> Plane a
shift (offX, offY) (Plane (width, height) _ as plane) =
    toList plane
        |> List.map
            (\( ( x, y ) , value) ->
                let
                    ( newX, newY ) =
                        ( x + offX
                        , y + offY
                        )
                in
                    if newX >= 0 && newX < (width + offX) &&
                       newY >= 0 && newY < (height + offY)
                        then Just ( ( newX, newY ), value )
                        else Nothing
            )
        |> List.filterMap identity
        |> fromList (width + offX, height + offY)


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
                Horizontal ->
                    \(x, y) -> (width - 1 - x, y)
                Vertical ->
                    \(x, y) -> (x, height - 1 - y)
            )


rotate : Plane a -> Plane a
rotate = rotateTo East


flip : Plane a -> Plane a
flip = flipBy Vertical


rotateTo : Orientation -> Plane a -> Plane a
rotateTo orientation (Plane (width, height) _ as plane) =
    plane |>
        transform
            (case orientation of
                North ->
                    identity
                West ->
                    \(x, y) -> (height - 1 - y, x)
                South ->
                    \(x, y) -> (width - 1 - x, height - 1 - y)
                East ->
                    \(x, y) -> (y, width - 1 - x)
            )


filledWithValues : Plane a -> Bool
filledWithValues (Plane _ grid) =
    grid
        |> Array.map Array.toList
        |> Array.toList
        |> List.concat
        |> List.foldl
            (\maybeV prev ->
                prev && case maybeV of
                    Just _ -> True
                    Nothing -> False
            )
            True


subPlane : Size -> Plane a -> Plane a
subPlane = subPlaneAt (0, 0)


subPlaneAt : Offset -> Size -> Plane a -> Plane a
subPlaneAt (shiftX, shiftY) (nX, nY) plane =
    Vec2.rectFlat
        { from = (shiftX, shiftY)
        , to = ( shiftX + nX - 1, shiftY + nY - 1)
        }
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
    source |> case symmetry of
        NoSymmetry -> List.singleton
        FlipOnly -> bothFlips
        RotateOnly -> allRotations
        FlipAndRotate -> allViews
