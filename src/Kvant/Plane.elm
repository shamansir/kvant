module Kvant.Plane exposing (..)


import Array exposing (Array)
import Dict
import Dict exposing (Dict)

import Kvant.Vec2 exposing (Vec2)
import Kvant.Vec2 as Vec2 exposing (rect)
import Kvant.Neighbours exposing (Neighbours)
import Kvant.Neighbours as Neighbours exposing (..)
import Kvant.Occurrence exposing (Occurrence, Frequency)
import Kvant.Occurrence as Occurrence exposing (times, toInt)

type Plane a = Plane ( Vec2, Vec2 ) (Array (Array (Maybe a)))


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


positionedMap : (Vec2 -> a -> b) -> Plane a -> Plane b
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


empty : Vec2 -> Plane a
empty size_ = Plane ( (0, 0), size_ ) Array.empty


fits : Vec2 -> Plane a -> Bool
fits (x, y) (Plane ( ( ox, oy ), ( w, h ) ) _) =
    (x >= ox) && (y >= oy) && (x < ox + w) && (y < oy + h)


get : Vec2 -> Plane a -> Maybe a
get (x, y) (Plane _ grid as plane) =
    if plane |> fits (x, y)
        then grid
            |> Array.get y
            |> Maybe.andThen
                (Array.get x >> Maybe.andThen identity)
    else Nothing


set : Vec2 -> a -> Plane a -> Plane a
set (x, y) value (Plane ( ( ox, oy ), (w, h) ) grid) =
    Plane ( ( ox, oy ), (w, h) )
        <| if x >= ox + w || y >= oy + h then grid
           else if x < ox || y < oy then grid
           else
                let
                    maybeRow = grid |> Array.get y
                in case maybeRow of
                    Just row ->
                        grid |> Array.set y (row |> Array.set x (Just value))
                    Nothing -> grid


origin : Plane a -> Vec2
origin (Plane ( o, _ ) _) = o


size : Plane a -> Vec2
size (Plane ( _, s ) _) = s


filled : Vec2 -> a -> Plane a
filled (w, h) v =
    Plane ( ( 0, 0 ), (w, h) )
        <| Array.repeat h
        <| Array.repeat w (Just v)


equal : Plane comparable -> Plane comparable -> Bool
equal = equalBy (==)


equalBy : (a -> a -> Bool) -> Plane a -> Plane a -> Bool
equalBy compare planeA planeB =
    if size planeA /= size planeB
        || origin planeA /= origin planeB then False
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





equalAt : List Vec2 -> Plane comparable -> Plane comparable -> Bool
equalAt = equalAtBy (==)


equalAtBy : (a -> a -> Bool) -> List Vec2 -> Plane a -> Plane a -> Bool
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


setAll : List (Vec2, a) -> Plane a -> Plane a
setAll values_ start =
    List.foldl
        (\(v, a) plane -> plane |> set v a)
        start
        values_


fromList : Vec2 -> List (Vec2, a) -> Plane (Maybe a)
fromList size_ list =
    filled size_ Nothing
        |> setAll (list |> List.map (Tuple.mapSecond Just))


coords : Plane a -> List Vec2
coords =
    coords2d >> List.concat


coords2d : Plane a -> List (List Vec2)
coords2d (Plane ( origin_, size_ ) _) =
    Vec2.rect { from = origin_, to = size_ }


values : Plane a -> List a
values (Plane _ grid) =
    grid
        |> Array.map Array.toList
        |> Array.toList
        |> List.concat
        |> List.filterMap identity


toList : Plane a -> List (Vec2, a) -- toList
toList (Plane ( (ox, oy), _ ) grid) =
    grid
        |> Array.indexedMap
            (\y row ->
                row
                    |> Array.indexedMap (\x v -> ( ( ox + x, oy + y ), v ) )
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


transform : (Vec2 -> Vec2) -> Plane a -> Plane a
transform f plane =
    (toList plane
        |> List.map (Tuple.mapFirst f)
        |> setAll) plane


flipBy : Flip -> Plane a -> Plane a
flipBy how (Plane (_, (width, height)) _ as plane) =
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
rotateTo orientation (Plane ( _, (width, height) ) _ as plane) =
    plane |>
        transform
            (case orientation of
                North -> identity
                West -> \(x, y) -> (height - 1 - y, x)
                South -> \(x, y) -> (width - 1 - x, height - 1 - y)
                East -> \(x, y) -> (y, width - 1 - x))


subPlane : Vec2 -> Plane a -> Maybe (Plane a)
subPlane = subPlaneAt (0, 0)


subPlaneAt : Vec2 -> Vec2 -> Plane a -> Maybe (Plane a)
subPlaneAt (shiftX, shiftY) (nX, nY) (Plane (offset, (srcWidth, srcHeight)) planeF) =
    if (shiftX + nX <= srcWidth) && (shiftY + nY <= srcHeight) then
        Just (Plane (offset, (nX, nY)) -- FIXME: ensure offset
            <| \(x, y) ->
                if (x < nX) && (y < nY) then
                    planeF (x + shiftX, y + shiftY)
                else
                    Nothing
            )
    else Nothing


periodicSubPlaneAt : Vec2 -> Vec2 -> Plane a -> Plane a
periodicSubPlaneAt (shiftX, shiftY) (nX, nY) (Plane ( offset, (srcWidth, srcHeight) ) planeF) =
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


isAmong : List (Plane comparable) -> Plane comparable -> Bool
isAmong = isAmongBy (==)


isAmongBy : (a -> a -> Bool) -> List (Plane a) -> Plane a -> Bool
isAmongBy compare planes subject =
    planes
        |> List.foldl
                (\other wasBefore ->
                    wasBefore
                        || equalBy compare subject other
                )
           False


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

subPlanes
    :  Symmetry
    -> Boundary
    -> Vec2
    -> Plane comparable
    -> List (Plane comparable)
subPlanes
    = subPlanesBy (==)


subPlanesBy
    :  (a -> a -> Bool)
    -> Symmetry
    -> Boundary
    -> Vec2
    -> Plane a
    -> List (Plane a)
subPlanesBy compare symmetry method ofSize inPlane =
    inPlane
        |> coords
        |> (case method of
                Periodic ->
                    List.map (\coord -> periodicSubPlaneAt coord ofSize inPlane)
                Bounded ->
                    List.map (\coord -> subPlaneAt coord ofSize inPlane)
                    >> List.filterMap identity)
        |> List.concatMap (views symmetry)


evaluateOccurrence
    :  List (Plane comparable)
    -> List ((Occurrence, Maybe Frequency), Plane comparable)
evaluateOccurrence = evaluateOccurrenceBy (==)


evaluateOccurrenceBy
    :  (a -> a -> Bool)
    -> List (Plane a)
    -> List ((Occurrence, Maybe Frequency), Plane a)
evaluateOccurrenceBy compare subPlanes_ =
    let
        _ =
            subPlanes_
                |> List.map toList
                |> Debug.log "allPlanes"
        uniquePatterns =
            subPlanes_
                |> List.foldr
                    (\pattern uniqueOthers ->
                        if pattern |> isAmongBy compare uniqueOthers
                            then uniqueOthers
                            else pattern :: uniqueOthers
                    )
                    []
        withOccurrence =
             uniquePatterns
                 |> List.map
                     (
                         \subPlane_ ->
                             ( subPlanes_
                                 |> List.filter (equalBy compare subPlane_)
                                 |> List.length
                                 |> Occurrence.times
                             , subPlane_
                             )
                     )
                 |> List.sortBy (Tuple.first >> Occurrence.toInt)
        total =
             withOccurrence
                |> List.foldl
                    (\(occurrence, _) sum ->
                         sum + Occurrence.toInt occurrence
                    )
                    0
                 |> toFloat
        _ =
            withOccurrence
                |> List.map (Tuple.mapSecond toList)
                |> Debug.log "withOccurence"
    in
        withOccurrence
            |> List.map
                (\(occurrence, v) ->
                    (
                        ( occurrence
                        , occurrence
                            |> Occurrence.toMaybe
                            |> Maybe.map
                                (\occurred ->
                                    Occurrence.frequencyFromFloat <| toFloat occurred / total))
                    , v
                    )
                )



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
