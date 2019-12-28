module WFC.Plane.Vec2 exposing (..)


import Dict
import Dict exposing (Dict)

import WFC.Vec2 exposing (..)
import WFC.Occured as Occured
import WFC.Occured exposing (Occured)
import WFC.Plane.Plane exposing (..)
import WFC.Plane.Offset exposing (..)


type SearchMethod
    = Bounded
    | Periodic


type Orientation
    = North
    | West
    | East
    | South


type Flip
    = Horizontal
    | Vertical


foldMap : (Cell Vec2 a -> b) -> Plane Vec2 a -> List (List b)
foldMap f (Plane _ planeF as plane) =
    coords plane
        |> List.map (List.map <| \v -> f (v, planeF v))


foldl : (Cell Vec2 a -> b -> b) -> b -> Plane Vec2 a -> b
foldl f def plane =
    foldMap identity plane
        |> List.concat
        |> List.foldl f def


toDict : Plane Vec2 a -> Dict Vec2 a
toDict = materializeExists >> Dict.fromList


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


materializeExists : Plane Vec2 a -> List (Vec2, a)
materializeExists =
    materializeFlatten
        >> List.map cellToMaybe
        >> List.filterMap identity


toList : Plane Vec2 a -> List (Cell Vec2 a)
toList = materializeFlatten


rotate : Plane Vec2 a -> Plane Vec2 a
rotate = rotateTo East


flip : Plane Vec2 a -> Plane Vec2 a
flip = flipBy Vertical


coordsFlat : Plane Vec2 a -> List Vec2
-- coords = foldMap Tuple.first >> List.concat
coordsFlat = coords >> List.concat


coords : Plane Vec2 a -> List (List Vec2)
coords (Plane size _) = above size


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


limitsFor : Vec2 -> { from: Vec2, to: Vec2 }
limitsFor (w, h) =
    -- TODO: offsets for 1x1
    if (w <= 0) || (h <= 0) then
        { from = (0, 0), to = (0, 0 ) }
    else
        { from =
            ( if w == 1 then 1 else -1 * (w - 1)
            , if h == 1 then 1 else -1 * (h - 1)
            )
        , to =
            ( if w == 1 then 1 else w - 1
            , if h == 1 then 1 else h - 1
            )
        }



shift : Offset Vec2 -> Plane Vec2 a -> Plane Vec2 a
shift (Offset (offX, offY)) plane =
    plane |> transform (\(x, y) -> (x - offX, y - offY))


shiftCut : Offset Vec2 -> Plane Vec2 a -> Plane Vec2 a
shiftCut (Offset (offX, offY) as offset) (Plane (width, height) f as plane) =
    plane
        |> shift offset
        |> adjust
            (\((x, y), maybeV) ->
                if
                    (x >= 0) &&
                    (y >= 0) &&
                    (x < width) &&
                    (y < height) then maybeV else Nothing
            )


overlappingCoords : Offset Vec2 -> Plane Vec2 a -> List Vec2
overlappingCoords (Offset (offX, offY)) (Plane (width, height) _ as plane) =
    coordsFlat plane
        |> List.foldl
            (\(x, y) prev ->
                if (x + offX >= 0) &&
                   (y + offY >= 0) &&
                   (x + offX < width) &&
                   (y + offY < height) then (x + offX, y + offY) :: prev else prev
            )
            []


matchesAt : Offset Vec2 -> Dict Int (Plane Vec2 a) -> Plane Vec2 a -> List Int
matchesAt offset from plane =
    let
        oCoords = plane |> overlappingCoords offset
        -- oCoords = plane |> overlappingCoords (Debug.log "offset" offset) |> Debug.log "coords"
    in from
        |> Dict.foldl
            (\idx otherPlane matches -> -- ensure plane is the same size as the source
                {- let
                    _ = Debug.log "plane" <| materializeExists plane
                    _ = Debug.log "otherPlane" <| materializeExists otherPlane
                    _ = Debug.log "shiftedOtherPlane" <| materializeExists (otherPlane |> shift offset)
                in -} if equalAt oCoords plane otherPlane
                    then idx :: matches
                    else matches
            )
            []


offsetsFor : { from: Vec2, to: Vec2 } -> List (Offset Vec2)
offsetsFor { from, to } =
    let
        ( fromW, fromH ) = from
        ( toW, toH ) = to
    in
        List.range fromW toW
            |> List.map (\x -> List.range fromH toH |> List.map (Tuple.pair x))
            |> List.concat
            |> List.map Offset


findMatches : Dict Int (Plane Vec2 a) -> Plane Vec2 a -> OffsetPlane Vec2 (List Int)
findMatches from (Plane size f as plane) =
    let
        limits = limitsFor size
        offsetToMatches =
            offsetsFor limits
                |> List.map (\(Offset offset) -> ( offset, matchesAt (Offset offset) from plane ))
                |> Dict.fromList
    in
        OffsetPlane limits (\(Offset v) -> Dict.get v offsetToMatches)
