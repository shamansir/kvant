module WFC.Solver exposing (..)


import WFC.Plane exposing (..)
import WFC.Plane as Plane exposing (foldl, coords, equal, sub)


type Pattern v a = Pattern v (v -> Maybe a)


type Approach
    = Overlapping
    | Tiled {- Rules -}


type alias Options v =
    { approach: Approach
    , patternSize: v
    , inputSize: v
    , outputSize: v
    }


type Step a
    = Step Int


type Solver v a = Solver (Options v) (Plane v a) (List (Pattern v a))


solve : Step a -> Solver v a -> ( Step a, Plane v a )
solve step (Solver options sourcePlane patterns) =
    ( step, sourcePlane )


type alias TextOptions = Options Vec2
type alias TextSolver = Solver Vec2 Char


isAmong : Plane Vec2 a -> List (Plane Vec2 a) -> Bool
isAmong subject planes =
    planes
        |> List.foldl
                (\other wasBefore ->
                    wasBefore
                        || Plane.equal subject other
                        -- TODO: rotate and compare
                )
           False


findPatterns : Vec2 -> Plane Vec2 a -> List (Pattern Vec2 a)
findPatterns ofSize inPlane =
    inPlane
        |> Plane.coords
        |> List.foldl
            (\coord foundPatterns ->
                let
                    subPlane = Plane.subAt coord ofSize inPlane
                    -- TODO: add all rotations and flips
                in
                    if not <| isAmong subPlane foundPatterns then
                        subPlane :: foundPatterns
                    else foundPatterns
            )
            []
        |> List.map toPattern


toPattern : Plane v a -> Pattern v a
toPattern (Plane size f) = Pattern size f


fromPattern : Pattern v a -> Plane v a
fromPattern (Pattern size f) = Plane size f
