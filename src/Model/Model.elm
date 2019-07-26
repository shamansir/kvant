module Model
    exposing (Model)


import Array exposing (Array)
import Random


type FMX = FMX Int
type FMY = FMY Int
type T = T Int
type Size = Size FMX FMY
type Limit = Limit Int


type alias CheckBoundary = { x : Int, y : Int } -> Bool


type WaveCell
    = Collapsed
    | Unknown
type Wave = Wave (List (List WaveCell))


type Observation
    = Continue -- null
    | Contradiction -- false
    | Finished -- true


type Graphics = Graphics {}
type UnfinishedGraphics = UnfinishedGraphics {}
type RunResolution
    = FoundContradiction
    | ReachedLimit UnfinishedGraphics
    | Success Graphics


type alias Compatibility =
    { a : Int
    , b : Int
    , c : Int
    , d : Int
    }


type alias Weights =
    { values : List Float
    , logged : List Float
    , sum : Float
    , sumLogged : Float
    }


type alias Sums =
    { ones : List Int
    , weights : List Float
    , weightsLogged : List Float
    }


type alias Model =
    { size : Size
    , distribution : List Int
    , wave : List (List WaveCell)
    , compatible : List (List Compatibility)
    , weights : Weights
    , sums : Sums
    , entropies : List Float
    , startingEntropy : Float
    , stack : List Int
    , stackSize : Int
    -- , startingEntropy : Float
    }


-- dx = [-1, 0, 1, 0]
-- dy = [0, 1, 0, -1]
-- opposite = [2, 3, 0, 1]


square : Size -> Int
square (Size (FMX fmx) (FMY fmy)) =
    fmx * fmy


init : Size -> T -> Model
init size (T t) =
    let
        count = square size
        weights =
            List.repeat t 0 |> weight
        sums =
            { ones = List.repeat count 0
            , weights = List.repeat count 0
            , weightsLogged = List.repeat count 0
            }
    in
        { distribution = List.repeat t 0
        , wave = List.repeat count <| List.repeat t Unknown
        , compatible =
            List.repeat count
                <| List.repeat t
                <| { a = 0, b = 0, c = 0, d = 0 }
        , weights = weights
        , startingEntropy = logBase weights.sum e - weights.sumLogged / weights.sum
        , entropies = List.repeat count 0
        , sums = sums
        , stack = List.repeat t 0
        , stackSize = 0
        , size = size
        }


weight : List Float -> Weights
weight values =
    let
        logged = List.map (\w -> w * logBase w e) values
    in
        { logged = logged
        , sum = List.foldl (+) 0 values
        , sumLogged = List.foldl (+) 0 logged
        , values = values
        }


observe
    :  Model
    -> Random.Seed
    -> ( Random.Seed, Observation, Model )
observe model seed =
    let
        ( min, argmin ) =
            List.foldl (\w t -> t) ( Random.minInt, -1 ) model.wave
    in
        ( seed, Finished, model )


propagate
    :  Model
    -> Random.Seed
    -> ( Random.Seed, Model )
propagate model seed =
    ( seed, model )


run : Wave -> Limit -> Model -> Random.Seed -> RunResolution
run wave (Limit limit) model seed =
    let
        iterate iterationsLeft ( prevSeed, prevObservation, prevModel ) =
            if iterationsLeft < 0 then ( prevSeed , prevObservation, prevModel )
            else case observe prevModel prevSeed of
                ( obsSeed, result, observedModel ) ->
                    if result == Continue then
                        case propagate observedModel obsSeed of
                            ( propSeed, propagatedModel ) ->
                                iterate (iterationsLeft - 1) ( propSeed, result, propagatedModel )
                    else ( obsSeed, result, observedModel )
    in
        case iterate limit ( seed, Continue, model ) of
                ( _, Continue, _ )  -> ReachedLimit (UnfinishedGraphics {})
                ( _, Finished, _ ) -> Success (Graphics {})
                ( _, Contradiction, _ ) -> FoundContradiction



