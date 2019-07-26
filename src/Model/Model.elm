module Model
    exposing (Model)


import Array exposing (Array)
import Random


type FMX = FMX Int
type FMY = FMY Int
type T = T Int
type Size = Size FMX FMY
type Limit = Limit Int

type alias CheckBoundary = Int -> Int -> Bool


type WaveCell
    = Collapsed
    | Unknown
type Wave = Wave (Array (Array WaveCell))


type Observation
    = Continue -- null
    | Contradiction -- false
    | Finished -- true


type Graphics = Graphics {}
type RunResolution
    = FoundContradiction
    | ReachedLimit
    | Success Graphics


type alias Compatibility =
    { a : Int
    , b : Int
    , c : Int
    , d : Int
    }


type alias Weights =
    { values : Array Float
    , logged : Array Float
    , sum : Float
    , sumLogged : Float
    }


type alias Model =
    { size : Size
    , distribution : Array Int
    , wave : Array (Array WaveCell)
    , compatible : Array (Array Compatibility)
    , weights : Weights
    , entropies : Array Float
    , startingEntropy : Float
    , stack : Array Int
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
            { values = Array.repeat t 0
            , logged = Array.repeat t 0
            , sum = 0
            , sumLogged = 0
            } |> weight
    in
        { distribution = Array.repeat t 0
        , wave = Array.repeat count <| Array.repeat t Unknown
        , compatible =
            Array.repeat count
                <| Array.repeat t
                <| { a = 0, b = 0, c = 0, d = 0 }
        , weights = weights
        , startingEntropy = logBase weights.sum e - weights.sumLogged / weights.sum
        , entropies = Array.repeat t 0
        , stack = Array.repeat t 0
        , size = size
        }


weight : Weights -> Weights
weight weights =
    let
        updateWeights w ( currentWeights, index ) =
            (
                let
                    logged = w + logBase w e
                in
                    { logged = Array.set index logged currentWeights.logged
                    , sum = currentWeights.sum + w
                    , sumLogged = currentWeights.sumLogged + w
                    , values = currentWeights.values
                    }
            , index + 1
            )
    in
        weights.values
            |> Array.foldl updateWeights ( weights, 0 )
            |> Tuple.first


observe
    :  Model
    -> Random.Seed
    -> ( Random.Seed, Observation )
observe model seed =
    let
        min = 1000
        argmin = -1
    in
        ( seed, Finished )


propagate
    :  Model
    -> Random.Seed
    -> ( Random.Seed, {} )
propagate model seed =
    ( seed, {} )


run : Wave -> Limit -> Model -> Random.Seed -> RunResolution
run wave (Limit limit) model seed =
    let
        iterate iterationsLeft ( prevSeed, observation ) =
            if iterationsLeft < 0 then ( prevSeed , observation )
            else case observe model prevSeed of
                ( obsSeed, result ) ->
                    if result == Continue then
                        case propagate model obsSeed of
                            ( propSeed, _ ) ->
                                iterate (iterationsLeft - 1) ( propSeed, result )
                    else ( obsSeed, result )

    in
        case iterate limit ( seed, Continue )
            |> Tuple.second of
                Continue -> ReachedLimit
                Finished -> Success (Graphics {})
                Contradiction -> FoundContradiction



