module Model
    exposing (Model)


import Array exposing (Array)


type FMX = FMX Int
type FMY = FMY Int
type T = T Int

type WaveCell
    = Collapsed
    | Unknown


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
    { distribution : Array Int
    , wave : Array (Array WaveCell)
    , compatible : Array (Array Compatibility)
    , weights : Weights
    , entropies : Array Float
    , stack : Array Int
    -- , startingEntropy : Float
    }


dx = [-1, 0, 1, 0]
dy = [0, 1, 0, -1]
opposite = [2, 3, 0, 1]


init : FMX -> FMY -> T -> Model
init (FMX fmx) (FMY fmy) (T t) =
    let
        size = fmx * fmy
    in
        { distribution = Array.repeat t 0
        , wave = Array.repeat size <| Array.repeat t Unknown
        , compatible =
            Array.repeat size
                <| Array.repeat t
                <| { a = 0, b = 0, c = 0, d = 0 }
        , weights =
            { values = Array.repeat t 0
            , logged = Array.repeat t 0
            , sum = 0
            , sumLogged = 0
            }
        , entropies = Array.repeat t 0
        , stack = Array.repeat t 0
        }


weight : T -> Weights -> Weights
weight (T t) weights =
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
