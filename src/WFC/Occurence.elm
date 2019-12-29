module WFC.Occurence exposing (..)


import Dict exposing (Dict)
import Dict as Dict


type Frequency = Frequency Float


type Occurence
    = Unknown
    | Times Int


toInt : Occurence -> Int
toInt occured =
    case occured of
        Unknown -> 0
        Times howMuch -> howMuch


toMaybe : Occurence -> Maybe Int
toMaybe occured =
    case occured of
        Unknown -> Nothing
        Times howMuch -> Just howMuch


compare_ : Occurence -> Occurence -> Order
compare_ a b =
    compare (toInt a) (toInt b)


once : Occurence
once = Times 1


times : Int -> Occurence
times = Times


inc : Occurence -> Occurence
inc occured =
    case occured of
        Unknown -> Times 1
        Times before -> Times <| before + 1


calculateFrequency : Dict Int Occurence -> Dict Int Frequency
calculateFrequency occuredData =
    let
        maxValue
            = occuredData
                |> Dict.foldl
                    (\_ val prevMax ->
                        case val of
                            Unknown -> prevMax
                            Times n -> max prevMax n
                    )
                    0
                |> toFloat
    in
        occuredData
            |> Dict.map (always toInt)
            |> Dict.filter (always <| (>) 0)
            |> Dict.map (\_ v -> Frequency <| toFloat v / maxValue)
