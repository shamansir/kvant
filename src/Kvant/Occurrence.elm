module Kvant.Occurrence exposing (..)


type Frequency = Frequency Float


type Occurrence
    = Unknown
    | Times Int


toInt : Occurrence -> Int
toInt occured =
    case occured of
        Unknown -> 0
        Times howMuch -> howMuch


toMaybe : Occurrence -> Maybe Int
toMaybe occured =
    case occured of
        Unknown -> Nothing
        Times howMuch -> Just howMuch


compare_ : Occurrence -> Occurrence -> Order
compare_ a b =
    compare (toInt a) (toInt b)


once : Occurrence
once = Times 1


times : Int -> Occurrence
times = Times


inc : Occurrence -> Occurrence
inc occured =
    case occured of
        Unknown -> Times 1
        Times before -> Times <| before + 1


frequencyToFloat : Frequency -> Float
frequencyToFloat (Frequency v) = v


frequencyFromFloat : Float -> Frequency
frequencyFromFloat = Frequency
