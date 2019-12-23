module WFC.Occured exposing (..)


type Occured
    = Unknown
    | Times Int


toInt : Occured -> Int
toInt occured =
    case occured of
        Unknown -> 0
        Times howMuch -> howMuch


compare_ : Occured -> Occured -> Order
compare_ a b =
    compare (toInt a) (toInt b)


once : Occured
once = Times 1


times : Int -> Occured
times = Times


inc : Occured -> Occured
inc occured =
    case occured of
        Unknown -> Times 1
        Times before -> Times <| before + 1
