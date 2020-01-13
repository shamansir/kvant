module WFC.Solver.History exposing (..)

import List.Zipper as Zipper exposing (..)
import List.Zipper exposing (Zipper)


type alias History a = Zipper a

-- type History a = History Int (Zipper a) -- contains a stack limit z


init : a -> History a
init = Zipper.singleton


push : a -> History a -> History a
push elem history =
    Zipper.from
        (Zipper.toList history)
        elem
        []


last : History a -> a
last = Zipper.last >> Zipper.current


pop : History a -> ( a, Maybe (History a) )
pop history =
    let
        moved = history |> Zipper.last
    in
        ( moved |> Zipper.current
        , moved |> Zipper.before |> Zipper.fromList
        )


back : History a -> History a
back history =
    history
        |> Zipper.previous
        |> Maybe.withDefault history


forward : History a -> History a
forward history =
    history
        |> Zipper.next
        |> Maybe.withDefault history


maybeBack : History a -> Maybe (History a)
maybeBack = Zipper.previous


maybeForward : History a -> Maybe (History a)
maybeForward = Zipper.next


toList : History a -> List a
toList = Zipper.toList
