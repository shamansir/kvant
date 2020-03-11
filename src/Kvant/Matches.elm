module Kvant.Matches exposing
    ( MoreThanOne
    , Matches
    , none, single, some, safeSome
    , count, isNone, extract
    , toList, fromList
    , toString
    , fromMaybe
    , run
    , exclude
    , and, or, intersect, union
    , equal
    )


import List.Extra as List exposing (unique)


type MoreThanOne a = MoreThanOne (List a)


type Matches a
    = None
    | Single a
    | Some (MoreThanOne a)
    -- TODO: Any


none : Matches a
none = None


single : a -> Matches a
single = Single


some : List a -> Maybe (Matches a)
some list =
    if List.length list < 2 then Nothing
    else Just <| Some <| MoreThanOne <| list


safeSome : a -> a -> List a -> Matches a
safeSome first second other =
    Some <| MoreThanOne <| first :: second :: other


count : Matches a -> Int
count matches =
    case matches of
        None -> 0
        Single _ -> 1
        Some (MoreThanOne list) -> List.length list


isNone : Matches a -> Bool
isNone matches =
    case matches of
        None -> True
        _ -> False


toList : Matches a -> List a
toList matches =
    case matches of
        None -> []
        Single v -> List.singleton v
        Some (MoreThanOne list) -> list


fromList : List a -> Matches a
fromList list =
    case List.head list of
        Nothing -> None
        Just head ->
            case List.tail list of
                Nothing -> Single head
                Just _ -> Some <| MoreThanOne <| list


fromMaybe : Maybe (List a) -> Matches a
fromMaybe = Maybe.map fromList >> Maybe.withDefault None


extract : Matches a -> ( Int, List a )
extract matches =
    let
        asList = toList matches
    in ( List.length asList, asList )


exclude : comparable -> Matches comparable -> Matches comparable
exclude val matches =
    case matches of
        None -> matches
        Single v -> if v == val then none else matches
        Some (MoreThanOne list) ->
            list |> List.filter ((/=) val) |> fromList


and : Matches comparable -> Matches comparable -> Matches comparable
and matchesA matchesB =
    case ( matchesA, matchesB ) of
        ( None, _ ) -> none
        ( _, None ) -> none
        ( a, b ) ->
            let
                aList = a |> toList
                bList = b |> toList
            in
                aList
                    |> List.concatMap (\aVal -> bList |> List.filter ((==) aVal))
                    |> fromList


or : Matches comparable -> Matches comparable -> Matches comparable
or matchesA matchesB =
    List.append
        (matchesA |> toList)
        (matchesB |> toList)
        |> List.unique
        |> fromList


intersect : List (Matches comparable) -> Matches comparable
intersect list =
    case list of
        [] -> none
        x::xs -> List.foldl and x xs


union : List (Matches comparable) -> Matches comparable
union list =
    case list of
        [] -> none
        x::xs -> List.foldl or x xs


equal : Matches comparable -> Matches comparable -> Bool
equal matchesA matchesB =
    case ( matchesA, matchesB ) of
        ( None, None ) -> True
        ( Single a, Single b ) -> a == b
        ( Some (MoreThanOne xs), Some (MoreThanOne ys) ) ->
            (List.length xs == List.length ys)
                && (List.map2 (==) (List.sort xs) (List.sort ys) |> List.foldl (&&) True)
        _ -> False

-- or : Matches comparable -> Matches comparable -> Matches comparable
-- or matchesA matchesB =
--     let
--         aList = matchesA |> toList
--         bList = matchesB |> toList
--     in
--         aList
--             |> append bList
--             |> fromList


run : (() -> x) -> (a -> List a -> x) -> Matches a -> x
run fNone fSome matches =
    case matches |> toList of
        head::tail -> fSome head tail
        [] -> fNone ()


toString : (a -> String) -> Matches a -> String
toString itemToString =
    toList
        >> List.map itemToString
        >> String.join ","
