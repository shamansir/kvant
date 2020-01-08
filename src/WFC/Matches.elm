module WFC.Matches exposing
    ( MoreThanOne
    , Matches
    , none, single, some, safeSome
    , count, extract
    , toList, fromList
    , fromMaybe
    )


type MoreThanOne a = MoreThanOne (List a)


type Matches a
    = None
    | Single a
    | Some (MoreThanOne a)


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

