module KDDict exposing
    ( KDDict
    , Key(..)
    , MatchKey
    , addAxis
    , addCoordinateAxis
    , coordinateKey
    , countDeleted
    , empty
    , find
    , findMatching
    , fromList
    , fromListBy
    , get
    , height
    , heightDifference
    , insert
    , insertAll
    , insertAllBy
    , key
    , merge
    , remove
    , removeAll
    , size
    , sortByAxis
    , toList
    , unsafeMap
    )

import KD.Match exposing (Match, compareWithMatch)


type KDDict comparable v
    = Empty
    | Node Int (KDDict comparable v) ( Key comparable, v ) (KDDict comparable v)
    | Deleted Int (KDDict comparable v) (KDDict comparable v)


type Key a
    = Key a (List a)


type alias MatchKey a =
    Key (Match a)


key : a -> Key a
key a =
    Key a []


coordinateKey : ( a, a ) -> Key a
coordinateKey ( x, y ) =
    Key x [ y ]


addAxis : a -> Key a -> Key a
addAxis a (Key head tail) =
    Key a (head :: tail)


addCoordinateAxis : ( a, a ) -> Key a -> Key a
addCoordinateAxis ( x, y ) (Key head tail) =
    Key x (y :: head :: tail)


getIndex : Int -> Key a -> a
getIndex index (Key head rest) =
    if index == 0 || rest == [] then
        head

    else
        rest
            |> List.drop (index - 1 |> modBy (List.length rest))
            |> List.head
            |> Maybe.withDefault head


empty : KDDict comparable v
empty =
    Empty


fromList : List ( Key comparable, v ) -> KDDict comparable v
fromList =
    fromList_ 0


fromListBy : (v -> Key comparable) -> List v -> KDDict comparable v
fromListBy by list =
    list
        |> List.map (\v -> ( by v, v ))
        |> fromList


splitOn : Int -> List a -> ( List a, List a )
splitOn n l =
    ( List.take n l, List.drop n l )


fromList_ : Int -> List ( Key comparable, v ) -> KDDict comparable v
fromList_ index list =
    let
        lookAhead =
            5

        median l =
            l
                |> splitOn lookAhead
                |> Tuple.mapFirst (List.sortWith getKey)
                |> (\( l1, l2 ) ->
                        let
                            amount =
                                List.length l1 // 2

                            ( a, b ) =
                                splitOn amount l1
                        in
                        ( b |> List.head
                        , a ++ List.drop 1 b ++ l2
                        )
                   )

        getKey : ( Key comparable, b ) -> ( Key comparable, b ) -> Order
        getKey ( k1, _ ) ( k2, _ ) =
            compareKey index k1 k2
    in
    case median list of
        ( Nothing, _ ) ->
            Empty

        ( Just ( key_, value ), rest ) ->
            let
                ( larger, smaller ) =
                    List.partition
                        (\( otherKey, _ ) -> compareKey index key_ otherKey == LT)
                        rest

                nextIndex =
                    index + 1
            in
            Node
                index
                (fromList_ nextIndex smaller)
                ( key_, value )
                (fromList_ nextIndex larger)


toList : KDDict comparable v -> List ( Key comparable, v )
toList dict =
    case dict of
        Empty ->
            []

        Node _ smaller item larger ->
            toList smaller ++ item :: toList larger

        Deleted _ smaller larger ->
            toList smaller ++ toList larger


height : KDDict comparable v -> Int
height dict =
    case dict of
        Empty ->
            0

        Node _ smaller _ larger ->
            1 + max (height smaller) (height larger)

        Deleted _ smaller larger ->
            1 + max (height smaller) (height larger)


heightDifference : KDDict comparable v -> Int
heightDifference dict =
    case dict of
        Empty ->
            0

        Node _ smaller _ larger ->
            let
                diff =
                    abs (height smaller - height larger)
            in
            max
                diff
                (max
                    (heightDifference smaller)
                    (heightDifference larger)
                )

        Deleted _ smaller larger ->
            let
                diff =
                    abs (height smaller - height larger)
            in
            max
                diff
                (max
                    (heightDifference smaller)
                    (heightDifference larger)
                )


size : KDDict comparable v -> number
size dict =
    case dict of
        Empty ->
            0

        Node _ smaller _ larger ->
            1 + size smaller + size larger

        Deleted _ smaller larger ->
            size smaller + size larger


countDeleted : KDDict comparable v -> Int
countDeleted dict =
    case dict of
        Empty ->
            0

        Node _ smaller _ larger ->
            countDeleted smaller + countDeleted larger

        Deleted _ smaller larger ->
            1 + countDeleted smaller + countDeleted larger


get : Key comparable -> KDDict comparable v -> Maybe v
get query dict =
    case dict of
        Empty ->
            Nothing

        Node index smaller ( key_, value ) larger ->
            if query == key_ then
                Just value

            else
                case compareKey index query key_ of
                    LT ->
                        get query smaller

                    _ ->
                        get query larger

        Deleted _ smaller larger ->
            case get query smaller of
                Nothing ->
                    get query larger

                a ->
                    a


mMatch : Key (Match comparable) -> Key comparable -> Bool
mMatch (Key q qs) (Key k ks) =
    List.map2 Tuple.pair (q :: qs) (k :: ks)
        |> List.all (\( q_, k_ ) -> KD.Match.match q_ k_)


remove : Key comparable -> KDDict comparable v -> KDDict comparable v
remove q dict =
    case dict of
        Empty ->
            Empty

        Node i s ( key_, v ) l ->
            if q == key_ then
                Deleted i s l

            else
                case compareKey i q key_ of
                    LT ->
                        Node i (remove q s) ( key_, v ) l

                    GT ->
                        Node i s ( key_, v ) (remove q l)

                    EQ ->
                        Node i (remove q s) ( key_, v ) (remove q l)

        Deleted i s l ->
            Deleted i (remove q s) (remove q l)


removeAll : List (Key comparable) -> KDDict comparable v -> KDDict comparable v
removeAll keys db =
    List.foldl remove db keys


insert : Key comparable -> v -> KDDict comparable v -> KDDict comparable v
insert =
    insert_ 0


insert_ : Int -> Key comparable -> v -> KDDict comparable v -> KDDict comparable v
insert_ i key_ value dict =
    case dict of
        Empty ->
            Node i Empty ( key_, value ) Empty

        Node _ s ( k, v ) l ->
            case compareKey i key_ k of
                LT ->
                    Node i (insert_ (i + 1) key_ value s) ( k, v ) l

                _ ->
                    Node i s ( k, v ) (insert_ (i + 1) key_ value l)

        Deleted _ s l ->
            if modBy 2 i == 0 then
                Deleted i (insert_ (i + 1) key_ value s) l

            else
                Deleted i s (insert_ (i + 1) key_ value l)


insertAll : List ( Key comparable, v ) -> KDDict comparable v -> KDDict comparable v
insertAll items db =
    List.foldl (\( a, b ) -> insert a b) db items


insertAllBy : (v -> Key comparable) -> List v -> KDDict comparable v -> KDDict comparable v
insertAllBy keyWith values db =
    insertAll
        (List.map2
            Tuple.pair
            (List.map keyWith values)
            values
        )
        db


merge : KDDict comparable v -> KDDict comparable v -> KDDict comparable v
merge d1 d2 =
    case ( d1, d2 ) of
        ( Empty, _ ) ->
            d2

        ( _, Empty ) ->
            d1

        ( Node i s1 ( k1, v1 ) l1, Node _ s2 ( k2, v2 ) l2 ) ->
            case compareKey i k1 k2 of
                EQ ->
                    Node i (merge s1 s2) ( k1, v1 ) (insert_ (i + 1) k2 v2 (merge l1 l2))

                LT ->
                    toList d1 ++ toList d2 |> fromList_ i

                GT ->
                    toList d1 ++ toList d2 |> fromList_ i

        ( Deleted i _ _, _ ) ->
            toList d1 ++ toList d2 |> fromList_ i

        ( _, Deleted i _ _ ) ->
            toList d1 ++ toList d2 |> fromList_ i


compareKey : Int -> Key comparable -> Key comparable -> Order
compareKey level k1 k2 =
    compare (getIndex level k1) (getIndex level k2)


sortByAxis : Int -> Int -> KDDict comparable b -> List b
sortByAxis numberOfLevels level dict =
    dict
        |> sortByAxis_ numberOfLevels level
        |> List.map Tuple.second


sortByAxis_ : Int -> Int -> KDDict comparable b -> List ( Key comparable, b )
sortByAxis_ numberOfLevels level dict =
    let
        merge_ : List ( Key comparable, b ) -> List ( Key comparable, b ) -> List ( Key comparable, b )
        merge_ x y =
            case ( x, y ) of
                ( [], ys ) ->
                    ys

                ( xs, [] ) ->
                    xs

                ( ( k1, v1 ) :: xs, ( k2, v2 ) :: ys ) ->
                    case compareKey level k1 k2 of
                        LT ->
                            ( k1, v1 ) :: merge_ xs (( k2, v2 ) :: ys)

                        _ ->
                            ( k2, v2 ) :: merge_ (( k1, v1 ) :: xs) ys

        insert__ : ( Key comparable, b ) -> List ( Key comparable, b ) -> List ( Key comparable, b )
        insert__ ( k1, v1 ) x =
            case x of
                [] ->
                    [ ( k1, v1 ) ]

                ( k2, v2 ) :: xs ->
                    case compareKey level k1 k2 of
                        LT ->
                            ( k1, v1 ) :: x

                        _ ->
                            ( k2, v2 ) :: insert__ ( k1, v1 ) xs
    in
    case dict of
        Empty ->
            []

        Node nodeLevel smaller ( key_, v ) larger ->
            let
                sortedSmaller =
                    sortByAxis_ numberOfLevels level smaller

                sortedLarger =
                    sortByAxis_ numberOfLevels level larger
            in
            if modBy numberOfLevels nodeLevel == level then
                sortedSmaller ++ (( key_, v ) :: sortedLarger)

            else
                merge_ sortedSmaller sortedLarger
                    |> insert__ ( key_, v )

        Deleted _ smaller larger ->
            let
                sortedSmaller =
                    sortByAxis_ numberOfLevels level smaller

                sortedLarger =
                    sortByAxis_ numberOfLevels level larger
            in
            merge_ sortedSmaller sortedLarger


compareWithMatchKey : Int -> Key comparable -> Key (Match comparable) -> Order
compareWithMatchKey index k1 k2 =
    compareWithMatch (getIndex index k1) (getIndex index k2)


find : MatchKey comparable -> KDDict comparable v -> KDDict comparable v
find query dict =
    case dict of
        Empty ->
            Empty

        Node level smaller ( key_, v ) larger ->
            case compareWithMatchKey level key_ query of
                EQ ->
                    if mMatch query key_ then
                        Node level (find query smaller) ( key_, v ) (find query larger)

                    else
                        Deleted level (find query smaller) (find query larger)

                LT ->
                    Deleted level Empty (find query larger)

                GT ->
                    Deleted level (find query smaller) Empty

        Deleted level smaller larger ->
            Deleted level (find query smaller) (find query larger)


findMatching : MatchKey comparable -> KDDict comparable v -> List v
findMatching query dict =
    find query dict
        |> toList
        |> List.map Tuple.second


unsafeMap : (v -> a) -> KDDict comparable v -> KDDict comparable a
unsafeMap f dict =
    case dict of
        Empty ->
            Empty

        Node level smaller ( key_, v ) larger ->
            Node level (unsafeMap f smaller) ( key_, f v ) (unsafeMap f larger)

        Deleted level smaller larger ->
            Deleted level (unsafeMap f smaller) (unsafeMap f larger)


smallest : Int -> KDDict comparable v -> Maybe ( Key comparable, v )
smallest axis dict =
    case dict of
        Empty ->
            Nothing

        Node level smaller item larger ->
            if level == axis then
                smallest axis smaller

            else
                case ( smallest axis smaller, smallest axis larger ) of
                    ( Nothing, Nothing ) ->
                        Just item

                    ( Just ( aKey, aValue ), Just ( bKey, bValue ) ) ->
                        case compareKey axis aKey bKey of
                            LT ->
                                Just ( aKey, aValue )

                            _ ->
                                Just ( bKey, bValue )

                    ( Just a, Nothing ) ->
                        Just a

                    ( Nothing, Just b ) ->
                        Just b

        Deleted level smaller larger ->
            if level == axis then
                smallest axis smaller

            else
                case ( smallest axis smaller, smallest axis larger ) of
                    ( Nothing, Nothing ) ->
                        Nothing

                    ( Just ( aKey, aValue ), Just ( bKey, bValue ) ) ->
                        case compareKey axis aKey bKey of
                            LT ->
                                Just ( aKey, aValue )

                            _ ->
                                Just ( bKey, bValue )

                    ( Just a, Nothing ) ->
                        Just a

                    ( Nothing, Just b ) ->
                        Just b
