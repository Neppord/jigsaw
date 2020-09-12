module KDDict exposing
    ( KDDict
    , Key(..)
    , MatchKey
    , addAxis
    , addCoordinateAxis
    , empty
    , find
    , findMatching
    , fromList
    , fromListBy
    , get
    , height
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


fromList_ : Int -> List ( Key comparable, v ) -> KDDict comparable v
fromList_ index list =
    case list of
        [] ->
            Empty

        ( key_, value ) :: rest ->
            let
                toCompare =
                    getIndex index key_

                ( larger, smaller ) =
                    List.partition
                        (\( otherKey, _ ) -> toCompare < getIndex index otherKey)
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


size : KDDict comparable v -> number
size dict =
    case dict of
        Empty ->
            0

        Node _ smaller _ larger ->
            1 + size smaller + size larger

        Deleted _ smaller larger ->
            size smaller + size larger


get : Key comparable -> KDDict comparable v -> Maybe v
get query dict =
    case dict of
        Empty ->
            Nothing

        Node index smaller ( key_, value ) larger ->
            if query == key_ then
                Just value

            else if getIndex index query < getIndex index key_ then
                get query smaller

            else
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
                case compare (getIndex i q) (getIndex i key_) of
                    LT ->
                        Node i (remove q s) ( key_, v ) l

                    GT ->
                        Node i (remove q s) ( key_, v ) (remove q l)

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
            if getIndex i key_ < getIndex i k then
                Node i (insert_ (i + 1) key_ value s) ( k, v ) l

            else
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
            case compare (getIndex i k1) (getIndex i k2) of
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


find : MatchKey comparable -> KDDict comparable v -> KDDict comparable v
find query dict =
    case dict of
        Empty ->
            Empty

        Node level smaller ( key_, v ) larger ->
            case compareWithMatch (getIndex level key_) (getIndex level query) of
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
