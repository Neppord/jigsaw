module KDDict exposing (KDDict, Key(..), addAxis, empty, findAll, findAllInRange, fromList, fromListBy, get, insert, key, makeRangeQuery, merge, remove, removeAll, toList)


type KDDict comparable v
    = Empty
    | Node Int (KDDict comparable v) ( Key comparable, v ) (KDDict comparable v)


type Key a
    = Key a (List a)


type alias Query a =
    Key (Maybe a)


type alias RangeQuery a =
    Query ( a, a )


makeRangeQuery : Query a -> Query a -> RangeQuery a
makeRangeQuery (Key x xs) (Key y ys) =
    let
        combine x_ y_ =
            case ( x_, y_ ) of
                ( Nothing, Nothing ) ->
                    Nothing

                ( Nothing, Just y__ ) ->
                    Just ( y__, y__ )

                ( Just x__, Nothing ) ->
                    Just ( x__, x__ )

                ( Just x__, Just y__ ) ->
                    Just ( x__, y__ )
    in
    Key (combine x y) (List.map2 combine xs ys)


key : a -> Key a
key a =
    Key a []


addAxis : a -> Key a -> Key a
addAxis a (Key head tail) =
    Key a (head :: tail)


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


match : Query comparable -> Key comparable -> Bool
match (Key q qs) (Key k ks) =
    List.map2 Tuple.pair (q :: qs) (k :: ks)
        |> List.filter (Tuple.first >> (/=) Nothing)
        |> List.all (\( query, key_ ) -> query == Just key_)


findAll : Query comparable -> KDDict comparable v -> List v
findAll query dict =
    case dict of
        Empty ->
            []

        Node index smaller ( key_, value ) larger ->
            let
                rest =
                    case getIndex index query of
                        Nothing ->
                            findAll query smaller ++ findAll query larger

                        Just thing ->
                            case compare thing <| getIndex index key_ of
                                EQ ->
                                    findAll query smaller ++ findAll query larger

                                LT ->
                                    findAll query smaller

                                GT ->
                                    findAll query larger
            in
            if match query key_ then
                value :: rest

            else
                rest


matchRange : RangeQuery comparable -> Key comparable -> Bool
matchRange (Key q qs) (Key k ks) =
    let
        concat ma b =
            case ma of
                Nothing ->
                    b

                Just a ->
                    a :: b

        promote ( ma, b ) =
            case ma of
                Nothing ->
                    Nothing

                Just a ->
                    Just ( a, b )

        inRange ( ( s, l ), i ) =
            s <= i && i <= l
    in
    List.map2 Tuple.pair (q :: qs) (k :: ks)
        |> List.map promote
        |> List.foldl concat []
        |> List.all inRange


findAllInRange : RangeQuery comparable -> KDDict comparable v -> List v
findAllInRange query dict =
    case dict of
        Empty ->
            []

        Node index smaller ( key_, value ) larger ->
            let
                rest =
                    case getIndex index query of
                        Nothing ->
                            findAllInRange query smaller ++ findAllInRange query larger

                        Just ( s, l ) ->
                            let
                                toCompare =
                                    getIndex index key_
                            in
                            case ( toCompare < s, l < toCompare ) of
                                ( True, _ ) ->
                                    findAllInRange query larger

                                ( _, True ) ->
                                    findAllInRange query smaller

                                ( _, _ ) ->
                                    {- s < toCompare < l -}
                                    findAllInRange query smaller ++ findAllInRange query larger
            in
            if matchRange query key_ then
                value :: rest

            else
                rest


remove : Key comparable -> KDDict comparable v -> KDDict comparable v
remove query dict =
    dict
        |> toList
        |> List.filter (Tuple.first >> (/=) query)
        |> fromList


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
                    toList d1 ++ toList d2 |> fromList

                GT ->
                    toList d1 ++ toList d2 |> fromList
