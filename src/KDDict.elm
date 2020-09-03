module KDDict exposing (KDDict, Key(..), addAxis, empty, findAll, fromList, fromListBy, get, insert, key, merge, remove, toList)


type KDDict comparable v
    = Empty
    | Node Int (KDDict comparable v) ( Key comparable, v ) (KDDict comparable v)


type Key a
    = Key a (List a)


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


match : Key (Maybe comparable) -> Key comparable -> Bool
match (Key q qs) (Key k ks) =
    List.map2 Tuple.pair (q :: qs) (k :: ks)
        |> List.filter (Tuple.first >> (/=) Nothing)
        |> List.all (\( query, key_ ) -> query == Just key_)


findAll : Key (Maybe comparable) -> KDDict comparable v -> List v
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


remove : Key comparable -> KDDict comparable v -> KDDict comparable v
remove query dict =
    case dict of
        Empty ->
            Empty

        Node index smaller ( key_, value ) larger ->
            if query == key_ then
                (toList smaller ++ toList larger) |> fromList

            else
                case compare (getIndex index query) (getIndex index key_) of
                    EQ ->
                        (toList smaller ++ ( key_, value ) :: toList larger) |> fromList

                    LT ->
                        Node index (remove query smaller) ( key_, value ) larger

                    GT ->
                        Node index smaller ( key_, value ) (remove query larger)


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
