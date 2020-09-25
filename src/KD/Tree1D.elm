module KD.Tree1D exposing
    ( Tree1D(..)
    , delete
    , find
    , insert
    , match
    , singleton
    , smallest
    )

import KD.Match as Match exposing (Match)


type Tree1D k v
    = Empty
    | Node
        { smaller : Tree1D k v
        , key : k
        , value : v
        , larger : Tree1D k v
        }


delete : comparable -> Tree1D comparable v -> Tree1D comparable v
delete k tree =
    case tree of
        Empty ->
            tree

        Node { key, value, smaller, larger } ->
            case compare k key of
                EQ ->
                    case smallest larger of
                        Just ( k1, v1 ) ->
                            Node
                                { smaller = smaller
                                , key = k1
                                , value = v1
                                , larger = delete k1 larger
                                }

                        Nothing ->
                            smaller

                LT ->
                    Node
                        { smaller = delete k smaller
                        , key = key
                        , value = value
                        , larger = larger
                        }

                GT ->
                    Node
                        { smaller = smaller
                        , key = key
                        , larger = delete k larger
                        , value = value
                        }


smallest : Tree1D k v -> Maybe ( k, v )
smallest tree =
    case tree of
        Empty ->
            Nothing

        Node { smaller, key, value } ->
            case smaller of
                Empty ->
                    Just ( key, value )

                _ ->
                    smallest smaller


singleton : k -> v -> Tree1D k v
singleton k v =
    Node
        { smaller = Empty
        , key = k
        , value = v
        , larger = Empty
        }


insert : comparable -> v -> Tree1D comparable v -> Tree1D comparable v
insert k v tree =
    case tree of
        Empty ->
            singleton k v

        Node { smaller, key, larger, value } ->
            if k < key then
                Node
                    { smaller = insert k v smaller
                    , key = key
                    , larger = larger
                    , value = value
                    }

            else
                Node
                    { smaller = smaller
                    , key = key
                    , value = value
                    , larger = insert k v larger
                    }


find : comparable -> Tree1D comparable v -> Maybe ( comparable, v )
find k tree =
    case tree of
        Empty ->
            Nothing

        Node { smaller, key, value, larger } ->
            case compare k key of
                LT ->
                    find k smaller

                GT ->
                    find k larger

                EQ ->
                    Just ( key, value )


match : Match comparable -> Tree1D comparable v -> Tree1D comparable v
match toMatch tree =
    case tree of
        Empty ->
            Empty

        Node data ->
            case Match.compareWithMatch data.key toMatch of
                EQ ->
                    Node
                        { data
                            | larger = match toMatch data.larger
                            , smaller = match toMatch data.smaller
                        }

                LT ->
                    match toMatch data.larger

                GT ->
                    match toMatch data.smaller



{-
   create from list
   destruct in to list

   delete:
    * range
    * region

   find:
    * region (Pair k)

   chain queries by returning trees
-}
