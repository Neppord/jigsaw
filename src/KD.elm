module KD exposing
    ( Tree(..)
    , delete
    , insert
    , singleton
    , smallest
    )


type Tree k
    = Empty
    | Node
        { smaller : Tree k
        , key : k
        , larger : Tree k
        }


delete : comparable -> Tree comparable -> Tree comparable
delete k tree =
    case tree of
        Empty ->
            tree

        Node { key, smaller, larger } ->
            case compare k key of
                EQ ->
                    case smallest larger of
                        Just a ->
                            Node
                                { smaller = smaller
                                , key = a
                                , larger = delete a larger
                                }

                        Nothing ->
                            smaller

                LT ->
                    Node
                        { smaller = delete k smaller
                        , key = key
                        , larger = larger
                        }

                GT ->
                    Node
                        { smaller = smaller
                        , key = key
                        , larger = delete k larger
                        }


smallest : Tree k -> Maybe k
smallest tree =
    case tree of
        Empty ->
            Nothing

        Node { smaller, key } ->
            case smaller of
                Empty ->
                    Just key

                _ ->
                    smallest smaller


singleton : k -> Tree k
singleton k =
    Node { smaller = Empty, key = k, larger = Empty }


insert : comparable -> Tree comparable -> Tree comparable
insert k tree =
    case tree of
        Empty ->
            singleton k

        Node { smaller, key, larger } ->
            if k < key then
                Node
                    { smaller = insert k smaller
                    , key = key
                    , larger = larger
                    }

            else
                Node
                    { smaller = smaller
                    , key = key
                    , larger = insert k larger
                    }



{-
   insert key

   create from list
   destruct in to list

   find smallest

   delete:
    * key
    * range
    * region

   find:
    * key
    * range
    * region (Pair k)

   chain queries by returning trees
-}
