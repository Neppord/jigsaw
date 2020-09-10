module KD exposing
    ( Tree(..)
    , delete
    , insert
    , singleton
    , smallest
    )


type Tree k
    = Empty
    | Node (Tree k) k (Tree k)


delete : comparable -> Tree comparable -> Tree comparable
delete k tree =
    case tree of
        Empty ->
            tree

        Node smaller k1 larger ->
            case compare k k1 of
                EQ ->
                    case smallest larger of
                        Just a ->
                            Node smaller a (delete a larger)

                        Nothing ->
                            smaller

                LT ->
                    Node (delete k smaller) k1 larger

                GT ->
                    Node smaller k1 (delete k larger)


smallest : Tree k -> Maybe k
smallest tree =
    case tree of
        Empty ->
            Nothing

        Node Empty k _ ->
            Just k

        Node smaller _ _ ->
            smallest smaller


singleton : k -> Tree k
singleton k =
    Node Empty k Empty


insert : comparable -> Tree comparable -> Tree comparable
insert k tree =
    case tree of
        Empty ->
            singleton k

        Node smaller k1 larger ->
            if k < k1 then
                Node (insert k smaller) k1 larger

            else
                Node smaller k1 (insert k larger)



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
