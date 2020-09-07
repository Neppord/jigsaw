module KD exposing
    ( Tree(..)
    , insert
    )


type Tree k
    = Empty
    | Node (Tree k) k (Tree k)


insert : comparable -> Tree comparable -> Tree comparable
insert k tree =
    case tree of
        Empty ->
            Node Empty k Empty

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
