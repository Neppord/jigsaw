module TestKD exposing (..)

import Expect exposing (all, equal)
import KD exposing (Tree(..), insert)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Tree"
        [ describe "insert"
            [ test "replaces empty trees with non empty when inserting" <|
                \_ ->
                    Empty
                        |> insert 1
                        |> equal (Node Empty 1 Empty)
            , test "insert smaller values to the left" <|
                \_ ->
                    Node Empty 2 Empty
                        |> insert 1
                        |> equal (Node (Node Empty 1 Empty) 2 Empty)
            , test "insert larger values to the right" <|
                \_ ->
                    Node Empty 2 Empty
                        |> insert 3
                        |> equal (Node Empty 2 (Node Empty 3 Empty))
            , test "dont discard subtrees, insert into them" <|
                all
                    [ \_ ->
                        Node (Node Empty 2 Empty) 3 Empty
                            |> insert 1
                            |> equal (Node (Node (Node Empty 1 Empty) 2 Empty) 3 Empty)
                    , \_ ->
                        Node Empty 2 (Node Empty 3 Empty)
                            |> insert 4
                            |> equal (Node Empty 2 (Node Empty 3 (Node Empty 4 Empty)))
                    ]
            ]
        ]
