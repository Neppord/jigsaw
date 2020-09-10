module TestKD exposing (..)

import Expect exposing (all, equal)
import KD exposing (Tree(..), delete, insert, singleton, smallest)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Tree"
        [ test "singleton return a tree with only the value" <|
            \_ ->
                singleton 1
                    |> equal
                        (Node
                            { smaller = Empty
                            , key = 1
                            , larger = Empty
                            }
                        )
        , describe "insert"
            [ test "replaces empty trees with non empty when inserting" <|
                \_ ->
                    Empty
                        |> insert 1
                        |> equal (singleton 1)
            , test "insert smaller values to the left" <|
                \_ ->
                    singleton 2
                        |> insert 1
                        |> equal
                            (Node
                                { smaller = singleton 1
                                , key = 2
                                , larger = Empty
                                }
                            )
            , test "insert larger values to the right" <|
                \_ ->
                    singleton 2
                        |> insert 3
                        |> equal
                            (Node
                                { smaller = Empty
                                , key = 2
                                , larger = singleton 3
                                }
                            )
            , test "dont discard subtrees, insert into them" <|
                all
                    [ \_ ->
                        Node
                            { smaller = singleton 2
                            , key = 3
                            , larger = Empty
                            }
                            |> insert 1
                            |> equal
                                (Node
                                    { smaller =
                                        Node
                                            { smaller = singleton 1
                                            , key = 2
                                            , larger = Empty
                                            }
                                    , key = 3
                                    , larger = Empty
                                    }
                                )
                    , \_ ->
                        Node { smaller = Empty, key = 2, larger = singleton 3 }
                            |> insert 4
                            |> equal
                                (Node
                                    { key = 2
                                    , smaller = Empty
                                    , larger =
                                        Node
                                            { key = 3
                                            , smaller = Empty
                                            , larger = singleton 4
                                            }
                                    }
                                )
                    ]
            ]
        , describe "smallest"
            [ test "returns Nothing when the tree is empty" <|
                \_ ->
                    Empty
                        |> smallest
                        |> equal Nothing
            , test "if there is only one value it returns it" <|
                \_ ->
                    singleton 1
                        |> smallest
                        |> equal (Just 1)
            , test "it looks for smallest in the smaller branch" <|
                \_ ->
                    singleton 2
                        |> insert 1
                        |> smallest
                        |> equal (Just 1)
            ]
        , describe "delete"
            [ test "it cant delete something that is not there" <|
                \_ ->
                    Empty
                        |> delete 1
                        |> equal Empty
            , test "it does not remove items that dont match the given key" <|
                \_ ->
                    singleton 1
                        |> delete 2
                        |> equal (singleton 1)
            , test "it removes the value if its the only value" <|
                \_ ->
                    singleton 1
                        |> delete 1
                        |> equal Empty
            , test "it removes from smaller subtree if value is smaller" <|
                \_ ->
                    Node { smaller = singleton 1, key = 2, larger = Empty }
                        |> delete 1
                        |> equal (singleton 2)
            , test "it removes from larger subtree if value is larger" <|
                \_ ->
                    Node
                        { smaller = Empty
                        , key = 2
                        , larger = singleton 3
                        }
                        |> delete 3
                        |> equal (singleton 2)
            , test "it replaces root element with smaller from larger" <|
                \_ ->
                    Node
                        { smaller = Empty
                        , key = 2
                        , larger = singleton 3
                        }
                        |> delete 2
                        |> equal (singleton 3)
            , test "it replaces root element smaller branch if larger is empty" <|
                \_ ->
                    Node
                        { smaller = singleton 1
                        , key = 2
                        , larger = Empty
                        }
                        |> delete 2
                        |> equal (singleton 1)
            ]
        ]
