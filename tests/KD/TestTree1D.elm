module KD.TestTree1D exposing (..)

import Expect exposing (all, equal)
import KD.Match exposing (Match(..))
import KD.Tree1D exposing (Tree1D(..), delete, find, insert, match, singleton, smallest)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Tree1D"
        [ test "singleton return a tree with only the value" <|
            \_ ->
                singleton 1 ()
                    |> equal
                        (Node
                            { smaller = Empty
                            , key = 1
                            , larger = Empty
                            , value = ()
                            }
                        )
        , describe "insert"
            [ test "replaces empty trees with non empty when inserting" <|
                \_ ->
                    Empty
                        |> insert 1 ()
                        |> equal (singleton 1 ())
            , test "insert smaller values to the left" <|
                \_ ->
                    singleton 2 ()
                        |> insert 1 ()
                        |> equal
                            (Node
                                { smaller = singleton 1 ()
                                , key = 2
                                , larger = Empty
                                , value = ()
                                }
                            )
            , test "insert larger values to the right" <|
                \_ ->
                    singleton 2 ()
                        |> insert 3 ()
                        |> equal
                            (Node
                                { key = 2
                                , value = ()
                                , smaller = Empty
                                , larger = singleton 3 ()
                                }
                            )
            , test "dont discard subtrees, insert into them" <|
                all
                    [ \_ ->
                        Node
                            { smaller = singleton 2 ()
                            , key = 3
                            , larger = Empty
                            , value = ()
                            }
                            |> insert 1 ()
                            |> equal
                                (Node
                                    { smaller =
                                        Node
                                            { smaller = singleton 1 ()
                                            , key = 2
                                            , larger = Empty
                                            , value = ()
                                            }
                                    , key = 3
                                    , larger = Empty
                                    , value = ()
                                    }
                                )
                    , \_ ->
                        Node
                            { smaller = Empty
                            , key = 2
                            , larger = singleton 3 ()
                            , value = ()
                            }
                            |> insert 4 ()
                            |> equal
                                (Node
                                    { key = 2
                                    , value = ()
                                    , smaller = Empty
                                    , larger =
                                        Node
                                            { key = 3
                                            , value = ()
                                            , smaller = Empty
                                            , larger = singleton 4 ()
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
                    singleton 1 ()
                        |> smallest
                        |> equal (Just ( 1, () ))
            , test "it looks for smallest in the smaller branch" <|
                \_ ->
                    singleton 2 ()
                        |> insert 1 ()
                        |> smallest
                        |> equal (Just ( 1, () ))
            ]
        , describe "delete"
            [ test "it cant delete something that is not there" <|
                \_ ->
                    Empty
                        |> delete 1
                        |> equal Empty
            , test "it does not remove items that dont match the given key" <|
                \_ ->
                    singleton 1 ()
                        |> delete 2
                        |> equal (singleton 1 ())
            , test "it removes the value if its the only value" <|
                \_ ->
                    singleton 1 ()
                        |> delete 1
                        |> equal Empty
            , test "it removes from smaller subtree if value is smaller" <|
                \_ ->
                    Node
                        { key = 2
                        , value = ()
                        , smaller = singleton 1 ()
                        , larger = Empty
                        }
                        |> delete 1
                        |> equal (singleton 2 ())
            , test "it removes from larger subtree if value is larger" <|
                \_ ->
                    Node
                        { key = 2
                        , value = ()
                        , smaller = Empty
                        , larger = singleton 3 ()
                        }
                        |> delete 3
                        |> equal (singleton 2 ())
            , test "it replaces root element with smaller from larger" <|
                \_ ->
                    Node
                        { smaller = Empty
                        , key = 2
                        , value = ()
                        , larger = singleton 3 ()
                        }
                        |> delete 2
                        |> equal (singleton 3 ())
            , test "it replaces root element smaller branch if larger is empty" <|
                \_ ->
                    Node
                        { key = 2
                        , value = ()
                        , smaller = singleton 1 ()
                        , larger = Empty
                        }
                        |> delete 2
                        |> equal (singleton 1 ())
            ]
        , describe "find"
            [ test "it always return Nothing for empty trees" <|
                \_ ->
                    find 1 Empty
                        |> equal Nothing
            , test "it finds key and value that are in the root node" <|
                \_ ->
                    singleton 1 ()
                        |> find 1
                        |> equal (Just ( 1, () ))
            , test "it finds key and values for keys that are smaller then the root" <|
                \_ ->
                    singleton 2 ()
                        |> insert 1 ()
                        |> find 1
                        |> equal (Just ( 1, () ))
            , test "it finds key and values for keys that are larger then the root" <|
                \_ ->
                    singleton 1 ()
                        |> insert 2 ()
                        |> find 2
                        |> equal (Just ( 2, () ))
            ]
        , describe "match"
            [ test "it finds nothing in a empty tree" <|
                \_ ->
                    Empty
                        |> match Anything
                        |> equal Empty
            , test "it finds everything in the tree if matching anything" <|
                \_ ->
                    singleton 1 ()
                        |> match Anything
                        |> equal (singleton 1 ())
            , test "it finds only things in range" <|
                \_ ->
                    let
                        tree =
                            singleton 1 ()
                                |> insert 2 ()
                    in
                    tree
                        |> insert 3 ()
                        |> match (WithinRange 1 2)
                        |> equal tree
            , test "it dont return values that dont match" <|
                all
                    [ \_ ->
                        singleton 1 ()
                            |> match (EqualTo 2)
                            |> equal Empty
                    , \_ ->
                        singleton 1 ()
                            |> insert 2 ()
                            |> match (EqualTo 2)
                            |> equal (singleton 2 ())
                    , \_ ->
                        singleton 2 ()
                            |> insert 1 ()
                            |> match (EqualTo 1)
                            |> equal (singleton 1 ())
                    ]
            ]
        ]
