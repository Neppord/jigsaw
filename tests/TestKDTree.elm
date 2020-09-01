module TestKDTree exposing (kdtree)

import Expect exposing (all, equal)
import KDTree exposing (KDTree)
import Test exposing (Test, describe, test)


kdtree =
    describe "KDTree"
        [ test "smallest" <|
            \_ ->
                [ 1, 2, 3 ]
                    |> KDTree.fromList_
                    |> KDTree.smallest
                    |> equal (Just 1)
        , test "toList" <|
            \_ ->
                [ 1, 2, 3 ]
                    |> KDTree.fromList_
                    |> KDTree.toList
                    |> equal [ 1, 2, 3 ]

        , test "remove" <|
            \_ ->
                [ 1, 2, 3 ]
                    |> KDTree.fromList_
                    |> KDTree.remove 2
                    |> KDTree.toList
                    |> equal [ 1, 3 ]
        ]
