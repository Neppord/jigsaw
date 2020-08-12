module TestEdge exposing (suite)

import Edge exposing (Orientation(..))
import Expect exposing (all, equal)
import Fuzz exposing (intRange)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "indexOf"
        [ describe "There are no edges when there is only one piece"
            ([ North, East, South, West ]
                |> List.map
                    (\o ->
                        test
                            (Debug.toString o)
                            (\_ -> Edge.indexOf o 1 1 0 |> equal -1)
                    )
            )
        , test "Pieces that share a edge has the same index " <|
            all
                [ \ _ -> Edge.indexOf North 3 3 4 |> equal (Edge.indexOf South 3 3 1)
                , \ _ -> Edge.indexOf East 3 3 4 |> equal (Edge.indexOf West 3 3 5)
                , \ _ -> Edge.indexOf South 3 3 4 |> equal (Edge.indexOf North 3 3 7)
                , \ _ -> Edge.indexOf West 3 3 4 |> equal (Edge.indexOf East 3 3 3)
                ]
        ]
